/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2018-2019. All Rights Reserved.
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
 * 'n<something>'.
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
#include <time.h>

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

#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif

/* SENDFILE STUFF HERE IF WE NEED IT... */

#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#define __DARWIN__ 1
#endif


#ifdef __WIN32__
#define STRNCASECMP               strncasecmp
#define INCL_WINSOCK_API_TYPEDEFS 1

#ifndef WINDOWS_H_INCLUDES_WINSOCK2_H
#include <winsock2.h>
#endif
#include <windows.h>
#include <Ws2tcpip.h>   /* NEED VC 6.0 or higher */

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




#else /* ifdef __WIN32__ */

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

/* Socket stuff */
#define INVALID_SOCKET -1
// #define INVALID_EVENT  -1
#define SOCKET_ERROR   -1

#endif /* ifdef __WIN32__ */

#include <erl_nif.h>

#include "socket_dbg.h"
#include "socket_tarray.h"
#include "socket_int.h"
#include "socket_util.h"


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

/* All platforms fail on malloc errors. */
#define FATAL_MALLOC


/* Debug stuff... */
#define SOCKET_GLOBAL_DEBUG_DEFAULT FALSE
#define SOCKET_DEBUG_DEFAULT        FALSE

/* Counters and stuff (Don't know where to sent this stuff anyway) */
#define SOCKET_NIF_IOW_DEFAULT FALSE



/* Socket stuff */
#define INVALID_EVENT  -1

#define SOCKET int
#define HANDLE long int


/* ==============================================================================
 * The IS_SOCKET_ERROR macro below is used for portability reasons.
 * While POSIX specifies that errors from socket-related system calls
 * should be indicated with a -1 return value, some users have experienced
 * non-Windows OS kernels that return negative values other than -1.
 * While one can argue that such kernels are technically broken, comparing
 * against values less than 0 covers their out-of-spec return values without
 * imposing incorrect semantics on systems that manage to correctly return -1
 * for errors, thus increasing Erlang's portability.
 */
#ifdef __WIN32__
#define IS_SOCKET_ERROR(val) ((val) == SOCKET_ERROR)
#else
#define IS_SOCKET_ERROR(val) ((val) < 0)
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
#define SOCKET_OPT_TCP_CONGESTION_NAME_MAX TCP_CA_NAME_MAX
#else
/* This is really excessive, but just in case... */
#define SOCKET_OPT_TCP_CONGESTION_NAME_MAX 256
#endif


#if defined(TCP_CONGESTION) || defined(SO_BINDTODEVICE)
#define USE_GETOPT_STR_OPT
#define USE_SETOPT_STR_OPT
#endif



/* *** Socket state defs *** */

#define SOCKET_FLAG_OPEN         0x0001
#define SOCKET_FLAG_ACTIVE       0x0004
#define SOCKET_FLAG_LISTEN       0x0008
#define SOCKET_FLAG_CON          0x0010
#define SOCKET_FLAG_ACC          0x0020
#define SOCKET_FLAG_BUSY         0x0040
#define SOCKET_FLAG_CLOSE        0x0080

#define SOCKET_STATE_CLOSED          (0)
#define SOCKET_STATE_OPEN            (SOCKET_FLAG_OPEN)
#define SOCKET_STATE_CONNECTED       (SOCKET_STATE_OPEN      | SOCKET_FLAG_ACTIVE)
#define SOCKET_STATE_LISTENING       (SOCKET_STATE_OPEN      | SOCKET_FLAG_LISTEN)
#define SOCKET_STATE_CONNECTING      (SOCKET_STATE_OPEN      | SOCKET_FLAG_CON)
#define SOCKET_STATE_ACCEPTING       (SOCKET_STATE_LISTENING | SOCKET_FLAG_ACC)
#define SOCKET_STATE_CLOSING         (SOCKET_FLAG_CLOSE)
#define SOCKET_STATE_DTOR            (0xFFFF)

#define IS_CLOSED(d)                            \
    ((d)->state == SOCKET_STATE_CLOSED)

/*
#define IS_STATE(d, f) \
    (((d)->state & (f)) == (f))
*/

#define IS_CLOSING(d)                                                   \
    (((d)->state & SOCKET_STATE_CLOSING) == SOCKET_STATE_CLOSING)

#define IS_OPEN(d)                                              \
    (((d)->state & SOCKET_FLAG_OPEN) == SOCKET_FLAG_OPEN)

#define IS_CONNECTED(d)                                                 \
    (((d)->state & SOCKET_STATE_CONNECTED) == SOCKET_STATE_CONNECTED)

#define IS_CONNECTING(d)                                \
    (((d)->state & SOCKET_FLAG_CON) == SOCKET_FLAG_CON)

/*
#define IS_BUSY(d)                                      \
    (((d)->state & SOCKET_FLAG_BUSY) == SOCKET_FLAG_BUSY)
*/

#define SOCKET_SEND_FLAG_CONFIRM    0
#define SOCKET_SEND_FLAG_DONTROUTE  1
#define SOCKET_SEND_FLAG_EOR        2
#define SOCKET_SEND_FLAG_MORE       3
#define SOCKET_SEND_FLAG_NOSIGNAL   4
#define SOCKET_SEND_FLAG_OOB        5
#define SOCKET_SEND_FLAG_LOW        SOCKET_SEND_FLAG_CONFIRM
#define SOCKET_SEND_FLAG_HIGH       SOCKET_SEND_FLAG_OOB

#define SOCKET_RECV_FLAG_CMSG_CLOEXEC 0
#define SOCKET_RECV_FLAG_ERRQUEUE     1
#define SOCKET_RECV_FLAG_OOB          2
#define SOCKET_RECV_FLAG_PEEK         3
#define SOCKET_RECV_FLAG_TRUNC        4
#define SOCKET_RECV_FLAG_LOW          SOCKET_RECV_FLAG_CMSG_CLOEXEC
#define SOCKET_RECV_FLAG_HIGH         SOCKET_RECV_FLAG_TRUNC

#define SOCKET_RECV_BUFFER_SIZE_DEFAULT      8192
#define SOCKET_RECV_CTRL_BUFFER_SIZE_DEFAULT 1024
#define SOCKET_SEND_CTRL_BUFFER_SIZE_DEFAULT 1024

#define VT2S(__VT__) (((__VT__) == SOCKET_OPT_VALUE_TYPE_UNSPEC) ? "unspec" : \
                      (((__VT__) == SOCKET_OPT_VALUE_TYPE_INT) ? "int" : \
                       ((__VT__) == SOCKET_OPT_VALUE_TYPE_BOOL) ? "bool" : \
                       "undef"))

#define SOCKET_OPT_VALUE_TYPE_UNSPEC 0
#define SOCKET_OPT_VALUE_TYPE_INT    1
#define SOCKET_OPT_VALUE_TYPE_BOOL   2

#define ESOCK_DESC_PATTERN_CREATED 0x03030303
#define ESOCK_DESC_PATTERN_DTOR    0xC0C0C0C0

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
        /* Room for more... */
    } flags;
    unsigned int field; // Make it easy to reset all flags...
} SocketState;

/*
#define IS_OPEN(d)       ((d)->state.flags.open)
#define IS_CONNECTED(d)  ((d)->state.flags.connect == SOCKET_STATE_CONNECTED)
#define IS_CONNECTING(d) ((d)->state.flags.connect == SOCKET_STATE_CONNECTING)
*/


/*----------------------------------------------------------------------------
 * Interface constants.
 *
 * This section must be "identical" to the corresponding socket.hrl
 */

/* domain */
#define SOCKET_DOMAIN_LOCAL       1
#define SOCKET_DOMAIN_INET        2
#define SOCKET_DOMAIN_INET6       3

/* type */
#define SOCKET_TYPE_STREAM        1
#define SOCKET_TYPE_DGRAM         2
#define SOCKET_TYPE_RAW           3
// #define SOCKET_TYPE_RDM           4
#define SOCKET_TYPE_SEQPACKET     5

/* protocol */
#define SOCKET_PROTOCOL_DEFAULT   0
#define SOCKET_PROTOCOL_IP        1
#define SOCKET_PROTOCOL_TCP       2
#define SOCKET_PROTOCOL_UDP       3
#define SOCKET_PROTOCOL_SCTP      4
#define SOCKET_PROTOCOL_ICMP      5
#define SOCKET_PROTOCOL_IGMP      6

/* shutdown how */
#define SOCKET_SHUTDOWN_HOW_RD    0
#define SOCKET_SHUTDOWN_HOW_WR    1
#define SOCKET_SHUTDOWN_HOW_RDWR  2


#define SOCKET_OPT_LEVEL_OTP        0
#define SOCKET_OPT_LEVEL_SOCKET     1
#define SOCKET_OPT_LEVEL_IP         2
#define SOCKET_OPT_LEVEL_IPV6       3
#define SOCKET_OPT_LEVEL_TCP        4
#define SOCKET_OPT_LEVEL_UDP        5
#define SOCKET_OPT_LEVEL_SCTP       6

#define SOCKET_OPT_OTP_DEBUG        1
#define SOCKET_OPT_OTP_IOW          2
#define SOCKET_OPT_OTP_CTRL_PROC    3
#define SOCKET_OPT_OTP_RCVBUF       4
#define SOCKET_OPT_OTP_RCVCTRLBUF   6
#define SOCKET_OPT_OTP_SNDCTRLBUF   7
#define SOCKET_OPT_OTP_FD           8
#define SOCKET_OPT_OTP_DOMAIN       0xFF01 // INTERNAL AND ONLY GET
#define SOCKET_OPT_OTP_TYPE         0xFF02 // INTERNAL AND ONLY GET
#define SOCKET_OPT_OTP_PROTOCOL     0xFF03 // INTERNAL AND ONLY GET

#define SOCKET_OPT_SOCK_ACCEPTCONN     1
#define SOCKET_OPT_SOCK_BINDTODEVICE   3
#define SOCKET_OPT_SOCK_BROADCAST      4
#define SOCKET_OPT_SOCK_DEBUG          6
#define SOCKET_OPT_SOCK_DOMAIN         7
#define SOCKET_OPT_SOCK_DONTROUTE      8
#define SOCKET_OPT_SOCK_KEEPALIVE     10
#define SOCKET_OPT_SOCK_LINGER        11
#define SOCKET_OPT_SOCK_OOBINLINE     13
#define SOCKET_OPT_SOCK_PEEK_OFF      15
#define SOCKET_OPT_SOCK_PRIORITY      17
#define SOCKET_OPT_SOCK_PROTOCOL      18
#define SOCKET_OPT_SOCK_RCVBUF        19
#define SOCKET_OPT_SOCK_RCVLOWAT      21
#define SOCKET_OPT_SOCK_RCVTIMEO      22
#define SOCKET_OPT_SOCK_REUSEADDR     23
#define SOCKET_OPT_SOCK_REUSEPORT     24
#define SOCKET_OPT_SOCK_SNDBUF        27
#define SOCKET_OPT_SOCK_SNDLOWAT      29
#define SOCKET_OPT_SOCK_SNDTIMEO      30
#define SOCKET_OPT_SOCK_TIMESTAMP     31
#define SOCKET_OPT_SOCK_TYPE          32

#define SOCKET_OPT_IP_ADD_MEMBERSHIP          1
#define SOCKET_OPT_IP_ADD_SOURCE_MEMBERSHIP   2
#define SOCKET_OPT_IP_BLOCK_SOURCE            3
#define SOCKET_OPT_IP_DROP_MEMBERSHIP         5
#define SOCKET_OPT_IP_DROP_SOURCE_MEMBERSHIP  6
#define SOCKET_OPT_IP_FREEBIND                7
#define SOCKET_OPT_IP_HDRINCL                 8
#define SOCKET_OPT_IP_MINTTL                  9
#define SOCKET_OPT_IP_MSFILTER               10
#define SOCKET_OPT_IP_MTU                    11
#define SOCKET_OPT_IP_MTU_DISCOVER           12
#define SOCKET_OPT_IP_MULTICAST_ALL          13
#define SOCKET_OPT_IP_MULTICAST_IF           14
#define SOCKET_OPT_IP_MULTICAST_LOOP         15
#define SOCKET_OPT_IP_MULTICAST_TTL          16
#define SOCKET_OPT_IP_NODEFRAG               17
#define SOCKET_OPT_IP_PKTINFO                19
#define SOCKET_OPT_IP_RECVDSTADDR            20
#define SOCKET_OPT_IP_RECVERR                21
#define SOCKET_OPT_IP_RECVIF                 22
#define SOCKET_OPT_IP_RECVOPTS               23
#define SOCKET_OPT_IP_RECVORIGDSTADDR        24
#define SOCKET_OPT_IP_RECVTOS                25
#define SOCKET_OPT_IP_RECVTTL                26
#define SOCKET_OPT_IP_RETOPTS                27
#define SOCKET_OPT_IP_ROUTER_ALERT           28
#define SOCKET_OPT_IP_SENDSRCADDR            29 // Same as IP_RECVDSTADDR?
#define SOCKET_OPT_IP_TOS                    30
#define SOCKET_OPT_IP_TRANSPARENT            31
#define SOCKET_OPT_IP_TTL                    32
#define SOCKET_OPT_IP_UNBLOCK_SOURCE         33

#define SOCKET_OPT_IPV6_ADDRFORM              1
#define SOCKET_OPT_IPV6_ADD_MEMBERSHIP        2
#define SOCKET_OPT_IPV6_AUTHHDR               3
#define SOCKET_OPT_IPV6_DROP_MEMBERSHIP       6
#define SOCKET_OPT_IPV6_DSTOPTS               7
#define SOCKET_OPT_IPV6_FLOWINFO             11
#define SOCKET_OPT_IPV6_HOPLIMIT             12
#define SOCKET_OPT_IPV6_HOPOPTS              13
#define SOCKET_OPT_IPV6_MTU                  17
#define SOCKET_OPT_IPV6_MTU_DISCOVER         18
#define SOCKET_OPT_IPV6_MULTICAST_HOPS       19
#define SOCKET_OPT_IPV6_MULTICAST_IF         20
#define SOCKET_OPT_IPV6_MULTICAST_LOOP       21
#define SOCKET_OPT_IPV6_RECVERR              24
#define SOCKET_OPT_IPV6_RECVPKTINFO          25 // PKTINFO on FreeBSD
#define SOCKET_OPT_IPV6_ROUTER_ALERT         27
#define SOCKET_OPT_IPV6_RTHDR                28
#define SOCKET_OPT_IPV6_UNICAST_HOPS         30
#define SOCKET_OPT_IPV6_V6ONLY               32

#define SOCKET_OPT_TCP_CONGESTION   1
#define SOCKET_OPT_TCP_CORK         2
#define SOCKET_OPT_TCP_MAXSEG       7
#define SOCKET_OPT_TCP_NODELAY      9

#define SOCKET_OPT_UDP_CORK         1

#define SOCKET_OPT_SCTP_ASSOCINFO           2
#define SOCKET_OPT_SCTP_AUTOCLOSE           8
#define SOCKET_OPT_SCTP_DISABLE_FRAGMENTS  12
#define SOCKET_OPT_SCTP_EVENTS             14
#define SOCKET_OPT_SCTP_INITMSG            18
#define SOCKET_OPT_SCTP_MAXSEG             21
#define SOCKET_OPT_SCTP_NODELAY            23
#define SOCKET_OPT_SCTP_RTOINFO            29

/* We should *eventually* use this instead of hard-coding the size (to 1) */
#define ESOCK_RECVMSG_IOVEC_SZ 1

#define SOCKET_CMD_DEBUG        0x0001

#define SOCKET_SUPPORTS_OPTIONS 0x0001
#define SOCKET_SUPPORTS_SCTP    0x0002
#define SOCKET_SUPPORTS_IPV6    0x0003
#define SOCKET_SUPPORTS_LOCAL   0x0004

#define ESOCK_WHICH_PROTO_ERROR -1
#define ESOCK_WHICH_PROTO_UNSUP -2



/* =================================================================== *
 *                                                                     *
 *                        Various esockmacros                          *
 *                                                                     *
 * =================================================================== */

/* Global socket debug */
#define SGDBG( proto )         ESOCK_DBG_PRINTF( data.dbg , proto )
/* Socket specific debug */
#define SSDBG( __D__ , proto ) ESOCK_DBG_PRINTF( (__D__)->dbg , proto )

#define SOCK_CNT_INC( __E__, __D__, SF, ACNT, CNT, INC)  \
    {                                                    \
        if (cnt_inc(CNT, INC) && (__D__)->iow) {         \
            esock_send_wrap_msg(__E__, __D__, SF, ACNT); \
        }                                                \
    }


/* =================================================================== *
 *                                                                     *
 *                    Basic socket operations                          *
 *                                                                     *
 * =================================================================== */

#ifdef __WIN32__

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


#else /* !__WIN32__ */


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

#endif /* !__WIN32__ */

#ifdef HAVE_SOCKLEN_T
#  define SOCKLEN_T socklen_t
#else
#  define SOCKLEN_T size_t
#endif

#ifdef __WIN32__
#define SOCKOPTLEN_T int
#else
#define SOCKOPTLEN_T SOCKLEN_T
#endif

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

typedef struct esock_request_queue_element {
    struct esock_request_queue_element* nextP;
    ESockRequestor                      data;
} ESockRequestQueueElement;

typedef struct {
    ESockRequestQueueElement* first;
    ESockRequestQueueElement* last;
} ESockRequestQueue;


typedef struct {
    /* 
     * +++ This is a way to, possibly, detect memory overrides "and stuff" +++
     *
     * We have two patterns. One is set when the descriptor is created (allocated)
     * and one is set when the descriptor is dtor'ed.
     */
    Uint32             pattern;

    /* +++ The actual socket +++ */
    SOCKET             sock;
    HANDLE             event;

    /* +++ Stuff "about" the socket +++ */
    int                domain;
    int                type;
    int                protocol;

    unsigned int       state;
    ESockAddress       remote;
    unsigned int       addrLen;

    /* +++ Controller (owner) process +++ */
    ErlNifPid          ctrlPid;
    ESockMonitor       ctrlMon;

    /* +++ Connector process +++ */
    ErlNifPid          connPid;
    ESockMonitor       connMon;

    /* +++ Write stuff +++ */
    ErlNifMutex*       writeMtx;
    ESockRequestor     currentWriter;
    ESockRequestor*    currentWriterP; // NULL or points to currentWriter
    ESockRequestQueue  writersQ;
    BOOLEAN_T          isWritable;
    Uint32             writePkgCnt;
    Uint32             writeByteCnt;
    Uint32             writeTries;
    Uint32             writeWaits;
    Uint32             writeFails;

    /* +++ Read stuff +++ */
    ErlNifMutex*       readMtx;
    ESockRequestor     currentReader;
    ESockRequestor*    currentReaderP; // NULL or points to currentReader
    ESockRequestQueue  readersQ;
    BOOLEAN_T          isReadable;
    ErlNifBinary       rbuffer;      // DO WE NEED THIS
    Uint32             readCapacity; // DO WE NEED THIS
    Uint32             readPkgCnt;
    Uint32             readByteCnt;
    Uint32             readTries;
    Uint32             readWaits;
    Uint32             readFails;

    /* +++ Accept stuff +++ */
    ErlNifMutex*       accMtx;
    ESockRequestor     currentAcceptor;
    ESockRequestor*    currentAcceptorP; // NULL or points to currentAcceptor
    ESockRequestQueue  acceptorsQ;

    /* +++ Config & Misc stuff +++ */
    ErlNifMutex*       cfgMtx;
    size_t       rBufSz;  // Read buffer size (when data length = 0)
    /* rNum and rNumCnt are used (together with rBufSz) when calling the recv 
     * function with the Length argument set to 0 (zero).
     * If rNum is 0 (zero), then rNumCnt is not used and only *one* read will
     * be done. Also, when get'ing the value of the option (rcvbuf) with 
     * getopt, the value will be reported as an integer. If the rNum has a 
     * value greater then 0 (zero), then it will instead be reported as {N, BufSz}.
     */
    unsigned int rNum;    // recv: Number of reads using rBufSz
    unsigned int rNumCnt; // recv: Current number of reads (so far)
    size_t       rCtrlSz; // Read control buffer size
    size_t       wCtrlSz; // Write control buffer size
    BOOLEAN_T    iow;     // Inform On (counter) Wrap
    BOOLEAN_T    dbg;

    /* +++ Close stuff +++ */
    ErlNifMutex*  closeMtx;
    ErlNifPid     closerPid;
    ESockMonitor  closerMon;
    ErlNifEnv*    closeEnv;
    ERL_NIF_TERM  closeRef;
    BOOLEAN_T     closeLocal;

} ESockDescriptor;


/* Global stuff.
 */
typedef struct {
    /* These are for debugging, testing and the like */
    // ERL_NIF_TERM version;
    // ERL_NIF_TERM buildDate;
    BOOLEAN_T    dbg;

    BOOLEAN_T    iow; // Where do we send this? Subscription?
    ErlNifMutex* cntMtx;
    Uint32       numSockets;
    Uint32       numTypeStreams;
    Uint32       numTypeDGrams;
    Uint32       numTypeSeqPkgs;
    Uint32       numDomainInet;
    Uint32       numDomainInet6;
    Uint32       numDomainLocal;
    Uint32       numProtoIP;
    Uint32       numProtoTCP;
    Uint32       numProtoUDP;
    Uint32       numProtoSCTP;
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
 * nif_recv
 * nif_recvfrom
 * nif_recvmsg
 * nif_close
 * nif_shutdown
 * nif_setopt
 * nif_getopt
 * nif_sockname
 * nif_peername
 * nif_finalize_connection
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
    ESOCK_NIF_FUNC_DEF(finalize_connection);        \
    ESOCK_NIF_FUNC_DEF(finalize_close);             \
    ESOCK_NIF_FUNC_DEF(cancel);

#define ESOCK_NIF_FUNC_DEF(F)                              \
    static ERL_NIF_TERM nif_##F(ErlNifEnv*         env,    \
                                int                argc,   \
                                const ERL_NIF_TERM argv[]);
ESOCK_NIF_FUNCS
#undef ESOCK_NIF_FUNC_DEF


#if !defined(__WIN32__)

/* And here comes the functions that does the actual work (for the most part) */

static BOOLEAN_T ecommand2command(ErlNifEnv*    env,
                                  ERL_NIF_TERM  ecommand,
                                  Uint16*       command,
                                  ERL_NIF_TERM* edata);
static ERL_NIF_TERM ncommand(ErlNifEnv*   env,
                             Uint16       cmd,
                             ERL_NIF_TERM ecdata);
static ERL_NIF_TERM ncommand_debug(ErlNifEnv* env, ERL_NIF_TERM ecdata);

static ERL_NIF_TERM esock_global_info(ErlNifEnv* env);
static ERL_NIF_TERM esock_socket_info(ErlNifEnv*       env,
                                      ESockDescriptor* descP);
static ERL_NIF_TERM esock_socket_info_counters(ErlNifEnv*       env,
                                               ESockDescriptor* descP);
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
                                     ErlNifMutex*       mtx,
                                     ESockRequestor*    crp,
                                     ESockRequestQueue* q);

static ERL_NIF_TERM nsupports(ErlNifEnv* env, int key);
static ERL_NIF_TERM nsupports_options(ErlNifEnv* env);
static ERL_NIF_TERM nsupports_options_socket(ErlNifEnv* env);
static ERL_NIF_TERM nsupports_options_ip(ErlNifEnv* env);
static ERL_NIF_TERM nsupports_options_ipv6(ErlNifEnv* env);
static ERL_NIF_TERM nsupports_options_tcp(ErlNifEnv* env);
static ERL_NIF_TERM nsupports_options_udp(ErlNifEnv* env);
static ERL_NIF_TERM nsupports_options_sctp(ErlNifEnv* env);
static ERL_NIF_TERM nsupports_sctp(ErlNifEnv* env);
static ERL_NIF_TERM nsupports_ipv6(ErlNifEnv* env);
static ERL_NIF_TERM nsupports_local(ErlNifEnv* env);

static ERL_NIF_TERM nopen(ErlNifEnv* env,
                          int        domain,
                          int        type,
                          int        protocol,
                          char*      netns);
static BOOLEAN_T nopen_which_protocol(SOCKET sock, int* proto);

static ERL_NIF_TERM nbind(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ESockAddress*    sockAddrP,
                          unsigned int     addrLen);
static ERL_NIF_TERM nconnect(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM nlisten(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            int              backlog);
static ERL_NIF_TERM naccept_erts(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     ref);
static ERL_NIF_TERM naccept_listening(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     ref);
static ERL_NIF_TERM naccept_listening_error(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     sockRef,
                                            ERL_NIF_TERM     accRef,
                                            ErlNifPid        caller,
                                            int              save_errno);
static ERL_NIF_TERM naccept_listening_accept(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             SOCKET           accSock,
                                             ErlNifPid        caller,
                                             ESockAddress*    remote);
static ERL_NIF_TERM naccept_accepting(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     ref);
static ERL_NIF_TERM naccept_accepting_current(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     sockRef,
                                              ERL_NIF_TERM     ref);
static ERL_NIF_TERM naccept_accepting_current_accept(ErlNifEnv*       env,
                                                     ESockDescriptor* descP,
                                                     ERL_NIF_TERM     sockRef,
                                                     SOCKET           accSock,
                                                     ESockAddress*    remote);
static ERL_NIF_TERM naccept_accepting_current_error(ErlNifEnv*       env,
                                                    ESockDescriptor* descP,
                                                    ERL_NIF_TERM     sockRef,
                                                    ERL_NIF_TERM     opRef,
                                                    int              save_errno);
static ERL_NIF_TERM naccept_accepting_other(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     ref,
                                            ErlNifPid        caller);
static ERL_NIF_TERM naccept_busy_retry(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef,
                                       ERL_NIF_TERM     accRef,
                                       ErlNifPid*       pid,
                                       unsigned int     nextState);
static BOOLEAN_T naccept_accepted(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  SOCKET           accSock,
                                  ErlNifPid        pid,
                                  ESockAddress*    remote,
                                  ERL_NIF_TERM*    result);
static ERL_NIF_TERM nsend(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     sendRef,
                          ErlNifBinary*    dataP,
                          int              flags);
static ERL_NIF_TERM nsendto(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     sockRef,
                            ERL_NIF_TERM     sendRef,
                            ErlNifBinary*    dataP,
                            int              flags,
                            ESockAddress*    toAddrP,
                            unsigned int     toAddrLen);
static ERL_NIF_TERM nsendmsg_erts(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     sendRef,
                                  ERL_NIF_TERM     eMsgHdr,
                                  int              flags);
static ERL_NIF_TERM nrecv(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sendRef,
                          ERL_NIF_TERM     recvRef,
                          int              len,
                          int              flags);
static ERL_NIF_TERM nrecvfrom_erts(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM     sockRef,
                                   ERL_NIF_TERM     recvRef,
                                   Uint16           bufSz,
                                   int              flags);
static ERL_NIF_TERM nrecvmsg_erts(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     recvRef,
                                  Uint16           bufLen,
                                  Uint16           ctrlLen,
                                  int              flags);
static ERL_NIF_TERM nclose(ErlNifEnv*       env,
                           ESockDescriptor* descP);
static BOOLEAN_T nclose_check(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ERL_NIF_TERM*    reason);
static ERL_NIF_TERM nclose_do(ErlNifEnv*       env,
                              ESockDescriptor* descP);
static ERL_NIF_TERM nshutdown(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              int              how);
static ERL_NIF_TERM nsetopt(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            BOOLEAN_T        isEncoded,
                            BOOLEAN_T        isOTP,
                            int              level,
                            int              eOpt,
                            ERL_NIF_TERM     eVal);

/* Set OTP level options */
static ERL_NIF_TERM nsetopt_otp(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                int              eOpt,
                                ERL_NIF_TERM     eVal);
/* *** nsetopt_otp_debug      ***
 * *** nsetopt_otp_iow        ***
 * *** nsetopt_otp_ctrl_proc  ***
 * *** nsetopt_otp_rcvbuf     ***
 * *** nsetopt_otp_rcvctrlbuf ***
 * *** nsetopt_otp_sndctrlbuf ***
 */
#define NSETOPT_OTP_FUNCS              \
    NSETOPT_OTP_FUNC_DEF(debug);      \
    NSETOPT_OTP_FUNC_DEF(iow);        \
    NSETOPT_OTP_FUNC_DEF(ctrl_proc);  \
    NSETOPT_OTP_FUNC_DEF(rcvbuf);     \
    NSETOPT_OTP_FUNC_DEF(rcvctrlbuf); \
    NSETOPT_OTP_FUNC_DEF(sndctrlbuf);
#define NSETOPT_OTP_FUNC_DEF(F)                                 \
    static ERL_NIF_TERM nsetopt_otp_##F(ErlNifEnv*       env,   \
                                        ESockDescriptor* descP, \
                                        ERL_NIF_TERM     eVal)
NSETOPT_OTP_FUNCS
#undef NSETOPT_OTP_FUNC_DEF

/* Set native options */
static ERL_NIF_TERM nsetopt_native(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              level,
                                   int              eOpt,
                                   ERL_NIF_TERM     eVal);
static ERL_NIF_TERM nsetopt_level(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  int              level,
                                  int              eOpt,
                                  ERL_NIF_TERM     eVal);
static ERL_NIF_TERM nsetopt_lvl_socket(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       int              eOpt,
                                       ERL_NIF_TERM     eVal);


/* *** Handling set of socket options for level = socket *** */

#if defined(SO_BINDTODEVICE)
static ERL_NIF_TERM nsetopt_lvl_sock_bindtodevice(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     eVal);
#endif
#if defined(SO_BROADCAST)
static ERL_NIF_TERM nsetopt_lvl_sock_broadcast(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     eVal);
#endif
#if defined(SO_DEBUG)
static ERL_NIF_TERM nsetopt_lvl_sock_debug(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal);
#endif
#if defined(SO_DONTROUTE)
static ERL_NIF_TERM nsetopt_lvl_sock_dontroute(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     eVal);
#endif
#if defined(SO_KEEPALIVE)
static ERL_NIF_TERM nsetopt_lvl_sock_keepalive(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     eVal);
#endif
#if defined(SO_LINGER)
static ERL_NIF_TERM nsetopt_lvl_sock_linger(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     eVal);
#endif
#if defined(SO_OOBINLINE)
static ERL_NIF_TERM nsetopt_lvl_sock_oobinline(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     eVal);
#endif
#if defined(SO_PEEK_OFF)
static ERL_NIF_TERM nsetopt_lvl_sock_peek_off(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     eVal);
#endif
#if defined(SO_PRIORITY)
static ERL_NIF_TERM nsetopt_lvl_sock_priority(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     eVal);
#endif
#if defined(SO_RCVBUF)
static ERL_NIF_TERM nsetopt_lvl_sock_rcvbuf(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     eVal);
#endif
#if defined(SO_RCVLOWAT)
static ERL_NIF_TERM nsetopt_lvl_sock_rcvlowat(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     eVal);
#endif
#if defined(SO_RCVTIMEO)
static ERL_NIF_TERM nsetopt_lvl_sock_rcvtimeo(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     eVal);
#endif
#if defined(SO_REUSEADDR)
static ERL_NIF_TERM nsetopt_lvl_sock_reuseaddr(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     eVal);
#endif
#if defined(SO_REUSEPORT)
static ERL_NIF_TERM nsetopt_lvl_sock_reuseport(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     eVal);
#endif
#if defined(SO_SNDBUF)
static ERL_NIF_TERM nsetopt_lvl_sock_sndbuf(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     eVal);
#endif
#if defined(SO_SNDLOWAT)
static ERL_NIF_TERM nsetopt_lvl_sock_sndlowat(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     eVal);
#endif
#if defined(SO_SNDTIMEO)
static ERL_NIF_TERM nsetopt_lvl_sock_sndtimeo(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     eVal);
#endif
#if defined(SO_TIMESTAMP)
static ERL_NIF_TERM nsetopt_lvl_sock_timestamp(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     eVal);
#endif
static ERL_NIF_TERM nsetopt_lvl_ip(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              eOpt,
                                   ERL_NIF_TERM     eVal);

/* *** Handling set of socket options for level = ip *** */
#if defined(IP_ADD_MEMBERSHIP)
static ERL_NIF_TERM nsetopt_lvl_ip_add_membership(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     eVal);
#endif
#if defined(IP_ADD_SOURCE_MEMBERSHIP)
static ERL_NIF_TERM nsetopt_lvl_ip_add_source_membership(ErlNifEnv*       env,
                                                         ESockDescriptor* descP,
                                                         ERL_NIF_TERM     eVal);
#endif
#if defined(IP_BLOCK_SOURCE)
static ERL_NIF_TERM nsetopt_lvl_ip_block_source(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                ERL_NIF_TERM     eVal);
#endif
#if defined(IP_DROP_MEMBERSHIP)
static ERL_NIF_TERM nsetopt_lvl_ip_drop_membership(ErlNifEnv*       env,
                                                   ESockDescriptor* descP,
                                                   ERL_NIF_TERM     eVal);
#endif
#if defined(IP_DROP_SOURCE_MEMBERSHIP)
static ERL_NIF_TERM nsetopt_lvl_ip_drop_source_membership(ErlNifEnv*       env,
                                                          ESockDescriptor* descP,
                                                          ERL_NIF_TERM     eVal);
#endif
#if defined(IP_FREEBIND)
static ERL_NIF_TERM nsetopt_lvl_ip_freebind(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     eVal);
#endif
#if defined(IP_HDRINCL)
static ERL_NIF_TERM nsetopt_lvl_ip_hdrincl(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal);
#endif
#if defined(IP_MINTTL)
static ERL_NIF_TERM nsetopt_lvl_ip_minttl(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ERL_NIF_TERM     eVal);
#endif
#if defined(IP_MSFILTER) && defined(IP_MSFILTER_SIZE)
static ERL_NIF_TERM nsetopt_lvl_ip_msfilter(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     eVal);
static BOOLEAN_T decode_ip_msfilter_mode(ErlNifEnv*   env,
                                         ERL_NIF_TERM eVal,
                                         Uint32*      mode);
static ERL_NIF_TERM nsetopt_lvl_ip_msfilter_set(ErlNifEnv*          env,
                                                SOCKET              sock,
                                                struct ip_msfilter* msfP,
                                                SOCKLEN_T           optLen);
#endif
#if defined(IP_MTU_DISCOVER)
static ERL_NIF_TERM nsetopt_lvl_ip_mtu_discover(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                ERL_NIF_TERM     eVal);
#endif
#if defined(IP_MULTICAST_ALL)
static ERL_NIF_TERM nsetopt_lvl_ip_multicast_all(ErlNifEnv*       env,
                                                 ESockDescriptor* descP,
                                                 ERL_NIF_TERM     eVal);
#endif
#if defined(IP_MULTICAST_IF)
static ERL_NIF_TERM nsetopt_lvl_ip_multicast_if(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                ERL_NIF_TERM     eVal);
#endif
#if defined(IP_MULTICAST_LOOP)
static ERL_NIF_TERM nsetopt_lvl_ip_multicast_loop(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     eVal);
#endif
#if defined(IP_MULTICAST_TTL)
static ERL_NIF_TERM nsetopt_lvl_ip_multicast_ttl(ErlNifEnv*       env,
                                                 ESockDescriptor* descP,
                                                 ERL_NIF_TERM     eVal);
#endif
#if defined(IP_NODEFRAG)
static ERL_NIF_TERM nsetopt_lvl_ip_nodefrag(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     eVal);
#endif
#if defined(IP_PKTINFO)
static ERL_NIF_TERM nsetopt_lvl_ip_pktinfo(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal);
#endif
#if defined(IP_RECVDSTADDR)
static ERL_NIF_TERM nsetopt_lvl_ip_recvdstaddr(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     eVal);
#endif
#if defined(IP_RECVERR)
static ERL_NIF_TERM nsetopt_lvl_ip_recverr(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal);
#endif
#if defined(IP_RECVIF)
static ERL_NIF_TERM nsetopt_lvl_ip_recvif(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ERL_NIF_TERM     eVal);
#endif
#if defined(IP_RECVOPTS)
static ERL_NIF_TERM nsetopt_lvl_ip_recvopts(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     eVal);
#endif
#if defined(IP_RECVORIGDSTADDR)
static ERL_NIF_TERM nsetopt_lvl_ip_recvorigdstaddr(ErlNifEnv*       env,
                                                   ESockDescriptor* descP,
                                                   ERL_NIF_TERM     eVal);
#endif
#if defined(IP_RECVTOS)
static ERL_NIF_TERM nsetopt_lvl_ip_recvtos(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal);
#endif
#if defined(IP_RECVTTL)
static ERL_NIF_TERM nsetopt_lvl_ip_recvttl(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal);
#endif
#if defined(IP_RETOPTS)
static ERL_NIF_TERM nsetopt_lvl_ip_retopts(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal);
#endif
#if defined(IP_ROUTER_ALERT)
static ERL_NIF_TERM nsetopt_lvl_ip_router_alert(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                ERL_NIF_TERM     eVal);
#endif
#if defined(IP_SENDSRCADDR)
static ERL_NIF_TERM nsetopt_lvl_ip_sendsrcaddr(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     eVal);
#endif
#if defined(IP_TOS)
static ERL_NIF_TERM nsetopt_lvl_ip_tos(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     eVal);
#endif
#if defined(IP_TRANSPARENT)
static ERL_NIF_TERM nsetopt_lvl_ip_transparent(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     eVal);
#endif
#if defined(IP_TTL)
static ERL_NIF_TERM nsetopt_lvl_ip_ttl(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     eVal);
#endif
#if defined(IP_UNBLOCK_SOURCE)
static ERL_NIF_TERM nsetopt_lvl_ip_unblock_source(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     eVal);
#endif

#if defined(IP_DROP_MEMBERSHIP) || defined(IP_ADD_MEMBERSHIP)
static
ERL_NIF_TERM nsetopt_lvl_ip_update_membership(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     eVal,
                                              int              opt);
#endif
#if defined(IP_ADD_SOURCE_MEMBERSHIP) || defined(IP_DROP_SOURCE_MEMBERSHIP) || defined(IP_BLOCK_SOURCE) || defined(IP_UNBLOCK_SOURCE)
static
ERL_NIF_TERM nsetopt_lvl_ip_update_source(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ERL_NIF_TERM     eVal,
                                          int              opt);
#endif


/* *** Handling set of socket options for level = ipv6 *** */
#if defined(HAVE_IPV6)
static ERL_NIF_TERM nsetopt_lvl_ipv6(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     int              eOpt,
                                     ERL_NIF_TERM     eVal);
#if defined(IPV6_ADDRFORM)
static ERL_NIF_TERM nsetopt_lvl_ipv6_addrform(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_ADD_MEMBERSHIP)
static ERL_NIF_TERM nsetopt_lvl_ipv6_add_membership(ErlNifEnv*       env,
                                                    ESockDescriptor* descP,
                                                    ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_AUTHHDR)
static ERL_NIF_TERM nsetopt_lvl_ipv6_authhdr(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_DROP_MEMBERSHIP)
static ERL_NIF_TERM nsetopt_lvl_ipv6_drop_membership(ErlNifEnv*       env,
                                                     ESockDescriptor* descP,
                                                     ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_DSTOPTS)
static ERL_NIF_TERM nsetopt_lvl_ipv6_dstopts(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_FLOWINFO)
static ERL_NIF_TERM nsetopt_lvl_ipv6_flowinfo(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_HOPLIMIT)
static ERL_NIF_TERM nsetopt_lvl_ipv6_hoplimit(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_HOPOPTS)
static ERL_NIF_TERM nsetopt_lvl_ipv6_hopopts(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_MTU)
static ERL_NIF_TERM nsetopt_lvl_ipv6_mtu(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_MTU_DISCOVER)
static ERL_NIF_TERM nsetopt_lvl_ipv6_mtu_discover(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_MULTICAST_HOPS)
static ERL_NIF_TERM nsetopt_lvl_ipv6_multicast_hops(ErlNifEnv*       env,
                                                    ESockDescriptor* descP,
                                                    ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_MULTICAST_IF)
static ERL_NIF_TERM nsetopt_lvl_ipv6_multicast_if(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_MULTICAST_LOOP)
static ERL_NIF_TERM nsetopt_lvl_ipv6_multicast_loop(ErlNifEnv*       env,
                                                    ESockDescriptor* descP,
                                                    ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_RECVERR)
static ERL_NIF_TERM nsetopt_lvl_ipv6_recverr(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_RECVPKTINFO) || defined(IPV6_PKTINFO)
static ERL_NIF_TERM nsetopt_lvl_ipv6_recvpktinfo(ErlNifEnv*       env,
                                                 ESockDescriptor* descP,
                                                 ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_ROUTER_ALERT)
static ERL_NIF_TERM nsetopt_lvl_ipv6_router_alert(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_RTHDR)
static ERL_NIF_TERM nsetopt_lvl_ipv6_rthdr(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_UNICAST_HOPS)
static ERL_NIF_TERM nsetopt_lvl_ipv6_unicast_hops(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     eVal);
#endif
#if defined(IPV6_V6ONLY)
static ERL_NIF_TERM nsetopt_lvl_ipv6_v6only(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     eVal);
#endif

#if defined(IPV6_ADD_MEMBERSHIP) || defined(IPV6_DROP_MEMBERSHIP)
static ERL_NIF_TERM nsetopt_lvl_ipv6_update_membership(ErlNifEnv*       env,
                                                       ESockDescriptor* descP,
                                                       ERL_NIF_TERM     eVal,
                                                       int              opt);
#endif

#endif // defined(HAVE_IPV6)
static ERL_NIF_TERM nsetopt_lvl_tcp(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              eOpt,
                                    ERL_NIF_TERM     eVal);
#if defined(TCP_CONGESTION)
static ERL_NIF_TERM nsetopt_lvl_tcp_congestion(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     eVal);
#endif
#if defined(TCP_MAXSEG)
static ERL_NIF_TERM nsetopt_lvl_tcp_maxseg(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal);
#endif
#if defined(TCP_NODELAY)
static ERL_NIF_TERM nsetopt_lvl_tcp_nodelay(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     eVal);
#endif
static ERL_NIF_TERM nsetopt_lvl_udp(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              eOpt,
                                    ERL_NIF_TERM     eVal);
#if defined(UDP_CORK)
static ERL_NIF_TERM nsetopt_lvl_udp_cork(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     eVal);
#endif
#if defined(HAVE_SCTP)
static ERL_NIF_TERM nsetopt_lvl_sctp(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     int              eOpt,
                                     ERL_NIF_TERM     eVal);
#if defined(SCTP_ASSOCINFO)
static ERL_NIF_TERM nsetopt_lvl_sctp_associnfo(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     eVal);
#endif
#if defined(SCTP_AUTOCLOSE)
static ERL_NIF_TERM nsetopt_lvl_sctp_autoclose(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     eVal);
#endif
#if defined(SCTP_DISABLE_FRAGMENTS)
static ERL_NIF_TERM nsetopt_lvl_sctp_disable_fragments(ErlNifEnv*       env,
                                                       ESockDescriptor* descP,
                                                       ERL_NIF_TERM     eVal);
#endif
#if defined(SCTP_EVENTS)
static ERL_NIF_TERM nsetopt_lvl_sctp_events(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     eVal);
#endif
#if defined(SCTP_INITMSG)
static ERL_NIF_TERM nsetopt_lvl_sctp_initmsg(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     eVal);
#endif
#if defined(SCTP_MAXSEG)
static ERL_NIF_TERM nsetopt_lvl_sctp_maxseg(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     eVal);
#endif
#if defined(SCTP_NODELAY)
static ERL_NIF_TERM nsetopt_lvl_sctp_nodelay(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     eVal);
#endif
#if defined(SCTP_RTOINFO)
static ERL_NIF_TERM nsetopt_lvl_sctp_rtoinfo(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     eVal);
#endif
#endif // defined(HAVE_SCTP)

static ERL_NIF_TERM ngetopt(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            BOOLEAN_T        isEncoded,
                            BOOLEAN_T        isOTP,
                            int              level,
                            ERL_NIF_TERM     eOpt);

static ERL_NIF_TERM ngetopt_otp(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                int              eOpt);
/* *** ngetopt_otp_debug      ***
 * *** ngetopt_otp_iow        ***
 * *** ngetopt_otp_ctrl_proc  ***
 * *** ngetopt_otp_rcvbuf     ***
 * *** ngetopt_otp_rcvctrlbuf ***
 * *** ngetopt_otp_sndctrlbuf ***
 * *** ngetopt_otp_fd         ***
 * *** ngetopt_otp_domain     ***
 * *** ngetopt_otp_type       ***
 * *** ngetopt_otp_protocol   ***
 */
#define NGETOPT_OTP_FUNCS              \
    NGETOPT_OTP_FUNC_DEF(debug);      \
    NGETOPT_OTP_FUNC_DEF(iow);        \
    NGETOPT_OTP_FUNC_DEF(ctrl_proc);  \
    NGETOPT_OTP_FUNC_DEF(rcvbuf);     \
    NGETOPT_OTP_FUNC_DEF(rcvctrlbuf); \
    NGETOPT_OTP_FUNC_DEF(sndctrlbuf); \
    NGETOPT_OTP_FUNC_DEF(fd);         \
    NGETOPT_OTP_FUNC_DEF(domain);     \
    NGETOPT_OTP_FUNC_DEF(type);       \
    NGETOPT_OTP_FUNC_DEF(protocol);
#define NGETOPT_OTP_FUNC_DEF(F)                               \
    static ERL_NIF_TERM ngetopt_otp_##F(ErlNifEnv*        env, \
                                        ESockDescriptor* descP)
NGETOPT_OTP_FUNCS
#undef NGETOPT_OTP_FUNC_DEF

static ERL_NIF_TERM ngetopt_native(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              level,
                                   ERL_NIF_TERM     eOpt);
static ERL_NIF_TERM ngetopt_native_unspec(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          int              level,
                                          int              opt,
                                          SOCKOPTLEN_T     valueSz);
static ERL_NIF_TERM ngetopt_level(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  int              level,
                                  int              eOpt);
static ERL_NIF_TERM ngetopt_lvl_socket(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       int              eOpt);
#if defined(SO_ACCEPTCONN)
static ERL_NIF_TERM ngetopt_lvl_sock_acceptconn(ErlNifEnv*       env,
                                                ESockDescriptor* descP);
#endif
#if defined(SO_BINDTODEVICE)
static ERL_NIF_TERM ngetopt_lvl_sock_bindtodevice(ErlNifEnv*       env,
                                                  ESockDescriptor* descP);
#endif
#if defined(SO_BROADCAST)
static ERL_NIF_TERM ngetopt_lvl_sock_broadcast(ErlNifEnv*       env,
                                               ESockDescriptor* descP);
#endif
#if defined(SO_DEBUG)
static ERL_NIF_TERM ngetopt_lvl_sock_debug(ErlNifEnv*       env,
                                           ESockDescriptor* descP);
#endif
#if defined(SO_DOMAIN)
static ERL_NIF_TERM ngetopt_lvl_sock_domain(ErlNifEnv*       env,
                                            ESockDescriptor* descP);
#endif
#if defined(SO_DONTROUTE)
static ERL_NIF_TERM ngetopt_lvl_sock_dontroute(ErlNifEnv*       env,
                                               ESockDescriptor* descP);
#endif
#if defined(SO_KEEPALIVE)
static ERL_NIF_TERM ngetopt_lvl_sock_keepalive(ErlNifEnv*       env,
                                               ESockDescriptor* descP);
#endif
#if defined(SO_LINGER)
static ERL_NIF_TERM ngetopt_lvl_sock_linger(ErlNifEnv*       env,
                                            ESockDescriptor* descP);
#endif
#if defined(SO_OOBINLINE)
static ERL_NIF_TERM ngetopt_lvl_sock_oobinline(ErlNifEnv*       env,
                                               ESockDescriptor* descP);
#endif
#if defined(SO_PEEK_OFF)
static ERL_NIF_TERM ngetopt_lvl_sock_peek_off(ErlNifEnv*       env,
                                              ESockDescriptor* descP);
#endif
#if defined(SO_PRIORITY)
static ERL_NIF_TERM ngetopt_lvl_sock_priority(ErlNifEnv*       env,
                                              ESockDescriptor* descP);
#endif
#if defined(SO_PROTOCOL)
static ERL_NIF_TERM ngetopt_lvl_sock_protocol(ErlNifEnv*       env,
                                              ESockDescriptor* descP);
#endif
#if defined(SO_RCVBUF)
static ERL_NIF_TERM ngetopt_lvl_sock_rcvbuf(ErlNifEnv*       env,
                                            ESockDescriptor* descP);
#endif
#if defined(SO_RCVLOWAT)
static ERL_NIF_TERM ngetopt_lvl_sock_rcvlowat(ErlNifEnv*       env,
                                              ESockDescriptor* descP);
#endif
#if defined(SO_RCVTIMEO)
static ERL_NIF_TERM ngetopt_lvl_sock_rcvtimeo(ErlNifEnv*       env,
                                              ESockDescriptor* descP);
#endif
#if defined(SO_REUSEADDR)
static ERL_NIF_TERM ngetopt_lvl_sock_reuseaddr(ErlNifEnv*       env,
                                               ESockDescriptor* descP);
#endif
#if defined(SO_REUSEPORT)
static ERL_NIF_TERM ngetopt_lvl_sock_reuseport(ErlNifEnv*       env,
                                               ESockDescriptor* descP);
#endif
#if defined(SO_SNDBUF)
static ERL_NIF_TERM ngetopt_lvl_sock_sndbuf(ErlNifEnv*       env,
                                            ESockDescriptor* descP);
#endif
#if defined(SO_SNDLOWAT)
static ERL_NIF_TERM ngetopt_lvl_sock_sndlowat(ErlNifEnv*       env,
                                              ESockDescriptor* descP);
#endif
#if defined(SO_SNDTIMEO)
static ERL_NIF_TERM ngetopt_lvl_sock_sndtimeo(ErlNifEnv*       env,
                                              ESockDescriptor* descP);
#endif
#if defined(SO_TIMESTAMP)
static ERL_NIF_TERM ngetopt_lvl_sock_timestamp(ErlNifEnv*       env,
                                               ESockDescriptor* descP);
#endif
#if defined(SO_TYPE)
static ERL_NIF_TERM ngetopt_lvl_sock_type(ErlNifEnv*       env,
                                          ESockDescriptor* descP);
#endif
static ERL_NIF_TERM ngetopt_lvl_ip(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              eOpt);
#if defined(IP_FREEBIND)
static ERL_NIF_TERM ngetopt_lvl_ip_freebind(ErlNifEnv*       env,
                                            ESockDescriptor* descP);
#endif
#if defined(IP_HDRINCL)
static ERL_NIF_TERM ngetopt_lvl_ip_hdrincl(ErlNifEnv*       env,
                                           ESockDescriptor* descP);
#endif
#if defined(IP_MINTTL)
static ERL_NIF_TERM ngetopt_lvl_ip_minttl(ErlNifEnv*       env,
                                          ESockDescriptor* descP);
#endif
#if defined(IP_MTU)
static ERL_NIF_TERM ngetopt_lvl_ip_mtu(ErlNifEnv*       env,
                                       ESockDescriptor* descP);
#endif
#if defined(IP_MTU_DISCOVER)
static ERL_NIF_TERM ngetopt_lvl_ip_mtu_discover(ErlNifEnv*       env,
                                                ESockDescriptor* descP);
#endif
#if defined(IP_MULTICAST_ALL)
static ERL_NIF_TERM ngetopt_lvl_ip_multicast_all(ErlNifEnv*       env,
                                                 ESockDescriptor* descP);
#endif
#if defined(IP_MULTICAST_IF)
static ERL_NIF_TERM ngetopt_lvl_ip_multicast_if(ErlNifEnv*       env,
                                                ESockDescriptor* descP);
#endif
#if defined(IP_MULTICAST_LOOP)
static ERL_NIF_TERM ngetopt_lvl_ip_multicast_loop(ErlNifEnv*       env,
                                                  ESockDescriptor* descP);
#endif
#if defined(IP_MULTICAST_TTL)
static ERL_NIF_TERM ngetopt_lvl_ip_multicast_ttl(ErlNifEnv*       env,
                                                 ESockDescriptor* descP);
#endif
#if defined(IP_NODEFRAG)
static ERL_NIF_TERM ngetopt_lvl_ip_nodefrag(ErlNifEnv*       env,
                                            ESockDescriptor* descP);
#endif
#if defined(IP_PKTINFO)
static ERL_NIF_TERM ngetopt_lvl_ip_pktinfo(ErlNifEnv*       env,
                                           ESockDescriptor* descP);
#endif
#if defined(IP_RECVDSTADDR)
static ERL_NIF_TERM ngetopt_lvl_ip_recvdstaddr(ErlNifEnv*       env,
                                               ESockDescriptor* descP);
#endif
#if defined(IP_RECVERR)
static ERL_NIF_TERM ngetopt_lvl_ip_recverr(ErlNifEnv*       env,
                                           ESockDescriptor* descP);
#endif
#if defined(IP_RECVIF)
static ERL_NIF_TERM ngetopt_lvl_ip_recvif(ErlNifEnv*       env,
                                          ESockDescriptor* descP);
#endif
#if defined(IP_RECVOPTS)
static ERL_NIF_TERM ngetopt_lvl_ip_recvopts(ErlNifEnv*       env,
                                            ESockDescriptor* descP);
#endif
#if defined(IP_RECVORIGDSTADDR)
static ERL_NIF_TERM ngetopt_lvl_ip_recvorigdstaddr(ErlNifEnv*       env,
                                                   ESockDescriptor* descP);
#endif
#if defined(IP_RECVTOS)
static ERL_NIF_TERM ngetopt_lvl_ip_recvtos(ErlNifEnv*       env,
                                           ESockDescriptor* descP);
#endif
#if defined(IP_RECVTTL)
static ERL_NIF_TERM ngetopt_lvl_ip_recvttl(ErlNifEnv*       env,
                                           ESockDescriptor* descP);
#endif
#if defined(IP_RETOPTS)
static ERL_NIF_TERM ngetopt_lvl_ip_retopts(ErlNifEnv*       env,
                                           ESockDescriptor* descP);
#endif
#if defined(IP_ROUTER_ALERT)
static ERL_NIF_TERM ngetopt_lvl_ip_router_alert(ErlNifEnv*       env,
                                                ESockDescriptor* descP);
#endif
#if defined(IP_SENDSRCADDR)
static ERL_NIF_TERM ngetopt_lvl_ip_sendsrcaddr(ErlNifEnv*       env,
                                               ESockDescriptor* descP);
#endif
#if defined(IP_TOS)
static ERL_NIF_TERM ngetopt_lvl_ip_tos(ErlNifEnv*       env,
                                       ESockDescriptor* descP);
#endif
#if defined(IP_TRANSPARENT)
static ERL_NIF_TERM ngetopt_lvl_ip_transparent(ErlNifEnv*       env,
                                               ESockDescriptor* descP);
#endif
#if defined(IP_TTL)
static ERL_NIF_TERM ngetopt_lvl_ip_ttl(ErlNifEnv*       env,
                                       ESockDescriptor* descP);
#endif
#if defined(HAVE_IPV6)
static ERL_NIF_TERM ngetopt_lvl_ipv6(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     int              eOpt);
#if defined(IPV6_AUTHHDR)
static ERL_NIF_TERM ngetopt_lvl_ipv6_authhdr(ErlNifEnv*       env,
                                             ESockDescriptor* descP);
#endif
#if defined(IPV6_DSTOPTS)
static ERL_NIF_TERM ngetopt_lvl_ipv6_dstopts(ErlNifEnv*       env,
                                             ESockDescriptor* descP);
#endif
#if defined(IPV6_FLOWINFO)
static ERL_NIF_TERM ngetopt_lvl_ipv6_flowinfo(ErlNifEnv*       env,
                                              ESockDescriptor* descP);
#endif
#if defined(IPV6_HOPLIMIT)
static ERL_NIF_TERM ngetopt_lvl_ipv6_hoplimit(ErlNifEnv*       env,
                                              ESockDescriptor* descP);
#endif
#if defined(IPV6_HOPOPTS)
static ERL_NIF_TERM ngetopt_lvl_ipv6_hopopts(ErlNifEnv*       env,
                                             ESockDescriptor* descP);
#endif
#if defined(IPV6_MTU)
static ERL_NIF_TERM ngetopt_lvl_ipv6_mtu(ErlNifEnv*       env,
                                         ESockDescriptor* descP);
#endif
#if defined(IPV6_MTU_DISCOVER)
static ERL_NIF_TERM ngetopt_lvl_ipv6_mtu_discover(ErlNifEnv*       env,
                                                  ESockDescriptor* descP);
#endif
#if defined(IPV6_MULTICAST_HOPS)
static ERL_NIF_TERM ngetopt_lvl_ipv6_multicast_hops(ErlNifEnv*       env,
                                                    ESockDescriptor* descP);
#endif
#if defined(IPV6_MULTICAST_IF)
static ERL_NIF_TERM ngetopt_lvl_ipv6_multicast_if(ErlNifEnv*       env,
                                                  ESockDescriptor* descP);
#endif
#if defined(IPV6_MULTICAST_LOOP)
static ERL_NIF_TERM ngetopt_lvl_ipv6_multicast_loop(ErlNifEnv*       env,
                                                    ESockDescriptor* descP);
#endif
#if defined(IPV6_RECVERR)
static ERL_NIF_TERM ngetopt_lvl_ipv6_recverr(ErlNifEnv*       env,
                                             ESockDescriptor* descP);
#endif
#if defined(IPV6_RECVPKTINFO) || defined(IPV6_PKTINFO)
static ERL_NIF_TERM ngetopt_lvl_ipv6_recvpktinfo(ErlNifEnv*       env,
                                                 ESockDescriptor* descP);
#endif
#if defined(IPV6_ROUTER_ALERT)
static ERL_NIF_TERM ngetopt_lvl_ipv6_router_alert(ErlNifEnv*       env,
                                                  ESockDescriptor* descP);
#endif
#if defined(IPV6_RTHDR)
static ERL_NIF_TERM ngetopt_lvl_ipv6_rthdr(ErlNifEnv*       env,
                                           ESockDescriptor* descP);
#endif
#if defined(IPV6_UNICAST_HOPS)
static ERL_NIF_TERM ngetopt_lvl_ipv6_unicast_hops(ErlNifEnv*       env,
                                                  ESockDescriptor* descP);
#endif
#if defined(IPV6_V6ONLY)
static ERL_NIF_TERM ngetopt_lvl_ipv6_v6only(ErlNifEnv*       env,
                                            ESockDescriptor* descP);
#endif

#endif // defined(HAVE_IPV6)

static ERL_NIF_TERM ngetopt_lvl_tcp(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              eOpt);
#if defined(TCP_CONGESTION)
static ERL_NIF_TERM ngetopt_lvl_tcp_congestion(ErlNifEnv*       env,
                                               ESockDescriptor* descP);
#endif
#if defined(TCP_MAXSEG)
static ERL_NIF_TERM ngetopt_lvl_tcp_maxseg(ErlNifEnv*       env,
                                           ESockDescriptor* descP);
#endif
#if defined(TCP_NODELAY)
static ERL_NIF_TERM ngetopt_lvl_tcp_nodelay(ErlNifEnv*       env,
                                            ESockDescriptor* descP);
#endif
static ERL_NIF_TERM ngetopt_lvl_udp(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              eOpt);
#if defined(UDP_CORK)
static ERL_NIF_TERM ngetopt_lvl_udp_cork(ErlNifEnv*       env,
                                         ESockDescriptor* descP);
#endif
#if defined(HAVE_SCTP)
static ERL_NIF_TERM ngetopt_lvl_sctp(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     int              eOpt);
#if defined(SCTP_ASSOCINFO)
static ERL_NIF_TERM ngetopt_lvl_sctp_associnfo(ErlNifEnv*       env,
                                               ESockDescriptor* descP);
#endif
#if defined(SCTP_AUTOCLOSE)
static ERL_NIF_TERM ngetopt_lvl_sctp_autoclose(ErlNifEnv*       env,
                                               ESockDescriptor* descP);
#endif
#if defined(SCTP_DISABLE_FRAGMENTS)
static ERL_NIF_TERM ngetopt_lvl_sctp_disable_fragments(ErlNifEnv*       env,
                                                       ESockDescriptor* descP);
#endif
#if defined(SCTP_MAXSEG)
static ERL_NIF_TERM ngetopt_lvl_sctp_maxseg(ErlNifEnv*       env,
                                            ESockDescriptor* descP);
#endif
#if defined(SCTP_INITMSG)
static ERL_NIF_TERM ngetopt_lvl_sctp_initmsg(ErlNifEnv*       env,
                                             ESockDescriptor* descP);
#endif
#if defined(SCTP_NODELAY)
static ERL_NIF_TERM ngetopt_lvl_sctp_nodelay(ErlNifEnv*       env,
                                             ESockDescriptor* descP);
#endif
#if defined(SCTP_RTOINFO)
static ERL_NIF_TERM ngetopt_lvl_sctp_rtoinfo(ErlNifEnv*       env,
                                             ESockDescriptor* descP);
#endif
#endif // defined(HAVE_SCTP)
static ERL_NIF_TERM nsockname(ErlNifEnv*       env,
                              ESockDescriptor* descP);
static ERL_NIF_TERM npeername(ErlNifEnv*       env,
                              ESockDescriptor* descP);
static ERL_NIF_TERM ncancel(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     op,
                            ERL_NIF_TERM     sockRef,
                            ERL_NIF_TERM     opRef);
static ERL_NIF_TERM ncancel_connect(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     opRef);
static ERL_NIF_TERM ncancel_accept(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM     sockRef,
                                   ERL_NIF_TERM     opRef);
static ERL_NIF_TERM ncancel_accept_current(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM ncancel_accept_waiting(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     opRef);
static ERL_NIF_TERM ncancel_send(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     opRef);
static ERL_NIF_TERM ncancel_send_current(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM ncancel_send_waiting(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     opRef);
static ERL_NIF_TERM ncancel_recv(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     opRef);
static ERL_NIF_TERM ncancel_recv_current(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM ncancel_recv_waiting(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     opRef);
static ERL_NIF_TERM ncancel_read_select(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     opRef);
static ERL_NIF_TERM ncancel_write_select(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     opRef);
static ERL_NIF_TERM ncancel_mode_select(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     opRef,
                                        int              smode,
                                        int              rmode);

#if defined(USE_SETOPT_STR_OPT)
static ERL_NIF_TERM nsetopt_str_opt(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              level,
                                    int              opt,
                                    int              max,
                                    ERL_NIF_TERM     eVal);
#endif
static ERL_NIF_TERM nsetopt_bool_opt(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     int              level,
                                     int              opt,
                                     ERL_NIF_TERM     eVal);
static ERL_NIF_TERM nsetopt_int_opt(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              level,
                                    int              opt,
                                    ERL_NIF_TERM     eVal);
static ERL_NIF_TERM nsetopt_timeval_opt(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        int              level,
                                        int              opt,
                                        ERL_NIF_TERM     eVal);

#if defined(USE_GETOPT_STR_OPT)
static ERL_NIF_TERM ngetopt_str_opt(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              level,
                                    int              opt,
                                    int              max);
#endif
static ERL_NIF_TERM ngetopt_bool_opt(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     int              level,
                                     int              opt);
static ERL_NIF_TERM ngetopt_int_opt(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              level,
                                    int              opt);
static ERL_NIF_TERM ngetopt_timeval_opt(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        int              level,
                                        int              opt);

static BOOLEAN_T send_check_writer(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM     ref,
                                   ERL_NIF_TERM*    checkResult);
static ERL_NIF_TERM send_check_result(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ssize_t          written,
                                      ssize_t          dataSize,
                                      int              saveErrno,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     sendRef);
static ERL_NIF_TERM send_check_ok(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ssize_t          written,
                                  ssize_t          dataSize,
                                  ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM send_check_fail(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              saveErrno,
                                    ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM send_check_retry(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ssize_t          written,
                                     ERL_NIF_TERM     sockRef,
                                     ERL_NIF_TERM     sendRef);
static BOOLEAN_T recv_check_reader(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM     ref,
                                   ERL_NIF_TERM*    checkResult);
static char* recv_init_current_reader(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     ref);
static ERL_NIF_TERM recv_update_current_reader(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               ERL_NIF_TERM     sockRef);
static void recv_error_current_reader(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     reason);
static ERL_NIF_TERM recv_check_result(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      int              read,
                                      int              toRead,
                                      int              saveErrno,
                                      ErlNifBinary*    bufP,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recv_check_full(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              read,
                                    int              toRead,
                                    ErlNifBinary*    bufP,
                                    ERL_NIF_TERM     sockRef,
                                    ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recv_check_full_maybe_done(ErlNifEnv*       env,
                                               ESockDescriptor* descP,
                                               int              read,
                                               int              toRead,
                                               ErlNifBinary*    bufP,
                                               ERL_NIF_TERM     sockRef,
                                               ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recv_check_full_done(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         int              read,
                                         ErlNifBinary*    bufP,
                                         ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM recv_check_fail(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    int              saveErrno,
                                    ErlNifBinary*    buf1P,
                                    ErlNifBinary*    buf2P,
                                    ERL_NIF_TERM     sockRef,
                                    ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recv_check_fail_closed(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     sockRef,
                                           ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recv_check_partial(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       int              read,
                                       int              toRead,
                                       ErlNifBinary*    bufP,
                                       ERL_NIF_TERM     sockRef,
                                       ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recv_check_partial_done(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            int              read,
                                            ErlNifBinary*    bufP,
                                            ERL_NIF_TERM     sockRef);
static ERL_NIF_TERM recv_check_partial_part(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            int              read,
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
                                          int              read,
                                          int              saveErrno,
                                          ErlNifBinary*    bufP,
                                          ESockAddress*    fromAddrP,
                                          unsigned int     fromAddrLen,
                                          ERL_NIF_TERM     sockRef,
                                          ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recvmsg_check_result(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         int              read,
                                         int              saveErrno,
                                         struct msghdr*   msgHdrP,
                                         ErlNifBinary*    dataBufP,
                                         ErlNifBinary*    ctrlBufP,
                                         ERL_NIF_TERM     sockRef,
                                         ERL_NIF_TERM     recvRef);
static ERL_NIF_TERM recvmsg_check_msg(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      int              read,
                                      struct msghdr*   msgHdrP,
                                      ErlNifBinary*    dataBufP,
                                      ErlNifBinary*    ctrlBufP,
                                      ERL_NIF_TERM     sockRef);

static ERL_NIF_TERM nfinalize_connection(ErlNifEnv*       env,
                                         ESockDescriptor* descP);
static ERL_NIF_TERM nfinalize_close(ErlNifEnv*       env,
                                    ESockDescriptor* descP);

extern char* encode_msghdr(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           int              read,
                           struct msghdr*   msgHdrP,
                           ErlNifBinary*    dataBufP,
                           ErlNifBinary*    ctrlBufP,
                           ERL_NIF_TERM*    eSockAddr);
extern char* encode_cmsghdrs(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             ErlNifBinary*    cmsgBinP,
                             struct msghdr*   msgHdrP,
                             ERL_NIF_TERM*    eCMsgHdr);
extern char* decode_cmsghdrs(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             ERL_NIF_TERM     eCMsgHdr,
                             char*            cmsgHdrBufP,
                             size_t           cmsgHdrBufLen,
                             size_t*          cmsgHdrBufUsed);
extern char* decode_cmsghdr(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     eCMsgHdr,
                            char*            bufP,
                            size_t           rem,
                            size_t*          used);
static char* encode_cmsghdr_level(ErlNifEnv*    env,
                                  int           level,
                                  ERL_NIF_TERM* eLevel);
static char* decode_cmsghdr_level(ErlNifEnv*   env,
                                  ERL_NIF_TERM eLevel,
                                  int*         level);
static char* encode_cmsghdr_type(ErlNifEnv*    env,
                                 int           level,
                                 int           type,
                                 ERL_NIF_TERM* eType);
static char* decode_cmsghdr_type(ErlNifEnv*   env,
                                 int          level,
                                 ERL_NIF_TERM eType,
                                 int*         type);
static char* encode_cmsghdr_data(ErlNifEnv*     env,
                                 ERL_NIF_TERM   ctrlBuf,
                                 int            level,
                                 int            type,
                                 unsigned char* dataP,
                                 size_t         dataPos,
                                 size_t         dataLen,
                                 ERL_NIF_TERM*  eCMsgHdrData);
static char* encode_cmsghdr_data_socket(ErlNifEnv*     env,
                                        ERL_NIF_TERM   ctrlBuf,
                                        int            type,
                                        unsigned char* dataP,
                                        size_t         dataPos,
                                        size_t         dataLen,
                                        ERL_NIF_TERM*  eCMsgHdrData);
static char* encode_cmsghdr_data_ip(ErlNifEnv*     env,
                                    ERL_NIF_TERM   ctrlBuf,
                                    int            type,
                                    unsigned char* dataP,
                                    size_t         dataPos,
                                    size_t         dataLen,
                                    ERL_NIF_TERM*  eCMsgHdrData);
#if defined(HAVE_IPV6)
static char* encode_cmsghdr_data_ipv6(ErlNifEnv*     env,
                                      ERL_NIF_TERM   ctrlBuf,
                                      int            type,
                                      unsigned char* dataP,
                                      size_t         dataPos,
                                      size_t         dataLen,
                                      ERL_NIF_TERM*  eCMsgHdrData);
#endif
extern char* encode_msghdr_flags(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 int              msgFlags,
                                 ERL_NIF_TERM*    flags);
static char* decode_cmsghdr_data(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 char*            bufP,
                                 size_t           rem,
                                 int              level,
                                 int              type,
                                 ERL_NIF_TERM     eData,
                                 size_t*          used);
static char* decode_cmsghdr_final(ESockDescriptor* descP,
                                  char*            bufP,
                                  size_t           rem,
                                  int              level,
                                  int              type,
                                  char*            data,
                                  int              sz,
                                  size_t*          used);
static BOOLEAN_T decode_sock_linger(ErlNifEnv*     env,
                                    ERL_NIF_TERM   eVal,
                                    struct linger* valP);
#if defined(IP_TOS)
static BOOLEAN_T decode_ip_tos(ErlNifEnv*   env,
                               ERL_NIF_TERM eVal,
                               int*         val);
#endif
#if defined(IP_MTU_DISCOVER)
static char* decode_ip_pmtudisc(ErlNifEnv*   env,
                                ERL_NIF_TERM eVal,
                                int*         val);
#endif
#if defined(IP_MTU_DISCOVER)
static void encode_ip_pmtudisc(ErlNifEnv*    env,
                               int           val,
                               ERL_NIF_TERM* eVal);
#endif
#if defined(IPV6_MTU_DISCOVER)
static char* decode_ipv6_pmtudisc(ErlNifEnv*   env,
                                  ERL_NIF_TERM eVal,
                                  int*         val);
#endif
#if defined(IPV6_MTU_DISCOVER)
static void encode_ipv6_pmtudisc(ErlNifEnv*    env,
                                 int           val,
                                 ERL_NIF_TERM* eVal);
#endif

/*
static BOOLEAN_T decode_bool(ErlNifEnv*   env,
                             ERL_NIF_TERM eVal,
                             BOOLEAN_T*   val);
*/
static BOOLEAN_T decode_native_get_opt(ErlNifEnv*   env,
                                       ERL_NIF_TERM eVal,
                                       int*         opt,
                                       Uint16*      valueType,
                                       int*         valueSz);
// static void encode_bool(BOOLEAN_T val, ERL_NIF_TERM* eVal);
static ERL_NIF_TERM encode_ip_tos(ErlNifEnv* env, int val);

static void socket_stop_handle_current(ErlNifEnv*       env,
                                       const char*      role,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef,
                                       ESockRequestor*  reqP);
static void inform_waiting_procs(ErlNifEnv*         env,
                                 const char*        role,
                                 ESockDescriptor*   descP,
                                 ERL_NIF_TERM       sockRef,
                                 ESockRequestQueue* q,
                                 BOOLEAN_T          free,
                                 ERL_NIF_TERM       reason);

static int socket_setopt(int             sock,
                         int             level,
                         int             opt,
                         const void*     optVal,
                         const socklen_t optLen);

static BOOLEAN_T is_connector(ErlNifEnv*       env,
                              ESockDescriptor* descP);
static BOOLEAN_T verify_is_connected(ESockDescriptor* descP, int* err);

static ESockDescriptor* alloc_descriptor(SOCKET sock, HANDLE event);


static BOOLEAN_T edomain2domain(int edomain, int* domain);
static BOOLEAN_T etype2type(int etype, int* type);
static BOOLEAN_T eproto2proto(ErlNifEnv*         env,
                              const ERL_NIF_TERM eproto,
                              int*               proto);
static BOOLEAN_T ehow2how(unsigned int ehow, int* how);
static BOOLEAN_T esendflags2sendflags(unsigned int esendflags, int* sendflags);
static BOOLEAN_T erecvflags2recvflags(unsigned int erecvflags, int* recvflags);
static BOOLEAN_T elevel2level(BOOLEAN_T  isEncoded,
                              int        eLevel,
                              BOOLEAN_T* isOTP,
                              int*       level);
#ifdef HAVE_SETNS
static BOOLEAN_T emap2netns(ErlNifEnv* env, ERL_NIF_TERM map, char** netns);
static BOOLEAN_T change_network_namespace(char* netns, int* cns, int* err);
static BOOLEAN_T restore_network_namespace(int ns, SOCKET sock, int* err);
#endif

static BOOLEAN_T cnt_inc(Uint32* cnt, Uint32 inc);
static void      cnt_dec(Uint32* cnt, Uint32 dec);

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
    static ERL_NIF_TERM O##_push(ErlNifEnv*       env,         \
                                 ESockDescriptor* descP,       \
                                 ErlNifPid        pid,         \
                                 ERL_NIF_TERM     ref);        \
    static BOOLEAN_T O##_pop(ErlNifEnv*       env,             \
                             ESockDescriptor* descP,           \
                             ESockRequestor*  reqP);           \
    static BOOLEAN_T O##_unqueue(ErlNifEnv*       env,         \
                                 ESockDescriptor* descP,       \
                                 const ErlNifPid* pid);
ESOCK_OPERATOR_FUNCS_DEFS
#undef ESOCK_OPERATOR_FUNCS_DEF

static BOOLEAN_T requestor_pop(ESockRequestQueue* q,
                               ESockRequestor*    reqP);

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
                          const ErlNifPid*   pid);

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


#endif // if defined(__WIN32__)

/*
#if defined(HAVE_SYS_UN_H) || defined(SO_BINDTODEVICE)
static size_t my_strnlen(const char *s, size_t maxlen);
#endif
*/

static void socket_dtor(ErlNifEnv* env, void* obj);
static void socket_stop(ErlNifEnv* env,
			void*      obj,
			int        fd,
			int        is_direct_call);
static void socket_down(ErlNifEnv*           env,
			void*                obj,
			const ErlNifPid*     pid,
			const ErlNifMonitor* mon);

#if !defined(__WIN32__)

static void socket_down_acceptor(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 const ErlNifPid* pid);
static void socket_down_writer(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               const ErlNifPid* pid);
static void socket_down_reader(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               const ErlNifPid* pid);

static char* esock_send_wrap_msg(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     cnt);
static char* esock_send_close_msg(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ErlNifPid*       pid);
static char* esock_send_abort_msg(ErlNifEnv*   env,
                                  ERL_NIF_TERM sockRef,
                                  ERL_NIF_TERM recvRef,
                                  ErlNifEnv*   msgEnv,
                                  ERL_NIF_TERM reason,
                                  ErlNifPid*   pid);
static char* esock_send_msg(ErlNifEnv*   env,
                            ErlNifPid*   pid,
                            ERL_NIF_TERM msg,
                            ErlNifEnv*   msgEnv);

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
                             const ErlNifPid* pid,
                             ERL_NIF_TERM     sockRef,
                             ERL_NIF_TERM     selectRef);
static int esock_select_write(ErlNifEnv*       env,
                              ErlNifEvent      event,
                              void*            obj,
                              const ErlNifPid* pid,
                              ERL_NIF_TERM     sockRef,
                              ERL_NIF_TERM     selectRef);
static int esock_select_stop(ErlNifEnv*  env,
                             ErlNifEvent event,
                             void*       obj);
static int esock_select_cancel(ErlNifEnv*             env,
                               ErlNifEvent            event,
                               enum ErlNifSelectFlags mode,
                               void*                  obj);

static BOOLEAN_T extract_debug(ErlNifEnv*   env,
                               ERL_NIF_TERM map);
static BOOLEAN_T extract_iow(ErlNifEnv*   env,
                             ERL_NIF_TERM map);

#endif // if defined(__WIN32__)

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



/* (special) error string constants */
static char str_exmon[]          = "exmonitor";  // failed monitor
static char str_exself[]         = "exself";     // failed self
static char str_exsend[]         = "exsend";     // failed send



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
    GLOBAL_ATOM_DECL(bindtodevice);                    \
    GLOBAL_ATOM_DECL(block_source);                    \
    GLOBAL_ATOM_DECL(broadcast);                       \
    GLOBAL_ATOM_DECL(busy_poll);                       \
    GLOBAL_ATOM_DECL(checksum);                        \
    GLOBAL_ATOM_DECL(close);                           \
    GLOBAL_ATOM_DECL(command);                         \
    GLOBAL_ATOM_DECL(connect);                         \
    GLOBAL_ATOM_DECL(congestion);                      \
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
    GLOBAL_ATOM_DECL(flags);                           \
    GLOBAL_ATOM_DECL(flowinfo);                        \
    GLOBAL_ATOM_DECL(fragment_interleave);             \
    GLOBAL_ATOM_DECL(freebind);                        \
    GLOBAL_ATOM_DECL(get_peer_addr_info);              \
    GLOBAL_ATOM_DECL(hdrincl);                         \
    GLOBAL_ATOM_DECL(hmac_ident);                      \
    GLOBAL_ATOM_DECL(hoplimit);                        \
    GLOBAL_ATOM_DECL(hopopts);                         \
    GLOBAL_ATOM_DECL(ifindex);                         \
    GLOBAL_ATOM_DECL(inet);                            \
    GLOBAL_ATOM_DECL(inet6);                           \
    GLOBAL_ATOM_DECL(info);                            \
    GLOBAL_ATOM_DECL(initmsg);                         \
    GLOBAL_ATOM_DECL(iov);                             \
    GLOBAL_ATOM_DECL(ip);                              \
    GLOBAL_ATOM_DECL(ipcomp_level);                    \
    GLOBAL_ATOM_DECL(ipv6);                            \
    GLOBAL_ATOM_DECL(i_want_mapped_v4_addr);           \
    GLOBAL_ATOM_DECL(join_group);                      \
    GLOBAL_ATOM_DECL(keepalive);                       \
    GLOBAL_ATOM_DECL(keepcnt);                         \
    GLOBAL_ATOM_DECL(keepidle);                        \
    GLOBAL_ATOM_DECL(keepintvl);                       \
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
    GLOBAL_ATOM_DECL(msfilter);                        \
    GLOBAL_ATOM_DECL(mtu);                             \
    GLOBAL_ATOM_DECL(mtu_discover);                    \
    GLOBAL_ATOM_DECL(multicast_all);                   \
    GLOBAL_ATOM_DECL(multicast_hops);                  \
    GLOBAL_ATOM_DECL(multicast_if);                    \
    GLOBAL_ATOM_DECL(multicast_loop);                  \
    GLOBAL_ATOM_DECL(multicast_ttl);                   \
    GLOBAL_ATOM_DECL(nodelay);                         \
    GLOBAL_ATOM_DECL(nodefrag);                        \
    GLOBAL_ATOM_DECL(noopt);                           \
    GLOBAL_ATOM_DECL(nopush);                          \
    GLOBAL_ATOM_DECL(not_found);                       \
    GLOBAL_ATOM_DECL(not_owner);                       \
    GLOBAL_ATOM_DECL(ok);                              \
    GLOBAL_ATOM_DECL(oob);                             \
    GLOBAL_ATOM_DECL(oobinline);                       \
    GLOBAL_ATOM_DECL(options);                         \
    GLOBAL_ATOM_DECL(origdstaddr);                     \
    GLOBAL_ATOM_DECL(partial_delivery_point);          \
    GLOBAL_ATOM_DECL(passcred);                        \
    GLOBAL_ATOM_DECL(path);                            \
    GLOBAL_ATOM_DECL(peekcred);                        \
    GLOBAL_ATOM_DECL(peek_off);                        \
    GLOBAL_ATOM_DECL(peer_addr_params);                \
    GLOBAL_ATOM_DECL(peer_auth_chunks);                \
    GLOBAL_ATOM_DECL(pktinfo);                         \
    GLOBAL_ATOM_DECL(pktoptions);                      \
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
    GLOBAL_ATOM_DECL(unknown);                         \
    GLOBAL_ATOM_DECL(usec);                            \
    GLOBAL_ATOM_DECL(user_timeout);                    \
    GLOBAL_ATOM_DECL(use_ext_recvinfo);                \
    GLOBAL_ATOM_DECL(use_min_mtu);                     \
    GLOBAL_ATOM_DECL(v6only);


/* *** Global error reason atoms *** */
#define GLOBAL_ERROR_REASON_ATOMS   \
    GLOBAL_ATOM_DECL(eagain);       \
    GLOBAL_ATOM_DECL(eafnosupport); \
    GLOBAL_ATOM_DECL(einval);


#define GLOBAL_ATOM_DECL(A) ERL_NIF_TERM esock_atom_##A
GLOBAL_ATOMS
GLOBAL_ERROR_REASON_ATOMS
#undef GLOBAL_ATOM_DECL
ERL_NIF_TERM esock_atom_socket_tag; // This has a "special" name ('$socket')

/* *** Local atoms *** */
#define LOCAL_ATOMS                    \
    LOCAL_ATOM_DECL(adaptation_layer); \
    LOCAL_ATOM_DECL(address);          \
    LOCAL_ATOM_DECL(association);      \
    LOCAL_ATOM_DECL(assoc_id);         \
    LOCAL_ATOM_DECL(authentication);   \
    LOCAL_ATOM_DECL(bool);             \
    LOCAL_ATOM_DECL(close);            \
    LOCAL_ATOM_DECL(closed);           \
    LOCAL_ATOM_DECL(closing);          \
    LOCAL_ATOM_DECL(cookie_life);      \
    LOCAL_ATOM_DECL(counter_wrap);     \
    LOCAL_ATOM_DECL(counters);         \
    LOCAL_ATOM_DECL(data_in);          \
    LOCAL_ATOM_DECL(do);               \
    LOCAL_ATOM_DECL(dont);             \
    LOCAL_ATOM_DECL(exclude);          \
    LOCAL_ATOM_DECL(false);            \
    LOCAL_ATOM_DECL(global_counters);  \
    LOCAL_ATOM_DECL(in4_sockaddr);     \
    LOCAL_ATOM_DECL(in6_sockaddr);     \
    LOCAL_ATOM_DECL(include);          \
    LOCAL_ATOM_DECL(initial);          \
    LOCAL_ATOM_DECL(int);              \
    LOCAL_ATOM_DECL(interface);        \
    LOCAL_ATOM_DECL(iow);              \
    LOCAL_ATOM_DECL(local_rwnd);       \
    LOCAL_ATOM_DECL(max);              \
    LOCAL_ATOM_DECL(max_attempts);     \
    LOCAL_ATOM_DECL(max_init_timeo);   \
    LOCAL_ATOM_DECL(max_instreams);    \
    LOCAL_ATOM_DECL(max_rxt);          \
    LOCAL_ATOM_DECL(min);              \
    LOCAL_ATOM_DECL(mode);             \
    LOCAL_ATOM_DECL(multiaddr);        \
    LOCAL_ATOM_DECL(null);             \
    LOCAL_ATOM_DECL(num_acceptors);    \
    LOCAL_ATOM_DECL(num_dinet);        \
    LOCAL_ATOM_DECL(num_dinet6);       \
    LOCAL_ATOM_DECL(num_dlocal);       \
    LOCAL_ATOM_DECL(num_outstreams);   \
    LOCAL_ATOM_DECL(num_peer_dests);   \
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
    LOCAL_ATOM_DECL(partial_delivery); \
    LOCAL_ATOM_DECL(peer_error);       \
    LOCAL_ATOM_DECL(peer_rwnd);        \
    LOCAL_ATOM_DECL(probe);            \
    LOCAL_ATOM_DECL(read_byte);        \
    LOCAL_ATOM_DECL(read_fails);       \
    LOCAL_ATOM_DECL(read_pkg);         \
    LOCAL_ATOM_DECL(read_tries);       \
    LOCAL_ATOM_DECL(read_waits);       \
    LOCAL_ATOM_DECL(select);           \
    LOCAL_ATOM_DECL(sender_dry);       \
    LOCAL_ATOM_DECL(send_failure);     \
    LOCAL_ATOM_DECL(shutdown);         \
    LOCAL_ATOM_DECL(slist);            \
    LOCAL_ATOM_DECL(sourceaddr);       \
    LOCAL_ATOM_DECL(timeout);          \
    LOCAL_ATOM_DECL(true);             \
    LOCAL_ATOM_DECL(want);             \
    LOCAL_ATOM_DECL(write_byte);       \
    LOCAL_ATOM_DECL(write_fails);      \
    LOCAL_ATOM_DECL(write_pkg);        \
    LOCAL_ATOM_DECL(write_tries);      \
    LOCAL_ATOM_DECL(write_waits);

/* Local error reason atoms */
#define LOCAL_ERROR_REASON_ATOMS                \
    LOCAL_ATOM_DECL(eisconn);                   \
    LOCAL_ATOM_DECL(enotclosing);               \
    LOCAL_ATOM_DECL(enotconn);                  \
    LOCAL_ATOM_DECL(exalloc);                   \
    LOCAL_ATOM_DECL(exbadstate);                \
    LOCAL_ATOM_DECL(exbusy);                    \
    LOCAL_ATOM_DECL(exmon);                     \
    LOCAL_ATOM_DECL(exself);                    \
    LOCAL_ATOM_DECL(exsend);

#define LOCAL_ATOM_DECL(LA) static ERL_NIF_TERM atom_##LA
LOCAL_ATOMS
LOCAL_ERROR_REASON_ATOMS
#undef LOCAL_ATOM_DECL


/* *** Sockets *** */
static ErlNifResourceType*    sockets;
static ErlNifResourceTypeInit socketInit = {
   socket_dtor,
   socket_stop,
   (ErlNifResourceDown*) socket_down
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
    ErlNifEnv* env = enif_alloc_env();

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
 * nif_open(Domain, Type, Protocol, Extra)
 * nif_bind(Sock, LocalAddr)
 * nif_connect(Sock, SockAddr)
 * nif_listen(Sock, Backlog)
 * nif_accept(LSock, Ref)
 * nif_send(Sock, SendRef, Data, Flags)
 * nif_sendto(Sock, SendRef, Data, Dest, Flags)
 * nif_sendmsg(Sock, SendRef, MsgHdr, Flags)
 * nif_recv(Sock, RecvRef, Length, Flags)
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
 * nif_getopt/4
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
#define MKCT(E, T, C) MKT2((E), (T), MKUI((E), (C)))

static
ERL_NIF_TERM nif_info(ErlNifEnv*         env,
                      int                argc,
                      const ERL_NIF_TERM argv[])
{
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM info;

    SGDBG( ("SOCKET", "nif_info -> entry with %d args\r\n", argc) );
    
    switch (argc) {
    case 0:
        info = esock_global_info(env);
        break;

    case 1:
        {
            ESockDescriptor* descP;

            if (!enif_get_resource(env, argv[0], sockets, (void**) &descP)) {
                return enif_make_badarg(env);
            }
            SSDBG( descP, ("SOCKET", "nif_info -> get socket info\r\n") );
            info = esock_socket_info(env, descP);
        }
        break;
        
    default:
        return enif_make_badarg(env);
    }

    return info;

#endif
}


/*
 * This function return a property list containing "global" info.
 */
#if !defined(__WIN32__)
static
ERL_NIF_TERM esock_global_info(ErlNifEnv* env)
{
    ERL_NIF_TERM numSockets     = MKCT(env, atom_num_sockets,  data.numSockets);
    ERL_NIF_TERM numTypeDGrams  = MKCT(env, atom_num_tdgrams,  data.numTypeDGrams);
    ERL_NIF_TERM numTypeStreams = MKCT(env, atom_num_tstreams, data.numTypeStreams);
    ERL_NIF_TERM numTypeSeqPkgs = MKCT(env, atom_num_tseqpkgs, data.numTypeSeqPkgs);
    ERL_NIF_TERM numDomLocal    = MKCT(env, atom_num_dlocal,   data.numDomainLocal);
    ERL_NIF_TERM numDomInet     = MKCT(env, atom_num_dinet,    data.numDomainInet);
    ERL_NIF_TERM numDomInet6    = MKCT(env, atom_num_dinet6,   data.numDomainInet6);
    ERL_NIF_TERM numProtoIP     = MKCT(env, atom_num_pip,      data.numProtoIP);
    ERL_NIF_TERM numProtoTCP    = MKCT(env, atom_num_ptcp,     data.numProtoTCP);
    ERL_NIF_TERM numProtoUDP    = MKCT(env, atom_num_pudp,     data.numProtoUDP);
    ERL_NIF_TERM numProtoSCTP   = MKCT(env, atom_num_psctp,    data.numProtoSCTP);
    ERL_NIF_TERM gcnt[]  = {numSockets,
                            numTypeDGrams, numTypeStreams, numTypeSeqPkgs,
                            numDomLocal, numDomInet, numDomInet6,
                            numProtoIP, numProtoTCP, numProtoUDP, numProtoSCTP};
    unsigned int lenGCnt = sizeof(gcnt) / sizeof(ERL_NIF_TERM);
    ERL_NIF_TERM lgcnt   = MKLA(env, gcnt, lenGCnt);
    ERL_NIF_TERM keys[]  = {esock_atom_debug, atom_iow, atom_global_counters};
    ERL_NIF_TERM vals[]  = {BOOL2ATOM(data.dbg), BOOL2ATOM(data.iow), lgcnt};
    ERL_NIF_TERM info;
    unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);
    unsigned int numVals = sizeof(vals) / sizeof(ERL_NIF_TERM);

    ESOCK_ASSERT( (numKeys == numVals) );

    if (!MKMA(env, keys, vals, numKeys, &info))
        return enif_make_badarg(env);

    return info;
}



/*
 * This function return a property *map*. The properties are: 
 *    counters:  A list of each socket counter and there current values
 *    readers:   The number of current and waiting readers
 *    writers:   The number of current and waiting writers
 *    acceptors: The number of current and waiting acceptors
 */
static
ERL_NIF_TERM esock_socket_info(ErlNifEnv*       env,
                               ESockDescriptor* descP)
{
    ERL_NIF_TERM counters  = esock_socket_info_counters(env, descP);
    ERL_NIF_TERM readers   = esock_socket_info_readers(env, descP);
    ERL_NIF_TERM writers   = esock_socket_info_writers(env, descP);
    ERL_NIF_TERM acceptors = esock_socket_info_acceptors(env, descP);
    ERL_NIF_TERM keys[]    = {atom_counters, atom_num_readers,
                              atom_num_writers, atom_num_acceptors};
    ERL_NIF_TERM vals[]    = {counters, readers, writers, acceptors};
    ERL_NIF_TERM info;
    unsigned int numKeys  = sizeof(keys) / sizeof(ERL_NIF_TERM);
    unsigned int numVals  = sizeof(vals) / sizeof(ERL_NIF_TERM);

    SSDBG( descP, ("SOCKET", "esock_socket_info -> "
                   "\r\n   numKeys: %d"
                   "\r\n   numVals: %d"
                   "\r\n", numKeys, numVals) );

    ESOCK_ASSERT( (numKeys == numVals) );

    if (!MKMA(env, keys, vals, numKeys, &info))
        return enif_make_badarg(env);

    SSDBG( descP, ("SOCKET", "esock_socket_info -> done with"
                   "\r\n   info: %T"
                   "\r\n", info) );

    return info;
    
}


/*
 * Collect all counters for a socket.
 */
static
ERL_NIF_TERM esock_socket_info_counters(ErlNifEnv*       env,
                                        ESockDescriptor* descP)
{
    ERL_NIF_TERM info;

    MLOCK(descP->writeMtx);
    MLOCK(descP->readMtx);

    {
        ERL_NIF_TERM readByteCnt  = MKCT(env, atom_read_byte, descP->readByteCnt);
        ERL_NIF_TERM readFails    = MKCT(env, atom_read_fails, descP->readFails);
        ERL_NIF_TERM readPkgCnt   = MKCT(env, atom_read_pkg, descP->readPkgCnt);
        ERL_NIF_TERM readTries    = MKCT(env, atom_read_tries, descP->readTries);
        ERL_NIF_TERM readWaits    = MKCT(env, atom_read_waits, descP->readWaits);
        ERL_NIF_TERM writeByteCnt = MKCT(env, atom_write_byte, descP->writeByteCnt);
        ERL_NIF_TERM writeFails   = MKCT(env, atom_write_fails, descP->writeFails);
        ERL_NIF_TERM writePkgCnt  = MKCT(env, atom_write_pkg, descP->writePkgCnt);
        ERL_NIF_TERM writeTries   = MKCT(env, atom_write_tries, descP->writeTries);
        ERL_NIF_TERM writeWaits   = MKCT(env, atom_write_waits, descP->writeWaits);
        ERL_NIF_TERM acnt[]       = {readByteCnt, readFails, readPkgCnt,
                                     readTries, readWaits,
                                     writeByteCnt, writeFails, writePkgCnt,
                                     writeTries, writeWaits};
        unsigned int lenACnt      = sizeof(acnt) / sizeof(ERL_NIF_TERM);

        info = MKLA(env, acnt, lenACnt);

        SSDBG( descP, ("SOCKET", "esock_socket_info_counters -> "
                       "\r\n   lenACnt: %d"
                       "\r\n   info:    %T"
                       "\r\n", lenACnt, info) );

    }

    MUNLOCK(descP->readMtx);
    MUNLOCK(descP->writeMtx);

    SSDBG( descP, ("SOCKET", "esock_socket_info_counters -> done with"
                   "\r\n   info: %T"
                   "\r\n", info) );

    return info;
}
#endif


/* ----------------------------------------------------------------------
 * nif_command
 *
 * Description:
 * This function is intended to handle "various" commands. That is,
 * commands and operations that are not part of the socket API proper.
 * Currently it handles setting the global debug. Its a map with two
 * attributes command and (command) data:
 * #{command :: atom(), data :: term()}
 *
 * Command                    Data
 * debug                      boolean()
 *
 */

static
ERL_NIF_TERM nif_command(ErlNifEnv*         env,
                         int                argc,
                         const ERL_NIF_TERM argv[])
{
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM ecmd, ecdata, result;
    Uint16       cmd;

    SGDBG( ("SOCKET", "nif_command -> entry with %d args\r\n", argc) );

    if ((argc != 1) ||
        !IS_MAP(env,  argv[0])) {
        return enif_make_badarg(env);
    }
    ecmd = argv[0];

    SGDBG( ("SOCKET", "nif_command -> "
            "\r\n   (e) command: %T"
            "\r\n", ecmd) );

    if (!ecommand2command(env, ecmd, &cmd, &ecdata)) {
        SGDBG( ("SOCKET", "nif_command -> invalid command\r\n") );
        return esock_make_error(env, esock_atom_einval);
    }

    SGDBG( ("SOCKET", "nif_command -> "
            "\r\n   command:          %d"
            "\r\n   (e) command data: %T"
            "\r\n", cmd, ecdata) );

    result = ncommand(env, cmd, ecdata);
    
    SGDBG( ("SOCKET", "nif_command -> done with result: "
           "\r\n   %T"
           "\r\n", result) );

    return result;

#endif
}


#if !defined(__WIN32__)
static
ERL_NIF_TERM ncommand(ErlNifEnv* env, Uint16 cmd, ERL_NIF_TERM ecdata)
{
    ERL_NIF_TERM result;

    SGDBG( ("SOCKET", "ncommand -> entry with 0x%lX\r\n", cmd) );

    switch (cmd) {
    case SOCKET_CMD_DEBUG:
        result = ncommand_debug(env, ecdata);
        break;

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}



static
ERL_NIF_TERM ncommand_debug(ErlNifEnv* env, ERL_NIF_TERM ecdata)
{
    ERL_NIF_TERM result;

    /* The data *should* be a boolean() */

    if (COMPARE(ecdata, esock_atom_true) == 0) {
        data.dbg = TRUE;
        result   = esock_atom_ok;
    } else if (COMPARE(ecdata, esock_atom_false) == 0) {
        data.dbg = FALSE;
        result   = esock_atom_ok;
    } else {
        SGDBG( ("SOCKET", "ncommand_debug -> invalid debug value: %T\r\n",
                ecdata) );
        result = esock_make_error(env, esock_atom_einval);
    }

    return result;
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

#if !defined(__WIN32__)
#define SOCKET_INFO_REQ_FUNCS                                                  \
    SOCKET_INFO_REQ_FUNC_DECL(readers,   readMtx,  currentReaderP,   readersQ) \
    SOCKET_INFO_REQ_FUNC_DECL(writers,   writeMtx, currentWriterP,   writersQ) \
    SOCKET_INFO_REQ_FUNC_DECL(acceptors, accMtx,   currentAcceptorP, acceptorsQ)

#define SOCKET_INFO_REQ_FUNC_DECL(F, MTX, CRP, Q)                               \
    static                                                                      \
    ERL_NIF_TERM esock_socket_info_##F(ErlNifEnv*       env,                    \
                                       ESockDescriptor* descP)                  \
    {                                                                           \
        return socket_info_reqs(env, descP, descP->MTX, descP->CRP, &descP->Q); \
    }
SOCKET_INFO_REQ_FUNCS
#undef SOCKET_INFO_REQ_FUNC_DECL


static
ERL_NIF_TERM socket_info_reqs(ErlNifEnv*         env,
                              ESockDescriptor*   descP,
                              ErlNifMutex*       mtx,
                              ESockRequestor*    crp,
                              ESockRequestQueue* q)
{
    ESockRequestQueueElement* tmp;
    ERL_NIF_TERM              info;
    unsigned int              cnt = 0;

    MLOCK(mtx);

    if (crp != NULL) {
        // We have an active requestor!
        cnt++;

        // And add all the waiting requestors
        tmp = q->first;
        while (tmp != NULL) {
            cnt++;
            tmp = tmp->nextP;
        }
    }

    MUNLOCK(mtx);

    info = MKUI(env, cnt);

    SSDBG( descP, ("SOCKET", "socket_info_reqs -> done with"
                   "\r\n   info: %T"
                   "\r\n", info) );

    return info;
}
#endif


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
 * options         [{socket, [{Opt, boolean()}]},
 *                  {ip,     [{Opt, boolean()}]},
 *                  {ipv6,   [{Opt, boolean()}]},
 *                  {tcp,    [{Opt, boolean()}]},
 *                  {udp,    [{Opt, boolean()}]},
 *                  {sctp,   [{Opt, boolean()}]}]
 * sctp            boolean()
 * ipv6            boolean()
 * local           boolean()
 */

static
ERL_NIF_TERM nif_supports(ErlNifEnv*         env,
                          int                argc,
                          const ERL_NIF_TERM argv[])
{
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    int key;
    
    SGDBG( ("SOCKET", "nif_supports -> entry with %d args\r\n", argc) );
    
    /* Extract arguments and perform preliminary validation */

    if ((argc != 1) ||
        !GET_INT(env, argv[0], &key)) {
        return enif_make_badarg(env);
    }

    return nsupports(env, key);
#endif
}



/* nsupports - what features do we support
 *
 * This is to prove information about what features actually
 * work on the current platform.
 */
#if !defined(__WIN32__)
static
ERL_NIF_TERM nsupports(ErlNifEnv* env, int key)
{
    ERL_NIF_TERM result;

    SGDBG( ("SOCKET", "nsupports -> entry with 0x%lX\r\n", key) );

    switch (key) {
    case SOCKET_SUPPORTS_OPTIONS:
        result = nsupports_options(env);
        break;

    case SOCKET_SUPPORTS_SCTP:
        result = nsupports_sctp(env);
        break;

    case SOCKET_SUPPORTS_IPV6:
        result = nsupports_ipv6(env);
        break;

    case SOCKET_SUPPORTS_LOCAL:
        result = nsupports_local(env);
        break;

    default:
        result = esock_atom_false;
        break;
    }

    return result;
}
#endif


#if !defined(__WIN32__)
static
ERL_NIF_TERM nsupports_options(ErlNifEnv* env)
{
    ERL_NIF_TERM sockOpts  = nsupports_options_socket(env);
    ERL_NIF_TERM sockOptsT = MKT2(env, esock_atom_socket, sockOpts);
    ERL_NIF_TERM ipOpts    = nsupports_options_ip(env);
    ERL_NIF_TERM ipOptsT   = MKT2(env, esock_atom_ip, ipOpts);
    ERL_NIF_TERM ipv6Opts  = nsupports_options_ipv6(env);
    ERL_NIF_TERM ipv6OptsT = MKT2(env, esock_atom_ipv6, ipv6Opts);
    ERL_NIF_TERM tcpOpts   = nsupports_options_tcp(env);
    ERL_NIF_TERM tcpOptsT  = MKT2(env, esock_atom_tcp, tcpOpts);
    ERL_NIF_TERM udpOpts   = nsupports_options_udp(env);
    ERL_NIF_TERM udpOptsT  = MKT2(env, esock_atom_udp, udpOpts);
    ERL_NIF_TERM sctpOpts  = nsupports_options_sctp(env);
    ERL_NIF_TERM sctpOptsT = MKT2(env, esock_atom_sctp, sctpOpts);
    ERL_NIF_TERM optsA[]   = {sockOptsT,
                              ipOptsT, ipv6OptsT,
                              tcpOptsT, udpOptsT, sctpOptsT};
    unsigned int lenOptsA  = sizeof(optsA) / sizeof(ERL_NIF_TERM);
    ERL_NIF_TERM optsL     = MKLA(env, optsA, lenOptsA);

    return optsL;
}
#endif



#if !defined(__WIN32__)
static
ERL_NIF_TERM nsupports_options_socket(ErlNifEnv* env)
{
    SocketTArray opts = TARRAY_CREATE(128);
    ERL_NIF_TERM tmp, optsL;


    /* *** SOCKET_OPT_SOCK_ACCEPTCONN => SO_ACCEPTCONN *** */
#if defined(SO_ACCEPTCONN)
    tmp = MKT2(env, esock_atom_acceptconn, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_acceptconn, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_ACCEPTFILTER => SO_ACCEPTFILTER *** */
    tmp = MKT2(env, esock_atom_acceptfilter, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_BINDTODEVICE => SO_BINDTODEVICE *** */
#if defined(SO_BINDTODEVICE)
    tmp = MKT2(env, esock_atom_bindtodevice, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_bindtodevice, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_BROADCAST => SO_BROADCAST *** */
#if defined(SO_BROADCAST)
    tmp = MKT2(env, esock_atom_broadcast, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_broadcast, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_BUSY_POLL => SO_BUSY_POLL *** */
    tmp = MKT2(env, esock_atom_busy_poll, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_DEBUG => SO_DEBUG *** */
#if defined(SO_DEBUG)
    tmp = MKT2(env, esock_atom_debug, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_debug, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_DOMAIN => SO_DOMAIN *** */
#if defined(SO_DOMAIN)
    tmp = MKT2(env, esock_atom_domain, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_domain, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_DONTROUTE => SO_DONTROUTE *** */
#if defined(SO_DONTROUTE)
    tmp = MKT2(env, esock_atom_dontroute, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_dontroute, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_ERROR => SO_ERROR *** */
    tmp = MKT2(env, esock_atom_error, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_KEEPALIVE => SO_KEEPALIVE *** */
#if defined(SO_KEEPALIVE)
    tmp = MKT2(env, esock_atom_keepalive, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_keepalive, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_LINGER => SO_LINGER *** */
#if defined(SO_LINGER)
    tmp = MKT2(env, esock_atom_linger, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_linger, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_MARK => SO_MARK *** */
    tmp = MKT2(env, esock_atom_mark, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_OOBINLINE => SO_OOBINLINE *** */
#if defined(SO_OOBINLINE)
    tmp = MKT2(env, esock_atom_oobinline, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_oobinline, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_PASSCRED => SO_PASSCRED *** */
    tmp = MKT2(env, esock_atom_passcred, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_PEEK_OFF => SO_PEEK_OFF *** */
#if defined(SO_PEEK_OFF)
    tmp = MKT2(env, esock_atom_peek_off, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_peek_off, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_PEEKCRED => SO_PEEKCRED *** */
    tmp = MKT2(env, esock_atom_peekcred, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_PRIORITY => SO_PRIORITY *** */
#if defined(SO_PRIORITY)
    tmp = MKT2(env, esock_atom_priority, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_priority, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_PROTOCOL => SO_PROTOCOL *** */
#if defined(SO_PROTOCOL)
    tmp = MKT2(env, esock_atom_protocol, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_protocol, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_RCVBUF => SO_RCVBUF *** */
#if defined(SO_RCVBUF)
    tmp = MKT2(env, esock_atom_rcvbuf, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_rcvbuf, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_RCVBUFFORCE => SO_RCVBUFFORCE *** */
    tmp = MKT2(env, esock_atom_rcvbufforce, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_RCVLOWAT => SO_RCVLOWAT *** */
#if defined(SO_RCVLOWAT)
    tmp = MKT2(env, esock_atom_rcvlowat, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_rcvlowat, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_RCVTIMEO => SO_RCVTIMEO *** */
#if defined(SO_RCVTIMEO)
    tmp = MKT2(env, esock_atom_rcvtimeo, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_rcvtimeo, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_REUSEADDR => SO_REUSEADDR *** */
#if defined(SO_REUSEADDR)
    tmp = MKT2(env, esock_atom_reuseaddr, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_reuseaddr, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_REUSEPORT => SO_REUSEPORT *** */
#if defined(SO_REUSEPORT)
    tmp = MKT2(env, esock_atom_reuseport, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_reuseport, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_RXQ_OVFL => SO_RXQ_OVFL *** */
    tmp = MKT2(env, esock_atom_rxq_ovfl, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_SETFIB => SO_SETFIB *** */
    tmp = MKT2(env, esock_atom_setfib, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_SNDBUF => SO_SNDBUF *** */
#if defined(SO_SNDBUF)
    tmp = MKT2(env, esock_atom_sndbuf, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_sndbuf, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_SNDBUFFORCE => SO_SNDBUFFORCE *** */
    tmp = MKT2(env, esock_atom_sndbufforce, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_SNDLOWAT => SO_SNDLOWAT *** */
#if defined(SO_SNDLOWAT)
    tmp = MKT2(env, esock_atom_sndlowat, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_sndlowat, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_SNDTIMEO => SO_SNDTIMEO *** */
#if defined(SO_SNDTIMEO)
    tmp = MKT2(env, esock_atom_sndtimeo, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_sndtimeo, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_TIMESTAMP => SO_TIMESTAMP *** */
#if defined(SO_TIMESTAMP)
    tmp = MKT2(env, esock_atom_timestamp, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_timestamp, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SOCK_TYPE => SO_TYPE *** */
#if defined(SO_TYPE)
    tmp = MKT2(env, esock_atom_type, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_type, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    TARRAY_TOLIST(opts, env, &optsL);
    
    return optsL;
}
#endif



#if !defined(__WIN32__)
static
ERL_NIF_TERM nsupports_options_ip(ErlNifEnv* env)
{
    SocketTArray opts = TARRAY_CREATE(128);
    ERL_NIF_TERM tmp, optsL;


    /* *** SOCKET_OPT_IP_ADD_MEMBERSHIP => IP_ADD_MEMBERSHIP *** */
#if defined(IP_ADD_MEMBERSHIP)
    tmp = MKT2(env, esock_atom_add_membership, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_add_membership, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_ADD_SOURCE_MEMBERSHIP => IP_ADD_SOURCE_MEMBERSHIP *** */
#if defined(IP_ADD_SOURCE_MEMBERSHIP)
    tmp = MKT2(env, esock_atom_add_source_membership, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_add_source_membership, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_BLOCK_SOURCE => IP_BLOCK_SOURCE *** */
#if defined(IP_BLOCK_SOURCE)
    tmp = MKT2(env, esock_atom_block_source, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_block_source, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_DONTFRAG => IP_DONTFRAG *** */
    tmp = MKT2(env, esock_atom_dontfrag, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_DROP_MEMBERSHIP => IP_DROP_MEMBERSHIP *** */
#if defined(IP_DROP_MEMBERSHIP)
    tmp = MKT2(env, esock_atom_drop_membership, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_drop_membership, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_DROP_SOURCE_MEMBERSHIP => IP_DROP_SOURCE_MEMBERSHIP *** */
#if defined(IP_DROP_SOURCE_MEMBERSHIP)
    tmp = MKT2(env, esock_atom_drop_source_membership, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_drop_source_membership, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_FREEBIND => IP_FREEBIND *** */
#if defined(IP_FREEBIND)
    tmp = MKT2(env, esock_atom_freebind, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_freebind, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_HDRINCL => IP_HDRINCL *** */
#if defined(IP_HDRINCL)
    tmp = MKT2(env, esock_atom_hdrincl, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_hdrincl, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_MINTTL => IP_MINTTL *** */
#if defined(IP_MINTTL)
    tmp = MKT2(env, esock_atom_minttl, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_minttl, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_MSFILTER => IP_MSFILTER / IP_MSFILTER_SIZE *** */
#if defined(IP_MSFILTER) && defined(IP_MSFILTER_SIZE)
    tmp = MKT2(env, esock_atom_msfilter, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_msfilter, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_MTU => IP_MTU *** */
    tmp = MKT2(env, esock_atom_mtu, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_MTU_DISCOVER => IP_MTU_DISCOVER *** */
#if defined(IP_MTU_DISCOVER)
    tmp = MKT2(env, esock_atom_mtu_discover, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_mtu_discover, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_MULTICAST_ALL => IP_MULTICAST_ALL *** */
#if defined(IP_MULTICAST_ALL)
    tmp = MKT2(env, esock_atom_multicast_all, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_multicast_all, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_MULTICAST_IF => IP_MULTICAST_IF *** */
#if defined(IP_MULTICAST_IF)
    tmp = MKT2(env, esock_atom_multicast_if, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_multicast_if, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_MULTICAST_LOOP => IP_MULTICAST_LOOP *** */
#if defined(IP_MULTICAST_LOOP)
    tmp = MKT2(env, esock_atom_multicast_loop, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_multicast_loop, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_MULTICAST_TTL => IP_MULTICAST_TTL *** */
#if defined(IP_MULTICAST_TTL)
    tmp = MKT2(env, esock_atom_multicast_ttl, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_multicast_ttl, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_NODEFRAG => IP_NODEFRAG *** */
#if defined(IP_NODEFRAG)
    tmp = MKT2(env, esock_atom_nodefrag, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_nodefrag, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_OPTIONS => IP_OPTIONS *** */
    tmp = MKT2(env, esock_atom_options, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_PKTINFO => IP_PKTINFO *** */
#if defined(IP_PKTINFO)
    tmp = MKT2(env, esock_atom_pktinfo, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_pktinfo, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_RECVDSTADDR => IP_RECVDSTADDR *** */
#if defined(IP_RECVDSTADDR)
    tmp = MKT2(env, esock_atom_recvdstaddr, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_recvdstaddr, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_RECVERR => IP_RECVERR *** */
#if defined(IP_RECVERR)
    tmp = MKT2(env, esock_atom_recverr, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_recverr, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_RECVIF => IP_RECVIF *** */
#if defined(IP_RECVIF)
    tmp = MKT2(env, esock_atom_recvif, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_recvif, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_RECVOPTS => IP_RECVOPTS *** */
#if defined(IP_RECVOPTS)
    tmp = MKT2(env, esock_atom_recvopts, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_recvopts, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_RECVORIGDSTADDR => IP_RECVORIGDSTADDR *** */
#if defined(IP_RECVORIGDSTADDR)
    tmp = MKT2(env, esock_atom_recvorigdstaddr, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_recvorigdstaddr, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_RECVTOS => IP_RECVTOS *** */
#if defined(IP_RECVTOS)
    tmp = MKT2(env, esock_atom_recvtos, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_recvtos, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_RECVTTL => IP_RECVTTL *** */
#if defined(IP_RECVTTL)
    tmp = MKT2(env, esock_atom_recvttl, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_recvttl, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_RETOPTS => IP_RETOPTS *** */
#if defined(IP_RETOPTS)
    tmp = MKT2(env, esock_atom_retopts, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_retopts, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_ROUTER_ALERT => IP_ROUTER_ALERT *** */
#if defined(IP_ROUTER_ALERT)
    tmp = MKT2(env, esock_atom_router_alert, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_router_alert, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_SENDSRCADDR => IP_SENDSRCADDR *** */
#if defined(IP_SENDSRCADDR)
    tmp = MKT2(env, esock_atom_sendsrcaddr, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_sendsrcaddr, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_TOS => IP_TOS *** */
#if defined(IP_TOS)
    tmp = MKT2(env, esock_atom_tos, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_tos, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_TRANSPARENT => IP_TRANSPARENT *** */
#if defined(IP_TRANSPARENT)
    tmp = MKT2(env, esock_atom_transparent, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_transparent, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_TTL => IP_TTL *** */
#if defined(IP_TTL)
    tmp = MKT2(env, esock_atom_ttl, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_ttl, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IP_UNBLOCK_SOURCE => IP_UNBLOCK_SOURCE *** */
#if defined(IP_UNBLOCK_SOURCE)
    tmp = MKT2(env, esock_atom_unblock_source, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_unblock_source, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    TARRAY_TOLIST(opts, env, &optsL);
    
    return optsL;
}
#endif



#if !defined(__WIN32__)
static
ERL_NIF_TERM nsupports_options_ipv6(ErlNifEnv* env)
{
    SocketTArray opts = TARRAY_CREATE(128);
    ERL_NIF_TERM tmp, optsL;


    /* *** SOCKET_OPT_IPV6_ADDRFORM => IPV6_ADDRFORM *** */
#if defined(IPV6_ADDRFORM)
    tmp = MKT2(env, esock_atom_addrform, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_addrform, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_ADD_MEMBERSHIP => IPV6_ADD_MEMBERSHIP *** */
#if defined(IPV6_ADD_MEMBERSHIP)
    tmp = MKT2(env, esock_atom_add_membership, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_add_membership, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_AUTHHDR => IPV6_AUTHHDR *** */
#if defined(IPV6_AUTHHDR)
    tmp = MKT2(env, esock_atom_authhdr, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_authhdr, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_AUTH_LEVEL => IPV6_AUTH_LEVEL *** */
    tmp = MKT2(env, esock_atom_auth_level, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_CHECKSUM => IPV6_CHECKSUM *** */
    tmp = MKT2(env, esock_atom_checksum, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_DROP_MEMBERSHIP => IPV6_DROP_MEMBERSHIP *** */
#if defined(IPV6_DROP_MEMBERSHIP)
    tmp = MKT2(env, esock_atom_drop_membership, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_drop_membership, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_DSTOPTS => IPV6_DSTOPTS *** */
#if defined(IPV6_DSTOPTS)
    tmp = MKT2(env, esock_atom_dstopts, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_dstopts, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_ESP_NETWORK_LEVEL => IPV6_ESP_NETWORK_LEVEL *** */
    tmp = MKT2(env, esock_atom_esp_network_level, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_ESP_TRANS_LEVEL => IPV6_ESP_TRANS_LEVEL *** */
    tmp = MKT2(env, esock_atom_esp_trans_level, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_FAITH => IPV6_FAITH *** */
    tmp = MKT2(env, esock_atom_faith, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_FLOWINFO => IPV6_FLOWINFO *** */
#if defined(IPV6_FLOWINFO)
    tmp = MKT2(env, esock_atom_flowinfo, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_flowinfo, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_HOPLIMIT => IPV6_HOPLIMIT *** */
#if defined(IPV6_HOPLIMIT)
    tmp = MKT2(env, esock_atom_hoplimit, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_hoplimit, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_HOPOPTS => IPV6_HOPOPTS *** */
#if defined(IPV6_HOPOPTS)
    tmp = MKT2(env, esock_atom_hopopts, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_hopopts, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_IPCOMP_LEVEL => IPV6_IPCOMP_LEVEL *** */
    tmp = MKT2(env, esock_atom_ipcomp_level, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_JOIN_GROUP => IPV6_JOIN_GROUP *** */
    tmp = MKT2(env, esock_atom_join_group, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_LEAVE_GROUP => IPV6_LEAVE_GROUP *** */
    tmp = MKT2(env, esock_atom_leave_group, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_MTU => IPV6_MTU *** */
#if defined(IPV6_MTU)
    tmp = MKT2(env, esock_atom_mtu, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_mtu, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_MTU_DISCOVER => IPV6_MTU_DISCOVER *** */
#if defined(IPV6_MTU_DISCOVER)
    tmp = MKT2(env, esock_atom_mtu_discover, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_mtu_discover, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_MULTICAST_HOPS => IPV6_MULTICAST_HOPS *** */
#if defined(IPV6_MULTICAST_HOPS)
    tmp = MKT2(env, esock_atom_multicast_hops, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_multicast_hops, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_MULTICAST_IF => IPV6_MULTICAST_IF *** */
#if defined(IPV6_MULTICAST_IF)
    tmp = MKT2(env, esock_atom_multicast_if, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_multicast_if, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_MULTICAST_LOOP => IPV6_MULTICAST_LOOP *** */
#if defined(IPV6_MULTICAST_LOOP)
    tmp = MKT2(env, esock_atom_multicast_loop, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_multicast_loop, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_PORTRANGE => IPV6_PORTRANGE *** */
    tmp = MKT2(env, esock_atom_portrange, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_PKTOPTIONS => IPV6_PKTOPTIONS *** */
    tmp = MKT2(env, esock_atom_pktoptions, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_RECVERR => IPV6_RECVERR *** */
#if defined(IPV6_RECVERR)
    tmp = MKT2(env, esock_atom_recverr, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_recverr, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_RECVPKTINFO => IPV6_RECVPKTINFO *** */
#if defined(IPV6_RECVPKTINFO)
    tmp = MKT2(env, esock_atom_recvpktinfo, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_recvpktinfo, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_RECVTCLASS => IPV6_RECVTCLASS *** */
    tmp = MKT2(env, esock_atom_recvtclass, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_ROUTER_ALERT => IPV6_ROUTER_ALERT *** */
#if defined(IPV6_ROUTER_ALERT)
    tmp = MKT2(env, esock_atom_router_alert, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_router_alert, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_RTHDR => IPV6_RTHDR *** */
#if defined(IPV6_RTHDR)
    tmp = MKT2(env, esock_atom_rthdr, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_rthdr, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_TCLASS => IPV6_TCLASS *** */
    tmp = MKT2(env, esock_atom_tclass, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_UNICAST_HOPS => IPV6_UNICAST_HOPS *** */
#if defined(IPV6_UNICAST_HOPS)
    tmp = MKT2(env, esock_atom_unicast_hops, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_unicast_hops, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_USE_MIN_MTU => IPV6_USE_MIN_MTU *** */
    tmp = MKT2(env, esock_atom_use_min_mtu, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_IPV6_V6ONLY => IPV6_V6ONLY *** */
#if defined(IPV6_V6ONLY)
    tmp = MKT2(env, esock_atom_v6only, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_v6only, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    TARRAY_TOLIST(opts, env, &optsL);
    
    return optsL;
}
#endif



#if !defined(__WIN32__)
static
ERL_NIF_TERM nsupports_options_tcp(ErlNifEnv* env)
{
    SocketTArray opts = TARRAY_CREATE(32);
    ERL_NIF_TERM tmp, optsL;


    /* *** SOCKET_OPT_TCP_CONGESTION => TCP_CONGESTION *** */
#if defined(TCP_CONGESTION)
    tmp = MKT2(env, esock_atom_congestion, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_congestion, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_TCP_CORK => TCP_CORK *** */
#if defined(TCP_CORK)
    tmp = MKT2(env, esock_atom_cork, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_cork, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_TCP_INFO => TCP_INFO *** */
    tmp = MKT2(env, esock_atom_info, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_TCP_KEEPCNT => TCP_KEEPCNT *** */
    tmp = MKT2(env, esock_atom_keepcnt, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_TCP_KEEPIDLE => TCP_KEEPIDLE *** */
    tmp = MKT2(env, esock_atom_keepidle, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_TCP_KEEPINTVL => TCP_KEEPINTVL *** */
    tmp = MKT2(env, esock_atom_keepintvl, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_TCP_MAXSEG => TCP_MAXSEG *** */
#if defined(TCP_MAXSEG)
    tmp = MKT2(env, esock_atom_maxseg, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_maxseg, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_TCP_MD5SIG => TCP_MD5SIG *** */
    tmp = MKT2(env, esock_atom_md5sig, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_TCP_NODELAY => TCP_NODELAY *** */
#if defined(TCP_NODELAY)
    tmp = MKT2(env, esock_atom_nodelay, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_nodelay, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_TCP_NOOPT => TCP_NOOPT *** */
    tmp = MKT2(env, esock_atom_noopt, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_TCP_NOPUSH => TCP_NOPUSH *** */
    tmp = MKT2(env, esock_atom_nopush, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_TCP_SYNCNT => TCP_SYNCNT *** */
    tmp = MKT2(env, esock_atom_syncnt, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_TCP_USER_TIMEOUT => TCP_USER_TIMEOUT *** */
    tmp = MKT2(env, esock_atom_user_timeout, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    TARRAY_TOLIST(opts, env, &optsL);
    
    return optsL;
}
#endif



#if !defined(__WIN32__)
static
ERL_NIF_TERM nsupports_options_udp(ErlNifEnv* env)
{
    SocketTArray opts = TARRAY_CREATE(8);
    ERL_NIF_TERM tmp, optsL;


    /* *** SOCKET_OPT_UDP_CORK => UDP_CORK *** */
#if defined(UDP_CORK)
    tmp = MKT2(env, esock_atom_cork, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_cork, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    TARRAY_TOLIST(opts, env, &optsL);
    
    return optsL;
}
#endif



#if !defined(__WIN32__)
static
ERL_NIF_TERM nsupports_options_sctp(ErlNifEnv* env)
{
    SocketTArray opts = TARRAY_CREATE(64);
    ERL_NIF_TERM tmp, optsL;


    /* *** SOCKET_OPT_SCTP_ADAPTION_LAYER => SCTP_ADAPTION_LAYER *** */
    tmp = MKT2(env, esock_atom_adaption_layer, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_ASSOCINFO => SCTP_ASSOCINFO *** */
#if defined(SCTP_ASSOCINFO)
    tmp = MKT2(env, esock_atom_associnfo, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_associnfo, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_AUTH_ACTIVE_KEY => SCTP_AUTH_ACTIVE_KEY *** */
    tmp = MKT2(env, esock_atom_auth_active_key, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_AUTH_ASCONF => SCTP_AUTH_ASCONF *** */
    tmp = MKT2(env, esock_atom_auth_asconf, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_AUTH_CHUNK => SCTP_AUTH_CHUNK *** */
    tmp = MKT2(env, esock_atom_auth_chunk, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_AUTH_DELETE_KEY => SCTP_AUTH_DELETE_KEY *** */
    tmp = MKT2(env, esock_atom_auth_delete_key, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_AUTH_KEY => SCTP_AUTH_KEY *** */
    tmp = MKT2(env, esock_atom_auth_key, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_AUTOCLOSE => SCTP_AUTOCLOSE *** */
#if defined(SCTP_AUTOCLOSE)
    tmp = MKT2(env, esock_atom_autoclose, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_autoclose, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_CONTEXT => SCTP_CONTEXT *** */
    tmp = MKT2(env, esock_atom_context, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_DEFAULT_SEND_PARAMS => SCTP_DEFAULT_SEND_PARAMS *** */
    tmp = MKT2(env, esock_atom_default_send_params, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_DELAYED_ACK_TIME => SCTP_DELAYED_ACK_TIME *** */
    tmp = MKT2(env, esock_atom_delayed_ack_time, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_DISABLE_FRAGMENTS => SCTP_DISABLE_FRAGMENTS *** */
#if defined(SCTP_DISABLE_FRAGMENTS)
    tmp = MKT2(env, esock_atom_disable_fragments, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_disable_fragments, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_HMAC_IDENT => SCTP_HMAC_IDENT *** */
    tmp = MKT2(env, esock_atom_hmac_ident, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_EVENTS => SCTP_EVENTS *** */
#if defined(SCTP_EVENTS)
    tmp = MKT2(env, esock_atom_events, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_events, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_EXPLICIT_EOR => SCTP_EXPLICIT_EOR *** */
    tmp = MKT2(env, esock_atom_explicit_eor, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_FRAGMENT_INTERLEAVE => SCTP_FRAGMENT_INTERLEAVE *** */
    tmp = MKT2(env, esock_atom_fragment_interleave, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_GET_PEER_ADDR_INFO => SCTP_GET_PEER_ADDR_INFO *** */
    tmp = MKT2(env, esock_atom_get_peer_addr_info, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_INITMSG => SCTP_INITMSG *** */
#if defined(SCTP_INITMSG)
    tmp = MKT2(env, esock_atom_initmsg, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_initmsg, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_I_WANT_MAPPED_V4_ADDR => SCTP_I_WANT_MAPPED_V4_ADDR *** */
    tmp = MKT2(env, esock_atom_i_want_mapped_v4_addr, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_LOCAL_AUTH_CHUNKS => SCTP_LOCAL_AUTH_CHUNKS *** */
    tmp = MKT2(env, esock_atom_local_auth_chunks, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_MAXSEG => SCTP_MAXSEG *** */
#if defined(SCTP_MAXSEG)
    tmp = MKT2(env, esock_atom_maxseg, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_maxseg, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_MAXBURST => SCTP_MAXBURST *** */
    tmp = MKT2(env, esock_atom_maxburst, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_NODELAY => SCTP_NODELAY *** */
#if defined(SCTP_NODELAY)
    tmp = MKT2(env, esock_atom_nodelay, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_nodelay, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_PARTIAL_DELIVERY_POINT => SCTP_PARTIAL_DELIVERY_POINT *** */
    tmp = MKT2(env, esock_atom_partial_delivery_point, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_PEER_ADDR_PARAMS => SCTP_PEER_ADDR_PARAMS *** */
    tmp = MKT2(env, esock_atom_peer_addr_params, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_PEER_AUTH_CHUNKS => SCTP_PEER_AUTH_CHUNKS *** */
    tmp = MKT2(env, esock_atom_peer_auth_chunks, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_PRIMARY_ADDR => SCTP_PRIMARY_ADDR *** */
    tmp = MKT2(env, esock_atom_primary_addr, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_RESET_STREAMS => SCTP_RESET_STREAMS *** */
    tmp = MKT2(env, esock_atom_reset_streams, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_RTOINFO => SCTP_RTOINFO *** */
#if defined(SCTP_RTOINFO)
    tmp = MKT2(env, esock_atom_rtoinfo, esock_atom_true);
#else
    tmp = MKT2(env, esock_atom_rtoinfo, esock_atom_false);
#endif
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_SET_PEER_PRIMARY_ADDR => SCTP_SET_PEER_PRIMARY_ADDR *** */
    tmp = MKT2(env, esock_atom_set_peer_primary_addr, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_STATUS => SCTP_STATUS *** */
    tmp = MKT2(env, esock_atom_status, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    /* *** SOCKET_OPT_SCTP_USE_EXT_RECVINFO => SCTP_USE_EXT_RECVINFO *** */
    tmp = MKT2(env, esock_atom_use_ext_recvinfo, esock_atom_false);
    TARRAY_ADD(opts, tmp);


    TARRAY_TOLIST(opts, env, &optsL);
    
    return optsL;
}
#endif



#if !defined(__WIN32__)
static
ERL_NIF_TERM nsupports_sctp(ErlNifEnv* env)
{
    ERL_NIF_TERM supports;

#if defined(HAVE_SCTP)
    supports = esock_atom_true;
#else
    supports = esock_atom_false;
#endif

    return supports;
}
#endif



#if !defined(__WIN32__)
static
ERL_NIF_TERM nsupports_ipv6(ErlNifEnv* env)
{
    ERL_NIF_TERM supports;

    /* Is this (test) really sufficient for testing if we support IPv6? */
#if defined(HAVE_IPV6)
    supports = esock_atom_true;
#else
    supports = esock_atom_false;
#endif

    return supports;
}
#endif



#if !defined(__WIN32__)
static
ERL_NIF_TERM nsupports_local(ErlNifEnv* env)
{
    ERL_NIF_TERM supports;

#if defined(AF_LOCAL)
    supports = esock_atom_true;
#else
    supports = esock_atom_false;
#endif

    return supports;
}
#endif



/* ----------------------------------------------------------------------
 * nif_open
 *
 * Description:
 * Create an endpoint for communication.
 *
 * Arguments:
 * Domain   - The domain, for example 'inet'
 * Type     - Type of socket, for example 'stream'
 * Protocol - The protocol, for example 'tcp'
 * Extra    - A map with "obscure" options.
 *            Currently the only allowed option is netns (network namespace).
 *            This is *only* allowed on linux!
 *            We sould also use this for the fd value, in case we should use
 *            an already existing (file) descriptor.
 */
static
ERL_NIF_TERM nif_open(ErlNifEnv*         env,
                      int                argc,
                      const ERL_NIF_TERM argv[])
{
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    int          edomain, etype, eproto;
    int          domain, type, proto;
    char*        netns;
    ERL_NIF_TERM emap;
    ERL_NIF_TERM result;

    SGDBG( ("SOCKET", "nif_open -> entry with %d args\r\n", argc) );
    
    /* Extract arguments and perform preliminary validation */

    if ((argc != 4) ||
        !GET_INT(env, argv[0], &edomain) ||
        !GET_INT(env, argv[1], &etype) ||
        !IS_MAP(env,  argv[3])) {
        return enif_make_badarg(env);
    }
    eproto = argv[2];
    emap   = argv[3];

    SGDBG( ("SOCKET", "nif_open -> "
            "\r\n   edomain: %T"
            "\r\n   etype:   %T"
            "\r\n   eproto:  %T"
            "\r\n   extra:   %T"
            "\r\n", argv[0], argv[1], eproto, emap) );

    if (!edomain2domain(edomain, &domain)) {
        SGDBG( ("SOCKET", "nif_open -> invalid domain: %d\r\n", edomain) );
        return esock_make_error(env, esock_atom_einval);
    }

    if (!etype2type(etype, &type)) {
        SGDBG( ("SOCKET", "nif_open -> invalid type: %d\r\n", etype) );
        return esock_make_error(env, esock_atom_einval);
    }

    if (!eproto2proto(env, eproto, &proto)) {
        SGDBG( ("SOCKET", "nif_open -> invalid protocol: %d\r\n", eproto) );
        return esock_make_error(env, esock_atom_einval);
    }

#ifdef HAVE_SETNS
    /* We *currently* only support one extra option: netns */
    if (!emap2netns(env, emap, &netns)) {
        SGDBG( ("SOCKET", "nif_open -> namespace: %s\r\n", netns) );
        return enif_make_badarg(env);
    }
#else
    netns = NULL;
#endif


    result = nopen(env, domain, type, proto, netns);

    SGDBG( ("SOCKET", "nif_open -> done with result: "
           "\r\n   %T"
           "\r\n", result) );

    return result;

#endif // if defined(__WIN32__)
}


/* nopen - create an endpoint for communication
 *
 * Assumes the input has been validated.
 *
 * Normally we want debugging on (individual) sockets to be controlled
 * by the sockets own debug flag. But since we don't even have a socket
 * yet, we must use the global debug flag.
 */
#if !defined(__WIN32__)

static
ERL_NIF_TERM nopen(ErlNifEnv* env,
                   int domain, int type, int protocol,
                   char* netns)
{
    ESockDescriptor* descP;
    ERL_NIF_TERM     res;
    int              proto = protocol, save_errno = 0;
    SOCKET           sock;
    HANDLE           event;
#ifdef HAVE_SETNS
    int              current_ns = 0;
#endif

    SGDBG( ("SOCKET", "nopen -> entry with"
            "\r\n   domain:   %d"
            "\r\n   type:     %d"
            "\r\n   protocol: %d"
            "\r\n   netns:    %s"
            "\r\n", domain, type, protocol, ((netns == NULL) ? "NULL" : netns)) );

#ifdef HAVE_SETNS
    if ((netns != NULL) &&
        !change_network_namespace(netns, &current_ns, &save_errno))
        return esock_make_error_errno(env, save_errno);
#endif

    if ((sock = sock_open(domain, type, proto)) == INVALID_SOCKET)
        return esock_make_error_errno(env, sock_errno());

    SGDBG( ("SOCKET", "nopen -> open success: %d\r\n", sock) );


    /* NOTE that if the protocol = 0 (default) and the domain is not
     * local (AF_LOCAL) we need to explicitly get the protocol here!
     */
    
    if ((proto == 0)
#if defined(AF_LOCAL)
        && (domain != AF_LOCAL)
#endif
        )
        if (!nopen_which_protocol(sock, &proto)) {
            if (proto == ESOCK_WHICH_PROTO_ERROR) {
                save_errno = sock_errno();
                while ((sock_close(sock) == INVALID_SOCKET) &&
                       (sock_errno() == EINTR));
                return esock_make_error_errno(env, save_errno);
            } else {
                while ((sock_close(sock) == INVALID_SOCKET) &&
                       (sock_errno() == EINTR));
                return esock_make_error(env, esock_atom_eafnosupport);
            }
        }


#ifdef HAVE_SETNS
    if ((netns != NULL) &&
        !restore_network_namespace(current_ns, sock, &save_errno))
        return esock_make_error_errno(env, save_errno);

    if (netns != NULL)
        FREE(netns);
#endif


    if ((event = sock_create_event(sock)) == INVALID_EVENT) {
        save_errno = sock_errno();
        while ((sock_close(sock) == INVALID_SOCKET) && (sock_errno() == EINTR));
        return esock_make_error_errno(env, save_errno);
    }

    SGDBG( ("SOCKET", "nopen -> event success: %d\r\n", event) );

    SET_NONBLOCKING(sock);


    /* Create and initiate the socket "descriptor" */
    if ((descP = alloc_descriptor(sock, event)) == NULL) {
        sock_close(sock);
        // Not sure if this is really the proper error, but...
        return enif_make_badarg(env);
    }

    descP->state    = SOCKET_STATE_OPEN;
    descP->domain   = domain;
    descP->type     = type;
    descP->protocol = proto;

    /* Does this apply to other types? Such as RAW?
     * Also, is this really correct? Should we not wait for bind?
     */
    if (type == SOCK_DGRAM) {
        descP->isReadable = TRUE;
        descP->isWritable = TRUE;
    }

    /*
     * Should we keep track of sockets (resources) in some way?
     * Doing it here will require mutex to ensure data integrity,
     * which will be costly. Send it somewhere?
     */
    res = enif_make_resource(env, descP);
    enif_release_resource(descP);

    /* Keep track of the creator
     * This should not be a problem, but just in case
     * the *open* function is used with the wrong kind
     * of environment...
     */
    if (enif_self(env, &descP->ctrlPid) == NULL)
        return esock_make_error(env, atom_exself);

    if (MONP("nopen -> ctrl",
             env, descP,
             &descP->ctrlPid,
             &descP->ctrlMon) != 0)
        return esock_make_error(env, atom_exmon);


    inc_socket(domain, type, protocol);

    return esock_make_ok2(env, res);
}


static
BOOLEAN_T nopen_which_protocol(SOCKET sock, int* proto)
{
#if defined(SO_PROTOCOL)
    int          val;
    SOCKOPTLEN_T valSz = sizeof(val);
    int          res;

    res = sock_getopt(sock, SOL_SOCKET, SO_PROTOCOL, &val, &valSz);

    if (res != 0) {
        *proto = ESOCK_WHICH_PROTO_ERROR;
        return FALSE;
    } else {
        *proto = val;
        return TRUE;
    }
#else
    *proto = ESOCK_WHICH_PROTO_UNSUP;
    return FALSE;
#endif
}


#endif // if !defined(__WIN32__)



#ifdef HAVE_SETNS
/* We should really have another API, so that we can return errno... */

/* *** change network namespace ***
 * Retreive the current namespace and set the new.
 * Return result and previous namespace if successfull.
 */
#if !defined(__WIN32__)
static
BOOLEAN_T change_network_namespace(char* netns, int* cns, int* err)
{
    int save_errno;
    int current_ns = 0;
    int new_ns     = 0;

    SGDBG( ("SOCKET", "change_network_namespace -> entry with"
            "\r\n   new ns: %s", netns) );

    if (netns != NULL) {
        current_ns = open("/proc/self/ns/net", O_RDONLY);
        if (current_ns == INVALID_SOCKET) {
            *cns = current_ns;
            *err = sock_errno();
            return FALSE;
        }
        new_ns = open(netns, O_RDONLY);
        if (new_ns == INVALID_SOCKET) {
            save_errno = sock_errno();
            while (close(current_ns) == INVALID_SOCKET &&
                   sock_errno() == EINTR);
            *cns = -1;
            *err = save_errno;
            return FALSE;
        }
        if (setns(new_ns, CLONE_NEWNET) != 0) {
            save_errno = sock_errno();
            while ((close(new_ns) == INVALID_SOCKET) &&
                   (sock_errno() == EINTR));
            while ((close(current_ns) == INVALID_SOCKET) &&
                   (sock_errno() == EINTR));
            *cns = -1;
            *err = save_errno;
            return FALSE;
        } else {
            while ((close(new_ns) == INVALID_SOCKET) &&
                   (sock_errno() == EINTR));
            *cns = current_ns;
            *err = 0;
            return TRUE;
        }
    } else {
        *cns = INVALID_SOCKET;
        *err = 0;
        return TRUE;
    }
}
#endif // if !defined(__WIN32__)


/* *** restore network namespace ***
 * Restore the previous namespace (see above).
 */
#if !defined(__WIN32__)
static
BOOLEAN_T restore_network_namespace(int ns, SOCKET sock, int* err)
{
    int save_errno;

    SGDBG( ("SOCKET", "restore_network_namespace -> entry with"
            "\r\n   ns: %d", ns) );

    if (ns != INVALID_SOCKET) {
        if (setns(ns, CLONE_NEWNET) != 0) {
            /* XXX Failed to restore network namespace.
             * What to do? Tidy up and return an error...
             * Note that the thread now might still be in the namespace.
             * Can this even happen? Should the emulator be aborted?
             */
            if (sock != INVALID_SOCKET)
                save_errno = sock_errno();
            while (close(sock) == INVALID_SOCKET &&
                   sock_errno() == EINTR);
            sock = INVALID_SOCKET;
            while (close(ns) == INVALID_SOCKET &&
                   sock_errno() == EINTR);
            *err = save_errno;
            return FALSE;
        } else {
            while (close(ns) == INVALID_SOCKET &&
                   sock_errno() == EINTR);
            *err = 0;
            return TRUE;
        }
  }

  *err = 0;
  return TRUE;
}
#endif // if !defined(__WIN32__)
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
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     eSockAddr;
    ESockAddress     sockAddr;
    unsigned int     addrLen;
    char*            xres;

    SGDBG( ("SOCKET", "nif_bind -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    if ((argc != 2) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }
    eSockAddr = argv[1];

    if (IS_CLOSED(descP) || IS_CLOSING(descP))
        return esock_make_error(env, atom_closed);
    
    SSDBG( descP,
           ("SOCKET", "nif_bind -> args when sock = %d (0x%lX)"
            "\r\n   Socket:   %T"
            "\r\n   SockAddr: %T"
            "\r\n", descP->sock, descP->state, argv[0], eSockAddr) );

    /* Make sure we are ready
     * Not sure how this would even happen, but...
     */
    if (descP->state != SOCKET_STATE_OPEN)
        return esock_make_error(env, atom_exbadstate);

    if ((xres = esock_decode_sockaddr(env, eSockAddr, &sockAddr, &addrLen)) != NULL)
        return esock_make_error_str(env, xres);
        
    return nbind(env, descP, &sockAddr, addrLen);

#endif // if defined(__WIN32__)
}


#if !defined(__WIN32__)
static
ERL_NIF_TERM nbind(ErlNifEnv*       env,
                   ESockDescriptor* descP,
                   ESockAddress*    sockAddrP,
                   unsigned int     addrLen)
{
    int port, ntohs_port;

    SSDBG( descP, ("SOCKET", "nbind -> try bind\r\n") );

    if (IS_SOCKET_ERROR(sock_bind(descP->sock,
                                  (struct sockaddr*) sockAddrP, addrLen))) {
        return esock_make_error_errno(env, sock_errno());
    }

    SSDBG( descP, ("SOCKET", "nbind -> bound - get port\r\n") );

    port = which_address_port(sockAddrP);
    SSDBG( descP, ("SOCKET", "nbind -> port: %d\r\n", port) );
    if (port == 0) {
        SOCKLEN_T len = sizeof(ESockAddress);
        sys_memzero((char *) sockAddrP, len);
        sock_name(descP->sock, &sockAddrP->sa, &len);
        port = which_address_port(sockAddrP);
    } else if (port == -1) {
        port = 0;
    }

    ntohs_port = sock_ntohs(port);
    
    SSDBG( descP, ("SOCKET", "nbind -> done with port = %d\r\n", ntohs_port) );

    return esock_make_ok2(env, MKI(env, ntohs_port));

}
#endif // if !defined(__WIN32__)




/* ----------------------------------------------------------------------
 * nif_connect
 *
 * Description:
 * Initiate a connection on a socket
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * SockAddr     - Socket Address of "remote" host.
 *                This is sockaddr(), which is either
 *                sockaddr_in4 or sockaddr_in6.
 */
static
ERL_NIF_TERM nif_connect(ErlNifEnv*         env,
                         int                argc,
                         const ERL_NIF_TERM argv[])
{
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     res, eSockAddr, sockRef;
    char*            xres;

    SGDBG( ("SOCKET", "nif_connect -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    sockRef = argv[0];
    if ((argc != 2) ||
        !enif_get_resource(env, sockRef, sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }
    eSockAddr = argv[1];

    SSDBG( descP,
           ("SOCKET", "nif_connect -> args when sock = %d:"
            "\r\n   Socket:   %T"
            "\r\n   SockAddr: %T"
            "\r\n", descP->sock, argv[0], eSockAddr) );
    
    if ((xres = esock_decode_sockaddr(env, eSockAddr,
                                      &descP->remote,
                                      &descP->addrLen)) != NULL) {
        return esock_make_error_str(env, xres);
    }


    /* Only a *!%&$*# would send an opened but non-connected socket
     * somewhere (before its actually usable), but just to be on the
     * safe side we do the best we can to avoid complications...
     */

    MLOCK(descP->readMtx);
    MLOCK(descP->writeMtx);
    MLOCK(descP->cfgMtx);

    res = nconnect(env, descP, sockRef);

    MUNLOCK(descP->cfgMtx);
    MUNLOCK(descP->writeMtx);
    MUNLOCK(descP->readMtx);

    return res;

#endif // if !defined(__WIN32__)
}


#if !defined(__WIN32__)
static
ERL_NIF_TERM nconnect(ErlNifEnv*       env,
                      ESockDescriptor* descP,
                      ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM res, ref;
    int          code, sres, save_errno = 0;

    /* 
     * Verify that we are where in the proper state
     */

    if (IS_CLOSED(descP) || IS_CLOSING(descP))
        return esock_make_error(env, atom_closed);
    
    if (!IS_OPEN(descP)) {
        SSDBG( descP, ("SOCKET", "nconnect -> not open\r\n") );
        return esock_make_error(env, atom_exbadstate);
    }

    if (IS_CONNECTED(descP)) {
        SSDBG( descP, ("SOCKET", "nconnect -> already connected\r\n") );
        return esock_make_error(env, atom_eisconn);
    }

    if (IS_CONNECTING(descP) && !is_connector(env, descP)) {
        SSDBG( descP, ("SOCKET", "nconnect -> already connecting\r\n") );
        return esock_make_error(env, esock_atom_einval);
    }
    

    /* 
     * And attempt to connect
     */

    code = sock_connect(descP->sock,
                        (struct sockaddr*) &descP->remote,
                        descP->addrLen);
    save_errno = sock_errno();

    SSDBG( descP, ("SOCKET", "nconnect -> connect result: %d, %d\r\n",
                   code, save_errno) );

    if (IS_SOCKET_ERROR(code)) {
        switch (save_errno) {
        case ERRNO_BLOCK:   /* Winsock2            */
        case EINPROGRESS:   /* Unix & OSE!!        */
            SSDBG( descP, ("SOCKET", "nconnect -> would block => select\r\n") );

            ref = MKREF(env);

            if (IS_CONNECTING(descP)) {
                /* Glitch */
                res = esock_make_ok2(env, ref);
            } else {

                /* First time here */

                if (enif_self(env, &descP->connPid) == NULL)
                    return esock_make_error(env, atom_exself);

                if (MONP("nconnect -> conn",
                         env, descP,
                         &descP->connPid,
                         &descP->connMon) != 0)
                    return esock_make_error(env, atom_exmon);

                descP->state = SOCKET_STATE_CONNECTING;

                if ((sres = esock_select_write(env, descP->sock, descP, NULL,
                                               sockRef, ref)) < 0) {
                    res = esock_make_error(env,
                                           MKT2(env,
                                                esock_atom_select_failed,
                                                MKI(env, sres)));
                } else {
                    res = esock_make_ok2(env, ref);
                }
            }
            break;

        case EISCONN:
            SSDBG( descP, ("SOCKET", "nconnect -> *already* connected\r\n") );
            {
                /* This is ***strange*** so make sure */
                int err = 0;
                if (!verify_is_connected(descP, &err)) {
                    descP->state = SOCKET_STATE_OPEN;  /* restore state */
                    res = esock_make_error_errno(env, err);
                } else {
                    descP->state = SOCKET_STATE_CONNECTED;
                    /* And just to be on the safe side, reset these */
                    enif_set_pid_undefined(&descP->connPid);
                    DEMONP("nconnect -> connected",
                           env, descP, &descP->connMon);
                    descP->isReadable = TRUE;
                    descP->isWritable = TRUE;
                    res = esock_atom_ok;
                }
            }
            break;

        default:
            SSDBG( descP, ("SOCKET", "nconnect -> other error(1): %d\r\n",
                           save_errno) );
            res = esock_make_error_errno(env, save_errno);
            break;
        }

    } else if (code == 0) {                 /* ok we are connected */

        SSDBG( descP, ("SOCKET", "nconnect -> connected\r\n") );

        descP->state      = SOCKET_STATE_CONNECTED;
        enif_set_pid_undefined(&descP->connPid);
        DEMONP("nconnect -> connected", env, descP, &descP->connMon);
        descP->isReadable = TRUE;
        descP->isWritable = TRUE;

        res = esock_atom_ok;

    } else {
        /* Do we really need this case? */

        SSDBG( descP, ("SOCKET", "nconnect -> other error(2): %d\r\n",
                       save_errno) );

        res = esock_make_error_errno(env, save_errno);
    }

    return res;

}
#endif // if !defined(__WIN32__)


/* ----------------------------------------------------------------------
 * nif_finalize_connection
 *
 * Description:
 * Make socket ready for input and output.
 * This function is called if we where made to wait when we called the
 * nif_connect function (we made a select, and the select message has
 * now been received).
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 */
static
ERL_NIF_TERM nif_finalize_connection(ErlNifEnv*         env,
                                     int                argc,
                                     const ERL_NIF_TERM argv[])
{
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;

    /* Extract arguments and perform preliminary validation */

    if ((argc != 1) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }

    return nfinalize_connection(env, descP);

#endif
}


/* *** nfinalize_connection ***
 * Perform the final check to verify a connection.
 */
#if !defined(__WIN32__)
static
ERL_NIF_TERM nfinalize_connection(ErlNifEnv*       env,
                                  ESockDescriptor* descP)
{
    int error;

    if (!IS_CONNECTING(descP))
        return esock_make_error(env, atom_enotconn);

    if (!verify_is_connected(descP, &error)) {
        descP->state = SOCKET_STATE_OPEN;  /* restore state */
        return esock_make_error_errno(env, error);
    }

    descP->state      = SOCKET_STATE_CONNECTED;
    enif_set_pid_undefined(&descP->connPid);
    DEMONP("nfinalize_connection -> connected", env, descP, &descP->connMon);
    descP->isReadable = TRUE;
    descP->isWritable = TRUE;

    return esock_atom_ok;
}
#endif


/* *** verify_is_connected ***
 * Check if a connection has been established.
 */
#if !defined(__WIN32__)
static
BOOLEAN_T verify_is_connected(ESockDescriptor* descP, int* err)
{
    /*
     * *** This is strange ***
     *
     * This *should* work on Windows NT too, but doesn't.
     * An bug in Winsock 2.0 for Windows NT?
     *
     * See "Unix Netwok Programming", W.R.Stevens, p 412 for a
     * discussion about Unix portability and non blocking connect.
     */

#ifndef SO_ERROR
    
    int sz, code;

    sz = sizeof(descP->remote);
    sys_memzero((char *) &descP->remote, sz);
    code = sock_peer(desc->sock,
                     (struct sockaddr*) &descP->remote, &sz);

    if (IS_SOCKET_ERROR(code)) {
        *err = sock_errno();
        return FALSE;
    }

#else

    int          error = 0;             /* Has to be initiated, we check it */
    unsigned int sz    = sizeof(error); /* even if we get -1                */
    int          code  = sock_getopt(descP->sock,
                                     SOL_SOCKET, SO_ERROR,
                                     (void *)&error, &sz);

    if ((code < 0) || error) {
        *err = error;
        return FALSE;
    }

#endif /* SO_ERROR */

    *err = 0;

    return TRUE;
}
#endif



/* *** is_connector ***
 * Check if the current process is the connector process.
 */
#if !defined(__WIN32__)
static
BOOLEAN_T is_connector(ErlNifEnv*       env,
                       ESockDescriptor* descP)
{
    ErlNifPid caller;

    if (enif_self(env, &caller) == NULL)
        return FALSE;

    if (COMPARE_PIDS(&descP->connPid, &caller) == 0)
        return TRUE;
    else
        return FALSE;
    
}
#endif



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
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    int              backlog;

    SGDBG( ("SOCKET", "nif_listen -> entry with argc: %d\r\n", argc) );
    
    /* Extract arguments and perform preliminary validation */

    if ((argc != 2) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP) ||
        !GET_INT(env, argv[1], &backlog)) {
        return enif_make_badarg(env);
    }

    SSDBG( descP,
           ("SOCKET", "nif_listen -> args when sock = %d:"
            "\r\n   Socket:  %T"
            "\r\n   backlog: %d"
            "\r\n", descP->sock, argv[0], backlog) );
    
    return nlisten(env, descP, backlog);

#endif // if defined(__WIN32__)
}



#if !defined(__WIN32__)
static
ERL_NIF_TERM nlisten(ErlNifEnv*       env,
                     ESockDescriptor* descP,
                     int              backlog)
{
    
    /* 
     * Verify that we are where in the proper state
     */

    if (IS_CLOSED(descP) || IS_CLOSING(descP))
        return esock_make_error(env, atom_closed);
    
    if (descP->state == SOCKET_STATE_CLOSED)
        return esock_make_error(env, atom_exbadstate);

    if (!IS_OPEN(descP))
        return esock_make_error(env, atom_exbadstate);


    /* 
     * And attempt to make socket listening
     */
    
    if (IS_SOCKET_ERROR(sock_listen(descP->sock, backlog)))
        return esock_make_error_errno(env, sock_errno());

    descP->state = SOCKET_STATE_LISTENING;

    return esock_atom_ok;

}
#endif // if !defined(__WIN32__)



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
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef, ref, res;

    SGDBG( ("SOCKET", "nif_accept -> entry with argc: %d\r\n", argc) );
    
    /* Extract arguments and perform preliminary validation */

    sockRef = argv[0];
    if ((argc != 2) ||
        !enif_get_resource(env, sockRef, sockets, (void**) &descP)) {
        return enif_make_badarg(env); 
    }
    ref = argv[1];
    
    MLOCK(descP->accMtx);

    SSDBG( descP,
           ("SOCKET", "nif_accept -> args when sock = %d:"
            "\r\n   Socket:                %T"
            "\r\n   ReqRef:                %T"
            "\r\nwhen"
            "\r\n   State:                 %s"
            "\r\n   Current Acceptor Addr: 0x%lX"
            "\r\n   Current Acceptor pid:  %T"
            "\r\n   Current Acceptor mon:  %T"
            "\r\n   Current Acceptor env:  0x%lX"
            "\r\n   Current Acceptor ref:  %T"
            "\r\n",
            descP->sock,
            sockRef, ref,
            ((descP->state == SOCKET_STATE_LISTENING) ? "listening" :
             ((descP->state == SOCKET_STATE_ACCEPTING) ? "accepting" : "other")),
            descP->currentAcceptorP,
            descP->currentAcceptor.pid,
            esock_make_monitor_term(env, &descP->currentAcceptor.mon),
            descP->currentAcceptor.env,
            descP->currentAcceptor.ref) );

    res = naccept_erts(env, descP, sockRef, ref);

    MUNLOCK(descP->accMtx);

    return res;

#endif // if defined(__WIN32__)
}


#if !defined(__WIN32__)
static
ERL_NIF_TERM naccept_erts(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     ref)
{
    ERL_NIF_TERM res;

    if (IS_CLOSED(descP) || IS_CLOSING(descP))
        return esock_make_error(env, atom_closed);
    
    switch (descP->state) {
    case SOCKET_STATE_LISTENING:
        res = naccept_listening(env, descP, sockRef, ref);
        break;

    case SOCKET_STATE_ACCEPTING:
        res = naccept_accepting(env, descP, sockRef, ref);
        break;

    default:
        res = esock_make_error(env, esock_atom_einval);
        break;
    }

    return res;
}
#endif // if !defined(__WIN32__)


/* *** naccept_listening ***
 *
 * We have no active acceptor (and therefor no acceptors in queue).
 */
#if !defined(__WIN32__)
static
ERL_NIF_TERM naccept_listening(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     accRef)
{
    ESockAddress  remote;
    unsigned int  n;
    SOCKET        accSock;
    int           save_errno;
    ErlNifPid     caller;
    ERL_NIF_TERM  res;

    SSDBG( descP, ("SOCKET", "naccept_listening -> get caller\r\n") );

    if (enif_self(env, &caller) == NULL)
        return esock_make_error(env, atom_exself);

    n = sizeof(remote);
    sys_memzero((char *) &remote, n);
    SSDBG( descP, ("SOCKET", "naccept_listening -> try accept\r\n") );
    accSock = sock_accept(descP->sock, (struct sockaddr*) &remote, &n);
    if (accSock == INVALID_SOCKET) {

        save_errno = sock_errno();

        SSDBG( descP,
               ("SOCKET",
                "naccept_listening -> accept failed (%d)\r\n", save_errno) );

        res = naccept_listening_error(env, descP, sockRef, accRef,
                                      caller, save_errno);

    } else {

        /*
         * We got one
         */

        SSDBG( descP, ("SOCKET", "naccept_listening -> success\r\n") );

        res = naccept_listening_accept(env, descP, accSock, caller, &remote);

    }

    return res;
}


/* *** naccept_listening_error ***
 *
 * The accept call resultet in an error - handle it.
 * There are only two cases: 
 * 1) BLOCK => Attempt a "retry"
 * 2) Other => Return the value (converted to an atom)
 */
static
ERL_NIF_TERM naccept_listening_error(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     sockRef,
                                     ERL_NIF_TERM     accRef,
                                     ErlNifPid        caller,
                                     int              save_errno)
{
    ERL_NIF_TERM res;

    if (save_errno == ERRNO_BLOCK) {

        /* *** Try again later *** */

        SSDBG( descP,
               ("SOCKET", "naccept_listening_error -> would block\r\n") );

        descP->currentAcceptor.pid = caller;
        if (MONP("naccept_listening -> current acceptor",
                 env, descP,
                 &descP->currentAcceptor.pid,
                 &descP->currentAcceptor.mon) != 0) {
            enif_set_pid_undefined(&descP->currentAcceptor.pid);
            res = esock_make_error(env, atom_exmon);
        } else {
            ESOCK_ASSERT(!descP->currentAcceptor.env);
            descP->currentAcceptor.env = esock_alloc_env("current acceptor");
            descP->currentAcceptor.ref = CP_TERM(descP->currentAcceptor.env,
                                                 accRef);
            descP->currentAcceptorP    = &descP->currentAcceptor;
            res = naccept_busy_retry(env, descP,
                                     sockRef, accRef,
                                     NULL, SOCKET_STATE_ACCEPTING);
        }
    } else {
        SSDBG( descP,
               ("SOCKET",
                "naccept_listening -> errno: %d\r\n", save_errno) );
        res = esock_make_error_errno(env, save_errno);
    }

    return res;
}


/* *** naccept_listening_accept ***
 *
 * The accept call was successful (accepted) - handle the new connection.
 */
static
ERL_NIF_TERM naccept_listening_accept(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      SOCKET           accSock,
                                      ErlNifPid        caller,
                                      ESockAddress*    remote)
{
    ERL_NIF_TERM res;

    naccept_accepted(env, descP, accSock, caller, remote, &res);
    
    return res;
}
#endif // if !defined(__WIN32__)



/* *** naccept_accepting ***
 *
 * We have an active acceptor and possibly acceptors waiting in queue.
 * If the pid of the calling process is not the pid of the "current process",
 * push the requester onto the (acceptor) queue.
 */
#if !defined(__WIN32__)
static
ERL_NIF_TERM naccept_accepting(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     ref)
{
    ErlNifPid     caller;
    ERL_NIF_TERM  res;

    SSDBG( descP, ("SOCKET", "naccept_accepting -> get caller\r\n") );

    if (enif_self(env, &caller) == NULL)
        return esock_make_error(env, atom_exself);

    SSDBG( descP, ("SOCKET", "naccept_accepting -> check: "
                   "are caller current acceptor:"
                   "\r\n   Caller:  %T"
                   "\r\n   Current: %T"
                   "\r\n", caller, descP->currentAcceptor.pid) );

    if (COMPARE_PIDS(&descP->currentAcceptor.pid, &caller) == 0) {

        SSDBG( descP,
               ("SOCKET", "naccept_accepting -> current acceptor\r\n") );

        res = naccept_accepting_current(env, descP, sockRef, ref);

    } else {

        /* Not the "current acceptor", so (maybe) push onto queue */

        SSDBG( descP,
               ("SOCKET", "naccept_accepting -> *not* current acceptor\r\n") );

        res = naccept_accepting_other(env, descP, ref, caller);

    }

    return res;

}



/* *** naccept_accepting_current ***
 * Handles when the current acceptor makes another attempt.
 */
static
ERL_NIF_TERM naccept_accepting_current(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef,
                                       ERL_NIF_TERM     accRef)
{
    ESockAddress  remote;
    unsigned int  n;
    SOCKET        accSock;
    int           save_errno;
    ERL_NIF_TERM  res;

    SSDBG( descP, ("SOCKET", "naccept_accepting_current -> try accept\r\n") );
    n = sizeof(descP->remote);
    sys_memzero((char *) &remote, n);
    accSock = sock_accept(descP->sock, (struct sockaddr*) &remote, &n);
    if (accSock == INVALID_SOCKET) {

        save_errno = sock_errno();

        SSDBG( descP,
               ("SOCKET",
                "naccept_accepting_current -> accept failed: %d\r\n",
                save_errno) );

        res = naccept_accepting_current_error(env, descP, sockRef,
                                              accRef, save_errno);
                                              
    } else {

        SSDBG( descP, ("SOCKET", "naccept_accepting_current -> accepted\r\n") );
        
        res = naccept_accepting_current_accept(env, descP, sockRef,
                                               accSock, &remote);

    }

    return res;
}


/* *** naccept_accepting_current_accept ***
 * Handles when the current acceptor succeeded in its accept call - 
 * handle the new connection.
 */
static
ERL_NIF_TERM naccept_accepting_current_accept(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     sockRef,
                                              SOCKET           accSock,
                                              ESockAddress*    remote)
{
    ERL_NIF_TERM res;

    if (naccept_accepted(env, descP, accSock,
                         descP->currentAcceptor.pid, remote, &res)) {

        /* Clean out the old cobweb's before trying to invite a new spider */

        descP->currentAcceptor.ref = esock_atom_undefined;
        enif_set_pid_undefined(&descP->currentAcceptor.pid);
        esock_free_env("naccept_accepting_current_accept - "
                       "current-accept-env",
                       descP->currentAcceptor.env);
        descP->currentAcceptor.env =  NULL;

        if (!activate_next_acceptor(env, descP, sockRef)) {

            SSDBG( descP,
                   ("SOCKET",
                    "naccept_accepting_current_accept -> "
                    "no more writers\r\n") );

            descP->state               = SOCKET_STATE_LISTENING;

            descP->currentAcceptorP    = NULL;
            ESOCK_ASSERT(!descP->currentAcceptor.env);
            descP->currentAcceptor.env = NULL;
            MON_INIT(&descP->currentAcceptor.mon);
        }

    }

    return res;
}


/* *** naccept_accepting_current_error ***
 * The accept call of current acceptor resultet in an error - handle it.
 * There are only two cases: 
 * 1) BLOCK => Attempt a "retry"
 * 2) Other => Return the value (converted to an atom)
 */
static
ERL_NIF_TERM naccept_accepting_current_error(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     sockRef,
                                             ERL_NIF_TERM     opRef,
                                             int              save_errno)
{
    ESockRequestor req;
    ERL_NIF_TERM   res, reason;

    req.env = NULL;
    if (save_errno == ERRNO_BLOCK) {

        /*
         * Just try again, no real error, just a ghost trigger from poll,
         */

        SSDBG( descP,
               ("SOCKET",
                "naccept_accepting_current_error -> "
                "would block: try again\r\n") );

        res = naccept_busy_retry(env, descP, sockRef, opRef,
                                 &descP->currentAcceptor.pid,
                                 /* No state change */
                                 descP->state);

    } else {

        reason = MKA(env, erl_errno_id(save_errno));
        res    = esock_make_error(env, reason);

        while (acceptor_pop(env, descP, &req)) {
            SSDBG( descP,
                   ("SOCKET", "naccept_accepting_current_error -> abort %T\r\n",
                    req.pid) );
            esock_send_abort_msg(env, sockRef, req.ref, req.env,
                                 reason, &req.pid);
            req.env = NULL;
            DEMONP("naccept_accepting_current_error -> pop'ed writer",
                   env, descP, &req.mon);
        }

    }

    return res;
}


/* *** naccept_accepting_other ***
 * Handles when the another acceptor makes an attempt, which
 * results (maybe) in the request beeing pushed onto the 
 * acceptor queue.
 */
static
ERL_NIF_TERM naccept_accepting_other(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     ref,
                                     ErlNifPid        caller)
{
    ERL_NIF_TERM  result;

    if (!acceptor_search4pid(env, descP, &caller)) // Ugh! (&caller)
        result = acceptor_push(env, descP, caller, ref);
    else
        result = esock_make_error(env, esock_atom_eagain);
    
    return result;
}
#endif // if !defined(__WIN32__)



/* *** naccept_busy_retry ***
 *
 * Perform a retry select. If successful, set nextState.
 */
#if !defined(__WIN32__)
static
ERL_NIF_TERM naccept_busy_retry(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     sockRef,
                                ERL_NIF_TERM     accRef,
                                ErlNifPid*       pid,
                                unsigned int     nextState)
{
    int          sres;
    ERL_NIF_TERM res, reason;

    if ((sres = esock_select_read(env, descP->sock, descP, pid,
                                  sockRef, accRef)) < 0) {
        reason = MKT2(env, esock_atom_select_failed, MKI(env, sres));
        res    = esock_make_error(env, reason);
    } else {
        descP->state = nextState;
        res          = esock_make_error(env, esock_atom_eagain); // OK!!
    }

    return res;
}



/* *** naccept_accepted ***
 *
 * Generic function handling a successful accept.
 */
static
BOOLEAN_T naccept_accepted(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           SOCKET           accSock,
                           ErlNifPid        pid,
                           ESockAddress*    remote,
                           ERL_NIF_TERM*    result)
{
    ESockDescriptor* accDescP;
    HANDLE           accEvent;
    ERL_NIF_TERM     accRef;
    int              save_errno;

    /*
     * We got one
     */

    if ((accEvent = sock_create_event(accSock)) == INVALID_EVENT) {
        save_errno = sock_errno();
        while ((sock_close(accSock) == INVALID_SOCKET) &&
               (sock_errno() == EINTR));
        *result = esock_make_error_errno(env, save_errno);
        return FALSE;
    }

    if ((accDescP = alloc_descriptor(accSock, accEvent)) == NULL) {
        sock_close(accSock);
        *result = enif_make_badarg(env);
        return FALSE;
    }

    accDescP->domain   = descP->domain;
    accDescP->type     = descP->type;
    accDescP->protocol = descP->protocol;
    accDescP->rBufSz   = descP->rBufSz;  // Inherit buffer size
    accDescP->rNum     = descP->rNum;    // Inherit buffer uses
    accDescP->rNumCnt  = 0;
    accDescP->rCtrlSz  = descP->rCtrlSz; // Inherit buffer size
    accDescP->wCtrlSz  = descP->wCtrlSz; // Inherit buffer size

    accRef = enif_make_resource(env, accDescP);
    enif_release_resource(accDescP);

    accDescP->ctrlPid = pid;
    if (MONP("naccept_accepted -> ctrl",
             env, accDescP,
             &accDescP->ctrlPid,
             &accDescP->ctrlMon) != 0) {
        sock_close(accSock);
        enif_set_pid_undefined(&descP->ctrlPid);
        *result = esock_make_error(env, atom_exmon);
        return FALSE;
    }

    accDescP->remote = *remote;
    SET_NONBLOCKING(accDescP->sock);

    accDescP->state      = SOCKET_STATE_CONNECTED;
    accDescP->isReadable = TRUE;
    accDescP->isWritable = TRUE;

    *result = esock_make_ok2(env, accRef);

    return TRUE;
    
}
#endif // if !defined(__WIN32__)



/* ----------------------------------------------------------------------
 * nif_send
 *
 * Description:
 * Send a message on a socket
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * SendRef      - A unique id for this (send) request.
 * Data         - The data to send in the form of a IOVec.
 * Flags        - Send flags.
 */

static
ERL_NIF_TERM nif_send(ErlNifEnv*         env,
                      int                argc,
                      const ERL_NIF_TERM argv[])
{
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef, sendRef;
    ErlNifBinary     sndData;
    unsigned int     eflags;
    int              flags;
    ERL_NIF_TERM     res;

    SGDBG( ("SOCKET", "nif_send -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    if ((argc != 4) ||
        !GET_BIN(env, argv[2], &sndData) ||
        !GET_UINT(env, argv[3], &eflags)) {
        return enif_make_badarg(env);
    }
    sockRef = argv[0]; // We need this in case we send in case we send abort
    sendRef = argv[1];

    if (!enif_get_resource(env, sockRef, sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }

    SSDBG( descP,
           ("SOCKET", "nif_send -> args when sock = %d:"
            "\r\n   Socket:       %T"
            "\r\n   SendRef:      %T"
            "\r\n   Size of data: %d"
            "\r\n   eFlags:       %d"
            "\r\n", descP->sock, sockRef, sendRef, sndData.size, eflags) );

    if (!esendflags2sendflags(eflags, &flags))
        return enif_make_badarg(env);

    MLOCK(descP->writeMtx);

    /* We need to handle the case when another process tries
     * to write at the same time.
     * If the current write could not write its entire package
     * this time (resulting in an select). The write of the
     * other process must be made to wait until current
     * is done!
     */

    res = nsend(env, descP, sockRef, sendRef, &sndData, flags);

    MUNLOCK(descP->writeMtx);

    return res;

#endif // if defined(__WIN32__)
}



/* *** nsend ***
 *
 * Do the actual send.
 * Do some initial writer checks, do the actual send and then
 * analyze the result. If we are done, another writer may be
 * scheduled (if there is one in the writer queue).
 */
#if !defined(__WIN32__)
static
ERL_NIF_TERM nsend(ErlNifEnv*       env,
                   ESockDescriptor* descP,
                   ERL_NIF_TERM     sockRef,
                   ERL_NIF_TERM     sendRef,
                   ErlNifBinary*    sndDataP,
                   int              flags)
{
    int          save_errno;
    ssize_t      written;
    ERL_NIF_TERM writerCheck;

    if (!descP->isWritable)
        return enif_make_badarg(env);

    /* Check if there is already a current writer and if its us */
    if (!send_check_writer(env, descP, sendRef, &writerCheck))
        return writerCheck;
    
    /* We ignore the wrap for the moment.
     * Maybe we should issue a wrap-message to controlling process...
     */
    // cnt_inc(&descP->writeTries, 1);
    SOCK_CNT_INC(env, descP, sockRef, atom_write_tries, &descP->writeTries, 1);

    written = sock_send(descP->sock, sndDataP->data, sndDataP->size, flags);
    if (IS_SOCKET_ERROR(written))
        save_errno = sock_errno();
    else
        save_errno = -1; // The value does not actually matter in this case
    
    return send_check_result(env, descP,
                             written, sndDataP->size, save_errno,
                             sockRef, sendRef);

}
#endif // if !defined(__WIN32__)



/* ----------------------------------------------------------------------
 * nif_sendto
 *
 * Description:
 * Send a message on a socket
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * SendRef      - A unique id for this (send) request.
 * Data         - The data to send in the form of a IOVec.
 * Dest         - Destination (socket) address.
 * Flags        - Send flags.
 */

static
ERL_NIF_TERM nif_sendto(ErlNifEnv*         env,
                        int                argc,
                        const ERL_NIF_TERM argv[])
{
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef, sendRef;
    ErlNifBinary     sndData;
    unsigned int     eflags;
    int              flags;
    ERL_NIF_TERM     eSockAddr;
    ESockAddress     remoteAddr;
    unsigned int     remoteAddrLen;
    char*            xres;
    ERL_NIF_TERM     res;

    SGDBG( ("SOCKET", "nif_sendto -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    if ((argc != 5) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP) ||
        !GET_BIN(env, argv[2], &sndData) ||
        !GET_UINT(env, argv[4], &eflags)) {
        return enif_make_badarg(env);
    }
    sockRef   = argv[0]; // We need this in case we send in case we send abort
    sendRef   = argv[1];
    eSockAddr = argv[3];

    if (!enif_get_resource(env, sockRef, sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }
    
    SSDBG( descP,
           ("SOCKET", "nif_sendto -> args when sock = %d:"
            "\r\n   Socket:       %T"
            "\r\n   sendRef:      %T"
            "\r\n   size of data: %d"
            "\r\n   eSockAddr:    %T"
            "\r\n   eflags:       %d"
            "\r\n",
            descP->sock, sockRef, sendRef, sndData.size, eSockAddr, eflags) );

    if (!esendflags2sendflags(eflags, &flags)) {
        SSDBG( descP, ("SOCKET", "nif_sendto -> sendflags decode failed\r\n") );
        return esock_make_error(env, esock_atom_einval);
    }

    if ((xres = esock_decode_sockaddr(env, eSockAddr,
                                      &remoteAddr,
                                      &remoteAddrLen)) != NULL) {
        SSDBG( descP,
               ("SOCKET", "nif_sendto -> sockaddr decode: %s\r\n", xres) );
        return esock_make_error_str(env, xres);
    }

    MLOCK(descP->writeMtx);

    res = nsendto(env, descP, sockRef, sendRef, &sndData, flags,
                  &remoteAddr, remoteAddrLen);

    MUNLOCK(descP->writeMtx);

    SGDBG( ("SOCKET", "nif_sendto -> done with result: "
            "\r\n   %T"
            "\r\n", res) );

    return res;

#endif // if defined(__WIN32__)
}


#if !defined(__WIN32__)
static
ERL_NIF_TERM nsendto(ErlNifEnv*       env,
                     ESockDescriptor* descP,
                     ERL_NIF_TERM     sockRef,
                     ERL_NIF_TERM     sendRef,
                     ErlNifBinary*    dataP,
                     int              flags,
                     ESockAddress*    toAddrP,
                     unsigned int     toAddrLen)
{
    int          save_errno;
    ssize_t      written;
    ERL_NIF_TERM writerCheck;

    if (!descP->isWritable)
        return enif_make_badarg(env);

    /* Check if there is already a current writer and if its us */
    if (!send_check_writer(env, descP, sendRef, &writerCheck))
        return writerCheck;
    
    /* We ignore the wrap for the moment.
     * Maybe we should issue a wrap-message to controlling process...
     */
    // cnt_inc(&descP->writeTries, 1);
    SOCK_CNT_INC(env, descP, sockRef, atom_write_tries, &descP->writeTries, 1);

    if (toAddrP != NULL) {
        written = sock_sendto(descP->sock,
                              dataP->data, dataP->size, flags,
                              &toAddrP->sa, toAddrLen);
    } else {
        written = sock_sendto(descP->sock,
                              dataP->data, dataP->size, flags,
                              NULL, 0);
    }
    if (IS_SOCKET_ERROR(written))
        save_errno = sock_errno();
    else
        save_errno = -1; // The value does not actually matter in this case

    return send_check_result(env, descP, written, dataP->size, save_errno,
                             sockRef, sendRef);
}
#endif // if !defined(__WIN32__)



/* ----------------------------------------------------------------------
 * nif_sendmsg
 *
 * Description:
 * Send a message on a socket
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * SendRef      - A unique id for this (send) request.
 * MsgHdr       - Message Header - data and (maybe) control and dest
 * Flags        - Send flags.
 */

static
ERL_NIF_TERM nif_sendmsg(ErlNifEnv*         env,
                         int                argc,
                         const ERL_NIF_TERM argv[])
{
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM     res, sockRef, sendRef, eMsgHdr;
    ESockDescriptor* descP;
    unsigned int     eflags;
    int              flags;

    SGDBG( ("SOCKET", "nif_sendmsg -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    if ((argc != 4) ||
        !IS_MAP(env, argv[2]) ||
        !GET_UINT(env, argv[3], &eflags)) {
        return enif_make_badarg(env);
    }
    sockRef = argv[0]; // We need this in case we send in case we send abort
    sendRef = argv[1];
    eMsgHdr = argv[2];

    if (!enif_get_resource(env, sockRef, sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }
    
    SSDBG( descP,
           ("SOCKET", "nif_sendmsg -> args when sock = %d:"
            "\r\n   Socket:  %T"
            "\r\n   sendRef: %T"
            "\r\n   eflags:  %d"
            "\r\n",
            descP->sock, argv[0], sendRef, eflags) );

    if (!esendflags2sendflags(eflags, &flags))
        return esock_make_error(env, esock_atom_einval);

    MLOCK(descP->writeMtx);

    res = nsendmsg_erts(env, descP, sockRef, sendRef, eMsgHdr, flags);

    MUNLOCK(descP->writeMtx);

    SSDBG( descP,
           ("SOCKET", "nif_sendmsg -> done with result: "
            "\r\n   %T"
            "\r\n", res) );

    return res;

#endif // if defined(__WIN32__)
}


#if !defined(__WIN32__)
static
ERL_NIF_TERM nsendmsg_erts(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     sendRef,
                           ERL_NIF_TERM     eMsgHdr,
                           int              flags)
{
    ERL_NIF_TERM  res, eAddr, eIOV, eCtrl;
    ESockAddress  addr;
    struct msghdr msgHdr;
    ErlNifBinary* iovBins;
    struct iovec* iov;
    unsigned int  iovLen;
    char*         ctrlBuf;
    size_t        ctrlBufLen, ctrlBufUsed;
    int           save_errno;
    ssize_t       written, dataSize;
    ERL_NIF_TERM  writerCheck;
    char*         xres;

    if (!descP->isWritable)
        return enif_make_badarg(env);

    /* Check if there is already a current writer and if its us */
    if (!send_check_writer(env, descP, sendRef, &writerCheck))
        return writerCheck;
    
    /* Depending on if we are *connected* or not, we require
     * different things in the msghdr map.
     */
    if (IS_CONNECTED(descP)) {

        /* We don't need the address */

        SSDBG( descP, ("SOCKET", "nsendmsg_erts -> connected: no address\r\n") );

        msgHdr.msg_name    = NULL;
        msgHdr.msg_namelen = 0;
        
    } else {

        /* We need the address */

        msgHdr.msg_name    = (void*) &addr;
        msgHdr.msg_namelen = sizeof(addr);
        sys_memzero((char *) msgHdr.msg_name, msgHdr.msg_namelen);
        if (!GET_MAP_VAL(env, eMsgHdr, esock_atom_addr, &eAddr))
            return esock_make_error(env, esock_atom_einval);

        SSDBG( descP, ("SOCKET", "nsendmsg_erts -> not connected: "
                       "\r\n   address: %T"
                       "\r\n", eAddr) );

        if ((xres = esock_decode_sockaddr(env, eAddr,
                                          msgHdr.msg_name,
                                          &msgHdr.msg_namelen)) != NULL)
            return esock_make_error_str(env, xres);
    }


    /* Extract the (other) attributes of the msghdr map: iov and maybe ctrl */

    /* The *mandatory* iov, which must be a list */
    if (!GET_MAP_VAL(env, eMsgHdr, esock_atom_iov, &eIOV))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_LIST_LEN(env, eIOV, &iovLen) && (iovLen > 0))
        return esock_make_error(env, esock_atom_einval);

    SSDBG( descP, ("SOCKET", "nsendmsg_erts -> iov length: %d\r\n", iovLen) );

    iovBins = MALLOC(iovLen * sizeof(ErlNifBinary));
    ESOCK_ASSERT( (iovBins != NULL) );

    iov     = MALLOC(iovLen * sizeof(struct iovec));
    ESOCK_ASSERT( (iov != NULL) );

    /* The *opional* ctrl */
    if (GET_MAP_VAL(env, eMsgHdr, esock_atom_ctrl, &eCtrl)) {
        ctrlBufLen = descP->wCtrlSz;
        ctrlBuf    = (char*) MALLOC(ctrlBufLen);
        ESOCK_ASSERT( (ctrlBuf != NULL) );
    } else {
        eCtrl      = esock_atom_undefined;
        ctrlBufLen = 0;
        ctrlBuf    = NULL;
    }
    SSDBG( descP, ("SOCKET", "nsendmsg_erts -> optional ctrl: "
                   "\r\n   ctrlBuf:    0x%lX"
                   "\r\n   ctrlBufLen: %d"
                   "\r\n   eCtrl:      %T\r\n", ctrlBuf, ctrlBufLen, eCtrl) );
    
    /* Decode the iov and initiate that part of the msghdr */
    if ((xres = esock_decode_iov(env, eIOV,
                                 iovBins, iov, iovLen, &dataSize)) != NULL) {
        FREE(iovBins);
        FREE(iov);
        if (ctrlBuf != NULL) FREE(ctrlBuf);
        return esock_make_error_str(env, xres);
    }
    msgHdr.msg_iov    = iov;
    msgHdr.msg_iovlen = iovLen;
    

    SSDBG( descP, ("SOCKET",
                   "nsendmsg_erts -> total (iov) data size: %d\r\n", dataSize) );


    /* Decode the ctrl and initiate that part of the msghdr.
     */
    if (ctrlBuf != NULL) {
        if ((xres = decode_cmsghdrs(env, descP,
                                    eCtrl,
                                    ctrlBuf, ctrlBufLen, &ctrlBufUsed)) != NULL) {
            FREE(iovBins);
            FREE(iov);
            if (ctrlBuf != NULL) FREE(ctrlBuf);
            return esock_make_error_str(env, xres);
        }
    } else {
        ctrlBufUsed = 0;
    }
    msgHdr.msg_control    = ctrlBuf;
    msgHdr.msg_controllen = ctrlBufUsed;
    

    /* The msg-flags field is not used when sending, but zero it just in case */
    msgHdr.msg_flags      = 0;
    

    /* We ignore the wrap for the moment.
     * Maybe we should issue a wrap-message to controlling process...
     */
    // cnt_inc(&descP->writeTries, 1);
    SOCK_CNT_INC(env, descP, sockRef, atom_write_tries, &descP->writeTries, 1);

    /* And now, finally, try to send the message */
    written = sock_sendmsg(descP->sock, &msgHdr, flags);

    if (IS_SOCKET_ERROR(written))
        save_errno = sock_errno();
    else
        save_errno = -1; // OK or not complete: this value should not matter in this case

    res = send_check_result(env, descP, written, dataSize, save_errno,
                            sockRef, sendRef);

    FREE(iovBins);
    FREE(iov);
    if (ctrlBuf != NULL) FREE(ctrlBuf);
    
    return res;

}
#endif // if !defined(__WIN32__)



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

#ifdef FOBAR
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

    if (!enif_inspect_iovec(env, MAX_VSZ, data, &tail, &iovec))
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
#endif



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
 * Socket (ref) - Points to the socket descriptor.
 * RecvRef      - A unique id for this (send) request.
 * Length       - The number of bytes to receive.
 * Flags        - Receive flags.
 */

static
ERL_NIF_TERM nif_recv(ErlNifEnv*         env,
                      int                argc,
                      const ERL_NIF_TERM argv[])
{
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef, recvRef;
    int              len;
    unsigned int     eflags;
    int              flags;
    ERL_NIF_TERM     res;

    if ((argc != 4) ||
        !GET_INT(env, argv[2], &len) ||
        !GET_UINT(env, argv[3], &eflags)) {
        return enif_make_badarg(env);
    }
    sockRef = argv[0]; // We need this in case we case we send abort
    recvRef = argv[1];

    if (!enif_get_resource(env, sockRef, sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }
    
    if (!erecvflags2recvflags(eflags, &flags))
        return enif_make_badarg(env);

    MLOCK(descP->readMtx);

    /* We need to handle the case when another process tries
     * to receive at the same time.
     * If the current recv could not read its entire package
     * this time (resulting in an select). The read of the
     * other process must be made to wait until current
     * is done!
     */

    res = nrecv(env, descP, sockRef, recvRef, len, flags);

    MUNLOCK(descP->readMtx);

    return res;

#endif // if defined(__WIN32__)
}


/* The (read) buffer handling should be optimized!
 * But for now we make it easy for ourselves by
 * allocating a binary (of the specified or default
 * size) and then throwing it away...
 */
#if !defined(__WIN32__)
static
ERL_NIF_TERM nrecv(ErlNifEnv*       env,
                   ESockDescriptor* descP,
                   ERL_NIF_TERM     sockRef,
                   ERL_NIF_TERM     recvRef,
                   int              len,
                   int              flags)
{
    ssize_t      read;
    ErlNifBinary buf;
    ERL_NIF_TERM readerCheck;
    int          save_errno;
    int          bufSz = (len ? len : descP->rBufSz);

    SSDBG( descP, ("SOCKET", "nrecv -> entry with"
                   "\r\n   len:   %d (%d:%d)"
                   "\r\n   flags: %d"
                   "\r\n", len, descP->rNumCnt, bufSz, flags) );

    if (!descP->isReadable)
        return enif_make_badarg(env);

    /* Check if there is already a current reader and if its us */
    if (!recv_check_reader(env, descP, recvRef, &readerCheck))
        return readerCheck;
    
    /* Allocate a buffer:
     * Either as much as we want to read or (if zero (0)) use the "default"
     * size (what has been configured).
     */
    if (!ALLOC_BIN(bufSz, &buf))
        return esock_make_error(env, atom_exalloc);

    // cnt_inc(&descP->readTries, 1);
    SOCK_CNT_INC(env, descP, sockRef, atom_read_tries, &descP->readTries, 1);

    // If it fails (read = -1), we need errno...
    SSDBG( descP, ("SOCKET", "nrecv -> try read (%d)\r\n", buf.size) );
    read = sock_recv(descP->sock, buf.data, buf.size, flags);
    if (IS_SOCKET_ERROR(read)) {
        save_errno = sock_errno();
    } else {
        save_errno = -1; // The value does not actually matter in this case
    }
    
    SSDBG( descP, ("SOCKET", "nrecv -> read: %d (%d)\r\n", read, save_errno) );

    return recv_check_result(env, descP,
                             read, len,
                             save_errno,
                             &buf,
                             sockRef,
                             recvRef);
}
#endif // if !defined(__WIN32__)



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
 * Socket (ref) - Points to the socket descriptor.
 * RecvRef      - A unique id for this (send) request.
 * BufSz        - Size of the buffer into which we put the received message.
 * Flags        - Receive flags.
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
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef, recvRef;
    unsigned int     bufSz;
    unsigned int     eflags;
    int              flags;
    ERL_NIF_TERM     res;

    SGDBG( ("SOCKET", "nif_recvfrom -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    if ((argc != 4) ||
        !GET_UINT(env, argv[2], &bufSz) ||
        !GET_UINT(env, argv[3], &eflags)) {
        return enif_make_badarg(env);
    }
    sockRef = argv[0]; // We need this in case we send in case we send abort
    recvRef = argv[1];

    if (!enif_get_resource(env, sockRef, sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }

    SSDBG( descP,
           ("SOCKET", "nif_recvfrom -> args when sock = %d:"
            "\r\n   Socket:  %T"
            "\r\n   recvRef: %T"
            "\r\n   bufSz:   %d"
            "\r\n   eflags:  %d"
            "\r\n", descP->sock, argv[0], recvRef, bufSz, eflags) );

    /* if (IS_OPEN(descP)) */
    /*     return esock_make_error(env, atom_enotconn); */

    if (!erecvflags2recvflags(eflags, &flags)) {
        SSDBG( descP, ("SOCKET", "nif_recvfrom -> recvflags decode failed\r\n") );
        return enif_make_badarg(env);
    }

    MLOCK(descP->readMtx);

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

    res = nrecvfrom_erts(env, descP, sockRef, recvRef, bufSz, flags);

    MUNLOCK(descP->readMtx);

    return res;
#endif // if defined(__WIN32__)
}


/* The (read) buffer handling *must* be optimized!
 * But for now we make it easy for ourselves by
 * allocating a binary (of the specified or default
 * size) and then throwing it away...
 */
#if !defined(__WIN32__)
static
ERL_NIF_TERM nrecvfrom_erts(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     sockRef,
                            ERL_NIF_TERM     recvRef,
                            Uint16           len,
                            int              flags)
{
    ESockAddress  fromAddr;
    unsigned int  addrLen;
    ssize_t       read;
    int           save_errno;
    ErlNifBinary  buf;
    ERL_NIF_TERM  readerCheck;
    int           bufSz = (len ? len : descP->rBufSz);

    SSDBG( descP, ("SOCKET", "nrecvfrom_erts -> entry with"
                   "\r\n   len:   %d (%d)"
                   "\r\n   flags: %d"
                   "\r\n", len, bufSz, flags) );

    if (!descP->isReadable)
        return enif_make_badarg(env);

    /* Check if there is already a current reader and if its us */
    if (!recv_check_reader(env, descP, recvRef, &readerCheck))
        return readerCheck;
    
    /* Allocate a buffer:
     * Either as much as we want to read or (if zero (0)) use the "default"
     * size (what has been configured).
     */
    if (!ALLOC_BIN(bufSz, &buf))
        return esock_make_error(env, atom_exalloc);

    // cnt_inc(&descP->readTries, 1);
    SOCK_CNT_INC(env, descP, sockRef, atom_read_tries, &descP->readTries, 1);

    addrLen = sizeof(fromAddr);
    sys_memzero((char*) &fromAddr, addrLen);

    read = sock_recvfrom(descP->sock, buf.data, buf.size, flags,
                         &fromAddr.sa, &addrLen);
    if (IS_SOCKET_ERROR(read))
        save_errno = sock_errno();
    else
        save_errno = -1; // The value does not actually matter in this case

    return recvfrom_check_result(env, descP,
                                 read,
                                 save_errno,
                                 &buf,
                                 &fromAddr, addrLen,
                                 sockRef,
                                 recvRef);
}
#endif // if !defined(__WIN32__)



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
 * Socket (ref) - Points to the socket descriptor.
 * RecvRef      - A unique id for this (send) request.
 * BufSz        - Size of the buffer into which we put the received message.
 * CtrlSz       - Size of the ctrl (buffer) into which we put the received 
 *                ancillary data.
 * Flags        - Receive flags.
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
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     sockRef, recvRef;
    unsigned int     bufSz;
    unsigned int     ctrlSz;
    unsigned int     eflags;
    int              flags;
    ERL_NIF_TERM     res;

    SGDBG( ("SOCKET", "nif_recvmsg -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    if ((argc != 5) ||
        !GET_UINT(env, argv[2], &bufSz) ||
        !GET_UINT(env, argv[3], &ctrlSz) ||
        !GET_UINT(env, argv[4], &eflags)) {
        return enif_make_badarg(env);
    }
    sockRef = argv[0]; // We need this in case we send in case we send abort
    recvRef = argv[1];

    if (!enif_get_resource(env, sockRef, sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }
    
    SSDBG( descP,
           ("SOCKET", "nif_recvmsg -> args when sock = %d:"
            "\r\n   Socket:  %T"
            "\r\n   recvRef: %T"
            "\r\n   bufSz:   %d"
            "\r\n   ctrlSz:  %d"
            "\r\n   eflags:  %d"
            "\r\n", descP->sock, argv[0], recvRef, bufSz, ctrlSz, eflags) );

    /* if (IS_OPEN(descP)) */
    /*     return esock_make_error(env, atom_enotconn); */

    if (!erecvflags2recvflags(eflags, &flags))
        return enif_make_badarg(env);

    MLOCK(descP->readMtx);

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

    res = nrecvmsg_erts(env, descP, sockRef, recvRef, bufSz, ctrlSz, flags);

    MUNLOCK(descP->readMtx);

    return res;
#endif // if defined(__WIN32__)
}


/* The (read) buffer handling *must* be optimized!
 * But for now we make it easy for ourselves by
 * allocating a binary (of the specified or default
 * size) and then throwing it away...
 */
#if !defined(__WIN32__)
static
ERL_NIF_TERM nrecvmsg_erts(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     recvRef,
                           Uint16           bufLen,
                           Uint16           ctrlLen,
                           int              flags)
{
    unsigned int  addrLen;
    ssize_t       read;
    int           save_errno;
    int           bufSz  = (bufLen  ? bufLen  : descP->rBufSz);
    int           ctrlSz = (ctrlLen ? ctrlLen : descP->rCtrlSz);
    struct msghdr msgHdr;
    struct iovec  iov[1];  // Shall we always use 1?
    ErlNifBinary  data[1]; // Shall we always use 1?
    ErlNifBinary  ctrl;
    ERL_NIF_TERM  readerCheck;
    ESockAddress  addr;

    SSDBG( descP, ("SOCKET", "nrecvmsg_erts -> entry with"
                   "\r\n   bufSz:  %d (%d)"
                   "\r\n   ctrlSz: %d (%d)"
                   "\r\n   flags:  %d"
                   "\r\n", bufSz, bufLen, ctrlSz, ctrlLen, flags) );

    if (!descP->isReadable)
        return enif_make_badarg(env);

    /* Check if there is already a current reader and if its us */
    if (!recv_check_reader(env, descP, recvRef, &readerCheck))
        return readerCheck;
    
    /*
    for (i = 0; i < sizeof(buf); i++) {
        if (!ALLOC_BIN(bifSz, &buf[i]))
            return esock_make_error(env, atom_exalloc);
        iov[i].iov_base = buf[i].data;
        iov[i].iov_len  = buf[i].size;
    }
    */
    
    /* Allocate the (msg) data buffer:
     */
    if (!ALLOC_BIN(bufSz, &data[0]))
        return esock_make_error(env, atom_exalloc);

    /* Allocate the ctrl (buffer):
     */
    if (!ALLOC_BIN(ctrlSz, &ctrl))
        return esock_make_error(env, atom_exalloc);

    // cnt_inc(&descP->readTries, 1);
    SOCK_CNT_INC(env, descP, sockRef, atom_read_tries, &descP->readTries, 1);

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
    if (IS_SOCKET_ERROR(read))
        save_errno = sock_errno();
    else
        save_errno = -1; // The value does not actually matter in this case

    return recvmsg_check_result(env, descP,
                                read,
                                save_errno,
                                &msgHdr,
                                data,  // Needed for iov encode
                                &ctrl, // Needed for ctrl header encode
                                sockRef,
                                recvRef);
}
#endif // if !defined(__WIN32__)



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
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;

    if ((argc != 1) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }

    if (IS_CLOSED(descP) || IS_CLOSING(descP))
        return esock_make_error(env, atom_closed);
    
    return nclose(env, descP);
#endif // if defined(__WIN32__)
}


#if !defined(__WIN32__)
static
ERL_NIF_TERM nclose(ErlNifEnv*       env,
                    ESockDescriptor* descP)
{
    ERL_NIF_TERM reply, reason;
    BOOLEAN_T    doClose;

    SSDBG( descP, ("SOCKET", 
                   "nclose -> [%d] entry (0x%lX, 0x%lX, 0x%lX, 0x%lX)\r\n",
                   descP->sock,
                   descP->state,
                   descP->currentWriterP,
                   descP->currentReaderP,
                   descP->currentAcceptorP) );

    MLOCK(descP->closeMtx);

    doClose = nclose_check(env, descP, &reason);

    if (doClose) {
        reply = nclose_do(env, descP);
    } else {
        reply = esock_make_error(env, reason);
    }

    MUNLOCK(descP->closeMtx);

    SSDBG( descP,
           ("SOCKET", "nclose -> [%d] done when: "
            "\r\n   state: 0x%lX"
            "\r\n   reply: %T"
            "\r\n", descP->sock, descP->state, reply) );

    return reply;
}



/* *** nclose_check ***
 *
 * Check if we should try to perform the first stage close.
 */
static
BOOLEAN_T nclose_check(ErlNifEnv*       env,
                       ESockDescriptor* descP,
                       ERL_NIF_TERM*    reason)
{
    BOOLEAN_T doClose;

    if (descP->state == SOCKET_STATE_CLOSED) {

        doClose = FALSE;
        *reason = atom_closed;

    } else if (descP->state == SOCKET_STATE_CLOSING) {

        doClose = FALSE;
        *reason = atom_closing;

    } else {

        /* Store the PID of the caller,
         * since we need to inform it when we
         * (that is, the stop callback function)
         * completes.
         */

        if (enif_self(env, &descP->closerPid) == NULL) {

            doClose = FALSE;
            *reason = atom_exself;

        } else {

            /* Monitor the caller, since we should complete this
             * operation even if the caller dies (for whatever reason).
             *
             * <KOLLA>
             *
             * Can we actually use this for anything?
             *
             * </KOLLA>
             */

            if (MONP("nclose_check -> closer",
                     env, descP,
                     &descP->closerPid,
                     &descP->closerMon) != 0) {

                doClose = FALSE;
                *reason = atom_exmon;

            } else {

                descP->closeLocal = TRUE;
                descP->state      = SOCKET_STATE_CLOSING;
                descP->isReadable = FALSE;
                descP->isWritable = FALSE;
                doClose           = TRUE;
                *reason           = esock_atom_undefined; // NOT used !!

            }
        }
    }

    return doClose;
    
}



/* *** nclose_do ***
 *
 * Perform (do) the first stage close.
 */
static
ERL_NIF_TERM nclose_do(ErlNifEnv*       env,
                       ESockDescriptor* descP)
{
    int          domain   = descP->domain;
    int          type     = descP->type;
    int          protocol = descP->protocol;
    int          sres;
    ERL_NIF_TERM reply, reason;

    descP->closeEnv = esock_alloc_env("nclose-do - close-env");
    descP->closeRef = MKREF(descP->closeEnv);

    sres            = esock_select_stop(env, descP->sock, descP);

    if (sres & ERL_NIF_SELECT_STOP_CALLED) {

        /* Prep done - inform the caller it can finalize (close) directly */
        SSDBG( descP,
               ("SOCKET", "nclose -> [%d] stop was called\r\n", descP->sock) );

        dec_socket(domain, type, protocol);
        reply = esock_atom_ok;

    } else if (sres & ERL_NIF_SELECT_STOP_SCHEDULED) {

        /* The stop callback function has been *scheduled* which means that we
         * have to wait for it to complete. */
        SSDBG( descP,
               ("SOCKET", "nclose -> [%d] stop was scheduled\r\n",
                descP->sock) );

        dec_socket(domain, type, protocol); // SHALL WE DO THIS AT finalize?
        reply = esock_make_ok2(env, enif_make_copy(env, descP->closeRef));

    } else {

        SSDBG( descP,
               ("SOCKET", "nclose -> [%d] stop failed: %d\r\n",
                descP->sock, sres) );

        /* <KOLLA>
         *
         * WE SHOULD REALLY HAVE A WAY TO CLOBBER THE SOCKET,
         * SO WE DON'T LET STUFF LEAK.
         * NOW, BECAUSE WE FAILED TO SELECT, WE CANNOT FINISH
         * THE CLOSE, WHAT TO DO? ABORT?
         *
         * </KOLLA>
         */

        // Do we need this?
        DEMONP("nclose_do -> closer", env, descP, &descP->closerMon); 

        reason = MKT2(env, esock_atom_select_failed, MKI(env, sres));
        reply  = esock_make_error(env, reason);
    }

    return reply;
}



#endif // if !defined(__WIN32__)



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
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;

    /* Extract arguments and perform preliminary validation */

    if ((argc != 1) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }

    return nfinalize_close(env, descP);
#endif // if defined(__WIN32__)
}


/* *** nfinalize_close ***
 * Perform the final step in the socket close.
 */
#if !defined(__WIN32__)
static
ERL_NIF_TERM nfinalize_close(ErlNifEnv*       env,
                             ESockDescriptor* descP)
{
    ERL_NIF_TERM reply;

    if (IS_CLOSED(descP))
        return esock_atom_ok;

    if (!IS_CLOSING(descP))
        return esock_make_error(env, atom_enotclosing);

    /* This nif is executed in a dirty scheduler just so that
     * it can "hang" (whith minumum effect on the VM) while the
     * kernel writes our buffers. IF we have set the linger option
     * for this ({true, integer() > 0}). For this to work we must
     * be blocking...
     */
    SET_BLOCKING(descP->sock);

    if (sock_close(descP->sock) != 0) {
        int save_errno = sock_errno();

        if (save_errno != ERRNO_BLOCK) {
            /* Not all data in the buffers where sent,
             * make sure the caller gets this.
             */
            reply = esock_make_error(env, atom_timeout);
        } else {
            reply = esock_make_error_errno(env, save_errno);
        }
    } else {
        reply = esock_atom_ok;
    }
    sock_close_event(descP->event);

    descP->sock  = INVALID_SOCKET;
    descP->event = INVALID_EVENT;

    descP->state = SOCKET_STATE_CLOSED;

    return reply;
}
#endif // if !defined(__WIN32__)



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
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    unsigned int     ehow;
    int              how;

    if ((argc != 2) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP) ||
        !GET_UINT(env, argv[1], &ehow)) {
        return enif_make_badarg(env);
    }

    if (IS_CLOSED(descP) || IS_CLOSING(descP))
        return esock_make_error(env, atom_closed);
    
    if (!ehow2how(ehow, &how))
        return enif_make_badarg(env);

    return nshutdown(env, descP, how);
#endif // if defined(__WIN32__)
}



#if !defined(__WIN32__)
static
ERL_NIF_TERM nshutdown(ErlNifEnv*       env,
                       ESockDescriptor* descP,
                       int              how)
{
    ERL_NIF_TERM reply;

    if (sock_shutdown(descP->sock, how) == 0) {
        switch (how) {
        case SHUT_RD:
            descP->isReadable = FALSE;
            break;
        case SHUT_WR:
            descP->isWritable = FALSE;
            break;
        case SHUT_RDWR:
            descP->isReadable = FALSE;
            descP->isWritable = FALSE;
            break;
        }
        reply = esock_atom_ok;
    } else {
        reply = esock_make_error_errno(env, sock_errno());
    }

    return reply;
}
#endif // if !defined(__WIN32__)




/* ----------------------------------------------------------------------
 * nif_setopt
 *
 * Description:
 * Set socket option.
 * Its possible to use a "raw" mode (not encoded). That is, we do not
 * interpret level, opt and value. They are passed "as is" to the
 * setsockopt function call (the value arguments is assumed to be a
 * binary, already encoded).
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * Encoded      - Are the "arguments" encoded or not.
 * Level        - Level of the socket option.
 * Opt          - The socket option.
 * Value        - Value of the socket option (type depend on the option).
 */

static
ERL_NIF_TERM nif_setopt(ErlNifEnv*         env,
                        int                argc,
                        const ERL_NIF_TERM argv[])
{
#if defined(__WIN32__) 
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP = NULL;
    int              eLevel, level = -1;
    int              eOpt;
    ERL_NIF_TERM     eIsEncoded;
    ERL_NIF_TERM     eVal;
    BOOLEAN_T        isEncoded, isOTP;
    ERL_NIF_TERM     result;

    SGDBG( ("SOCKET", "nif_setopt -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    if ((argc != 5) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP) ||
        !GET_INT(env, argv[2], &eLevel) ||
        !GET_INT(env, argv[3], &eOpt)) {
        SGDBG( ("SOCKET", "nif_setopt -> failed initial arg check\r\n") );
        return enif_make_badarg(env);
    }
    eIsEncoded = argv[1];
    eVal       = argv[4];

    if (IS_CLOSED(descP) || IS_CLOSING(descP))
        return esock_make_error(env, atom_closed);
    
    isEncoded = esock_decode_bool(eIsEncoded);

    /* SGDBG( ("SOCKET", "nif_setopt -> eIsDecoded (%T) decoded: %d\r\n", */
    /*         eIsEncoded, isEncoded) ); */

    if (!elevel2level(isEncoded, eLevel, &isOTP, &level)) {
        SSDBG( descP, ("SOCKET", "nif_seopt -> failed decode level\r\n") );
        return esock_make_error(env, esock_atom_einval);
    }

    SSDBG( descP,
           ("SOCKET", "nif_setopt -> args when sock = %d:"
            "\r\n   Socket:  %T"
            "\r\n   Encoded: %d (%T)"
            "\r\n   Level:   %d (%d)"
            "\r\n   Opt:     %d"
            "\r\n   Value:   %T"
            "\r\n",
            descP->sock, argv[0],
            isEncoded, eIsEncoded,
            level, eLevel,
            eOpt, eVal) );

    MLOCK(descP->cfgMtx);

    result = nsetopt(env, descP, isEncoded, isOTP, level, eOpt, eVal);

    MUNLOCK(descP->cfgMtx);

    SSDBG( descP,
           ("SOCKET", "nif_setopt -> done when"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;

#endif // if defined(__WIN32__)
}


#if !defined(__WIN32__)
static
ERL_NIF_TERM nsetopt(ErlNifEnv*       env,
                     ESockDescriptor* descP,
                     BOOLEAN_T        isEncoded,
                     BOOLEAN_T        isOTP,
                     int              level,
                     int              eOpt,
                     ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;

    if (isOTP) {
        /* These are not actual socket options,
         * but options for our implementation.
         */
        result = nsetopt_otp(env, descP, eOpt, eVal);
    } else if (!isEncoded) {
        result = nsetopt_native(env, descP, level, eOpt, eVal);
    } else {
        result = nsetopt_level(env, descP, level, eOpt, eVal);
    }

    return result;
}



/* nsetopt_otp - Handle OTP (level) options
 */
static
ERL_NIF_TERM nsetopt_otp(ErlNifEnv*       env,
                         ESockDescriptor* descP,
                         int              eOpt,
                         ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("SOCKET", "nsetopt_otp -> entry with"
            "\r\n   eOpt: %d"
            "\r\n   eVal: %T"
            "\r\n", eOpt, eVal) );

    switch (eOpt) {
    case SOCKET_OPT_OTP_DEBUG:
        result = nsetopt_otp_debug(env, descP, eVal);
        break;

    case SOCKET_OPT_OTP_IOW:
        result = nsetopt_otp_iow(env, descP, eVal);
        break;

    case SOCKET_OPT_OTP_CTRL_PROC:
        result = nsetopt_otp_ctrl_proc(env, descP, eVal);
        break;

    case SOCKET_OPT_OTP_RCVBUF:
        result = nsetopt_otp_rcvbuf(env, descP, eVal);
        break;

    case SOCKET_OPT_OTP_RCVCTRLBUF:
        result = nsetopt_otp_rcvctrlbuf(env, descP, eVal);
        break;

    case SOCKET_OPT_OTP_SNDCTRLBUF:
        result = nsetopt_otp_sndctrlbuf(env, descP, eVal);
        break;

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}


/* nsetopt_otp_debug - Handle the OTP (level) debug options
 */
static
ERL_NIF_TERM nsetopt_otp_debug(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     eVal)
{
    descP->dbg = esock_decode_bool(eVal);

    return esock_atom_ok;
}


/* nsetopt_otp_iow - Handle the OTP (level) iow options
 */
static
ERL_NIF_TERM nsetopt_otp_iow(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             ERL_NIF_TERM     eVal)
{
    descP->iow = esock_decode_bool(eVal);

    return esock_atom_ok;
}



/* nsetopt_otp_ctrl_proc - Handle the OTP (level) controlling_process options
 */
static
ERL_NIF_TERM nsetopt_otp_ctrl_proc(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM     eVal)
{
    ErlNifPid     caller, newCtrlPid;
    ESockMonitor  newCtrlMon;
    int           xres;

    SSDBG( descP,
           ("SOCKET", "nsetopt_otp_ctrl_proc -> entry with"
            "\r\n   eVal: %T"
            "\r\n", eVal) );

    /* Before we begin, ensure that caller is (current) controlling-process */
    if (enif_self(env, &caller) == NULL)
        return esock_make_error(env, atom_exself);

    if (COMPARE_PIDS(&descP->ctrlPid, &caller) != 0) {
        SSDBG( descP, ("SOCKET", "nsetopt_otp_ctrl_proc -> not owner (%T)\r\n",
                       descP->ctrlPid) );
        return esock_make_error(env, esock_atom_not_owner);
    }
    
    if (!GET_LPID(env, eVal, &newCtrlPid)) {
        esock_warning_msg("Failed get pid of new controlling process\r\n");
        return esock_make_error(env, esock_atom_einval);
    }

    if ((xres = MONP("nsetopt_otp_ctrl_proc -> (new) ctrl",
                     env, descP, &newCtrlPid, &newCtrlMon)) != 0) {
        esock_warning_msg("Failed monitor %d) (new) controlling process\r\n", xres);
        return esock_make_error(env, esock_atom_einval);
    }

    if ((xres = DEMONP("nsetopt_otp_ctrl_proc -> (old) ctrl",
                       env, descP, &descP->ctrlMon)) != 0) {
        esock_warning_msg("Failed demonitor (%d) "
                          "old controlling process %T (%T)\r\n",
                          xres, descP->ctrlPid, descP->ctrlMon);
    }

    descP->ctrlPid = newCtrlPid;
    descP->ctrlMon = newCtrlMon;
    
    SSDBG( descP, ("SOCKET", "nsetopt_otp_ctrl_proc -> done\r\n") );

    return esock_atom_ok;
}



/* nsetopt_otp_rcvbuf - Handle the OTP (level) rcvbuf option
 * The (otp) rcvbuf option is provided as:
 *
 *       BufSz :: integer() | {N :: pos_integer(), BufSz :: pod_integer()}
 *
 * Where N is the max number of reads.
 */
static
ERL_NIF_TERM nsetopt_otp_rcvbuf(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     eVal)
{
    const ERL_NIF_TERM* t;   // The array of the elements of the tuple
    int                 tsz; // The size of the tuple - should be 2
    unsigned int        n;
    size_t              bufSz;
    char*               xres;

    if (IS_NUM(env, eVal)) {

        /* This will have the effect that the buffer size will be
         * reported as an integer (getopt).
         */
        n = 0;

        if ((xres = esock_decode_bufsz(env,
                                       eVal,
                                       SOCKET_RECV_BUFFER_SIZE_DEFAULT,
                                       &bufSz)) != NULL)
            return esock_make_error_str(env, xres);

    } else if (IS_TUPLE(env, eVal)) {

        if (!GET_TUPLE(env, eVal, &tsz, &t))
            return enif_make_badarg(env); // We should use a "proper" error value...

        if (tsz != 2)
            return enif_make_badarg(env); // We should use a "proper" error value...
    
        if (!GET_UINT(env, t[0], &n))
            return enif_make_badarg(env); // We should use a "proper" error value...

        if ((xres = esock_decode_bufsz(env,
                                       t[1],
                                       SOCKET_RECV_BUFFER_SIZE_DEFAULT,
                                       &bufSz)) != NULL)
            return esock_make_error_str(env, xres);

    } else {
        return enif_make_badarg(env); // We should use a "proper" error value...
    }

    descP->rNum   = n;
    descP->rBufSz = bufSz;

    return esock_atom_ok;
}



/* nsetopt_otp_rcvctrlbuf - Handle the OTP (level) rcvctrlbuf option
 */
static
ERL_NIF_TERM nsetopt_otp_rcvctrlbuf(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     eVal)
{
    size_t val;
    char*  xres;

    if ((xres = esock_decode_bufsz(env,
                                   eVal,
                                   SOCKET_RECV_CTRL_BUFFER_SIZE_DEFAULT,
                                   &val)) != NULL)
        return esock_make_error_str(env, xres);

    descP->rCtrlSz = val;
    
    return esock_atom_ok;
}



/* nsetopt_otp_sndctrlbuf - Handle the OTP (level) sndctrlbuf option
 */
static
ERL_NIF_TERM nsetopt_otp_sndctrlbuf(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     eVal)
{
    size_t val;
    char*  xres;

    if ((xres = esock_decode_bufsz(env,
                                   eVal,
                                   SOCKET_SEND_CTRL_BUFFER_SIZE_DEFAULT,
                                   &val)) != NULL)
        return esock_make_error_str(env, xres);

    descP->wCtrlSz = val;
    
    return esock_atom_ok;
}



/* The option has *not* been encoded. Instead it has been provided
 * in "native mode" (option is provided as is and value as a binary).
 */
static
ERL_NIF_TERM nsetopt_native(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            int              level,
                            int              opt,
                            ERL_NIF_TERM     eVal)
{
    ErlNifBinary val;
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("SOCKET", "nsetopt_native -> entry with"
            "\r\n   level: %d"
            "\r\n   opt:   %d"
            "\r\n   eVal:  %T"
            "\r\n", level, opt, eVal) );

    if (GET_BIN(env, eVal, &val)) {
        int res = socket_setopt(descP->sock, level, opt,
                                val.data, val.size);
        if (res != 0)
            result = esock_make_error_errno(env, sock_errno());
        else
            result = esock_atom_ok;
    } else {
        result = esock_make_error(env, esock_atom_einval);
    }

    SSDBG( descP,
           ("SOCKET", "nsetopt_native -> done when"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}



/* nsetopt_level - A "proper" level (option) has been specified
 */
static
ERL_NIF_TERM nsetopt_level(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           int              level,
                           int              eOpt,
                           ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("SOCKET", "nsetopt_level -> entry with"
            "\r\n   level: %d"
            "\r\n", level) );

    switch (level) {
    case SOL_SOCKET:
        result = nsetopt_lvl_socket(env, descP, eOpt, eVal);
        break;

#if defined(SOL_IP)
    case SOL_IP:
#else
    case IPPROTO_IP:
#endif
        result = nsetopt_lvl_ip(env, descP, eOpt, eVal);
        break;

#if defined(HAVE_IPV6)
#if defined(SOL_IPV6)
    case SOL_IPV6:
#else
    case IPPROTO_IPV6:
#endif
        result = nsetopt_lvl_ipv6(env, descP, eOpt, eVal);
        break;
#endif

    case IPPROTO_TCP:
        result = nsetopt_lvl_tcp(env, descP, eOpt, eVal);
        break;

    case IPPROTO_UDP:
        result = nsetopt_lvl_udp(env, descP, eOpt, eVal);
        break;

#if defined(HAVE_SCTP)
    case IPPROTO_SCTP:
        result = nsetopt_lvl_sctp(env, descP, eOpt, eVal);
        break;
#endif

    default:
        SSDBG( descP,
               ("SOCKET", "nsetopt_level -> unknown level (%d)\r\n", level) );
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    SSDBG( descP,
           ("SOCKET", "nsetopt_level -> done when"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}



/* nsetopt_lvl_socket - Level *SOCKET* option
 */
static
ERL_NIF_TERM nsetopt_lvl_socket(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                int              eOpt,
                                ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_socket -> entry with"
            "\r\n   opt: %d"
            "\r\n", eOpt) );

    switch (eOpt) {
#if defined(SO_BINDTODEVICE)
    case SOCKET_OPT_SOCK_BINDTODEVICE:
        result = nsetopt_lvl_sock_bindtodevice(env, descP, eVal);
        break;
#endif

#if defined(SO_BROADCAST)
    case SOCKET_OPT_SOCK_BROADCAST:
        result = nsetopt_lvl_sock_broadcast(env, descP, eVal);
        break;
#endif

#if defined(SO_DEBUG)
    case SOCKET_OPT_SOCK_DEBUG:
        result = nsetopt_lvl_sock_debug(env, descP, eVal);
        break;
#endif

#if defined(SO_DONTROUTE)
    case SOCKET_OPT_SOCK_DONTROUTE:
        result = nsetopt_lvl_sock_dontroute(env, descP, eVal);
        break;
#endif

#if defined(SO_KEEPALIVE)
    case SOCKET_OPT_SOCK_KEEPALIVE:
        result = nsetopt_lvl_sock_keepalive(env, descP, eVal);
        break;
#endif

#if defined(SO_LINGER)
    case SOCKET_OPT_SOCK_LINGER:
        result = nsetopt_lvl_sock_linger(env, descP, eVal);
        break;
#endif

#if defined(SO_PEEK_OFF)
    case SOCKET_OPT_SOCK_PEEK_OFF:
        result = nsetopt_lvl_sock_peek_off(env, descP, eVal);
        break;
#endif

#if defined(SO_OOBINLINE)
    case SOCKET_OPT_SOCK_OOBINLINE:
        result = nsetopt_lvl_sock_oobinline(env, descP, eVal);
        break;
#endif

#if defined(SO_PRIORITY)
    case SOCKET_OPT_SOCK_PRIORITY:
        result = nsetopt_lvl_sock_priority(env, descP, eVal);
        break;
#endif

#if defined(SO_RCVBUF)
    case SOCKET_OPT_SOCK_RCVBUF:
        result = nsetopt_lvl_sock_rcvbuf(env, descP, eVal);
        break;
#endif

#if defined(SO_RCVLOWAT)
    case SOCKET_OPT_SOCK_RCVLOWAT:
        result = nsetopt_lvl_sock_rcvlowat(env, descP, eVal);
        break;
#endif

#if defined(SO_RCVTIMEO)
    case SOCKET_OPT_SOCK_RCVTIMEO:
        result = nsetopt_lvl_sock_rcvtimeo(env, descP, eVal);
        break;
#endif

#if defined(SO_REUSEADDR)
    case SOCKET_OPT_SOCK_REUSEADDR:
        result = nsetopt_lvl_sock_reuseaddr(env, descP, eVal);
        break;
#endif

#if defined(SO_REUSEPORT)
    case SOCKET_OPT_SOCK_REUSEPORT:
        result = nsetopt_lvl_sock_reuseport(env, descP, eVal);
        break;
#endif

#if defined(SO_SNDBUF)
    case SOCKET_OPT_SOCK_SNDBUF:
        result = nsetopt_lvl_sock_sndbuf(env, descP, eVal);
        break;
#endif

#if defined(SO_SNDLOWAT)
    case SOCKET_OPT_SOCK_SNDLOWAT:
        result = nsetopt_lvl_sock_sndlowat(env, descP, eVal);
        break;
#endif

#if defined(SO_SNDTIMEO)
    case SOCKET_OPT_SOCK_SNDTIMEO:
        result = nsetopt_lvl_sock_sndtimeo(env, descP, eVal);
        break;
#endif

#if defined(SO_TIMESTAMP)
    case SOCKET_OPT_SOCK_TIMESTAMP:
        result = nsetopt_lvl_sock_timestamp(env, descP, eVal);
        break;
#endif

    default:
        SSDBG( descP,
               ("SOCKET", "nsetopt_lvl_socket -> unknown opt (%d)\r\n", eOpt) );
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_socket -> done when"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}


#if defined(SO_BINDTODEVICE)
static
ERL_NIF_TERM nsetopt_lvl_sock_bindtodevice(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal)
{
    return nsetopt_str_opt(env, descP,
                           SOL_SOCKET, SO_BROADCAST,
                           IFNAMSIZ, eVal);
}
#endif


#if defined(SO_BROADCAST)
static
ERL_NIF_TERM nsetopt_lvl_sock_broadcast(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     eVal)
{
    return nsetopt_bool_opt(env, descP, SOL_SOCKET, SO_BROADCAST, eVal);
}
#endif


#if defined(SO_DEBUG)
static
ERL_NIF_TERM nsetopt_lvl_sock_debug(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     eVal)
{
    return nsetopt_int_opt(env, descP, SOL_SOCKET, SO_DEBUG, eVal);
}
#endif


#if defined(SO_DONTROUTE)
static
ERL_NIF_TERM nsetopt_lvl_sock_dontroute(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     eVal)
{
    return nsetopt_bool_opt(env, descP, SOL_SOCKET, SO_DONTROUTE, eVal);
}
#endif


#if defined(SO_KEEPALIVE)
static
ERL_NIF_TERM nsetopt_lvl_sock_keepalive(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     eVal)
{
    return nsetopt_bool_opt(env, descP, SOL_SOCKET, SO_KEEPALIVE, eVal);
}
#endif


#if defined(SO_LINGER)
static
ERL_NIF_TERM nsetopt_lvl_sock_linger(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM  result;
    struct linger val;

    if (decode_sock_linger(env, eVal, &val)) {
        int optLen = sizeof(val);
        int res    = socket_setopt(descP->sock, SOL_SOCKET, SO_LINGER,
                                   (void*) &val, optLen);
        if (res != 0)
            result = esock_make_error_errno(env, sock_errno());
        else
            result = esock_atom_ok;
    } else {
        result = esock_make_error(env, esock_atom_einval);
    }

    return result;
}
#endif


#if defined(SO_OOBINLINE)
static
ERL_NIF_TERM nsetopt_lvl_sock_oobinline(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     eVal)
{
    return nsetopt_bool_opt(env, descP, SOL_SOCKET, SO_OOBINLINE, eVal);
}
#endif


#if defined(SO_PEEK_OFF)
static
ERL_NIF_TERM nsetopt_lvl_sock_peek_off(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     eVal)
{
    return nsetopt_int_opt(env, descP, SOL_SOCKET, SO_PEEK_OFF, eVal);
}
#endif


#if defined(SO_PRIORITY)
static
ERL_NIF_TERM nsetopt_lvl_sock_priority(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     eVal)
{
    return nsetopt_int_opt(env, descP, SOL_SOCKET, SO_PRIORITY, eVal);
}
#endif


#if defined(SO_RCVBUF)
static
ERL_NIF_TERM nsetopt_lvl_sock_rcvbuf(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     eVal)
{
    return nsetopt_int_opt(env, descP, SOL_SOCKET, SO_RCVBUF, eVal);
}
#endif


#if defined(SO_RCVLOWAT)
static
ERL_NIF_TERM nsetopt_lvl_sock_rcvlowat(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     eVal)
{
    return nsetopt_int_opt(env, descP, SOL_SOCKET, SO_RCVLOWAT, eVal);
}
#endif


#if defined(SO_RCVTIMEO)
static
ERL_NIF_TERM nsetopt_lvl_sock_rcvtimeo(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     eVal)
{
    return nsetopt_timeval_opt(env, descP, SOL_SOCKET, SO_RCVTIMEO, eVal);
}
#endif


#if defined(SO_REUSEADDR)
static
ERL_NIF_TERM nsetopt_lvl_sock_reuseaddr(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     eVal)
{
    return nsetopt_bool_opt(env, descP, SOL_SOCKET, SO_REUSEADDR, eVal);
}
#endif


#if defined(SO_REUSEPORT)
static
ERL_NIF_TERM nsetopt_lvl_sock_reuseport(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     eVal)
{
    return nsetopt_bool_opt(env, descP, SOL_SOCKET, SO_REUSEPORT, eVal);
}
#endif


#if defined(SO_SNDBUF)
static
ERL_NIF_TERM nsetopt_lvl_sock_sndbuf(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     eVal)
{
    return nsetopt_int_opt(env, descP, SOL_SOCKET, SO_SNDBUF, eVal);
}
#endif


#if defined(SO_SNDLOWAT)
static
ERL_NIF_TERM nsetopt_lvl_sock_sndlowat(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     eVal)
{
    return nsetopt_int_opt(env, descP, SOL_SOCKET, SO_SNDLOWAT, eVal);
}
#endif


#if defined(SO_SNDTIMEO)
static
ERL_NIF_TERM nsetopt_lvl_sock_sndtimeo(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     eVal)
{
    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sock_sndtimeo -> entry with"
            "\r\n   eVal: %T"
            "\r\n", eVal) );

    return nsetopt_timeval_opt(env, descP, SOL_SOCKET, SO_SNDTIMEO, eVal);
}
#endif


#if defined(SO_TIMESTAMP)
static
ERL_NIF_TERM nsetopt_lvl_sock_timestamp(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     eVal)
{
    return nsetopt_bool_opt(env, descP, SOL_SOCKET, SO_TIMESTAMP, eVal);
}
#endif



/* nsetopt_lvl_ip - Level *IP* option(s)
 */
static
ERL_NIF_TERM nsetopt_lvl_ip(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            int              eOpt,
                            ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_ip -> entry with"
            "\r\n   opt: %d"
            "\r\n", eOpt) );

    switch (eOpt) {
#if defined(IP_ADD_MEMBERSHIP)
    case SOCKET_OPT_IP_ADD_MEMBERSHIP:
        result = nsetopt_lvl_ip_add_membership(env, descP, eVal);
        break;
#endif

#if defined(IP_ADD_SOURCE_MEMBERSHIP)
    case SOCKET_OPT_IP_ADD_SOURCE_MEMBERSHIP:
        result = nsetopt_lvl_ip_add_source_membership(env, descP, eVal);
        break;
#endif

#if defined(IP_BLOCK_SOURCE)
    case SOCKET_OPT_IP_BLOCK_SOURCE:
        result = nsetopt_lvl_ip_block_source(env, descP, eVal);
        break;
#endif

#if defined(IP_DROP_MEMBERSHIP)
    case SOCKET_OPT_IP_DROP_MEMBERSHIP:
        result = nsetopt_lvl_ip_drop_membership(env, descP, eVal);
        break;
#endif

#if defined(IP_DROP_SOURCE_MEMBERSHIP)
    case SOCKET_OPT_IP_DROP_SOURCE_MEMBERSHIP:
        result = nsetopt_lvl_ip_drop_source_membership(env, descP, eVal);
        break;
#endif

#if defined(IP_FREEBIND)
    case SOCKET_OPT_IP_FREEBIND:
        result = nsetopt_lvl_ip_freebind(env, descP, eVal);
        break;
#endif

#if defined(IP_HDRINCL)
    case SOCKET_OPT_IP_HDRINCL:
        result = nsetopt_lvl_ip_hdrincl(env, descP, eVal);
        break;
#endif

#if defined(IP_MINTTL)
    case SOCKET_OPT_IP_MINTTL:
        result = nsetopt_lvl_ip_minttl(env, descP, eVal);
        break;
#endif

#if defined(IP_MSFILTER) && defined(IP_MSFILTER_SIZE)
    case SOCKET_OPT_IP_MSFILTER:
        result = nsetopt_lvl_ip_msfilter(env, descP, eVal);
        break;
#endif

#if defined(IP_MTU_DISCOVER)
    case SOCKET_OPT_IP_MTU_DISCOVER:
        result = nsetopt_lvl_ip_mtu_discover(env, descP, eVal);
        break;
#endif

#if defined(IP_MULTICAST_ALL)
    case SOCKET_OPT_IP_MULTICAST_ALL:
        result = nsetopt_lvl_ip_multicast_all(env, descP, eVal);
        break;
#endif

#if defined(IP_MULTICAST_IF)
    case SOCKET_OPT_IP_MULTICAST_IF:
        result = nsetopt_lvl_ip_multicast_if(env, descP, eVal);
        break;
#endif

#if defined(IP_MULTICAST_LOOP)
    case SOCKET_OPT_IP_MULTICAST_LOOP:
        result = nsetopt_lvl_ip_multicast_loop(env, descP, eVal);
        break;
#endif

#if defined(IP_MULTICAST_TTL)
    case SOCKET_OPT_IP_MULTICAST_TTL:
        result = nsetopt_lvl_ip_multicast_ttl(env, descP, eVal);
        break;
#endif

#if defined(IP_NODEFRAG)
    case SOCKET_OPT_IP_NODEFRAG:
        result = nsetopt_lvl_ip_nodefrag(env, descP, eVal);
        break;
#endif

#if defined(IP_PKTINFO)
    case SOCKET_OPT_IP_PKTINFO:
        result = nsetopt_lvl_ip_pktinfo(env, descP, eVal);
        break;
#endif

#if defined(IP_RECVDSTADDR)
    case SOCKET_OPT_IP_RECVDSTADDR:
        result = nsetopt_lvl_ip_recvdstaddr(env, descP, eVal);
        break;
#endif

#if defined(IP_RECVERR)
    case SOCKET_OPT_IP_RECVERR:
        result = nsetopt_lvl_ip_recverr(env, descP, eVal);
        break;
#endif

#if defined(IP_RECVIF)
    case SOCKET_OPT_IP_RECVIF:
        result = nsetopt_lvl_ip_recvif(env, descP, eVal);
        break;
#endif

#if defined(IP_RECVOPTS)
    case SOCKET_OPT_IP_RECVOPTS:
        result = nsetopt_lvl_ip_recvopts(env, descP, eVal);
        break;
#endif

#if defined(IP_RECVORIGDSTADDR)
    case SOCKET_OPT_IP_RECVORIGDSTADDR:
        result = nsetopt_lvl_ip_recvorigdstaddr(env, descP, eVal);
        break;
#endif

#if defined(IP_RECVTOS)
    case SOCKET_OPT_IP_RECVTOS:
        result = nsetopt_lvl_ip_recvtos(env, descP, eVal);
        break;
#endif

#if defined(IP_RECVTTL)
    case SOCKET_OPT_IP_RECVTTL:
        result = nsetopt_lvl_ip_recvttl(env, descP, eVal);
        break;
#endif

#if defined(IP_RETOPTS)
    case SOCKET_OPT_IP_RETOPTS:
        result = nsetopt_lvl_ip_retopts(env, descP, eVal);
        break;
#endif

#if defined(IP_ROUTER_ALERT)
    case SOCKET_OPT_IP_ROUTER_ALERT:
        result = nsetopt_lvl_ip_router_alert(env, descP, eVal);
        break;
#endif

#if defined(IP_SENDSRCADDR)
    case SOCKET_OPT_IP_SENDSRCADDR:
        result = nsetopt_lvl_ip_sendsrcaddr(env, descP, eVal);
        break;
#endif

#if defined(IP_TOS)
    case SOCKET_OPT_IP_TOS:
        result = nsetopt_lvl_ip_tos(env, descP, eVal);
        break;
#endif

#if defined(IP_TRANSPARENT)
    case SOCKET_OPT_IP_TRANSPARENT:
        result = nsetopt_lvl_ip_transparent(env, descP, eVal);
        break;
#endif

#if defined(IP_TTL)
    case SOCKET_OPT_IP_TTL:
        result = nsetopt_lvl_ip_ttl(env, descP, eVal);
        break;
#endif

#if defined(IP_UNBLOCK_SOURCE)
    case SOCKET_OPT_IP_UNBLOCK_SOURCE:
        result = nsetopt_lvl_ip_unblock_source(env, descP, eVal);
        break;
#endif

    default:
        SSDBG( descP, ("SOCKET", "nsetopt_lvl_ip -> unknown opt (%d)\r\n", eOpt) );
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_ip -> done when"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}


/* nsetopt_lvl_ip_add_membership - Level IP ADD_MEMBERSHIP option
 *
 * The value is a map with two attributes: multiaddr and interface.
 * The attribute 'multiaddr' is always a 4-tuple (IPv4 address).
 * The attribute 'interface' is either the atom 'any' or a 4-tuple
 * (IPv4 address).
 */
#if defined(IP_ADD_MEMBERSHIP)
static
ERL_NIF_TERM nsetopt_lvl_ip_add_membership(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal)
{
    return nsetopt_lvl_ip_update_membership(env, descP, eVal, IP_ADD_MEMBERSHIP);
}
#endif


/* nsetopt_lvl_ip_add_source_membership - Level IP ADD_SOURCE_MEMBERSHIP option
 *
 * The value is a map with three attributes: multiaddr, interface and
 * sourceaddr.
 * The attribute 'multiaddr' is always a 4-tuple (IPv4 address).
 * The attribute 'interface' is always a 4-tuple (IPv4 address).
 * The attribute 'sourceaddr' is always a 4-tuple (IPv4 address).
 * (IPv4 address).
 */
#if defined(IP_ADD_SOURCE_MEMBERSHIP)
static
ERL_NIF_TERM nsetopt_lvl_ip_add_source_membership(ErlNifEnv*       env,
                                                  ESockDescriptor* descP,
                                                  ERL_NIF_TERM     eVal)
{
    return nsetopt_lvl_ip_update_source(env, descP, eVal,
                                        IP_ADD_SOURCE_MEMBERSHIP);
}
#endif


/* nsetopt_lvl_ip_block_source - Level IP BLOCK_SOURCE option
 *
 * The value is a map with three attributes: multiaddr, interface and
 * sourceaddr.
 * The attribute 'multiaddr' is always a 4-tuple (IPv4 address).
 * The attribute 'interface' is always a 4-tuple (IPv4 address).
 * The attribute 'sourceaddr' is always a 4-tuple (IPv4 address).
 * (IPv4 address).
 */
#if defined(IP_BLOCK_SOURCE)
static
ERL_NIF_TERM nsetopt_lvl_ip_block_source(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     eVal)
{
    return nsetopt_lvl_ip_update_source(env, descP, eVal, IP_BLOCK_SOURCE);
}
#endif


/* nsetopt_lvl_ip_drop_membership - Level IP DROP_MEMBERSHIP option
 *
 * The value is a map with two attributes: multiaddr and interface.
 * The attribute 'multiaddr' is always a 4-tuple (IPv4 address).
 * The attribute 'interface' is either the atom 'any' or a 4-tuple
 * (IPv4 address).
 *
 * We should really have a common function with add_membership,
 * since the code is virtually identical (except for the option
 * value).
 */
#if defined(IP_DROP_MEMBERSHIP)
static
ERL_NIF_TERM nsetopt_lvl_ip_drop_membership(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     eVal)
{
    return nsetopt_lvl_ip_update_membership(env, descP, eVal,
                                            IP_DROP_MEMBERSHIP);
}
#endif



/* nsetopt_lvl_ip_drop_source_membership - Level IP DROP_SOURCE_MEMBERSHIP option
 *
 * The value is a map with three attributes: multiaddr, interface and
 * sourceaddr.
 * The attribute 'multiaddr' is always a 4-tuple (IPv4 address).
 * The attribute 'interface' is always a 4-tuple (IPv4 address).
 * The attribute 'sourceaddr' is always a 4-tuple (IPv4 address).
 * (IPv4 address).
 */
#if defined(IP_DROP_SOURCE_MEMBERSHIP)
static
ERL_NIF_TERM nsetopt_lvl_ip_drop_source_membership(ErlNifEnv*       env,
                                                   ESockDescriptor* descP,
                                                   ERL_NIF_TERM     eVal)
{
    return nsetopt_lvl_ip_update_source(env, descP, eVal,
                                        IP_DROP_SOURCE_MEMBERSHIP);
}
#endif



/* nsetopt_lvl_ip_freebind - Level IP FREEBIND option
 */
#if defined(IP_FREEBIND)
static
ERL_NIF_TERM nsetopt_lvl_ip_freebind(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_FREEBIND, eVal);
}
#endif



/* nsetopt_lvl_ip_hdrincl - Level IP HDRINCL option
 */
#if defined(IP_HDRINCL)
static
ERL_NIF_TERM nsetopt_lvl_ip_hdrincl(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_HDRINCL, eVal);
}
#endif



/* nsetopt_lvl_ip_minttl - Level IP MINTTL option
 */
#if defined(IP_MINTTL)
static
ERL_NIF_TERM nsetopt_lvl_ip_minttl(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_int_opt(env, descP, level, IP_MINTTL, eVal);
}
#endif



/* nsetopt_lvl_ip_msfilter - Level IP MSFILTER option
 *
 * The value can be *either* the atom 'null' or a map of type ip_msfilter().
 */
#if defined(IP_MSFILTER) && defined(IP_MSFILTER_SIZE)
static
ERL_NIF_TERM nsetopt_lvl_ip_msfilter(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;

    if (COMPARE(eVal, atom_null) == 0) {
        return nsetopt_lvl_ip_msfilter_set(env, descP->sock, NULL, 0);
    } else {
        struct ip_msfilter* msfP;
        Uint32              msfSz;
        ERL_NIF_TERM        eMultiAddr, eInterface, eFMode, eSList, elem, tail;
        size_t              sz;
        unsigned int        slistLen, idx;

        if (!IS_MAP(env, eVal))
            return esock_make_error(env, esock_atom_einval);
        
        // It must have atleast four attributes
        if (!enif_get_map_size(env, eVal, &sz) || (sz < 4))
            return esock_make_error(env, esock_atom_einval);

        if (!GET_MAP_VAL(env, eVal, atom_multiaddr, &eMultiAddr))
            return esock_make_error(env, esock_atom_einval);
        
        if (!GET_MAP_VAL(env, eVal, atom_interface, &eInterface))
            return esock_make_error(env, esock_atom_einval);
        
        if (!GET_MAP_VAL(env, eVal, atom_mode, &eFMode))
            return esock_make_error(env, esock_atom_einval);
        
        if (!GET_MAP_VAL(env, eVal, atom_slist, &eSList))
            return esock_make_error(env, esock_atom_einval);

        /* We start (decoding) with the slist, since without it we don't
         * really know how much (memory) to allocate.
         */
        if (!GET_LIST_LEN(env, eSList, &slistLen))
            return esock_make_error(env, esock_atom_einval);

        msfSz = IP_MSFILTER_SIZE(slistLen);
        msfP  = MALLOC(msfSz);

        if (!esock_decode_ip4_address(env, eMultiAddr, &msfP->imsf_multiaddr)) {
            FREE(msfP);
            return esock_make_error(env, esock_atom_einval);
        }
        
        if (!esock_decode_ip4_address(env, eInterface, &msfP->imsf_interface)) {
            FREE(msfP);
            return esock_make_error(env, esock_atom_einval);
        }
        
        if (!decode_ip_msfilter_mode(env, eFMode, (Uint32*) &msfP->imsf_fmode)) {
            FREE(msfP);
            return esock_make_error(env, esock_atom_einval);
        }

        /* And finally, extract the source addresses */
        msfP->imsf_numsrc = slistLen;
        for (idx = 0; idx < slistLen; idx++) {
            if (GET_LIST_ELEM(env, eSList, &elem, &tail)) {
                if (!esock_decode_ip4_address(env, elem, &msfP->imsf_slist[idx])) {
                    FREE(msfP);
                    return esock_make_error(env, esock_atom_einval);
                } else {
                    eSList = tail;
                }
            }
        }

        /* And now, finally, set the option */
        result = nsetopt_lvl_ip_msfilter_set(env, descP->sock, msfP, msfSz);
        FREE(msfP);
        return result;
    }

}


static
BOOLEAN_T decode_ip_msfilter_mode(ErlNifEnv*   env,
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


static
ERL_NIF_TERM nsetopt_lvl_ip_msfilter_set(ErlNifEnv*          env,
                                         SOCKET              sock,
                                         struct ip_msfilter* msfP,
                                         SOCKLEN_T           optLen)
{
    ERL_NIF_TERM result;
    int          res;
#if defined(SOL_IP)
    int          level = SOL_IP;
#else
    int          level = IPPROTO_IP;
#endif

    res = socket_setopt(sock, level, IP_MSFILTER, (void*) msfP, optLen);
    if (res != 0)
        result = esock_make_error_errno(env, sock_errno());
    else
        result = esock_atom_ok;

    return result;
}
#endif // IP_MSFILTER



/* nsetopt_lvl_ip_mtu_discover - Level IP MTU_DISCOVER option
 *
 * The value is an atom of the type ip_pmtudisc().
 */
#if defined(IP_MTU_DISCOVER)
static
ERL_NIF_TERM nsetopt_lvl_ip_mtu_discover(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM   result;
    int            val;
    char*          xres;
    int            res;
#if defined(SOL_IP)
    int            level = SOL_IP;
#else
    int            level = IPPROTO_IP;
#endif

    if ((xres = decode_ip_pmtudisc(env, eVal, &val)) != NULL) {

        result = esock_make_error_str(env, xres);

    } else {

        res = socket_setopt(descP->sock, level, IP_MTU_DISCOVER,
                            &val, sizeof(val));

        if (res != 0)
            result = esock_make_error_errno(env, sock_errno());
        else
            result = esock_atom_ok;

    }

    return result;
}
#endif


/* nsetopt_lvl_ip_multicast_all - Level IP MULTICAST_ALL option
 */
#if defined(IP_MULTICAST_ALL)
static
ERL_NIF_TERM nsetopt_lvl_ip_multicast_all(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_MULTICAST_ALL, eVal);
}
#endif


/* nsetopt_lvl_ip_multicast_if - Level IP MULTICAST_IF option
 *
 * The value is either the atom 'any' or a 4-tuple.
 */
#if defined(IP_MULTICAST_IF)
static
ERL_NIF_TERM nsetopt_lvl_ip_multicast_if(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM   result;
    struct in_addr ifAddr;
    char*          xres;
    int            res;
#if defined(SOL_IP)
    int            level = SOL_IP;
#else
    int            level = IPPROTO_IP;
#endif

    if ((xres = esock_decode_ip4_address(env, eVal, &ifAddr)) != NULL) {
        result = esock_make_error_str(env, xres);
    } else {
        
        res = socket_setopt(descP->sock, level, IP_MULTICAST_LOOP,
                            &ifAddr, sizeof(ifAddr));

        if (res != 0)
            result = esock_make_error_errno(env, sock_errno());
        else
            result = esock_atom_ok;

    }

    return result;
}
#endif


/* nsetopt_lvl_ip_multicast_loop - Level IP MULTICAST_LOOP option
 */
#if defined(IP_MULTICAST_LOOP)
static
ERL_NIF_TERM nsetopt_lvl_ip_multicast_loop(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_MULTICAST_LOOP, eVal);
}
#endif


/* nsetopt_lvl_ip_multicast_ttl - Level IP MULTICAST_TTL option
 */
#if defined(IP_MULTICAST_TTL)
static
ERL_NIF_TERM nsetopt_lvl_ip_multicast_ttl(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_int_opt(env, descP, level, IP_MULTICAST_TTL, eVal);
}
#endif


/* nsetopt_lvl_ip_nodefrag - Level IP NODEFRAG option
 */
#if defined(IP_NODEFRAG)
static
ERL_NIF_TERM nsetopt_lvl_ip_nodefrag(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_NODEFRAG, eVal);
}
#endif


/* nsetopt_lvl_ip_pktinfo - Level IP PKTINFO option
 */
#if defined(IP_PKTINFO)
static
ERL_NIF_TERM nsetopt_lvl_ip_pktinfo(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_PKTINFO, eVal);
}
#endif


/* nsetopt_lvl_ip_recvdstaddr - Level IP RECVDSTADDR option
 */
#if defined(IP_RECVDSTADDR)
static
ERL_NIF_TERM nsetopt_lvl_ip_recvdstaddr(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_RECVDSTADDR, eVal);
}
#endif


/* nsetopt_lvl_ip_recverr - Level IP RECVERR option
 */
#if defined(IP_RECVERR)
static
ERL_NIF_TERM nsetopt_lvl_ip_recverr(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_RECVERR, eVal);
}
#endif


/* nsetopt_lvl_ip_recvif - Level IP RECVIF option
 */
#if defined(IP_RECVIF)
static
ERL_NIF_TERM nsetopt_lvl_ip_recvif(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_RECVIF, eVal);
}
#endif


/* nsetopt_lvl_ip_recvopts - Level IP RECVOPTS option
 */
#if defined(IP_RECVOPTS)
static
ERL_NIF_TERM nsetopt_lvl_ip_recvopts(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_RECVOPTS, eVal);
}
#endif


/* nsetopt_lvl_ip_recvorigdstaddr - Level IP RECVORIGDSTADDR option
 */
#if defined(IP_RECVORIGDSTADDR)
static
ERL_NIF_TERM nsetopt_lvl_ip_recvorigdstaddr(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_RECVORIGDSTADDR, eVal);
}
#endif


/* nsetopt_lvl_ip_recvtos - Level IP RECVTOS option
 */
#if defined(IP_RECVTOS)
static
ERL_NIF_TERM nsetopt_lvl_ip_recvtos(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_RECVTOS, eVal);
}
#endif


/* nsetopt_lvl_ip_recvttl - Level IP RECVTTL option
 */
#if defined(IP_RECVTTL)
static
ERL_NIF_TERM nsetopt_lvl_ip_recvttl(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_RECVTTL, eVal);
}
#endif


/* nsetopt_lvl_ip_retopts - Level IP RETOPTS option
 */
#if defined(IP_RETOPTS)
static
ERL_NIF_TERM nsetopt_lvl_ip_retopts(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_RETOPTS, eVal);
}
#endif


/* nsetopt_lvl_ip_router_alert - Level IP ROUTER_ALERT option
 */
#if defined(IP_ROUTER_ALERT)
static
ERL_NIF_TERM nsetopt_lvl_ip_router_alert(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_int_opt(env, descP, level, IP_ROUTER_ALERT, eVal);
}
#endif


/* nsetopt_lvl_ip_sendsrcaddr - Level IP SENDSRCADDR option
 */
#if defined(IP_SENDSRCADDR)
static
ERL_NIF_TERM nsetopt_lvl_ip_sendsrcaddr(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_SENDSRCADDR, eVal);
}
#endif


/* nsetopt_lvl_ip_tos - Level IP TOS option
 */
#if defined(IP_TOS)
static
ERL_NIF_TERM nsetopt_lvl_ip_tos(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int          level = SOL_IP;
#else
    int          level = IPPROTO_IP;
#endif
    ERL_NIF_TERM result;
    int          val;

    if (decode_ip_tos(env, eVal, &val)) {
        int res = socket_setopt(descP->sock, level, IP_TOS, &val, sizeof(val));

        if (res != 0)
            result = esock_make_error_errno(env, sock_errno());
        else
            result = esock_atom_ok;

    } else {
        result = esock_make_error(env, esock_atom_einval);
    }

    return result;
}
#endif


/* nsetopt_lvl_ip_transparent - Level IP TRANSPARENT option
 */
#if defined(IP_TRANSPARENT)
static
ERL_NIF_TERM nsetopt_lvl_ip_transparent(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_TRANSPARENT, eVal);
}
#endif



/* nsetopt_lvl_ip_ttl - Level IP TTL option
 */
#if defined(IP_TTL)
static
ERL_NIF_TERM nsetopt_lvl_ip_ttl(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_int_opt(env, descP, level, IP_TTL, eVal);
}
#endif



/* nsetopt_lvl_ip_unblock_source - Level IP UNBLOCK_SOURCE option
 *
 * The value is a map with three attributes: multiaddr, interface and
 * sourceaddr.
 * The attribute 'multiaddr' is always a 4-tuple (IPv4 address).
 * The attribute 'interface' is always a 4-tuple (IPv4 address).
 * The attribute 'sourceaddr' is always a 4-tuple (IPv4 address).
 * (IPv4 address).
 */
#if defined(IP_UNBLOCK_SOURCE)
static
ERL_NIF_TERM nsetopt_lvl_ip_unblock_source(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal)
{
    return nsetopt_lvl_ip_update_source(env, descP, eVal, IP_UNBLOCK_SOURCE);
}
#endif



#if defined(IP_ADD_MEMBERSHIP) || defined(IP_DROP_MEMBERSHIP)
static
ERL_NIF_TERM nsetopt_lvl_ip_update_membership(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     eVal,
                                              int              opt)
{
    ERL_NIF_TERM   result, eMultiAddr, eInterface;
    struct ip_mreq mreq;
    char*          xres;
    int            res;
    size_t         sz;
#if defined(SOL_IP)
    int            level = SOL_IP;
#else
    int            level = IPPROTO_IP;
#endif

    // It must be a map
    if (!IS_MAP(env, eVal)) {
        SSDBG( descP,
               ("SOCKET", "nsetopt_lvl_ip_update_membership -> "
                "value *not* a map\r\n") );
        return enif_make_badarg(env);
    }

    // It must have atleast two attributes
    if (!enif_get_map_size(env, eVal, &sz) || (sz < 2)) {
        SSDBG( descP,
               ("SOCKET", "nsetopt_lvl_ip_update_membership -> "
                "invalid map value: %T\r\n", eVal) );
        return enif_make_badarg(env);
    }

    if (!GET_MAP_VAL(env, eVal, atom_multiaddr, &eMultiAddr)) {
        SSDBG( descP,
               ("SOCKET", "nsetopt_lvl_ip_update_membership -> "
                "failed get multiaddr (map) attribute\r\n") );
        return enif_make_badarg(env);
    }

    if (!GET_MAP_VAL(env, eVal, atom_interface, &eInterface)) {
        SSDBG( descP,
               ("SOCKET", "nsetopt_lvl_ip_update_membership -> "
                "failed get interface (map) attribute\r\n") );
        return enif_make_badarg(env);
    }

    if ((xres = esock_decode_ip4_address(env,
                                         eMultiAddr,
                                         &mreq.imr_multiaddr)) != NULL) {
        SSDBG( descP,
               ("SOCKET", "nsetopt_lvl_ip_update_membership -> "
                "failed decode multiaddr %T: %s\r\n", eMultiAddr, xres) );
        return esock_make_error_str(env, xres);
    }

    if ((xres = esock_decode_ip4_address(env,
                                         eInterface,
                                         &mreq.imr_interface)) != NULL) {
        SSDBG( descP,
               ("SOCKET", "nsetopt_lvl_ip_update_membership -> "
                "failed decode interface %T: %s\r\n", eInterface, xres) );
        return esock_make_error_str(env, xres);
    }

    res = socket_setopt(descP->sock, level, opt, &mreq, sizeof(mreq));

    if (res != 0) {
        int save_errno = sock_errno();

        result = esock_make_error_errno(env, save_errno);

        SSDBG( descP,
               ("SOCKET", "nsetopt_lvl_ip_update_membership -> "
                "failed setopt: %T (%d)\r\n", result, save_errno) );

    } else {
        result = esock_atom_ok;
    }

    return result;
}
#endif


#if defined(IP_ADD_SOURCE_MEMBERSHIP) || defined(IP_DROP_SOURCE_MEMBERSHIP) || defined(IP_BLOCK_SOURCE) || defined(IP_UNBLOCK_SOURCE)
static
ERL_NIF_TERM nsetopt_lvl_ip_update_source(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ERL_NIF_TERM     eVal,
                                          int              opt)
{
    ERL_NIF_TERM          result, eMultiAddr, eInterface, eSourceAddr;
    struct ip_mreq_source mreq;
    char*                 xres;
    int                   res;
    size_t                sz;
#if defined(SOL_IP)
    int            level = SOL_IP;
#else
    int            level = IPPROTO_IP;
#endif

    // It must be a map
    if (!IS_MAP(env, eVal))
        return enif_make_badarg(env);

    // It must have atleast three attributes
    if (!enif_get_map_size(env, eVal, &sz) || (sz >= 3))
        return enif_make_badarg(env);

    if (!GET_MAP_VAL(env, eVal, atom_multiaddr, &eMultiAddr))
        return enif_make_badarg(env);

    if (!GET_MAP_VAL(env, eVal, atom_interface, &eInterface))
        return enif_make_badarg(env);

    if (!GET_MAP_VAL(env, eVal, atom_sourceaddr, &eSourceAddr))
        return enif_make_badarg(env);

    if ((xres = esock_decode_ip4_address(env,
                                         eMultiAddr,
                                         &mreq.imr_multiaddr)) != NULL)
        return esock_make_error_str(env, xres);

    if ((xres = esock_decode_ip4_address(env,
                                         eInterface,
                                         &mreq.imr_interface)) != NULL)
        return esock_make_error_str(env, xres);

    if ((xres = esock_decode_ip4_address(env,
                                         eSourceAddr,
                                         &mreq.imr_sourceaddr)) != NULL)
        return esock_make_error_str(env, xres);

    res = socket_setopt(descP->sock, level, opt, &mreq, sizeof(mreq));

    if (res != 0)
        result = esock_make_error_errno(env, sock_errno());
    else
        result = esock_atom_ok;

    return result;
}
#endif



/* *** Handling set of socket options for level = ipv6 *** */

/* nsetopt_lvl_ipv6 - Level *IPv6* option(s)
 */
#if defined(HAVE_IPV6)
static
ERL_NIF_TERM nsetopt_lvl_ipv6(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              int              eOpt,
                              ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_ipv6 -> entry with"
            "\r\n   opt: %d"
            "\r\n", eOpt) );

    switch (eOpt) {
#if defined(IPV6_ADDRFORM)
    case SOCKET_OPT_IPV6_ADDRFORM:
        result = nsetopt_lvl_ipv6_addrform(env, descP, eVal);
        break;
#endif

#if defined(IPV6_ADD_MEMBERSHIP)
    case SOCKET_OPT_IPV6_ADD_MEMBERSHIP:
        result = nsetopt_lvl_ipv6_add_membership(env, descP, eVal);
        break;
#endif

#if defined(IPV6_AUTHHDR)
    case SOCKET_OPT_IPV6_AUTHHDR:
        result = nsetopt_lvl_ipv6_authhdr(env, descP, eVal);
        break;
#endif

#if defined(IPV6_DROP_MEMBERSHIP)
    case SOCKET_OPT_IPV6_DROP_MEMBERSHIP:
        result = nsetopt_lvl_ipv6_drop_membership(env, descP, eVal);
        break;
#endif

#if defined(IPV6_DSTOPTS)
    case SOCKET_OPT_IPV6_DSTOPTS:
        result = nsetopt_lvl_ipv6_dstopts(env, descP, eVal);
        break;
#endif

#if defined(IPV6_FLOWINFO)
    case SOCKET_OPT_IPV6_FLOWINFO:
        result = nsetopt_lvl_ipv6_flowinfo(env, descP, eVal);
        break;
#endif

#if defined(IPV6_HOPLIMIT)
    case SOCKET_OPT_IPV6_HOPLIMIT:
        result = nsetopt_lvl_ipv6_hoplimit(env, descP, eVal);
        break;
#endif

#if defined(IPV6_HOPOPTS)
    case SOCKET_OPT_IPV6_HOPOPTS:
        result = nsetopt_lvl_ipv6_hopopts(env, descP, eVal);
        break;
#endif

#if defined(IPV6_MTU)
    case SOCKET_OPT_IPV6_MTU:
        result = nsetopt_lvl_ipv6_mtu(env, descP, eVal);
        break;
#endif

#if defined(IPV6_MTU_DISCOVER)
    case SOCKET_OPT_IPV6_MTU_DISCOVER:
        result = nsetopt_lvl_ipv6_mtu_discover(env, descP, eVal);
        break;
#endif

#if defined(IPV6_MULTICAST_HOPS)
    case SOCKET_OPT_IPV6_MULTICAST_HOPS:
        result = nsetopt_lvl_ipv6_multicast_hops(env, descP, eVal);
        break;
#endif

#if defined(IPV6_MULTICAST_IF)
    case SOCKET_OPT_IPV6_MULTICAST_IF:
        result = nsetopt_lvl_ipv6_multicast_if(env, descP, eVal);
        break;
#endif

#if defined(IPV6_MULTICAST_LOOP)
    case SOCKET_OPT_IPV6_MULTICAST_LOOP:
        result = nsetopt_lvl_ipv6_multicast_loop(env, descP, eVal);
        break;
#endif

#if defined(IPV6_RECVERR)
    case SOCKET_OPT_IPV6_RECVERR:
        result = nsetopt_lvl_ipv6_recverr(env, descP, eVal);
        break;
#endif

#if defined(IPV6_RECVPKTINFO) || defined(IPV6_PKTINFO)
    case SOCKET_OPT_IPV6_RECVPKTINFO:
        result = nsetopt_lvl_ipv6_recvpktinfo(env, descP, eVal);
        break;
#endif

#if defined(IPV6_ROUTER_ALERT)
    case SOCKET_OPT_IPV6_ROUTER_ALERT:
        result = nsetopt_lvl_ipv6_router_alert(env, descP, eVal);
        break;
#endif

#if defined(IPV6_RTHDR)
    case SOCKET_OPT_IPV6_RTHDR:
        result = nsetopt_lvl_ipv6_rthdr(env, descP, eVal);
        break;
#endif

#if defined(IPV6_UNICAST_HOPS)
    case SOCKET_OPT_IPV6_UNICAST_HOPS:
        result = nsetopt_lvl_ipv6_unicast_hops(env, descP, eVal);
        break;
#endif

#if defined(IPV6_V6ONLY)
    case SOCKET_OPT_IPV6_V6ONLY:
        result = nsetopt_lvl_ipv6_v6only(env, descP, eVal);
        break;
#endif

    default:
        SSDBG( descP,
               ("SOCKET", "nsetopt_lvl_ipv6 -> unknown opt (%d)\r\n", eOpt) );
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_ipv6 -> done when"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}


#if defined(IPV6_ADDRFORM)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_addrform(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;
    int          res, edomain, domain;

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_ipv6_addrform -> entry with"
            "\r\n   eVal: %T"
            "\r\n", eVal) );

    if (!GET_INT(env, eVal, &edomain))
        return esock_make_error(env, esock_atom_einval);

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_ipv6_addrform -> decode"
            "\r\n   edomain: %d"
            "\r\n", edomain) );

    if (!edomain2domain(edomain, &domain))
        return esock_make_error(env, esock_atom_einval);

    SSDBG( descP, ("SOCKET", "nsetopt_lvl_ipv6_addrform -> try set opt to %d\r\n",
                   domain) );
    
    res = socket_setopt(descP->sock,
#if defined(SOL_IPV6)
                        SOL_IPV6,
#else
                        IPPROTO_IPV6,
#endif
                        IPV6_ADDRFORM, &domain, sizeof(domain));

    if (res != 0)
        result = esock_make_error_errno(env, sock_errno());
    else
        result = esock_atom_ok;

    return result;
}
#endif


#if defined(IPV6_ADD_MEMBERSHIP)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_add_membership(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     eVal)
{
    return nsetopt_lvl_ipv6_update_membership(env, descP, eVal,
                                              IPV6_ADD_MEMBERSHIP);
}
#endif


#if defined(IPV6_AUTHHDR)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_authhdr(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     eVal)
{
    return nsetopt_bool_opt(env, descP,
#if defined(SOL_IPV6)
                        SOL_IPV6,
#else
                        IPPROTO_IPV6,
#endif
                        IPV6_AUTHHDR, eVal);
}
#endif


#if defined(IPV6_DROP_MEMBERSHIP)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_drop_membership(ErlNifEnv*       env,
                                              ESockDescriptor* descP,
                                              ERL_NIF_TERM     eVal)
{
    return nsetopt_lvl_ipv6_update_membership(env, descP, eVal,
                                              IPV6_DROP_MEMBERSHIP);
}
#endif


#if defined(IPV6_DSTOPTS)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_dstopts(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     eVal)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return nsetopt_bool_opt(env, descP, level, IPV6_DSTOPTS, eVal);
}
#endif


#if defined(IPV6_FLOWINFO)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_flowinfo(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     eVal)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return nsetopt_bool_opt(env, descP, level, IPV6_FLOWINFO, eVal);
}
#endif


#if defined(IPV6_HOPLIMIT)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_hoplimit(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     eVal)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return nsetopt_bool_opt(env, descP, level, IPV6_HOPLIMIT, eVal);
}
#endif


#if defined(IPV6_HOPOPTS)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_hopopts(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     eVal)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return nsetopt_bool_opt(env, descP, level, IPV6_HOPOPTS, eVal);
}
#endif


#if defined(IPV6_MTU)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_mtu(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     eVal)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return nsetopt_int_opt(env, descP, level, IPV6_MTU, eVal);
}
#endif


/* nsetopt_lvl_ipv6_mtu_discover - Level IPv6 MTU_DISCOVER option
 *
 * The value is an atom of the type ipv6_pmtudisc().
 */
#if defined(IPV6_MTU_DISCOVER)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_mtu_discover(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM  result;
    int           val;
    char*         xres;
    int           res;

    if ((xres = decode_ipv6_pmtudisc(env, eVal, &val)) != NULL) {

        result = esock_make_error_str(env, xres);

    } else {
#if defined(SOL_IPV6)
        int level = SOL_IPV6;
#else
        int level = IPPROTO_IPV6;
#endif


        res = socket_setopt(descP->sock, level, IPV6_MTU_DISCOVER,
                            &val, sizeof(val));

        if (res != 0)
            result = esock_make_error_errno(env, sock_errno());
        else
            result = esock_atom_ok;

    }

    return result;
}
#endif


#if defined(IPV6_MULTICAST_HOPS)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_multicast_hops(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     eVal)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return nsetopt_int_opt(env, descP, level, IPV6_MULTICAST_HOPS, eVal);
}
#endif



#if defined(IPV6_MULTICAST_IF)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_multicast_if(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return nsetopt_int_opt(env, descP, level, IPV6_MULTICAST_IF, eVal);
}
#endif



#if defined(IPV6_MULTICAST_LOOP)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_multicast_loop(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     eVal)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return nsetopt_bool_opt(env, descP, level, IPV6_MULTICAST_LOOP, eVal);
}
#endif


#if defined(IPV6_RECVERR)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_recverr(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     eVal)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return nsetopt_bool_opt(env, descP, level, IPV6_RECVERR, eVal);
}
#endif


#if defined(IPV6_RECVPKTINFO) || defined(IPV6_PKTINFO)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_recvpktinfo(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ERL_NIF_TERM     eVal)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif
#if defined(IPV6_RECVPKTINFO)
    int opt = IPV6_RECVPKTINFO;
#else
    int opt = IPV6_PKTINFO;
#endif

    return nsetopt_bool_opt(env, descP, level, opt, eVal);
}
#endif


#if defined(IPV6_ROUTER_ALERT)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_router_alert(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return nsetopt_int_opt(env, descP, level, IPV6_ROUTER_ALERT, eVal);
}
#endif



#if defined(IPV6_RTHDR)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_rthdr(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     eVal)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return nsetopt_bool_opt(env, descP, level, IPV6_RTHDR, eVal);
}
#endif


#if defined(IPV6_UNICAST_HOPS)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_unicast_hops(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     eVal)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return nsetopt_int_opt(env, descP, level, IPV6_UNICAST_HOPS, eVal);
}
#endif



#if defined(IPV6_V6ONLY)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_v6only(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     eVal)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return nsetopt_bool_opt(env, descP, level, IPV6_V6ONLY, eVal);
}
#endif


#if defined(IPV6_ADD_MEMBERSHIP) || defined(IPV6_DROP_MEMBERSHIP)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_update_membership(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                ERL_NIF_TERM     eVal,
                                                int              opt)
{
    ERL_NIF_TERM     result, eMultiAddr, eInterface;
    struct ipv6_mreq mreq;
    char*            xres;
    int              res;
    size_t           sz;
#if defined(SOL_IPV6)
    int              level = SOL_IPV6;
#else
    int              level = IPPROTO_IPV6;
#endif

    // It must be a map
    if (!IS_MAP(env, eVal)) {
        SSDBG( descP,
               ("SOCKET", "nsetopt_lvl_ipv6_update_membership -> "
                "value *not* a map\r\n") );
        return enif_make_badarg(env);
    }

    // It must have atleast two attributes
    if (!enif_get_map_size(env, eVal, &sz) || (sz < 2)) {
        SSDBG( descP,
               ("SOCKET", "nsetopt_lvl_ipv6_update_membership -> "
                "invalid map value: %T\r\n", eVal) );
        return enif_make_badarg(env);
    }

    if (!GET_MAP_VAL(env, eVal, atom_multiaddr, &eMultiAddr)) {
        SSDBG( descP,
               ("SOCKET", "nsetopt_lvl_ipv6_update_membership -> "
                "failed get multiaddr (map) attribute\r\n") );
        return enif_make_badarg(env);
    }

    if (!GET_MAP_VAL(env, eVal, atom_interface, &eInterface)) {
        SSDBG( descP,
               ("SOCKET", "nsetopt_lvl_ipv6_update_membership -> "
                "failed get interface (map) attribute\r\n") );
        return enif_make_badarg(env);
    }

    if ((xres = esock_decode_ip6_address(env,
                                         eMultiAddr,
                                         &mreq.ipv6mr_multiaddr)) != NULL) {
        SSDBG( descP,
               ("SOCKET", "nsetopt_lvl_ipv6_update_membership -> "
                "failed decode multiaddr %T: %s\r\n", eMultiAddr, xres) );
        return esock_make_error_str(env, xres);
    }

    if (!GET_UINT(env, eInterface, &mreq.ipv6mr_interface)) {
        SSDBG( descP,
               ("SOCKET", "nsetopt_lvl_ip_update_membership -> "
                "failed decode interface %T: %s\r\n", eInterface, xres) );
        return esock_make_error(env, esock_atom_einval);
    }

    res = socket_setopt(descP->sock, level, opt, &mreq, sizeof(mreq));

    if (res != 0) {
        int save_errno = sock_errno();

        result = esock_make_error_errno(env, save_errno);

        SSDBG( descP,
               ("SOCKET", "nsetopt_lvl_ipv6_update_membership -> "
                "failed setopt: %T (%d)\r\n", result, save_errno) );

    } else {
        result = esock_atom_ok;
    }

    return result;
}
#endif



#endif // defined(HAVE_IPV6)



/* nsetopt_lvl_tcp - Level *TCP* option(s)
 */
static
ERL_NIF_TERM nsetopt_lvl_tcp(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             int              eOpt,
                             ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_tcp -> entry with"
            "\r\n   opt: %d"
            "\r\n", eOpt) );

    switch (eOpt) {
#if defined(TCP_CONGESTION)
    case SOCKET_OPT_TCP_CONGESTION:
        result = nsetopt_lvl_tcp_congestion(env, descP, eVal);
        break;
#endif

#if defined(TCP_MAXSEG)
    case SOCKET_OPT_TCP_MAXSEG:
        result = nsetopt_lvl_tcp_maxseg(env, descP, eVal);
        break;
#endif

#if defined(TCP_NODELAY)
    case SOCKET_OPT_TCP_NODELAY:
        result = nsetopt_lvl_tcp_nodelay(env, descP, eVal);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}


/* nsetopt_lvl_tcp_congestion - Level TCP CONGESTION option
 */
#if defined(TCP_CONGESTION)
static
ERL_NIF_TERM nsetopt_lvl_tcp_congestion(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     eVal)
{
    int max = SOCKET_OPT_TCP_CONGESTION_NAME_MAX+1;

    return nsetopt_str_opt(env, descP, IPPROTO_TCP, TCP_CONGESTION, max, eVal);
}
#endif


/* nsetopt_lvl_tcp_maxseg - Level TCP MAXSEG option
 */
#if defined(TCP_MAXSEG)
static
ERL_NIF_TERM nsetopt_lvl_tcp_maxseg(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     eVal)
{
    return nsetopt_int_opt(env, descP, IPPROTO_TCP, TCP_MAXSEG, eVal);
}
#endif


/* nsetopt_lvl_tcp_nodelay - Level TCP NODELAY option
 */
#if defined(TCP_NODELAY)
static
ERL_NIF_TERM nsetopt_lvl_tcp_nodelay(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     eVal)
{
    return nsetopt_bool_opt(env, descP, IPPROTO_TCP, TCP_NODELAY, eVal);
}
#endif



/* nsetopt_lvl_udp - Level *UDP* option(s)
 */
static
ERL_NIF_TERM nsetopt_lvl_udp(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             int              eOpt,
                             ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_udp -> entry with"
            "\r\n   opt: %d"
            "\r\n", eOpt) );

    switch (eOpt) {
#if defined(UDP_CORK)
    case SOCKET_OPT_UDP_CORK:
        result = nsetopt_lvl_udp_cork(env, descP, eVal);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}


/* nsetopt_lvl_udp_cork - Level UDP CORK option
 */
#if defined(UDP_CORK)
static
ERL_NIF_TERM nsetopt_lvl_udp_cork(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     eVal)
{
    return nsetopt_bool_opt(env, descP, IPPROTO_UDP, UDP_CORK, eVal);
}
#endif




/* nsetopt_lvl_sctp - Level *SCTP* option(s)
 */
#if defined(HAVE_SCTP)
static
ERL_NIF_TERM nsetopt_lvl_sctp(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              int              eOpt,
                              ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp -> entry with"
            "\r\n   opt: %d"
            "\r\n", eOpt) );

    switch (eOpt) {
#if defined(SCTP_ASSOCINFO)
    case SOCKET_OPT_SCTP_ASSOCINFO:
        result = nsetopt_lvl_sctp_associnfo(env, descP, eVal);
        break;
#endif

#if defined(SCTP_AUTOCLOSE)
    case SOCKET_OPT_SCTP_AUTOCLOSE:
        result = nsetopt_lvl_sctp_autoclose(env, descP, eVal);
        break;
#endif

#if defined(SCTP_DISABLE_FRAGMENTS)
    case SOCKET_OPT_SCTP_DISABLE_FRAGMENTS:
        result = nsetopt_lvl_sctp_disable_fragments(env, descP, eVal);
        break;
#endif

#if defined(SCTP_EVENTS)
    case SOCKET_OPT_SCTP_EVENTS:
        result = nsetopt_lvl_sctp_events(env, descP, eVal);
        break;
#endif

#if defined(SCTP_INITMSG)
    case SOCKET_OPT_SCTP_INITMSG:
        result = nsetopt_lvl_sctp_initmsg(env, descP, eVal);
        break;
#endif

#if defined(SCTP_MAXSEG)
    case SOCKET_OPT_SCTP_MAXSEG:
        result = nsetopt_lvl_sctp_maxseg(env, descP, eVal);
        break;
#endif

#if defined(SCTP_NODELAY)
    case SOCKET_OPT_SCTP_NODELAY:
        result = nsetopt_lvl_sctp_nodelay(env, descP, eVal);
        break;
#endif

#if defined(SCTP_RTOINFO)
    case SOCKET_OPT_SCTP_RTOINFO:
        result = nsetopt_lvl_sctp_rtoinfo(env, descP, eVal);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}


/* nsetopt_lvl_sctp_associnfo - Level SCTP ASSOCINFO option
 */
#if defined(SCTP_ASSOCINFO)
static
ERL_NIF_TERM nsetopt_lvl_sctp_associnfo(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM            result;
    ERL_NIF_TERM            eAssocId, eMaxRxt, eNumPeerDests;
    ERL_NIF_TERM            ePeerRWND, eLocalRWND, eCookieLife;
    struct sctp_assocparams assocParams;
    int                     res;
    size_t                  sz;
    unsigned int            tmp;

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_associnfo -> entry with"
            "\r\n   eVal: %T"
            "\r\n", eVal) );

    // It must be a map
    if (!IS_MAP(env, eVal))
        return esock_make_error(env, esock_atom_einval);

    // It must have atleast ten attributes
    if (!enif_get_map_size(env, eVal, &sz) || (sz < 6))
        return esock_make_error(env, esock_atom_einval);

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_associnfo -> extract attributes\r\n") );    

    if (!GET_MAP_VAL(env, eVal, atom_assoc_id,          &eAssocId))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_max_rxt,        &eMaxRxt))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_num_peer_dests, &eNumPeerDests))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_peer_rwnd,      &ePeerRWND))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_local_rwnd,     &eLocalRWND))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_cookie_life,    &eCookieLife))
        return esock_make_error(env, esock_atom_einval);

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_associnfo -> decode attributes\r\n") );

    /* On some platforms the assoc id is typed as an unsigned integer (uint32)
     * So, to avoid warnings there, we always make an explicit cast... 
     * Also, size of types matter, so adjust for that...
     */

#if (SIZEOF_INT == 4)
    {
        int tmpAssocId;
        if (!GET_INT(env, eAssocId, &tmpAssocId))
            return esock_make_error(env, esock_atom_einval);
        assocParams.sasoc_assoc_id =
            (typeof(assocParams.sasoc_assoc_id)) tmpAssocId;
    }
#elif (SIZEOF_LONG == 4)
    {
        long tmpAssocId;
        if (!GET_LONG(env, eAssocId, &tmpAssocId))
            return esock_make_error(env, esock_atom_einval);
        assocParams.sasoc_assoc_id =
            (typeof(assocParams.sasoc_assoc_id)) tmpAssocId;
    }
#else
    SIZE CHECK FOR ASSOC ID FAILED
#endif

    
    /*
     * We should really make sure this is ok in erlang (to ensure that 
     * the values (max-rxt and num-peer-dests) fits in 16-bits).
     * The value should be a 16-bit unsigned int...
     * Both sasoc_asocmaxrxt and sasoc_number_peer_destinations.
     */
    
    if (!GET_UINT(env, eMaxRxt, &tmp))
        return esock_make_error(env, esock_atom_einval);
    assocParams.sasoc_asocmaxrxt = (Uint16) tmp;

    if (!GET_UINT(env, eNumPeerDests, &tmp))
        return esock_make_error(env, esock_atom_einval);
    assocParams.sasoc_number_peer_destinations = (Uint16) tmp;

    if (!GET_UINT(env, ePeerRWND, &assocParams.sasoc_peer_rwnd))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_UINT(env, eLocalRWND, &assocParams.sasoc_local_rwnd))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_UINT(env, eCookieLife, &assocParams.sasoc_cookie_life))
        return esock_make_error(env, esock_atom_einval);
    
    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_associnfo -> set associnfo option\r\n") );

    res = socket_setopt(descP->sock, IPPROTO_SCTP, SCTP_ASSOCINFO,
                        &assocParams, sizeof(assocParams));

    if (res != 0)
        result = esock_make_error_errno(env, sock_errno());
    else
        result = esock_atom_ok;

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_associnfo -> done with"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
    
}
#endif


/* nsetopt_lvl_sctp_autoclose - Level SCTP AUTOCLOSE option
 */
#if defined(SCTP_AUTOCLOSE)
static
ERL_NIF_TERM nsetopt_lvl_sctp_autoclose(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     eVal)
{
    return nsetopt_int_opt(env, descP, IPPROTO_SCTP, SCTP_AUTOCLOSE, eVal);
}
#endif


/* nsetopt_lvl_sctp_disable_fragments - Level SCTP DISABLE_FRAGMENTS option
 */
#if defined(SCTP_DISABLE_FRAGMENTS)
static
ERL_NIF_TERM nsetopt_lvl_sctp_disable_fragments(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                ERL_NIF_TERM     eVal)
{
    return nsetopt_bool_opt(env, descP, IPPROTO_SCTP, SCTP_DISABLE_FRAGMENTS, eVal);
}
#endif


/* nsetopt_lvl_sctp_events - Level SCTP EVENTS option
 */
#if defined(SCTP_EVENTS)
static
ERL_NIF_TERM nsetopt_lvl_sctp_events(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM                result;
    ERL_NIF_TERM                eDataIn, eAssoc, eAddr, eSndFailure;
    ERL_NIF_TERM                ePeerError, eShutdown, ePartialDelivery;
    ERL_NIF_TERM                eAdaptLayer;
#if defined(HAVE_STRUCT_SCTP_EVENT_SUBSCRIBE_SCTP_AUTHENTICATION_EVENT)
    ERL_NIF_TERM                eAuth;
#endif
#if defined(HAVE_STRUCT_SCTP_EVENT_SUBSCRIBE_SCTP_SENDER_DRY_EVENT)
    ERL_NIF_TERM                eSndDry;
#endif
    struct sctp_event_subscribe events;
    int                         res;
    size_t                      sz;

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_events -> entry with"
            "\r\n   eVal: %T"
            "\r\n", eVal) );

    // It must be a map
    if (!IS_MAP(env, eVal))
        return esock_make_error(env, esock_atom_einval);

    // It must have atleast ten attributes
    if (!enif_get_map_size(env, eVal, &sz) || (sz < 10))
        return esock_make_error(env, esock_atom_einval);

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_events -> extract attributes\r\n") );    

    if (!GET_MAP_VAL(env, eVal, atom_data_in,          &eDataIn))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_association,      &eAssoc))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_address,          &eAddr))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_send_failure,     &eSndFailure))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_peer_error,       &ePeerError))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_shutdown,         &eShutdown))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_partial_delivery, &ePartialDelivery))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_adaptation_layer, &eAdaptLayer))
        return esock_make_error(env, esock_atom_einval);

#if defined(HAVE_STRUCT_SCTP_EVENT_SUBSCRIBE_SCTP_AUTHENTICATION_EVENT)
    if (!GET_MAP_VAL(env, eVal, atom_authentication,   &eAuth))
        return esock_make_error(env, esock_atom_einval);
#endif

#if defined(HAVE_STRUCT_SCTP_EVENT_SUBSCRIBE_SCTP_SENDER_DRY_EVENT)
    if (!GET_MAP_VAL(env, eVal, atom_sender_dry,       &eSndDry))
        return esock_make_error(env, esock_atom_einval);
#endif

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_events -> decode attributes\r\n") );

    events.sctp_data_io_event          = esock_decode_bool(eDataIn);
    events.sctp_association_event      = esock_decode_bool(eAssoc);
    events.sctp_address_event          = esock_decode_bool(eAddr);
    events.sctp_send_failure_event     = esock_decode_bool(eSndFailure);
    events.sctp_peer_error_event       = esock_decode_bool(ePeerError);
    events.sctp_shutdown_event         = esock_decode_bool(eShutdown);
    events.sctp_partial_delivery_event = esock_decode_bool(ePartialDelivery);
    events.sctp_adaptation_layer_event = esock_decode_bool(eAdaptLayer);
#if defined(HAVE_STRUCT_SCTP_EVENT_SUBSCRIBE_SCTP_AUTHENTICATION_EVENT)
    events.sctp_authentication_event   = esock_decode_bool(eAuth);
#endif
#if defined(HAVE_STRUCT_SCTP_EVENT_SUBSCRIBE_SCTP_SENDER_DRY_EVENT)
    events.sctp_sender_dry_event       = esock_decode_bool(eSndDry);
#endif
    
    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_events -> set events option\r\n") );

    res = socket_setopt(descP->sock, IPPROTO_SCTP, SCTP_EVENTS,
                        &events, sizeof(events));

    if (res != 0)
        result = esock_make_error_errno(env, sock_errno());
    else
        result = esock_atom_ok;

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_events -> done with"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
    
}
#endif


/* nsetopt_lvl_sctp_initmsg - Level SCTP INITMSG option
 */
#if defined(SCTP_INITMSG)
static
ERL_NIF_TERM nsetopt_lvl_sctp_initmsg(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM        result;
    ERL_NIF_TERM        eNumOut, eMaxIn, eMaxAttempts, eMaxInitTO;
    struct sctp_initmsg initMsg;
    int                 res;
    size_t              sz;
    unsigned int        tmp;

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_initmsg -> entry with"
            "\r\n   eVal: %T"
            "\r\n", eVal) );

    // It must be a map
    if (!IS_MAP(env, eVal))
        return esock_make_error(env, esock_atom_einval);

    // It must have atleast ten attributes
    if (!enif_get_map_size(env, eVal, &sz) || (sz < 4))
        return esock_make_error(env, esock_atom_einval);

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_initmsg -> extract attributes\r\n") );    

    if (!GET_MAP_VAL(env, eVal, atom_num_outstreams, &eNumOut))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_max_instreams,  &eMaxIn))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_max_attempts,   &eMaxAttempts))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_max_init_timeo, &eMaxInitTO))
        return esock_make_error(env, esock_atom_einval);

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_initmsg -> decode attributes\r\n") );

    if (!GET_UINT(env, eNumOut, &tmp))
        return esock_make_error(env, esock_atom_einval);
    initMsg.sinit_num_ostreams = (Uint16) tmp;
    
    if (!GET_UINT(env, eMaxIn, &tmp))
        return esock_make_error(env, esock_atom_einval);
    initMsg.sinit_max_instreams = (Uint16) tmp;
    
    if (!GET_UINT(env, eMaxAttempts, &tmp))
        return esock_make_error(env, esock_atom_einval);
    initMsg.sinit_max_attempts = (Uint16) tmp;
    
    if (!GET_UINT(env, eMaxInitTO, &tmp))
        return esock_make_error(env, esock_atom_einval);
    initMsg.sinit_max_init_timeo = (Uint16) tmp;
    
    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_initmsg -> set initmsg option\r\n") );

    res = socket_setopt(descP->sock, IPPROTO_SCTP, SCTP_INITMSG,
                        &initMsg, sizeof(initMsg));

    if (res != 0)
        result = esock_make_error_errno(env, sock_errno());
    else
        result = esock_atom_ok;

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_initmsg -> done with"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
    
}
#endif


/* nsetopt_lvl_sctp_maxseg - Level SCTP MAXSEG option
 */
#if defined(SCTP_MAXSEG)
static
ERL_NIF_TERM nsetopt_lvl_sctp_maxseg(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     eVal)
{
    return nsetopt_int_opt(env, descP, IPPROTO_SCTP, SCTP_MAXSEG, eVal);
}
#endif


/* nsetopt_lvl_sctp_nodelay - Level SCTP NODELAY option
 */
#if defined(SCTP_NODELAY)
static
ERL_NIF_TERM nsetopt_lvl_sctp_nodelay(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     eVal)
{
    return nsetopt_bool_opt(env, descP, IPPROTO_SCTP, SCTP_NODELAY, eVal);
}
#endif


/* nsetopt_lvl_sctp_rtoinfo - Level SCTP RTOINFO option
 */
#if defined(SCTP_RTOINFO)
static
ERL_NIF_TERM nsetopt_lvl_sctp_rtoinfo(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM        result;
    ERL_NIF_TERM        eAssocId, eInitial, eMax, eMin;
    struct sctp_rtoinfo rtoInfo;
    int                 res;
    size_t              sz;

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_rtoinfo -> entry with"
            "\r\n   eVal: %T"
            "\r\n", eVal) );

    // It must be a map
    if (!IS_MAP(env, eVal))
        return esock_make_error(env, esock_atom_einval);

    // It must have atleast ten attributes
    if (!enif_get_map_size(env, eVal, &sz) || (sz < 4))
        return esock_make_error(env, esock_atom_einval);

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_rtoinfo -> extract attributes\r\n") );    

    if (!GET_MAP_VAL(env, eVal, atom_assoc_id, &eAssocId))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_initial,  &eInitial))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_max,      &eMax))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_MAP_VAL(env, eVal, atom_min,      &eMin))
        return esock_make_error(env, esock_atom_einval);

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_rtoinfo -> decode attributes\r\n") );

    /* On some platforms the assoc id is typed as an unsigned integer (uint32)
     * So, to avoid warnings there, we always make an explicit cast... 
     * Also, size of types matter, so adjust for that...
     */

#if (SIZEOF_INT == 4)
    {
        int tmpAssocId;
        if (!GET_INT(env, eAssocId, &tmpAssocId))
            return esock_make_error(env, esock_atom_einval);
        rtoInfo.srto_assoc_id = (typeof(rtoInfo.srto_assoc_id)) tmpAssocId;
    }
#elif (SIZEOF_LONG == 4)
    {
        long tmpAssocId;
        if (!GET_LONG(env, eAssocId, &tmpAssocId))
            return esock_make_error(env, esock_atom_einval);
        rtoInfo.srto_assoc_id = (typeof(rtoInfo.srto_assoc_id)) tmpAssocId;
    }
#else
    SIZE CHECK FOR ASSOC ID FAILED
#endif
    
    if (!GET_UINT(env, eInitial, &rtoInfo.srto_initial))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_UINT(env, eMax, &rtoInfo.srto_max))
        return esock_make_error(env, esock_atom_einval);

    if (!GET_UINT(env, eMin, &rtoInfo.srto_min))
        return esock_make_error(env, esock_atom_einval);

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_rtoinfo -> set associnfo option\r\n") );

    res = socket_setopt(descP->sock, IPPROTO_SCTP, SCTP_RTOINFO,
                        &rtoInfo, sizeof(rtoInfo));

    if (res != 0)
        result = esock_make_error_errno(env, sock_errno());
    else
        result = esock_atom_ok;

    SSDBG( descP,
           ("SOCKET", "nsetopt_lvl_sctp_rtoinfo -> done with"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
    
}
#endif



#endif // defined(HAVE_SCTP)




/* nsetopt_bool_opt - set an option that has an (integer) bool value
 */
static
ERL_NIF_TERM nsetopt_bool_opt(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              int              level,
                              int              opt,
                              ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;
    BOOLEAN_T    val;
    int          ival, res;

    val = esock_decode_bool(eVal);
    
    ival = (val) ? 1 : 0;
    res  = socket_setopt(descP->sock, level, opt, &ival, sizeof(ival));

    if (res != 0)
        result = esock_make_error_errno(env, sock_errno());
    else
        result = esock_atom_ok;

    return result;
}


/* nsetopt_int_opt - set an option that has an integer value
 */
static
ERL_NIF_TERM nsetopt_int_opt(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             int              level,
                             int              opt,
                             ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;
    int          val;

    if (GET_INT(env, eVal, &val)) {
        int res;

        /*
        SSDBG( descP,
               ("SOCKET", "nsetopt_int_opt -> set option"
                "\r\n   opt: %d"
                "\r\n   val: %d"
                "\r\n", opt, val) );
        */

        res = socket_setopt(descP->sock, level, opt, &val, sizeof(val));

        if (res != 0)
            result = esock_make_error_errno(env, sock_errno());
        else
            result = esock_atom_ok;

    } else {
        result = esock_make_error(env, esock_atom_einval);
    }

    return result;
}


/* nsetopt_str_opt - set an option that has an string value
 */
#if defined(USE_SETOPT_STR_OPT)
static
ERL_NIF_TERM nsetopt_str_opt(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             int              level,
                             int              opt,
                             int              max,
                             ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM result;
    char*        val = MALLOC(max);

    if (GET_STR(env, eVal, val, max) > 0) {
        int optLen = strlen(val);
        int res    = socket_setopt(descP->sock, level, opt, &val, optLen);

        if (res != 0)
            result = esock_make_error_errno(env, sock_errno());
        else
            result = esock_atom_ok;

    } else {
        result = esock_make_error(env, esock_atom_einval);
    }

    FREE(val);

    return result;
}
#endif


/* nsetopt_timeval_opt - set an option that has an (timeval) bool value
 */
static
ERL_NIF_TERM nsetopt_timeval_opt(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 int              level,
                                 int              opt,
                                 ERL_NIF_TERM     eVal)
{
    ERL_NIF_TERM   result;
    struct timeval timeVal;
    int            res;
    char*          xres;

    SSDBG( descP,
           ("SOCKET", "nsetopt_timeval_opt -> entry with"
            "\r\n   eVal: %T"
            "\r\n", eVal) );

    if ((xres = esock_decode_timeval(env, eVal, &timeVal)) != NULL)
        return esock_make_error_str(env, xres);

    SSDBG( descP,
           ("SOCKET", "nsetopt_timeval_opt -> set timeval option\r\n") );

    res = socket_setopt(descP->sock, level, opt, &timeVal, sizeof(timeVal));

    if (res != 0)
        result = esock_make_error_errno(env, sock_errno());
    else
        result = esock_atom_ok;

    SSDBG( descP,
           ("SOCKET", "nsetopt_timeval_opt -> done with"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
    
}



static
BOOLEAN_T elevel2level(BOOLEAN_T  isEncoded,
                       int        eLevel,
                       BOOLEAN_T* isOTP,
                       int*       level)
{
    BOOLEAN_T result;

    if (isEncoded) {
        switch (eLevel) {
        case SOCKET_OPT_LEVEL_OTP:
            *isOTP = TRUE;
            *level = -1;
            result = TRUE;
            break;

        case SOCKET_OPT_LEVEL_SOCKET:
            *isOTP = FALSE;
            *level = SOL_SOCKET;
            result = TRUE;
            break;

        case SOCKET_OPT_LEVEL_IP:
            *isOTP = FALSE;
#if defined(SOL_IP)
            *level = SOL_IP;
#else
            *level = IPPROTO_IP;
#endif
            result = TRUE;
            break;

#if defined(HAVE_IPV6)
        case SOCKET_OPT_LEVEL_IPV6:
            *isOTP = FALSE;
#if defined(SOL_IPV6)
            *level = SOL_IPV6;
#else
            *level = IPPROTO_IPV6;
#endif
            result = TRUE;
            break;
#endif

        case SOCKET_OPT_LEVEL_TCP:
            *isOTP = FALSE;
            *level = IPPROTO_TCP;
            result = TRUE;
            break;

        case SOCKET_OPT_LEVEL_UDP:
            *isOTP = FALSE;
            *level = IPPROTO_UDP;
            result = TRUE;
            break;

#ifdef HAVE_SCTP
        case SOCKET_OPT_LEVEL_SCTP:
            *isOTP = FALSE;
            *level = IPPROTO_SCTP;
            result = TRUE;
            break;
#endif

        default:
            *isOTP = FALSE;
            *level = -1;
            result = FALSE;
            break;
        }
    } else {
        *isOTP = FALSE;
        *level = eLevel;
        result = TRUE;
    }

    return result;
}



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
static
int socket_setopt(int sock, int level, int opt,
                  const void* optVal, const socklen_t optLen)
{
    int res;

#if  defined(IP_TOS) && defined(SOL_IP) && defined(SO_PRIORITY)
    int          tmpIValPRIO;
    int          tmpIValTOS;
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
#endif // if !defined(__WIN32__)



/* ----------------------------------------------------------------------
 * nif_getopt
 *
 * Description:
 * Get socket option.
 * Its possible to use a "raw" mode (not encoded). That is, we do not
 * interpret level and opt. They are passed "as is" to the
 * getsockopt function call. The value in this case will "copied" as
 * is and provided to the user in the form of a binary.
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * IsEncoded    - Are the "arguments" encoded or not.
 * Level        - Level of the socket option.
 * Opt          - The socket option.
 */

static
ERL_NIF_TERM nif_getopt(ErlNifEnv*         env,
                        int                argc,
                        const ERL_NIF_TERM argv[])
{
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    int              eLevel, level = -1;
    ERL_NIF_TERM     eIsEncoded, eOpt;
    BOOLEAN_T        isEncoded, isOTP;
    ERL_NIF_TERM     result;

    SGDBG( ("SOCKET", "nif_getopt -> entry with argc: %d\r\n", argc) );

    if ((argc != 4) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP) ||
        !GET_INT(env, argv[2], &eLevel)) {
        SGDBG( ("SOCKET", "nif_getopt -> failed processing args\r\n") );
        return enif_make_badarg(env);
    }
    eIsEncoded = argv[1];
    eOpt       = argv[3]; // Is "normally" an int, but if raw mode: {Int, ValueSz}

    if (IS_CLOSED(descP) || IS_CLOSING(descP))
        return esock_make_error(env, atom_closed);
    
    SSDBG( descP,
           ("SOCKET", "nif_getopt -> args when sock = %d:"
            "\r\n   Socket:     %T"
            "\r\n   eIsEncoded: %T"
            "\r\n   eLevel:     %d"
            "\r\n   eOpt:       %T"
            "\r\n", descP->sock, argv[0], eIsEncoded, eLevel, eOpt) );

    isEncoded = esock_decode_bool(eIsEncoded);

    if (!elevel2level(isEncoded, eLevel, &isOTP, &level))
        return esock_make_error(env, esock_atom_einval);

    MLOCK(descP->cfgMtx);

    result = ngetopt(env, descP, isEncoded, isOTP, level, eOpt);
    
    MUNLOCK(descP->cfgMtx);

    return result;

#endif // if defined(__WIN32__)
}



#if !defined(__WIN32__)
static
ERL_NIF_TERM ngetopt(ErlNifEnv*       env,
                     ESockDescriptor* descP,
                     BOOLEAN_T        isEncoded,
                     BOOLEAN_T        isOTP,
                     int              level,
                     ERL_NIF_TERM     eOpt)
{
    ERL_NIF_TERM result;
    int          opt;

    SSDBG( descP,
           ("SOCKET", "ngetopt -> entry with"
            "\r\n   isEncoded: %s"
            "\r\n   isOTP:     %s"
            "\r\n   level:     %d"
            "\r\n   eOpt:      %T"
            "\r\n", B2S(isEncoded), B2S(isOTP), level, eOpt) );

    if (isOTP) {
        /* These are not actual socket options,
         * but options for our implementation.
         */
        if (GET_INT(env, eOpt, &opt))
            result = ngetopt_otp(env, descP, opt);
        else
            result = esock_make_error(env, esock_atom_einval);
    } else if (!isEncoded) {
        result = ngetopt_native(env, descP, level, eOpt);
    } else {
        if (GET_INT(env, eOpt, &opt))
            result = ngetopt_level(env, descP, level, opt);
        else
            result = esock_make_error(env, esock_atom_einval);
    }

    SSDBG( descP,
           ("SOCKET", "ngetopt -> done when"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}



/* ngetopt_otp - Handle OTP (level) options
 */
static
ERL_NIF_TERM ngetopt_otp(ErlNifEnv*       env,
                         ESockDescriptor* descP,
                         int              eOpt)
{
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("SOCKET", "ngetopt_otp -> entry with"
            "\r\n   eOpt: %d"
            "\r\n", eOpt) );

    switch (eOpt) {
    case SOCKET_OPT_OTP_DEBUG:
        result = ngetopt_otp_debug(env, descP);
        break;

    case SOCKET_OPT_OTP_IOW:
        result = ngetopt_otp_iow(env, descP);
        break;

    case SOCKET_OPT_OTP_CTRL_PROC:
        result = ngetopt_otp_ctrl_proc(env, descP);
        break;

    case SOCKET_OPT_OTP_RCVBUF:
        result = ngetopt_otp_rcvbuf(env, descP);
        break;

    case SOCKET_OPT_OTP_RCVCTRLBUF:
        result = ngetopt_otp_rcvctrlbuf(env, descP);
        break;

    case SOCKET_OPT_OTP_SNDCTRLBUF:
        result = ngetopt_otp_sndctrlbuf(env, descP);
        break;

    case SOCKET_OPT_OTP_FD:
        result = ngetopt_otp_fd(env, descP);
        break;

        /* *** INTERNAL *** */
    case SOCKET_OPT_OTP_DOMAIN:
        result = ngetopt_otp_domain(env, descP);
        break;

    case SOCKET_OPT_OTP_TYPE:
        result = ngetopt_otp_type(env, descP);
        break;

    case SOCKET_OPT_OTP_PROTOCOL:
        result = ngetopt_otp_protocol(env, descP);
        break;

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    SSDBG( descP,
           ("SOCKET", "ngetopt_otp -> done when"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}


/* ngetopt_otp_debug - Handle the OTP (level) debug option
 */
static
ERL_NIF_TERM ngetopt_otp_debug(ErlNifEnv*       env,
                               ESockDescriptor* descP)
{
    ERL_NIF_TERM eVal = esock_encode_bool(descP->dbg);

    return esock_make_ok2(env, eVal);
}


/* ngetopt_otp_iow - Handle the OTP (level) iow option
 */
static
ERL_NIF_TERM ngetopt_otp_iow(ErlNifEnv*       env,
                             ESockDescriptor* descP)
{
    ERL_NIF_TERM eVal = esock_encode_bool(descP->iow);

    return esock_make_ok2(env, eVal);
}


/* ngetopt_otp_ctrl_proc - Handle the OTP (level) controlling_process option
 */
static
ERL_NIF_TERM ngetopt_otp_ctrl_proc(ErlNifEnv*       env,
                                   ESockDescriptor* descP)
{
    ERL_NIF_TERM eVal = MKPID(env, &descP->ctrlPid);

    return esock_make_ok2(env, eVal);
}



/* ngetopt_otp_rcvbuf - Handle the OTP (level) rcvbuf option
 */
static
ERL_NIF_TERM ngetopt_otp_rcvbuf(ErlNifEnv*       env,
                                ESockDescriptor* descP)
{
    ERL_NIF_TERM eVal;

    if (descP->rNum == 0) {
        eVal = MKI(env, descP->rBufSz);
    } else {
        eVal = MKT2(env, MKI(env, descP->rNum), MKI(env, descP->rBufSz));
    }

    return esock_make_ok2(env, eVal);
}


/* ngetopt_otp_rcvctrlbuf - Handle the OTP (level) rcvctrlbuf option
 */
static
ERL_NIF_TERM ngetopt_otp_rcvctrlbuf(ErlNifEnv*       env,
                                    ESockDescriptor* descP)
{
    ERL_NIF_TERM eVal = MKI(env, descP->rCtrlSz);

    return esock_make_ok2(env, eVal);
}


/* ngetopt_otp_sndctrlbuf - Handle the OTP (level) sndctrlbuf option
 */
static
ERL_NIF_TERM ngetopt_otp_sndctrlbuf(ErlNifEnv*       env,
                                    ESockDescriptor* descP)
{
    ERL_NIF_TERM eVal = MKI(env, descP->wCtrlSz);

    return esock_make_ok2(env, eVal);
}


/* ngetopt_otp_fd - Handle the OTP (level) fd option
 */
static
ERL_NIF_TERM ngetopt_otp_fd(ErlNifEnv*       env,
                            ESockDescriptor* descP)
{
    ERL_NIF_TERM eVal = MKI(env, descP->sock);

    return esock_make_ok2(env, eVal);
}


/* ngetopt_otp_domain - Handle the OTP (level) domain option
 */
static
ERL_NIF_TERM ngetopt_otp_domain(ErlNifEnv*       env,
                                ESockDescriptor* descP)
{
    ERL_NIF_TERM result, reason;
    int          val = descP->domain;

    switch (val) {
    case AF_INET:
        result = esock_make_ok2(env, esock_atom_inet);
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        result = esock_make_ok2(env, esock_atom_inet6);
        break;
#endif

#if defined(HAVE_SYS_UN_H)
    case AF_UNIX:
        result = esock_make_ok2(env, esock_atom_local);
        break;
#endif

    default:
        reason = MKT2(env, esock_atom_unknown, MKI(env, val));
        result = esock_make_error(env, reason);
        break;
    }
    
    return result;
}


/* ngetopt_otp_type - Handle the OTP (level) type options.
 */
static
ERL_NIF_TERM ngetopt_otp_type(ErlNifEnv*       env,
                              ESockDescriptor* descP)
{
    ERL_NIF_TERM result, reason;
    int          val = descP->type;

    switch (val) {
    case SOCK_STREAM:
        result = esock_make_ok2(env, esock_atom_stream);
        break;

    case SOCK_DGRAM:
        result = esock_make_ok2(env, esock_atom_dgram);
        break;

#ifdef HAVE_SCTP
    case SOCK_SEQPACKET:
        result = esock_make_ok2(env, esock_atom_seqpacket);
        break;
#endif
    case SOCK_RAW:
        result = esock_make_ok2(env, esock_atom_raw);
        break;

    case SOCK_RDM:
        result = esock_make_ok2(env, esock_atom_rdm);
        break;

    default:
        reason = MKT2(env, esock_atom_unknown, MKI(env, val));
        result = esock_make_error(env, reason);
        break;
    }

    return result;
}


/* ngetopt_otp_protocol - Handle the OTP (level) protocol options.
 */
static
ERL_NIF_TERM ngetopt_otp_protocol(ErlNifEnv*       env,
                                  ESockDescriptor* descP)
{
    ERL_NIF_TERM result, reason;
    int          val = descP->protocol;

        switch (val) {
        case IPPROTO_IP:
#if defined(AF_LOCAL)
            if (descP->domain == AF_LOCAL) {
                result = esock_make_ok2(env, esock_atom_default);
            } else {
                result = esock_make_ok2(env, esock_atom_ip);
            }
#else
            result = esock_make_ok2(env, esock_atom_ip);
#endif
            break;

        case IPPROTO_TCP:
            result = esock_make_ok2(env, esock_atom_tcp);
            break;

        case IPPROTO_UDP:
            result = esock_make_ok2(env, esock_atom_udp);
            break;

#if defined(HAVE_SCTP)
        case IPPROTO_SCTP:
            result = esock_make_ok2(env, esock_atom_sctp);
            break;
#endif

        default:
            reason = MKT2(env, esock_atom_unknown, MKI(env, val));
            result = esock_make_error(env, reason);
            break;
    }

    return result;
}



/* The option has *not* been encoded. Instead it has been provided
 * in "native mode" (option is provided as is). In this case it will have the
 * format: {NativeOpt :: integer(), ValueSize :: non_neg_integer()}
 */
static
ERL_NIF_TERM ngetopt_native(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            int              level,
                            ERL_NIF_TERM     eOpt)
{
    ERL_NIF_TERM result = enif_make_badarg(env);
    int          opt;
    Uint16       valueType;
    SOCKOPTLEN_T valueSz;

    SSDBG( descP,
           ("SOCKET", "ngetopt_native -> entry with"
            "\r\n   level: %d"
            "\r\n   eOpt:  %T"
            "\r\n", level, eOpt) );

    /* <KOLLA>
     * We should really make it possible to specify more common specific types,
     * such as integer or boolean (instead of the size)...
     * </KOLLA>
     */

    if (decode_native_get_opt(env, eOpt, &opt, &valueType, (int*) &valueSz)) {

        SSDBG( descP,
               ("SOCKET", "ngetopt_native -> decoded opt"
                "\r\n   valueType: %d (%s)"
                "\r\n   ValueSize: %d"
                "\r\n", valueType, VT2S(valueType), valueSz) );

        switch (valueType) {
        case SOCKET_OPT_VALUE_TYPE_UNSPEC:
            result = ngetopt_native_unspec(env, descP, level, opt, valueSz);
            break;
        case SOCKET_OPT_VALUE_TYPE_INT:
            result = ngetopt_int_opt(env, descP, level, opt);
            break;
        case SOCKET_OPT_VALUE_TYPE_BOOL:
            result = ngetopt_bool_opt(env, descP, level, opt);
            break;
        default:
            result = esock_make_error(env, esock_atom_einval);
            break;
        }
    } else {
        result = esock_make_error(env, esock_atom_einval);
    }

    SSDBG( descP,
           ("SOCKET", "ngetopt_native -> done when"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}


static
ERL_NIF_TERM ngetopt_native_unspec(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              level,
                                   int              opt,
                                   SOCKOPTLEN_T     valueSz)
{
    ERL_NIF_TERM result = esock_make_error(env, esock_atom_einval);
    int          res;

    SSDBG( descP,
           ("SOCKET", "ngetopt_native_unspec -> entry with"
            "\r\n   level:   %d"
            "\r\n   opt:     %d"
            "\r\n   valueSz: %d"
            "\r\n", level, opt, valueSz) );

    if (valueSz == 0) {
        res = sock_getopt(descP->sock, level, opt, NULL, NULL);
        if (res != 0)
            result = esock_make_error_errno(env, sock_errno());
        else
            result = esock_atom_ok;
    } else {
        SOCKOPTLEN_T vsz = valueSz;
        ErlNifBinary val;

        SSDBG( descP, ("SOCKET", "ngetopt_native_unspec -> try alloc buffer\r\n") );

        if (ALLOC_BIN(vsz, &val)) {
            int saveErrno;
            res = sock_getopt(descP->sock, level, opt, val.data, &vsz);
            if (res != 0) {
                saveErrno = sock_errno();
                
                result = esock_make_error_errno(env, saveErrno);
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
        } else {
            result = enif_make_badarg(env);
        }
    }

    SSDBG( descP,
           ("SOCKET", "ngetopt_native_unspec -> done when"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}



/* ngetopt_level - A "proper" level (option) has been specified
 */
static
ERL_NIF_TERM ngetopt_level(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           int              level,
                           int              eOpt)
{
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("SOCKET", "ngetopt_level -> entry with"
            "\r\n   level: %d"
            "\r\n   eOpt:  %d"
            "\r\n", level, eOpt) );

    switch (level) {
    case SOL_SOCKET:
        result = ngetopt_lvl_socket(env, descP, eOpt);
        break;

#if defined(SOL_IP)
    case SOL_IP:
#else
    case IPPROTO_IP:
#endif
        result = ngetopt_lvl_ip(env, descP, eOpt);
        break;

#if defined(HAVE_IPV6)
#if defined(SOL_IPV6)
    case SOL_IPV6:
#else
    case IPPROTO_IPV6:
#endif
        result = ngetopt_lvl_ipv6(env, descP, eOpt);
        break;
#endif

    case IPPROTO_TCP:
        result = ngetopt_lvl_tcp(env, descP, eOpt);
        break;

    case IPPROTO_UDP:
        result = ngetopt_lvl_udp(env, descP, eOpt);
        break;

#if defined(HAVE_SCTP)
    case IPPROTO_SCTP:
        result = ngetopt_lvl_sctp(env, descP, eOpt);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    SSDBG( descP,
           ("SOCKET", "ngetopt_level -> done when"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}


/* ngetopt_lvl_socket - Level *SOCKET* option
 */
static
ERL_NIF_TERM ngetopt_lvl_socket(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                int              eOpt)
{
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("SOCKET", "ngetopt_lvl_socket -> entry with"
            "\r\n   eOpt:  %d"
            "\r\n", eOpt) );

    switch (eOpt) {
#if defined(SO_ACCEPTCONN)
    case SOCKET_OPT_SOCK_ACCEPTCONN:
        result = ngetopt_lvl_sock_acceptconn(env, descP);
        break;
#endif

#if defined(SO_BINDTODEVICE)
    case SOCKET_OPT_SOCK_BINDTODEVICE:
        result = ngetopt_lvl_sock_bindtodevice(env, descP);
        break;
#endif

#if defined(SO_BROADCAST)
    case SOCKET_OPT_SOCK_BROADCAST:
        result = ngetopt_lvl_sock_broadcast(env, descP);
        break;
#endif

#if defined(SO_DEBUG)
    case SOCKET_OPT_SOCK_DEBUG:
        result = ngetopt_lvl_sock_debug(env, descP);
        break;
#endif

#if defined(SO_DOMAIN)
    case SOCKET_OPT_SOCK_DOMAIN:
        result = ngetopt_lvl_sock_domain(env, descP);
        break;
#endif

#if defined(SO_DONTROUTE)
    case SOCKET_OPT_SOCK_DONTROUTE:
        result = ngetopt_lvl_sock_dontroute(env, descP);
        break;
#endif

#if defined(SO_KEEPALIVE)
    case SOCKET_OPT_SOCK_KEEPALIVE:
        result = ngetopt_lvl_sock_keepalive(env, descP);
        break;
#endif

#if defined(SO_LINGER)
    case SOCKET_OPT_SOCK_LINGER:
        result = ngetopt_lvl_sock_linger(env, descP);
        break;
#endif

#if defined(SO_OOBINLINE)
    case SOCKET_OPT_SOCK_OOBINLINE:
        result = ngetopt_lvl_sock_oobinline(env, descP);
        break;
#endif

#if defined(SO_PEEK_OFF)
    case SOCKET_OPT_SOCK_PEEK_OFF:
        result = ngetopt_lvl_sock_peek_off(env, descP);
        break;
#endif

#if defined(SO_PRIORITY)
    case SOCKET_OPT_SOCK_PRIORITY:
        result = ngetopt_lvl_sock_priority(env, descP);
        break;
#endif

#if defined(SO_PROTOCOL)
    case SOCKET_OPT_SOCK_PROTOCOL:
        result = ngetopt_lvl_sock_protocol(env, descP);
        break;
#endif

#if defined(SO_RCVBUF)
    case SOCKET_OPT_SOCK_RCVBUF:
        result = ngetopt_lvl_sock_rcvbuf(env, descP);
        break;
#endif

#if defined(SO_RCVLOWAT)
    case SOCKET_OPT_SOCK_RCVLOWAT:
        result = ngetopt_lvl_sock_rcvlowat(env, descP);
        break;
#endif

#if defined(SO_RCVTIMEO)
    case SOCKET_OPT_SOCK_RCVTIMEO:
        result = ngetopt_lvl_sock_rcvtimeo(env, descP);
        break;
#endif

#if defined(SO_REUSEADDR)
    case SOCKET_OPT_SOCK_REUSEADDR:
        result = ngetopt_lvl_sock_reuseaddr(env, descP);
        break;
#endif

#if defined(SO_REUSEPORT)
    case SOCKET_OPT_SOCK_REUSEPORT:
        result = ngetopt_lvl_sock_reuseport(env, descP);
        break;
#endif

#if defined(SO_SNDBUF)
    case SOCKET_OPT_SOCK_SNDBUF:
        result = ngetopt_lvl_sock_sndbuf(env, descP);
        break;
#endif

#if defined(SO_SNDLOWAT)
    case SOCKET_OPT_SOCK_SNDLOWAT:
        result = ngetopt_lvl_sock_sndlowat(env, descP);
        break;
#endif

#if defined(SO_SNDTIMEO)
    case SOCKET_OPT_SOCK_SNDTIMEO:
        result = ngetopt_lvl_sock_sndtimeo(env, descP);
        break;
#endif

#if defined(SO_TIMESTAMP)
    case SOCKET_OPT_SOCK_TIMESTAMP:
        result = ngetopt_lvl_sock_timestamp(env, descP);
        break;
#endif

#if defined(SO_TYPE)
    case SOCKET_OPT_SOCK_TYPE:
        result = ngetopt_lvl_sock_type(env, descP);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    SSDBG( descP,
           ("SOCKET", "ngetopt_lvl_socket -> done when"
            "\r\n   result:  %T"
            "\r\n", result) );

    return result;
}


#if defined(SO_ACCEPTCONN)
static
ERL_NIF_TERM ngetopt_lvl_sock_acceptconn(ErlNifEnv*       env,
                                         ESockDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, SOL_SOCKET, SO_ACCEPTCONN);
}
#endif


#if defined(SO_BINDTODEVICE)
static
ERL_NIF_TERM ngetopt_lvl_sock_bindtodevice(ErlNifEnv*       env,
                                           ESockDescriptor* descP)
{
    SSDBG( descP,
           ("SOCKET", "ngetopt_lvl_sock_bindtodevice -> entry with\r\n") );

    return ngetopt_str_opt(env, descP, SOL_SOCKET, SO_BROADCAST, IFNAMSIZ+1);
}
#endif


#if defined(SO_BROADCAST)
static
ERL_NIF_TERM ngetopt_lvl_sock_broadcast(ErlNifEnv*       env,
                                        ESockDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, SOL_SOCKET, SO_BROADCAST);
}
#endif


#if defined(SO_DEBUG)
static
ERL_NIF_TERM ngetopt_lvl_sock_debug(ErlNifEnv*       env,
                                    ESockDescriptor* descP)
{
    return ngetopt_int_opt(env, descP, SOL_SOCKET, SO_DEBUG);
}
#endif


#if defined(SO_DOMAIN)
static
ERL_NIF_TERM ngetopt_lvl_sock_domain(ErlNifEnv*       env,
                                     ESockDescriptor* descP)
{
    ERL_NIF_TERM result, reason;
    int          val;
    SOCKOPTLEN_T valSz = sizeof(val);
    int          res;

    res = sock_getopt(descP->sock, SOL_SOCKET, SO_DOMAIN,
                      &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        switch (val) {
        case AF_INET:
            result = esock_make_ok2(env, esock_atom_inet);
            break;

#if defined(HAVE_IN6) && defined(AF_INET6)
        case AF_INET6:
            result = esock_make_ok2(env, esock_atom_inet6);
            break;
#endif

#ifdef HAVE_SYS_UN_H
        case AF_UNIX:
        result = esock_make_ok2(env, esock_atom_local);
        break;
#endif

        default:
            reason = MKT2(env, esock_atom_unknown, MKI(env, val));
            result = esock_make_error(env, reason);
            break;
        }
    }

    return result;
}
#endif


#if defined(SO_DONTROUTE)
static
ERL_NIF_TERM ngetopt_lvl_sock_dontroute(ErlNifEnv*       env,
                                        ESockDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, SOL_SOCKET, SO_DONTROUTE);
}
#endif


#if defined(SO_KEEPALIVE)
static
ERL_NIF_TERM ngetopt_lvl_sock_keepalive(ErlNifEnv*       env,
                                        ESockDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, SOL_SOCKET, SO_KEEPALIVE);
}
#endif


#if defined(SO_LINGER)
static
ERL_NIF_TERM ngetopt_lvl_sock_linger(ErlNifEnv*       env,
                                     ESockDescriptor* descP)
{
    ERL_NIF_TERM  result;
    struct linger val;
    SOCKOPTLEN_T  valSz = sizeof(val);
    int           res;

    sys_memzero((void *) &val, sizeof(val));

    res = sock_getopt(descP->sock, SOL_SOCKET, SO_LINGER,
                      &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM lOnOff = ((val.l_onoff) ? atom_true : atom_false);
        ERL_NIF_TERM lSecs  = MKI(env, val.l_linger);
        ERL_NIF_TERM linger = MKT2(env, lOnOff, lSecs);

        result = esock_make_ok2(env, linger);
    }

    return result;
}
#endif


#if defined(SO_OOBINLINE)
static
ERL_NIF_TERM ngetopt_lvl_sock_oobinline(ErlNifEnv*       env,
                                        ESockDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, SOL_SOCKET, SO_OOBINLINE);
}
#endif


#if defined(SO_PEEK_OFF)
static
ERL_NIF_TERM ngetopt_lvl_sock_peek_off(ErlNifEnv*       env,
                                       ESockDescriptor* descP)
{
    return ngetopt_int_opt(env, descP, SOL_SOCKET, SO_PEEK_OFF);
}
#endif


#if defined(SO_PRIORITY)
static
ERL_NIF_TERM ngetopt_lvl_sock_priority(ErlNifEnv*       env,
                                       ESockDescriptor* descP)
{
    return ngetopt_int_opt(env, descP, SOL_SOCKET, SO_PRIORITY);
}
#endif


#if defined(SO_PROTOCOL)
static
ERL_NIF_TERM ngetopt_lvl_sock_protocol(ErlNifEnv*       env,
                                       ESockDescriptor* descP)
{
    ERL_NIF_TERM result, reason;
    int          val;
    SOCKOPTLEN_T valSz = sizeof(val);
    int          res;

    res = sock_getopt(descP->sock, SOL_SOCKET, SO_PROTOCOL,
                      &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        switch (val) {
        case IPPROTO_IP:
#if defined(AF_LOCAL)
            if (descP->domain == AF_LOCAL)
                result = esock_make_ok2(env, esock_atom_default);
            else
                result = esock_make_ok2(env, esock_atom_ip);
#else
            result = esock_make_ok2(env, esock_atom_ip);
#endif
            break;

        case IPPROTO_TCP:
            result = esock_make_ok2(env, esock_atom_tcp);
            break;

        case IPPROTO_UDP:
            result = esock_make_ok2(env, esock_atom_udp);
            break;

#if defined(HAVE_SCTP)
        case IPPROTO_SCTP:
            result = esock_make_ok2(env, esock_atom_sctp);
            break;
#endif

        default:
            reason = MKT2(env, esock_atom_unknown, MKI(env, val));
            result = esock_make_error(env, reason);
            break;
        }
    }

    return result;
}
#endif


#if defined(SO_RCVBUF)
static
ERL_NIF_TERM ngetopt_lvl_sock_rcvbuf(ErlNifEnv*       env,
                                     ESockDescriptor* descP)
{
    return ngetopt_int_opt(env, descP, SOL_SOCKET, SO_RCVBUF);
}
#endif


#if defined(SO_RCVLOWAT)
static
ERL_NIF_TERM ngetopt_lvl_sock_rcvlowat(ErlNifEnv*       env,
                                       ESockDescriptor* descP)
{
    return ngetopt_int_opt(env, descP, SOL_SOCKET, SO_RCVLOWAT);
}
#endif


#if defined(SO_RCVTIMEO)
static
ERL_NIF_TERM ngetopt_lvl_sock_rcvtimeo(ErlNifEnv*       env,
                                       ESockDescriptor* descP)
{
    return ngetopt_timeval_opt(env, descP, SOL_SOCKET, SO_RCVTIMEO);
}
#endif


#if defined(SO_REUSEADDR)
static
ERL_NIF_TERM ngetopt_lvl_sock_reuseaddr(ErlNifEnv*       env,
                                        ESockDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, SOL_SOCKET, SO_REUSEADDR);
}
#endif


#if defined(SO_REUSEPORT)
static
ERL_NIF_TERM ngetopt_lvl_sock_reuseport(ErlNifEnv*       env,
                                        ESockDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, SOL_SOCKET, SO_REUSEPORT);
}
#endif


#if defined(SO_SNDBUF)
static
ERL_NIF_TERM ngetopt_lvl_sock_sndbuf(ErlNifEnv*       env,
                                     ESockDescriptor* descP)
{
    return ngetopt_int_opt(env, descP, SOL_SOCKET, SO_SNDBUF);
}
#endif


#if defined(SO_SNDLOWAT)
static
ERL_NIF_TERM ngetopt_lvl_sock_sndlowat(ErlNifEnv*       env,
                                       ESockDescriptor* descP)
{
    return ngetopt_int_opt(env, descP, SOL_SOCKET, SO_SNDLOWAT);
}
#endif


#if defined(SO_SNDTIMEO)
static
ERL_NIF_TERM ngetopt_lvl_sock_sndtimeo(ErlNifEnv*       env,
                                       ESockDescriptor* descP)
{
    return ngetopt_timeval_opt(env, descP, SOL_SOCKET, SO_SNDTIMEO);
}
#endif


#if defined(SO_TIMESTAMP)
static
ERL_NIF_TERM ngetopt_lvl_sock_timestamp(ErlNifEnv*       env,
                                        ESockDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, SOL_SOCKET, SO_TIMESTAMP);
}
#endif


#if defined(SO_TYPE)
static
ERL_NIF_TERM ngetopt_lvl_sock_type(ErlNifEnv*       env,
                                   ESockDescriptor* descP)
{
    ERL_NIF_TERM result, reason;
    int          val;
    SOCKOPTLEN_T valSz = sizeof(val);
    int          res;

    res = sock_getopt(descP->sock, SOL_SOCKET, SO_TYPE, &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        switch (val) {
        case SOCK_STREAM:
            result = esock_make_ok2(env, esock_atom_stream);
            break;
        case SOCK_DGRAM:
            result = esock_make_ok2(env, esock_atom_dgram);
            break;
#ifdef HAVE_SCTP
        case SOCK_SEQPACKET:
            result = esock_make_ok2(env, esock_atom_seqpacket);
            break;
#endif
        case SOCK_RAW:
            result = esock_make_ok2(env, esock_atom_raw);
            break;
        case SOCK_RDM:
            result = esock_make_ok2(env, esock_atom_rdm);
            break;
        default:
            reason = MKT2(env, esock_atom_unknown, MKI(env, val));
            result = esock_make_error(env, reason);
            break;
        }
    }

    return result;
}
#endif


/* ngetopt_lvl_ip - Level *IP* option(s)
 */
static
ERL_NIF_TERM ngetopt_lvl_ip(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            int              eOpt)
{
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("SOCKET", "ngetopt_lvl_ip -> entry with"
            "\r\n   eOpt: %d"
            "\r\n", eOpt) );

    switch (eOpt) {
#if defined(IP_FREEBIND)
    case SOCKET_OPT_IP_FREEBIND:
        result = ngetopt_lvl_ip_freebind(env, descP);
        break;
#endif

#if defined(IP_HDRINCL)
    case SOCKET_OPT_IP_HDRINCL:
        result = ngetopt_lvl_ip_hdrincl(env, descP);
        break;
#endif

#if defined(IP_MINTTL)
    case SOCKET_OPT_IP_MINTTL:
        result = ngetopt_lvl_ip_minttl(env, descP);
        break;
#endif

#if defined(IP_MTU)
    case SOCKET_OPT_IP_MTU:
        result = ngetopt_lvl_ip_mtu(env, descP);
        break;
#endif

#if defined(IP_MTU_DISCOVER)
    case SOCKET_OPT_IP_MTU_DISCOVER:
        result = ngetopt_lvl_ip_mtu_discover(env, descP);
        break;
#endif

#if defined(IP_MULTICAST_ALL)
    case SOCKET_OPT_IP_MULTICAST_ALL:
        result = ngetopt_lvl_ip_multicast_all(env, descP);
        break;
#endif

#if defined(IP_MULTICAST_IF)
    case SOCKET_OPT_IP_MULTICAST_IF:
        result = ngetopt_lvl_ip_multicast_if(env, descP);
        break;
#endif

#if defined(IP_MULTICAST_LOOP)
    case SOCKET_OPT_IP_MULTICAST_LOOP:
        result = ngetopt_lvl_ip_multicast_loop(env, descP);
        break;
#endif

#if defined(IP_MULTICAST_TTL)
    case SOCKET_OPT_IP_MULTICAST_TTL:
        result = ngetopt_lvl_ip_multicast_ttl(env, descP);
        break;
#endif

#if defined(IP_NODEFRAG)
    case SOCKET_OPT_IP_NODEFRAG:
        result = ngetopt_lvl_ip_nodefrag(env, descP);
        break;
#endif

#if defined(IP_PKTINFO)
    case SOCKET_OPT_IP_PKTINFO:
        result = ngetopt_lvl_ip_pktinfo(env, descP);
        break;
#endif

#if defined(IP_RECVDSTADDR)
    case SOCKET_OPT_IP_RECVDSTADDR:
        result = ngetopt_lvl_ip_recvdstaddr(env, descP);
        break;
#endif

#if defined(IP_RECVERR)
    case SOCKET_OPT_IP_RECVERR:
        result = ngetopt_lvl_ip_recverr(env, descP);
        break;
#endif

#if defined(IP_RECVIF)
    case SOCKET_OPT_IP_RECVIF:
        result = ngetopt_lvl_ip_recvif(env, descP);
        break;
#endif

#if defined(IP_RECVOPTS)
    case SOCKET_OPT_IP_RECVOPTS:
        result = ngetopt_lvl_ip_recvopts(env, descP);
        break;
#endif

#if defined(IP_RECVORIGDSTADDR)
    case SOCKET_OPT_IP_RECVORIGDSTADDR:
        result = ngetopt_lvl_ip_recvorigdstaddr(env, descP);
        break;
#endif

#if defined(IP_RECVTOS)
    case SOCKET_OPT_IP_RECVTOS:
        result = ngetopt_lvl_ip_recvtos(env, descP);
        break;
#endif

#if defined(IP_RECVTTL)
    case SOCKET_OPT_IP_RECVTTL:
        result = ngetopt_lvl_ip_recvttl(env, descP);
        break;
#endif

#if defined(IP_RETOPTS)
    case SOCKET_OPT_IP_RETOPTS:
        result = ngetopt_lvl_ip_retopts(env, descP);
        break;
#endif

#if defined(IP_ROUTER_ALERT)
    case SOCKET_OPT_IP_ROUTER_ALERT:
        result = ngetopt_lvl_ip_router_alert(env, descP);
        break;
#endif

#if defined(IP_SENDSRCADDR)
    case SOCKET_OPT_IP_SENDSRCADDR:
        result = ngetopt_lvl_ip_sendsrcaddr(env, descP);
        break;
#endif

#if defined(IP_TOS)
    case SOCKET_OPT_IP_TOS:
        result = ngetopt_lvl_ip_tos(env, descP);
        break;
#endif

#if defined(IP_TRANSPARENT)
    case SOCKET_OPT_IP_TRANSPARENT:
        result = ngetopt_lvl_ip_transparent(env, descP);
        break;
#endif

#if defined(IP_TTL)
    case SOCKET_OPT_IP_TTL:
        result = ngetopt_lvl_ip_ttl(env, descP);
        break;
#endif

    default:
        SSDBG( descP,
               ("SOCKET", "ngetopt_lvl_ip -> unknown opt %d\r\n", eOpt) );
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    SSDBG( descP,
           ("SOCKET", "ngetopt_lvl_ip -> done when"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}


/* ngetopt_lvl_ip_minttl - Level IP MINTTL option
 */
#if defined(IP_MINTTL)
static
ERL_NIF_TERM ngetopt_lvl_ip_minttl(ErlNifEnv*       env,
                                   ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_int_opt(env, descP, level, IP_MINTTL);
}
#endif


/* ngetopt_lvl_ip_freebind - Level IP FREEBIND option
 */
#if defined(IP_FREEBIND)
static
ERL_NIF_TERM ngetopt_lvl_ip_freebind(ErlNifEnv*       env,
                                     ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_FREEBIND);
}
#endif


/* ngetopt_lvl_ip_hdrincl - Level IP HDRINCL option
 */
#if defined(IP_HDRINCL)
static
ERL_NIF_TERM ngetopt_lvl_ip_hdrincl(ErlNifEnv*       env,
                                    ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_HDRINCL);
}
#endif


/* ngetopt_lvl_ip_mtu - Level IP MTU option
 */
#if defined(IP_MTU)
static
ERL_NIF_TERM ngetopt_lvl_ip_mtu(ErlNifEnv*       env,
                                ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_int_opt(env, descP, level, IP_MTU);
}
#endif


/* ngetopt_lvl_ip_mtu_discover - Level IP MTU_DISCOVER option
 */
#if defined(IP_MTU_DISCOVER)
static
ERL_NIF_TERM ngetopt_lvl_ip_mtu_discover(ErlNifEnv*       env,
                                         ESockDescriptor* descP)
{
    ERL_NIF_TERM   result;
    ERL_NIF_TERM   eMtuDisc;
    int            mtuDisc;
    SOCKOPTLEN_T   mtuDiscSz = sizeof(mtuDisc);
    int            res;
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    res = sock_getopt(descP->sock, level, IP_MTU_DISCOVER,
                      &mtuDisc, &mtuDiscSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        encode_ip_pmtudisc(env, mtuDisc, &eMtuDisc);
        result = esock_make_ok2(env, eMtuDisc);
    }

    return result;

}
#endif


/* ngetopt_lvl_ip_multicast_all - Level IP MULTICAST_ALL option
 */
#if defined(IP_MULTICAST_ALL)
static
ERL_NIF_TERM ngetopt_lvl_ip_multicast_all(ErlNifEnv*       env,
                                          ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_MULTICAST_ALL);
}
#endif


/* ngetopt_lvl_ip_multicast_if - Level IP MULTICAST_IF option
 */
#if defined(IP_MULTICAST_IF)
static
ERL_NIF_TERM ngetopt_lvl_ip_multicast_if(ErlNifEnv*       env,
                                         ESockDescriptor* descP)
{
    ERL_NIF_TERM   result;
    ERL_NIF_TERM   eAddr;
    struct in_addr ifAddr;
    SOCKOPTLEN_T   ifAddrSz = sizeof(ifAddr);
    char*          xres;
    int            res;
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    res = sock_getopt(descP->sock, level, IP_MULTICAST_IF, &ifAddr, &ifAddrSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        if ((xres = esock_encode_ip4_address(env, &ifAddr, &eAddr)) != NULL) {
            result = esock_make_error_str(env, xres);
        } else {
            result = esock_make_ok2(env, eAddr);
        }
    }

    return result;

}
#endif


/* ngetopt_lvl_ip_multicast_loop - Level IP MULTICAST_LOOP option
 */
#if defined(IP_MULTICAST_LOOP)
static
ERL_NIF_TERM ngetopt_lvl_ip_multicast_loop(ErlNifEnv*       env,
                                           ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_MULTICAST_LOOP);
}
#endif


/* ngetopt_lvl_ip_multicast_ttl - Level IP MULTICAST_TTL option
 */
#if defined(IP_MULTICAST_TTL)
static
ERL_NIF_TERM ngetopt_lvl_ip_multicast_ttl(ErlNifEnv*       env,
                                          ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_int_opt(env, descP, level, IP_MULTICAST_TTL);
}
#endif


/* ngetopt_lvl_ip_nodefrag - Level IP NODEFRAG option
 */
#if defined(IP_NODEFRAG)
static
ERL_NIF_TERM ngetopt_lvl_ip_nodefrag(ErlNifEnv*       env,
                                     ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_NODEFRAG);
}
#endif


/* ngetopt_lvl_ip_pktinfo - Level IP PKTINFO option
 */
#if defined(IP_PKTINFO)
static
ERL_NIF_TERM ngetopt_lvl_ip_pktinfo(ErlNifEnv*       env,
                                    ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_PKTINFO);
}
#endif


/* ngetopt_lvl_ip_recvtos - Level IP RECVTOS option
 */
#if defined(IP_RECVTOS)
static
ERL_NIF_TERM ngetopt_lvl_ip_recvtos(ErlNifEnv*       env,
                                    ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_RECVTOS);
}
#endif


/* ngetopt_lvl_ip_recvdstaddr - Level IP RECVDSTADDR option
 */
#if defined(IP_RECVDSTADDR)
static
ERL_NIF_TERM ngetopt_lvl_ip_recvdstaddr(ErlNifEnv*       env,
                                        ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_RECVDSTADDR);
}
#endif


/* ngetopt_lvl_ip_recverr - Level IP RECVERR option
 */
#if defined(IP_RECVERR)
static
ERL_NIF_TERM ngetopt_lvl_ip_recverr(ErlNifEnv*       env,
                                    ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_RECVERR);
}
#endif


/* ngetopt_lvl_ip_recvif - Level IP RECVIF option
 */
#if defined(IP_RECVIF)
static
ERL_NIF_TERM ngetopt_lvl_ip_recvif(ErlNifEnv*       env,
                                   ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_RECVIF);
}
#endif


/* ngetopt_lvl_ip_recvopt - Level IP RECVOPTS option
 */
#if defined(IP_RECVOPTS)
static
ERL_NIF_TERM ngetopt_lvl_ip_recvopts(ErlNifEnv*       env,
                                     ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_RECVOPTS);
}
#endif


/* ngetopt_lvl_ip_recvorigdstaddr - Level IP RECVORIGDSTADDR option
 */
#if defined(IP_RECVORIGDSTADDR)
static
ERL_NIF_TERM ngetopt_lvl_ip_recvorigdstaddr(ErlNifEnv*       env,
                                            ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_RECVORIGDSTADDR);
}
#endif


/* ngetopt_lvl_ip_recvttl - Level IP RECVTTL option
 */
#if defined(IP_RECVTTL)
static
ERL_NIF_TERM ngetopt_lvl_ip_recvttl(ErlNifEnv*       env,
                                    ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_RECVTTL);
}
#endif


/* ngetopt_lvl_ip_retopts - Level IP RETOPTS option
 */
#if defined(IP_RETOPTS)
static
ERL_NIF_TERM ngetopt_lvl_ip_retopts(ErlNifEnv*       env,
                                    ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_RETOPTS);
}
#endif


/* ngetopt_lvl_ip_router_alert - Level IP ROUTER_ALERT option
 */
#if defined(IP_ROUTER_ALERT)
static
ERL_NIF_TERM ngetopt_lvl_ip_router_alert(ErlNifEnv*       env,
                                         ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_int_opt(env, descP, level, IP_ROUTER_ALERT);
}
#endif


/* ngetopt_lvl_ip_sendsrcaddr - Level IP SENDSRCADDR option
 */
#if defined(IP_SENDSRCADDR)
static
ERL_NIF_TERM ngetopt_lvl_ip_sendsrcaddr(ErlNifEnv*       env,
                                        ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_SENDSRCADDR);
}
#endif


/* ngetopt_lvl_ip_tos - Level IP TOS option
 */
#if defined(IP_TOS)
static
ERL_NIF_TERM ngetopt_lvl_ip_tos(ErlNifEnv*       env,
                                ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int          level = SOL_IP;
#else
    int          level = IPPROTO_IP;
#endif
    ERL_NIF_TERM result;
    int          val;
    SOCKOPTLEN_T valSz = sizeof(val);
    int          res;

    res = sock_getopt(descP->sock, level, IP_TOS, &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        result = encode_ip_tos(env, val);
    }

    return result;
}
#endif


/* ngetopt_lvl_ip_transparent - Level IP TRANSPARENT option
 */
#if defined(IP_TRANSPARENT)
static
ERL_NIF_TERM ngetopt_lvl_ip_transparent(ErlNifEnv*       env,
                                        ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_TRANSPARENT);
}
#endif



/* ngetopt_lvl_ip_ttl - Level IP TTL option
 */
#if defined(IP_TTL)
static
ERL_NIF_TERM ngetopt_lvl_ip_ttl(ErlNifEnv*       env,
                                ESockDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_int_opt(env, descP, level, IP_TTL);
}
#endif



/* ngetopt_lvl_ipv6 - Level *IPv6* option(s)
 */
#if defined(HAVE_IPV6)
static
ERL_NIF_TERM ngetopt_lvl_ipv6(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              int              eOpt)
{
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("SOCKET", "ngetopt_lvl_ipv6 -> entry with"
            "\r\n   eOpt: %d"
            "\r\n", eOpt) );

    switch (eOpt) {
#if defined(IPV6_AUTHHDR)
    case SOCKET_OPT_IPV6_AUTHHDR:
        result = ngetopt_lvl_ipv6_authhdr(env, descP);
        break;
#endif

#if defined(IPV6_DSTOPTS)
    case SOCKET_OPT_IPV6_DSTOPTS:
        result = ngetopt_lvl_ipv6_dstopts(env, descP);
        break;
#endif

#if defined(IPV6_FLOWINFO)
    case SOCKET_OPT_IPV6_FLOWINFO:
        result = ngetopt_lvl_ipv6_flowinfo(env, descP);
        break;
#endif

#if defined(IPV6_HOPLIMIT)
    case SOCKET_OPT_IPV6_HOPLIMIT:
        result = ngetopt_lvl_ipv6_hoplimit(env, descP);
        break;
#endif

#if defined(IPV6_HOPOPTS)
    case SOCKET_OPT_IPV6_HOPOPTS:
        result = ngetopt_lvl_ipv6_hopopts(env, descP);
        break;
#endif

#if defined(IPV6_MTU)
    case SOCKET_OPT_IPV6_MTU:
        result = ngetopt_lvl_ipv6_mtu(env, descP);
        break;
#endif

#if defined(IPV6_MTU_DISCOVER)
    case SOCKET_OPT_IPV6_MTU_DISCOVER:
        result = ngetopt_lvl_ipv6_mtu_discover(env, descP);
        break;
#endif

#if defined(IPV6_MULTICAST_HOPS)
    case SOCKET_OPT_IPV6_MULTICAST_HOPS:
        result = ngetopt_lvl_ipv6_multicast_hops(env, descP);
        break;
#endif

#if defined(IPV6_MULTICAST_IF)
    case SOCKET_OPT_IPV6_MULTICAST_IF:
        result = ngetopt_lvl_ipv6_multicast_if(env, descP);
        break;
#endif

#if defined(IPV6_MULTICAST_LOOP)
    case SOCKET_OPT_IPV6_MULTICAST_LOOP:
        result = ngetopt_lvl_ipv6_multicast_loop(env, descP);
        break;
#endif

#if defined(IPV6_RECVERR)
    case SOCKET_OPT_IPV6_RECVERR:
        result = ngetopt_lvl_ipv6_recverr(env, descP);
        break;
#endif

#if defined(IPV6_RECVPKTINFO) || defined(IPV6_PKTINFO)
    case SOCKET_OPT_IPV6_RECVPKTINFO:
        result = ngetopt_lvl_ipv6_recvpktinfo(env, descP);
        break;
#endif

#if defined(IPV6_ROUTER_ALERT)
    case SOCKET_OPT_IPV6_ROUTER_ALERT:
        result = ngetopt_lvl_ipv6_router_alert(env, descP);
        break;
#endif

#if defined(IPV6_RTHDR)
    case SOCKET_OPT_IPV6_RTHDR:
        result = ngetopt_lvl_ipv6_rthdr(env, descP);
        break;
#endif

#if defined(IPV6_UNICAST_HOPS)
    case SOCKET_OPT_IPV6_UNICAST_HOPS:
        result = ngetopt_lvl_ipv6_unicast_hops(env, descP);
        break;
#endif

#if defined(IPV6_V6ONLY)
    case SOCKET_OPT_IPV6_V6ONLY:
        result = ngetopt_lvl_ipv6_v6only(env, descP);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    SSDBG( descP,
           ("SOCKET", "ngetopt_lvl_ipv6 -> done when"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}


#if defined(IPV6_AUTHHDR)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_authhdr(ErlNifEnv*       env,
                                      ESockDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, SOL_IPV6, IPV6_AUTHHDR);
}
#endif


#if defined(IPV6_DSTOPTS)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_dstopts(ErlNifEnv*       env,
                                      ESockDescriptor* descP)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif
    return ngetopt_bool_opt(env, descP, level, IPV6_DSTOPTS);
}
#endif


#if defined(IPV6_FLOWINFO)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_flowinfo(ErlNifEnv*       env,
                                       ESockDescriptor* descP)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return ngetopt_bool_opt(env, descP, level, IPV6_FLOWINFO);
}
#endif


#if defined(IPV6_HOPLIMIT)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_hoplimit(ErlNifEnv*       env,
                                       ESockDescriptor* descP)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return ngetopt_bool_opt(env, descP, level, IPV6_HOPLIMIT);
}
#endif


#if defined(IPV6_HOPOPTS)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_hopopts(ErlNifEnv*       env,
                                      ESockDescriptor* descP)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return ngetopt_bool_opt(env, descP, level, IPV6_HOPOPTS);
}
#endif


#if defined(IPV6_MTU)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_mtu(ErlNifEnv*       env,
                                  ESockDescriptor* descP)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return ngetopt_int_opt(env, descP, level, IPV6_MTU);
}
#endif


/* ngetopt_lvl_ipv6_mtu_discover - Level IPv6 MTU_DISCOVER option
 */
#if defined(IPV6_MTU_DISCOVER)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_mtu_discover(ErlNifEnv*       env,
                                           ESockDescriptor* descP)
{
    ERL_NIF_TERM  result;
    ERL_NIF_TERM  eMtuDisc;
    int           mtuDisc;
    SOCKOPTLEN_T  mtuDiscSz = sizeof(mtuDisc);
    int           res;
#if defined(SOL_IPV6)
    int           level = SOL_IPV6;
#else
    int           level = IPPROTO_IPV6;
#endif

    res = sock_getopt(descP->sock, level, IPV6_MTU_DISCOVER,
                      &mtuDisc, &mtuDiscSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        encode_ipv6_pmtudisc(env, mtuDisc, &eMtuDisc);
        result = esock_make_ok2(env, eMtuDisc);
    }

    return result;

}
#endif


#if defined(IPV6_MULTICAST_HOPS)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_multicast_hops(ErlNifEnv*       env,
                                             ESockDescriptor* descP)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return ngetopt_int_opt(env, descP, level, IPV6_MULTICAST_HOPS);
}
#endif


#if defined(IPV6_MULTICAST_IF)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_multicast_if(ErlNifEnv*       env,
                                           ESockDescriptor* descP)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return ngetopt_int_opt(env, descP, level, IPV6_MULTICAST_IF);
}
#endif


#if defined(IPV6_MULTICAST_LOOP)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_multicast_loop(ErlNifEnv*       env,
                                             ESockDescriptor* descP)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return ngetopt_bool_opt(env, descP, level, IPV6_MULTICAST_LOOP);
}
#endif


#if defined(IPV6_RECVERR)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_recverr(ErlNifEnv*       env,
                                      ESockDescriptor* descP)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return ngetopt_bool_opt(env, descP, level, IPV6_RECVERR);
}
#endif


#if defined(IPV6_RECVPKTINFO) || defined(IPV6_PKTINFO)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_recvpktinfo(ErlNifEnv*       env,
                                          ESockDescriptor* descP)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif
#if defined(IPV6_RECVPKTINFO)
    int opt   = IPV6_RECVPKTINFO;
#else
    int opt   = IPV6_PKTINFO;
#endif

    return ngetopt_bool_opt(env, descP, level, opt);
}
#endif


#if defined(IPV6_ROUTER_ALERT)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_router_alert(ErlNifEnv*       env,
                                           ESockDescriptor* descP)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return ngetopt_int_opt(env, descP, level, IPV6_ROUTER_ALERT);
}
#endif


#if defined(IPV6_RTHDR)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_rthdr(ErlNifEnv*       env,
                                    ESockDescriptor* descP)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return ngetopt_bool_opt(env, descP, level, IPV6_RTHDR);
}
#endif


#if defined(IPV6_UNICAST_HOPS)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_unicast_hops(ErlNifEnv*       env,
                                           ESockDescriptor* descP)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return ngetopt_int_opt(env, descP, level, IPV6_UNICAST_HOPS);
}
#endif


#if defined(IPV6_V6ONLY)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_v6only(ErlNifEnv*       env,
                                     ESockDescriptor* descP)
{
#if defined(SOL_IPV6)
    int level = SOL_IPV6;
#else
    int level = IPPROTO_IPV6;
#endif

    return ngetopt_bool_opt(env, descP, level, IPV6_V6ONLY);
}
#endif


#endif // defined(HAVE_IPV6)



/* ngetopt_lvl_tcp - Level *TCP* option(s)
 */
static
ERL_NIF_TERM ngetopt_lvl_tcp(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             int              eOpt)
{
    ERL_NIF_TERM result;

    switch (eOpt) {
#if defined(TCP_CONGESTION)
    case SOCKET_OPT_TCP_CONGESTION:
        result = ngetopt_lvl_tcp_congestion(env, descP);
        break;
#endif

#if defined(TCP_MAXSEG)
    case SOCKET_OPT_TCP_MAXSEG:
        result = ngetopt_lvl_tcp_maxseg(env, descP);
        break;
#endif

#if defined(TCP_NODELAY)
    case SOCKET_OPT_TCP_NODELAY:
        result = ngetopt_lvl_tcp_nodelay(env, descP);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}


/* ngetopt_lvl_tcp_congestion - Level TCP CONGESTION option
 */
#if defined(TCP_CONGESTION)
static
ERL_NIF_TERM ngetopt_lvl_tcp_congestion(ErlNifEnv*       env,
                                        ESockDescriptor* descP)
{
    int max = SOCKET_OPT_TCP_CONGESTION_NAME_MAX+1;

    return ngetopt_str_opt(env, descP, IPPROTO_TCP, TCP_CONGESTION, max);
}
#endif


/* ngetopt_lvl_tcp_maxseg - Level TCP MAXSEG option
 */
#if defined(TCP_MAXSEG)
static
ERL_NIF_TERM ngetopt_lvl_tcp_maxseg(ErlNifEnv*       env,
                                    ESockDescriptor* descP)
{
    return ngetopt_int_opt(env, descP, IPPROTO_TCP, TCP_MAXSEG);
}
#endif


/* ngetopt_lvl_tcp_nodelay - Level TCP NODELAY option
 */
#if defined(TCP_NODELAY)
static
ERL_NIF_TERM ngetopt_lvl_tcp_nodelay(ErlNifEnv*       env,
                                     ESockDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, IPPROTO_TCP, TCP_NODELAY);
}
#endif



/* ngetopt_lvl_udp - Level *UDP* option(s)
 */
static
ERL_NIF_TERM ngetopt_lvl_udp(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             int              eOpt)
{
    ERL_NIF_TERM result;

    switch (eOpt) {
#if defined(UDP_CORK)
    case SOCKET_OPT_UDP_CORK:
        result = ngetopt_lvl_udp_cork(env, descP);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}


/* ngetopt_lvl_udp_cork - Level UDP CORK option
 */
#if defined(UDP_CORK)
static
ERL_NIF_TERM ngetopt_lvl_udp_cork(ErlNifEnv*       env,
                                  ESockDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, IPPROTO_UDP, UDP_CORK);
}
#endif



/* ngetopt_lvl_sctp - Level *SCTP* option(s)
 */
#if defined(HAVE_SCTP)
static
ERL_NIF_TERM ngetopt_lvl_sctp(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              int              eOpt)
{
    ERL_NIF_TERM result;

    SSDBG( descP,
           ("SOCKET", "ngetopt_lvl_sctp -> entry with"
            "\r\n   opt: %d"
            "\r\n", eOpt) );

    switch (eOpt) {
#if defined(SCTP_ASSOCINFO)
    case SOCKET_OPT_SCTP_ASSOCINFO:
        result = ngetopt_lvl_sctp_associnfo(env, descP);
        break;
#endif

#if defined(SCTP_AUTOCLOSE)
    case SOCKET_OPT_SCTP_AUTOCLOSE:
        result = ngetopt_lvl_sctp_autoclose(env, descP);
        break;
#endif

#if defined(SCTP_DISABLE_FRAGMENTS)
    case SOCKET_OPT_SCTP_DISABLE_FRAGMENTS:
        result = ngetopt_lvl_sctp_disable_fragments(env, descP);
        break;
#endif

#if defined(SCTP_INITMSG)
    case SOCKET_OPT_SCTP_INITMSG:
        result = ngetopt_lvl_sctp_initmsg(env, descP);
        break;
#endif

#if defined(SCTP_MAXSEG)
    case SOCKET_OPT_SCTP_MAXSEG:
        result = ngetopt_lvl_sctp_maxseg(env, descP);
        break;
#endif

#if defined(SCTP_NODELAY)
    case SOCKET_OPT_SCTP_NODELAY:
        result = ngetopt_lvl_sctp_nodelay(env, descP);
        break;
#endif

#if defined(SCTP_RTOINFO)
    case SOCKET_OPT_SCTP_RTOINFO:
        result = ngetopt_lvl_sctp_rtoinfo(env, descP);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    SSDBG( descP,
           ("SOCKET", "ngetopt_lvl_sctp -> done when"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}


/* ngetopt_lvl_sctp_associnfo - Level SCTP ASSOCINFO option
 *
 * <KOLLA>
 *
 * We should really specify which association this relates to,
 * as it is now we get assoc-id = 0. If this socket is an 
 * association (and not an endpoint) then it will have an
 * assoc id. But since the sctp support at present is "limited",
 * we leave it for now.
 * What do we do if this is an endpoint? Invalid op?
 *
 * </KOLLA>
 */
#if defined(SCTP_ASSOCINFO)
static
ERL_NIF_TERM ngetopt_lvl_sctp_associnfo(ErlNifEnv*       env,
                                        ESockDescriptor* descP)
{
    ERL_NIF_TERM            result;
    struct sctp_assocparams val;
    SOCKOPTLEN_T            valSz = sizeof(val);
    int                     res;

    SSDBG( descP, ("SOCKET", "ngetopt_lvl_sctp_associnfo -> entry\r\n") );
    
    sys_memzero((char*) &val, valSz);
    res = sock_getopt(descP->sock, IPPROTO_SCTP, SCTP_ASSOCINFO, &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM eAssocParams;
        ERL_NIF_TERM keys[]  = {atom_assoc_id, atom_max_rxt, atom_num_peer_dests,
                                atom_peer_rwnd, atom_local_rwnd, atom_cookie_life};
        ERL_NIF_TERM vals[]  = {MKUI(env, val.sasoc_assoc_id),
                                MKUI(env, val.sasoc_asocmaxrxt),
                                MKUI(env, val.sasoc_number_peer_destinations),
                                MKUI(env, val.sasoc_peer_rwnd),
                                MKUI(env, val.sasoc_local_rwnd),
                                MKUI(env, val.sasoc_cookie_life)};
        unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);
        unsigned int numVals = sizeof(vals) / sizeof(ERL_NIF_TERM);

        ESOCK_ASSERT( (numKeys == numVals) );

        if (!MKMA(env, keys, vals, numKeys, &eAssocParams))
            return esock_make_error(env, esock_atom_einval);;
    
        result = esock_make_ok2(env, eAssocParams);
    }

    SSDBG( descP,
           ("SOCKET", "ngetopt_lvl_sctp_associnfo -> done with"
            "\r\n   res:    %d"
            "\r\n   result: %T"
            "\r\n", res, result) );

    return result;
}
#endif


/* ngetopt_lvl_sctp_autoclose - Level SCTP AUTOCLOSE option
 */
#if defined(SCTP_AUTOCLOSE)
static
ERL_NIF_TERM ngetopt_lvl_sctp_autoclose(ErlNifEnv*       env,
                                        ESockDescriptor* descP)
{
    return ngetopt_int_opt(env, descP, IPPROTO_SCTP, SCTP_AUTOCLOSE);
}
#endif


/* ngetopt_lvl_sctp_disable_fragments - Level SCTP DISABLE:FRAGMENTS option
 */
#if defined(SCTP_DISABLE_FRAGMENTS)
static
ERL_NIF_TERM ngetopt_lvl_sctp_disable_fragments(ErlNifEnv*       env,
                                                ESockDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, IPPROTO_SCTP, SCTP_DISABLE_FRAGMENTS);
}
#endif


/* ngetopt_lvl_sctp_initmsg - Level SCTP INITMSG option
 *
 */
#if defined(SCTP_INITMSG)
static
ERL_NIF_TERM ngetopt_lvl_sctp_initmsg(ErlNifEnv*       env,
                                      ESockDescriptor* descP)
{
    ERL_NIF_TERM        result;
    struct sctp_initmsg val;
    SOCKOPTLEN_T        valSz = sizeof(val);
    int                 res;

    SSDBG( descP, ("SOCKET", "ngetopt_lvl_sctp_initmsg -> entry\r\n") );
    
    sys_memzero((char*) &val, valSz);
    res = sock_getopt(descP->sock, IPPROTO_SCTP, SCTP_INITMSG, &val, &valSz);

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
        unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);
        unsigned int numVals = sizeof(vals) / sizeof(ERL_NIF_TERM);

        ESOCK_ASSERT( (numKeys == numVals) );

        if (!MKMA(env, keys, vals, numKeys, &eInitMsg))
            return esock_make_error(env, esock_atom_einval);;
    
        result = esock_make_ok2(env, eInitMsg);
    }

    SSDBG( descP,
           ("SOCKET", "ngetopt_lvl_sctp_initmsg -> done with"
            "\r\n   res:    %d"
            "\r\n   result: %T"
            "\r\n", res, result) );

    return result;
}
#endif


/* ngetopt_lvl_sctp_maxseg - Level SCTP MAXSEG option
 */
#if defined(SCTP_MAXSEG)
static
ERL_NIF_TERM ngetopt_lvl_sctp_maxseg(ErlNifEnv*       env,
                                     ESockDescriptor* descP)
{
    return ngetopt_int_opt(env, descP, IPPROTO_SCTP, SCTP_MAXSEG);
}
#endif


/* ngetopt_lvl_sctp_nodelay - Level SCTP NODELAY option
 */
#if defined(SCTP_NODELAY)
static
ERL_NIF_TERM ngetopt_lvl_sctp_nodelay(ErlNifEnv*       env,
                                      ESockDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, IPPROTO_SCTP, SCTP_NODELAY);
}
#endif


/* ngetopt_lvl_sctp_associnfo - Level SCTP ASSOCINFO option
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
#if defined(SCTP_RTOINFO)
static
ERL_NIF_TERM ngetopt_lvl_sctp_rtoinfo(ErlNifEnv*       env,
                                      ESockDescriptor* descP)
{
    ERL_NIF_TERM        result;
    struct sctp_rtoinfo val;
    SOCKOPTLEN_T        valSz = sizeof(val);
    int                 res;

    SSDBG( descP, ("SOCKET", "ngetopt_lvl_sctp_rtoinfo -> entry\r\n") );
    
    sys_memzero((char*) &val, valSz);
    res = sock_getopt(descP->sock, IPPROTO_SCTP, SCTP_RTOINFO, &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM eRTOInfo;        
        ERL_NIF_TERM keys[]  = {atom_assoc_id, atom_initial, atom_max, atom_min};
        ERL_NIF_TERM vals[]  = {MKUI(env, val.srto_assoc_id),
                                MKUI(env, val.srto_initial),
                                MKUI(env, val.srto_max),
                                MKUI(env, val.srto_min)};
        unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);
        unsigned int numVals = sizeof(vals) / sizeof(ERL_NIF_TERM);

        ESOCK_ASSERT( (numKeys == numVals) );

        if (!MKMA(env, keys, vals, numKeys, &eRTOInfo))
            return esock_make_error(env, esock_atom_einval);;
    
        result = esock_make_ok2(env, eRTOInfo);
    }

    SSDBG( descP,
           ("SOCKET", "ngetopt_lvl_sctp_rtoinfo -> done with"
            "\r\n   res:    %d"
            "\r\n   result: %T"
            "\r\n", res, result) );

    return result;
}
#endif



#endif // defined(HAVE_SCTP)



/* ngetopt_bool_opt - get an (integer) bool option
 */
static
ERL_NIF_TERM ngetopt_bool_opt(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              int              level,
                              int              opt)
{
    ERL_NIF_TERM result;
    int          val;
    SOCKOPTLEN_T valSz = sizeof(val);
    int          res;

    /*
    SSDBG( descP, ("SOCKET", "ngetopt_bool_opt -> entry with"
                   "\r\n: level: %d"
                   "\r\n: opt:   %d"
                   "\r\n", level, opt) );
    */

    res = sock_getopt(descP->sock, level, opt, &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM bval = ((val) ? atom_true : atom_false);

        result = esock_make_ok2(env, bval);
    }

    /*
    SSDBG( descP, ("SOCKET", "ngetopt_bool_opt -> done when"
                   "\r\n: res:    %d"
                   "\r\n: result: %T"
                   "\r\n", res, result) );
    */

    return result;
}


/* ngetopt_int_opt - get an integer option
 */
static
ERL_NIF_TERM ngetopt_int_opt(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             int              level,
                             int              opt)
{
    ERL_NIF_TERM result;
    int          val;
    SOCKOPTLEN_T valSz = sizeof(val);
    int          res;

    res = sock_getopt(descP->sock, level, opt, &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        result = esock_make_ok2(env, MKI(env, val));
    }

    return result;
}



/* ngetopt_timeval_opt - get an timeval option
 */
static
ERL_NIF_TERM ngetopt_timeval_opt(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 int              level,
                                 int              opt)
{
    ERL_NIF_TERM   result;
    struct timeval val;
    SOCKOPTLEN_T   valSz = sizeof(val);
    int            res;

    SSDBG( descP,
           ("SOCKET", "ngetopt_timeval_opt -> entry with"
            "\r\n   level: %d"
            "\r\n   opt:   %d"
            "\r\n", level, opt) );

    sys_memzero((char*) &val, valSz);
    res = sock_getopt(descP->sock, level, opt, &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM eTimeVal;
        char*        xres;

        if ((xres = esock_encode_timeval(env, &val, &eTimeVal)) != NULL)
            result = esock_make_error_str(env, xres);
        else
            result = esock_make_ok2(env, eTimeVal);
    }

    SSDBG( descP,
           ("SOCKET", "ngetopt_timeval_opt -> done when"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}



/* ngetopt_str_opt - get an string option
 *
 * We provide the max size of the string. This is the
 * size of the buffer we allocate for the value.
 * The actual size of the (read) value will be communicated
 * in the optSz variable.
 */
#if defined(USE_GETOPT_STR_OPT)
static
ERL_NIF_TERM ngetopt_str_opt(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             int              level,
                             int              opt,
                             int              max)
{
    ERL_NIF_TERM result;
    char*        val   = MALLOC(max);
    SOCKOPTLEN_T valSz = max;
    int          res;

    SSDBG( descP,
           ("SOCKET", "ngetopt_str_opt -> entry with"
            "\r\n   level: %d"
            "\r\n   opt:   %d"
            "\r\n   max:   %d"
            "\r\n", level, opt, max) );

    res = sock_getopt(descP->sock, level, opt, val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM sval = MKSL(env, val, valSz);

        result = esock_make_ok2(env, sval);
    }

    SSDBG( descP,
           ("SOCKET", "ngetopt_str_opt -> done when"
            "\r\n   result: %T"
            "\r\n", result) );

    FREE(val);

    return result;
}
#endif // if defined(USE_GETOPT_STR_OPT)
#endif // if !defined(__WIN32__)



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
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     res;

    SGDBG( ("SOCKET", "nif_sockname -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    if ((argc != 1) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }

    if (IS_CLOSED(descP) || IS_CLOSING(descP))
        return esock_make_error(env, atom_closed);
    
    SSDBG( descP,
           ("SOCKET", "nif_sockname -> args when sock = %d:"
            "\r\n   Socket: %T"
            "\r\n", descP->sock, argv[0]) );

    res = nsockname(env, descP);

    SSDBG( descP, ("SOCKET", "nif_sockname -> done with res = %T\r\n", res) );

    return res;
#endif // if defined(__WIN32__)
}



#if !defined(__WIN32__)
static
ERL_NIF_TERM nsockname(ErlNifEnv*       env,
                       ESockDescriptor* descP)
{
    ESockAddress  sa;
    ESockAddress* saP = &sa;
    unsigned int  sz  = sizeof(ESockAddress);

    sys_memzero((char*) saP, sz);
    if (IS_SOCKET_ERROR(sock_name(descP->sock, (struct sockaddr*) saP, &sz))) {
        return esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM esa;
        char*        xres;

        if ((xres = esock_encode_sockaddr(env, saP, sz, &esa)) != NULL)
            return esock_make_error_str(env, xres);
        else
            return esock_make_ok2(env, esa);
    }
}
#endif // if !defined(__WIN32__)



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
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     res;

    SGDBG( ("SOCKET", "nif_peername -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    if ((argc != 1) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }

    if (IS_CLOSED(descP) || IS_CLOSING(descP))
        return esock_make_error(env, atom_closed);
    
    SSDBG( descP,
           ("SOCKET", "nif_peername -> args when sock = %d:"
            "\r\n   Socket: %T"
            "\r\n", descP->sock, argv[0]) );

    res = npeername(env, descP);

    SSDBG( descP, ("SOCKET", "nif_peername -> done with res = %T\r\n", res) );

    return res;
#endif // if defined(__WIN32__)
}



#if !defined(__WIN32__)
static
ERL_NIF_TERM npeername(ErlNifEnv*       env,
                       ESockDescriptor* descP)
{
    ESockAddress  sa;
    ESockAddress* saP = &sa;
    unsigned int  sz  = sizeof(ESockAddress);

    sys_memzero((char*) saP, sz);
    if (IS_SOCKET_ERROR(sock_peer(descP->sock, (struct sockaddr*) saP, &sz))) {
        return esock_make_error_errno(env, sock_errno());
    } else {
        ERL_NIF_TERM esa;
        char*        xres;

        if ((xres = esock_encode_sockaddr(env, saP, sz, &esa)) != NULL)
            return esock_make_error_str(env, xres);
        else
            return esock_make_ok2(env, esa);
    }
}
#endif // if !defined(__WIN32__)



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
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ESockDescriptor* descP;
    ERL_NIF_TERM     op, sockRef, opRef, result;

    SGDBG( ("SOCKET", "nif_cancel -> entry with argc: %d\r\n", argc) );

    /* Extract arguments and perform preliminary validation */

    sockRef = argv[0];
    if ((argc != 3) ||
        !enif_get_resource(env, sockRef, sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }
    op    = argv[1];
    opRef = argv[2];
        
    if (IS_CLOSED(descP) || IS_CLOSING(descP))
        return esock_make_error(env, atom_closed);
    
    SSDBG( descP,
           ("SOCKET", "nif_cancel -> args when sock = %d:"
            "\r\n   op:    %T"
            "\r\n   opRef: %T"
            "\r\n", descP->sock, op, opRef) );
    
    result = ncancel(env, descP, op, sockRef, opRef);

    SSDBG( descP,
           ("SOCKET", "nif_cancel -> done with result: "
           "\r\n   %T"
           "\r\n", result) );

    return result;
#endif // if !defined(__WIN32__)
}


#if !defined(__WIN32__)
static
ERL_NIF_TERM ncancel(ErlNifEnv*       env,
                     ESockDescriptor* descP,
                     ERL_NIF_TERM     op,
                     ERL_NIF_TERM     sockRef,
                     ERL_NIF_TERM     opRef)
{
    /* <KOLLA>
     *
     * Do we really need all these variants? Should it not be enough with: 
     *
     *     connect | accept | send | recv
     *
     * </KOLLA>
     */
    if (COMPARE(op, esock_atom_connect) == 0) {
        return ncancel_connect(env, descP, opRef);
    } else if (COMPARE(op, esock_atom_accept) == 0) {
        return ncancel_accept(env, descP, sockRef, opRef);
    } else if (COMPARE(op, esock_atom_send) == 0) {
        return ncancel_send(env, descP, sockRef, opRef);
    } else if (COMPARE(op, esock_atom_sendto) == 0) {
        return ncancel_send(env, descP, sockRef, opRef);
    } else if (COMPARE(op, esock_atom_sendmsg) == 0) {
        return ncancel_send(env, descP, sockRef, opRef);
    } else if (COMPARE(op, esock_atom_recv) == 0) {
        return ncancel_recv(env, descP, sockRef, opRef);
    } else if (COMPARE(op, esock_atom_recvfrom) == 0) {
        return ncancel_recv(env, descP, sockRef, opRef);
    } else if (COMPARE(op, esock_atom_recvmsg) == 0) {
        return ncancel_recv(env, descP, sockRef, opRef);
    } else {
        return esock_make_error(env, esock_atom_einval);
    }
}



/* *** ncancel_connect ***
 *
 *
 */
static
ERL_NIF_TERM ncancel_connect(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             ERL_NIF_TERM     opRef)
{
    return ncancel_write_select(env, descP, opRef);
}


/* *** ncancel_accept ***
 *
 * We have two different cases:
 *   *) Its the current acceptor
 *      Cancel the select!
 *      We need to activate one of the waiting acceptors.
 *   *) Its one of the acceptors ("waiting") in the queue
 *      Simply remove the acceptor from the queue.
 *
 */
static
ERL_NIF_TERM ncancel_accept(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     sockRef,
                            ERL_NIF_TERM     opRef)
{
    ERL_NIF_TERM res;

    SSDBG( descP,
           ("SOCKET", "ncancel_accept -> entry with"
            "\r\n   opRef: %T"
            "\r\n   %s"
            "\r\n", opRef,
            ((descP->currentAcceptorP == NULL) ? "without acceptor" : "with acceptor")) );

    MLOCK(descP->accMtx);

    if (descP->currentAcceptorP != NULL) {
        if (COMPARE(opRef, descP->currentAcceptor.ref) == 0) {
            res = ncancel_accept_current(env, descP, sockRef);
        } else {
            res = ncancel_accept_waiting(env, descP, opRef);
        }
    } else {
        /* Or badarg? */
        res =  esock_make_error(env, esock_atom_einval);
    }

    MUNLOCK(descP->accMtx);

    SSDBG( descP,
           ("SOCKET", "ncancel_accept -> done with result:"
            "\r\n   %T"
            "\r\n", res) );

    return res;
}


/* The current acceptor process has an ongoing select we first must
 * cancel. Then we must re-activate the "first" (the first
 * in the acceptor queue).
 */
static
ERL_NIF_TERM ncancel_accept_current(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM res;

    SSDBG( descP, ("SOCKET", "ncancel_accept_current -> entry\r\n") );

    DEMONP("ncancel_accept_current -> current acceptor",
           env, descP, &descP->currentAcceptor.mon);
    res = ncancel_read_select(env, descP, descP->currentAcceptor.ref);

    SSDBG( descP, ("SOCKET",
                   "ncancel_accept_current -> cancel res: %T\r\n", res) );

    if (!activate_next_acceptor(env, descP, sockRef)) {

        SSDBG( descP,
               ("SOCKET", "ncancel_accept_current -> no more writers\r\n") );

        descP->state               = SOCKET_STATE_LISTENING;

        descP->currentAcceptorP    = NULL;
        descP->currentAcceptor.ref = esock_atom_undefined;
        enif_set_pid_undefined(&descP->currentAcceptor.pid);
        esock_monitor_init(&descP->currentAcceptor.mon);
    }

    SSDBG( descP, ("SOCKET", "ncancel_accept_current -> done with result:"
                   "\r\n   %T"
                   "\r\n", res) );

    return res;
}


/* These processes have not performed a select, so we can simply
 * remove them from the acceptor queue.
 */
static
ERL_NIF_TERM ncancel_accept_waiting(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     opRef)
{
    ErlNifPid caller;

    if (enif_self(env, &caller) == NULL)
        return esock_make_error(env, atom_exself);

    /* unqueue request from (acceptor) queue */

    if (acceptor_unqueue(env, descP, &caller)) {
        return esock_atom_ok;
    } else {
        /* Race? */
        return esock_make_error(env, esock_atom_not_found);
    }
}



/* *** ncancel_send ***
 *
 * Cancel a send operation.
 * Its either the current writer or one of the waiting writers.
 */
static
ERL_NIF_TERM ncancel_send(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     opRef)
{
    ERL_NIF_TERM res;

    MLOCK(descP->writeMtx);

    SSDBG( descP,
           ("SOCKET", "ncancel_send -> entry with"
            "\r\n   opRef: %T"
            "\r\n   %s"
            "\r\n", opRef,
            ((descP->currentWriterP == NULL) ? "without writer" : "with writer")) );

    if (descP->currentWriterP != NULL) {
        if (COMPARE(opRef, descP->currentWriter.ref) == 0) {
            res = ncancel_send_current(env, descP, sockRef);
        } else {
            res = ncancel_send_waiting(env, descP, opRef);
        }
    } else {
        /* Or badarg? */
        res =  esock_make_error(env, esock_atom_einval);
    }

    MUNLOCK(descP->writeMtx);

    SSDBG( descP,
           ("SOCKET", "ncancel_send -> done with result:"
            "\r\n   %T"
            "\r\n", res) );

    return res;
}



/* The current writer process has an ongoing select we first must
 * cancel. Then we must re-activate the "first" (the first
 * in the writer queue).
 */
static
ERL_NIF_TERM ncancel_send_current(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM res;

    SSDBG( descP, ("SOCKET", "ncancel_send_current -> entry\r\n") );

    DEMONP("ncancel_send_current -> current writer",
           env, descP, &descP->currentWriter.mon);
    res = ncancel_write_select(env, descP, descP->currentWriter.ref);

    SSDBG( descP,
           ("SOCKET", "ncancel_send_current -> cancel res: %T\r\n", res) );

    if (!activate_next_writer(env, descP, sockRef)) {
        SSDBG( descP,
               ("SOCKET", "ncancel_send_current -> no more writers\r\n") );
        descP->currentWriterP    = NULL;
        descP->currentWriter.ref = esock_atom_undefined;
        enif_set_pid_undefined(&descP->currentWriter.pid);
        esock_monitor_init(&descP->currentWriter.mon);
    }

    SSDBG( descP, ("SOCKET", "ncancel_send_current -> done with result:"
                   "\r\n   %T"
                   "\r\n", res) );

    return res;
}


/* These processes have not performed a select, so we can simply
 * remove them from the writer queue.
 */
static
ERL_NIF_TERM ncancel_send_waiting(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     opRef)
{
    ErlNifPid caller;

    if (enif_self(env, &caller) == NULL)
        return esock_make_error(env, atom_exself);

    /* unqueue request from (writer) queue */

    if (writer_unqueue(env, descP, &caller)) {
        return esock_atom_ok;
    } else {
        /* Race? */
        return esock_make_error(env, esock_atom_not_found);
    }
}



/* *** ncancel_recv ***
 *
 * Cancel a read operation.
 * Its either the current reader or one of the waiting readers.
 */
static
ERL_NIF_TERM ncancel_recv(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     opRef)
{
    ERL_NIF_TERM res;

    MLOCK(descP->readMtx);

    SSDBG( descP,
           ("SOCKET", "ncancel_recv -> entry with"
            "\r\n   opRef: %T"
            "\r\n   %s"
            "\r\n", opRef,
            ((descP->currentReaderP == NULL) ? "without reader" : "with reader")) );

    if (descP->currentReaderP != NULL) {
        if (COMPARE(opRef, descP->currentReader.ref) == 0) {
            res = ncancel_recv_current(env, descP, sockRef);
        } else {
            res = ncancel_recv_waiting(env, descP, opRef);
        }
    } else {
        /* Or badarg? */
        res =  esock_make_error(env, esock_atom_einval);
    }

    MUNLOCK(descP->readMtx);

    SSDBG( descP,
           ("SOCKET", "ncancel_recv -> done with result:"
            "\r\n   %T"
            "\r\n", res) );

    return res;
}


/* The current reader process has an ongoing select we first must
 * cancel. Then we must re-activate the "first" (the first
 * in the reader queue).
 */
static
ERL_NIF_TERM ncancel_recv_current(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM res;

    SSDBG( descP, ("SOCKET", "ncancel_recv_current -> entry\r\n") );

    DEMONP("ncancel_recv_current -> current reader",
           env, descP, &descP->currentReader.mon);
    res = ncancel_read_select(env, descP, descP->currentReader.ref);

    SSDBG( descP,
           ("SOCKET", "ncancel_recv_current -> cancel res: %T\r\n", res) );

    if (!activate_next_reader(env, descP, sockRef)) {
        SSDBG( descP,
               ("SOCKET", "ncancel_recv_current -> no more readers\r\n") );
        descP->currentReaderP    = NULL;
        descP->currentReader.ref = esock_atom_undefined;
        enif_set_pid_undefined(&descP->currentReader.pid);
        esock_monitor_init(&descP->currentReader.mon);
    }

    SSDBG( descP, ("SOCKET", "ncancel_recv_current -> done with result:"
                   "\r\n   %T"
                   "\r\n", res) );

    return res;
}


/* These processes have not performed a select, so we can simply
 * remove them from the reader queue.
 */
static
ERL_NIF_TERM ncancel_recv_waiting(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     opRef)
{
    ErlNifPid caller;

    if (enif_self(env, &caller) == NULL)
        return esock_make_error(env, atom_exself);

    /* unqueue request from (reader) queue */

    if (reader_unqueue(env, descP, &caller)) {
        return esock_atom_ok;
    } else {
        /* Race? */
        return esock_make_error(env, esock_atom_not_found);
    }
}



static
ERL_NIF_TERM ncancel_read_select(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     opRef)
{
    return ncancel_mode_select(env, descP, opRef,
                               ERL_NIF_SELECT_READ,
                               ERL_NIF_SELECT_READ_CANCELLED);
}


static
ERL_NIF_TERM ncancel_write_select(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     opRef)
{
    return ncancel_mode_select(env, descP, opRef,
                               ERL_NIF_SELECT_WRITE,
                               ERL_NIF_SELECT_WRITE_CANCELLED);
}


static
ERL_NIF_TERM ncancel_mode_select(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     opRef,
                                 int              smode,
                                 int              rmode)
{
    int selectRes = esock_select_cancel(env, descP->sock, smode, descP);

    if (selectRes & rmode) {
        /* Was cancelled */
        return esock_atom_ok;
    } else if (selectRes > 0) {
        /* Has already sent the message */
        return esock_make_error(env, esock_atom_select_sent);
    } else {
        /* Stopped? */
        SSDBG( descP, ("SOCKET", "ncancel_mode_select -> failed: %d (0x%lX)"
                       "\r\n", selectRes, selectRes) );
        return esock_make_error(env, esock_atom_einval);
    }

}
#endif // if !defined(__WIN32__)




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
#if !defined(__WIN32__)
static
BOOLEAN_T send_check_writer(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     ref,
                            ERL_NIF_TERM*    checkResult)
{
    if (descP->currentWriterP != NULL) {
        ErlNifPid caller;
        
        if (enif_self(env, &caller) == NULL) {
            *checkResult = esock_make_error(env, atom_exself);
            return FALSE;
        }

        if (COMPARE_PIDS(&descP->currentWriter.pid, &caller) != 0) {
            /* Not the "current writer", so (maybe) push onto queue */

            SSDBG( descP,
                   ("SOCKET",
                    "send_check_writer -> not (current) writer\r\n") );

            if (!writer_search4pid(env, descP, &caller))
                *checkResult = writer_push(env, descP, caller, ref);
            else
                *checkResult = esock_make_error(env, esock_atom_eagain);
            
            SSDBG( descP,
                   ("SOCKET",
                    "send_check_writer -> queue (push) result: %T\r\n",
                    checkResult) );
            
            return FALSE;

        }
        
    }

    // Does not actually matter in this case, but ...
    *checkResult = esock_atom_ok;

    return TRUE;
}



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
static
ERL_NIF_TERM send_check_result(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ssize_t          written,
                               ssize_t          dataSize,
                               int              saveErrno,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     sendRef)
{
    ERL_NIF_TERM res;

    SSDBG( descP,
           ("SOCKET", "send_check_result -> entry with"
            "\r\n   written:   %d"
            "\r\n   dataSize:  %d"
            "\r\n   saveErrno: %d"
            "\r\n", written, dataSize, saveErrno) );

    if (written >= dataSize) {

        res = send_check_ok(env, descP, written, dataSize, sockRef);

    } else if (written < 0) {

        /* Some kind of send failure - check what kind */

        if ((saveErrno != EAGAIN) && (saveErrno != EINTR)) {

            res = send_check_fail(env, descP, saveErrno, sockRef);

        } else {

            /* Ok, try again later */

            SSDBG( descP, ("SOCKET", "send_check_result -> try again\r\n") );

            res = send_check_retry(env, descP, written, sockRef, sendRef);

        }

    } else {

        /* Not the entire package */

        SSDBG( descP,
               ("SOCKET", "send_check_result -> "
                "not entire package written (%d of %d)\r\n",
                written, dataSize) );

        res = send_check_retry(env, descP, written, sockRef, sendRef);

    }

    SSDBG( descP, ("SOCKET", "send_check_result -> done: %T\r\n", res) );

    return res;
}


/* *** send_check_ok ***
 *
 * Processing done upon successful send.
 */
static
ERL_NIF_TERM send_check_ok(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ssize_t          written,
                           ssize_t          dataSize,
                           ERL_NIF_TERM     sockRef)
{
    // cnt_inc(&descP->writePkgCnt,  1);
    SOCK_CNT_INC(env, descP, sockRef,
                 atom_write_pkg, &descP->writePkgCnt, 1);
    // cnt_inc(&descP->writeByteCnt, written);
    SOCK_CNT_INC(env, descP, sockRef,
                 atom_write_byte, &descP->writeByteCnt, written);

    if (descP->currentWriterP != NULL) {
        DEMONP("send_check_ok -> current writer",
               env, descP, &descP->currentWriter.mon);
        esock_free_env("send_check_ok", descP->currentWriter.env);
        descP->currentWriter.env = NULL;
    }

    SSDBG( descP,
           ("SOCKET", "send_check_ok -> "
            "everything written (%d,%d) - done\r\n", dataSize, written) );

    /*
     * Ok, this write is done maybe activate the next (if any)
     */

    if (!activate_next_writer(env, descP, sockRef)) {
        descP->currentWriterP    = NULL;
        ESOCK_ASSERT(!descP->currentWriter.env);
        descP->currentWriter.env = NULL;
        descP->currentWriter.ref = esock_atom_undefined;
        enif_set_pid_undefined(&descP->currentWriter.pid);
        esock_monitor_init(&descP->currentWriter.mon);
    }

    return esock_atom_ok;
}



/* *** send_check_failure ***
 *
 * Processing done upon failed send.
 * An actual failure - we (and everyone waiting) give up.
 */
static
ERL_NIF_TERM send_check_fail(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             int              saveErrno,
                             ERL_NIF_TERM     sockRef)
{
    ESockRequestor req;
    ERL_NIF_TERM   reason;

    req.env = NULL;
    // cnt_inc(&descP->writeFails, 1);
    SOCK_CNT_INC(env, descP, sockRef, atom_write_fails, &descP->writeFails, 1);

    SSDBG( descP, ("SOCKET", "send_check_fail -> error: %d\r\n", saveErrno) );

    reason = MKA(env, erl_errno_id(saveErrno));

    if (descP->currentWriterP != NULL) {

        DEMONP("send_check_fail -> current writer",
               env, descP, &descP->currentWriter.mon);

        while (writer_pop(env, descP, &req)) {
            SSDBG( descP,
                   ("SOCKET", "send_check_fail -> abort %T\r\n", req.pid) );
            esock_send_abort_msg(env, sockRef, req.ref, req.env,
                                 reason, &req.pid);
            req.env = NULL;
            DEMONP("send_check_fail -> pop'ed writer", env, descP, &req.mon);
        }
    }

    return esock_make_error(env, reason);
}



/* *** send_check_retry ***
 *
 * Processing done upon uncomplete or blocked send.
 *
 * We failed to write the *entire* packet (anything less
 * then size of the packet, which is 0 <= written < sizeof
 * packet, so schedule the rest for later.
 */
static
ERL_NIF_TERM send_check_retry(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ssize_t          written,
                              ERL_NIF_TERM     sockRef,
                              ERL_NIF_TERM     sendRef)
{
    int          sres;
    ErlNifPid    caller;
    ERL_NIF_TERM res;

    if (descP->currentWriterP == NULL) {

        if (enif_self(env, &caller) == NULL)
            return esock_make_error(env, atom_exself);
        descP->currentWriter.pid = caller;

        if (MONP("send_check_retry -> current writer",
                 env, descP,
                 &descP->currentWriter.pid,
                 &descP->currentWriter.mon) != 0) {
            enif_set_pid_undefined(&descP->currentWriter.pid);
            return esock_make_error(env, atom_exmon);
        } else {
            ESOCK_ASSERT(!descP->currentWriter.env);
            descP->currentWriter.env = esock_alloc_env("current-writer");
            descP->currentWriter.ref = CP_TERM(descP->currentWriter.env, sendRef);
            descP->currentWriterP    = &descP->currentWriter;
        }
    }

    // cnt_inc(&descP->writeWaits, 1);
    SOCK_CNT_INC(env, descP, sockRef, atom_write_waits, &descP->writeWaits, 1);

    sres = esock_select_write(env, descP->sock, descP, NULL, sockRef, sendRef);

    if (written >= 0) {

        /* Partial *write* success */

        if (sres < 0) {
            /* Returned: {error, Reason}
             * Reason:   {select_failed, sres, written}
             */
            res = esock_make_error(env,
                                   MKT3(env,
                                        esock_atom_select_failed,
                                        MKI(env, sres),
                                        MKI(env, written)));
        } else {
            res = esock_make_ok2(env, MKI(env, written));
        }

    } else {

        if (sres < 0) {
            /* Returned: {error, Reason}
             * Reason:   {select_failed, sres}
             */
            res = esock_make_error(env,
                                   MKT2(env,
                                        esock_atom_select_failed,
                                        MKI(env, sres)));
        } else {
            res = esock_make_error(env, esock_atom_eagain);
        }
    }

    return res;
}




/* *** recv_check_reader ***
 *
 * Checks if we have a current reader and if that is us. If not,
 * then we must be made to wait for our turn. This is done by pushing
 * us unto the reader queue.
 * Note that we do *not* actually initiate the currentReader structure
 * here, since we do not actually know yet if we need to! We do that in
 * the [recv|recvfrom|recvmsg]_check_result function.
 */
static
BOOLEAN_T recv_check_reader(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     ref,
                            ERL_NIF_TERM*    checkResult)
{
    if (descP->currentReaderP != NULL) {
        ErlNifPid caller;
        
        if (enif_self(env, &caller) == NULL) {
            *checkResult = esock_make_error(env, atom_exself);
            return FALSE;
        }

        if (COMPARE_PIDS(&descP->currentReader.pid, &caller) != 0) {
            ERL_NIF_TERM tmp;

            /* Not the "current reader", so (maybe) push onto queue */

            SSDBG( descP,
                   ("SOCKET",
                    "recv_check_reader -> not (current) reader\r\n") );

            if (!reader_search4pid(env, descP, &caller))
                tmp = reader_push(env, descP, caller, ref);
            else
                tmp = esock_make_error(env, esock_atom_eagain);
            
            SSDBG( descP,
                   ("SOCKET",
                    "recv_check_reader -> queue (push) result: %T\r\n", tmp) );

            *checkResult = tmp;

            return FALSE;

        }
        
    }

    // Does not actually matter in this case, but ...
    *checkResult = esock_atom_ok;

    return TRUE;
}



/* *** recv_init_current_reader ***
 *
 * Initiate (maybe) the currentReader structure of the descriptor.
 * Including monitoring the calling process.
 */
static
char* recv_init_current_reader(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     recvRef)
{
    if (descP->currentReaderP == NULL) {
        ErlNifPid caller;

        if (enif_self(env, &caller) == NULL)
            return str_exself;

        descP->currentReader.pid = caller;
        if (MONP("recv_init_current_reader -> current reader",
                 env, descP,
                 &descP->currentReader.pid,
                 &descP->currentReader.mon) != 0) {
            enif_set_pid_undefined(&descP->currentReader.pid);
            return str_exmon;
        } else {
            ESOCK_ASSERT(!descP->currentReader.env);
            descP->currentReader.env = esock_alloc_env("current-reader");
            descP->currentReader.ref = CP_TERM(descP->currentReader.env,
                                               recvRef);
            descP->currentReaderP    = &descP->currentReader;
        }
    } else {

        /*
         * This is a retry:
         * We have done, for instance, recv(Sock, X), but only received Y < X.
         * We then call recv again with size = X-Y. So, we then get a new ref.
         * 
         * Make use of the existing environment
         */

        descP->currentReader.ref = CP_TERM(descP->currentReader.env, recvRef);
    }

    return NULL;
}



/* *** recv_update_current_reader ***
 *
 * Demonitors the current reader process and pop's the reader queue.
 * If there is a waiting (reader) process, then it will be assigned
 * as the new current reader and a new (read) select will be done.
 */

static
ERL_NIF_TERM recv_update_current_reader(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM res = esock_atom_ok;

    if (descP->currentReaderP != NULL) {
        
        DEMONP("recv_update_current_reader",
               env, descP, &descP->currentReader.mon);

        esock_free_env("recv_update_current_reader - current-read-env",
                       descP->currentReader.env);
        descP->currentReader.env = NULL;

        if (!activate_next_reader(env, descP, sockRef)) {

            SSDBG( descP,
                   ("SOCKET",
                    "recv_update_current_reader -> no more readers\r\n") );

            descP->currentReaderP    = NULL;
            descP->currentReader.ref = esock_atom_undefined;
            enif_set_pid_undefined(&descP->currentReader.pid);
            esock_monitor_init(&descP->currentReader.mon);
        }

    }

    return res;
}



/* *** recv_error_current_reader ***
 *
 * Process the current reader and any waiting readers
 * when a read (fatal) error has occured.
 * All waiting readers will be "aborted", that is a 
 * nif_abort message will be sent (with reaf and reason).
 */
static
void recv_error_current_reader(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     reason)
{
    ESockRequestor req;

    req.env = NULL;
    if (descP->currentReaderP != NULL) {

        DEMONP("recv_error_current_reader -> current reader",
               env, descP, &descP->currentReader.mon);

        while (reader_pop(env, descP, &req)) {
            SSDBG( descP,
                   ("SOCKET", "recv_error_current_reader -> abort %T\r\n",
                    req.pid) );
            esock_send_abort_msg(env, sockRef, req.ref, req.env,
                                 reason, &req.pid);
            req.env = NULL;
            DEMONP("recv_error_current_reader -> pop'ed reader",
                   env, descP, &req.mon);
        }
    }
}



/* *** recv_check_result ***
 *
 * Process the result of a call to recv.
 */
static
ERL_NIF_TERM recv_check_result(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               int              read,
                               int              toRead,
                               int              saveErrno,
                               ErlNifBinary*    bufP,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM res;

    SSDBG( descP,
           ("SOCKET", "recv_check_result -> entry with"
            "\r\n   read:      %d"
            "\r\n   toRead:    %d"
            "\r\n   saveErrno: %d"
            "\r\n   recvRef:   %T"
            "\r\n", read, toRead, saveErrno, recvRef) );


    /* <KOLLA>
     *
     * We need to handle read = 0 for other type(s) (DGRAM) when
     * its actually valid to read 0 bytes.
     *
     * </KOLLA>
     */
    
    if ((read == 0) && (descP->type == SOCK_STREAM)) {

        res = esock_make_error(env, atom_closed);
        
        SOCK_CNT_INC(env, descP, sockRef, atom_read_fails, &descP->readFails, 1);

        /*
         * When a stream socket peer has performed an orderly shutdown,
         * the return value will be 0 (the traditional "end-of-file" return).
         *
         * *We* do never actually try to read 0 bytes from a stream socket!
         *
         * We must also notify any waiting readers!
         */

        recv_error_current_reader(env, descP, sockRef, res);

        FREE_BIN(bufP);

    } else {
    
        /* There is a special case: If the provided 'to read' value is
         * zero (0) (only for type =/= stream).
         * That means that we reads as much as we can, using the default
         * read buffer size.
         */

        if (bufP->size == read) {

            /* +++ We filled the buffer +++ */

            SSDBG( descP,
                   ("SOCKET",
                    "recv_check_result -> [%d] filled the buffer\r\n",
                    toRead) );

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
                    "recv_check_result -> [%d] "
                    "did not fill the buffer (%d of %d)\r\n",
                    toRead, read, bufP->size) );

            res = recv_check_partial(env, descP, read, toRead, bufP,
                                     sockRef, recvRef);

        }
    }

    return res;
}



/* *** recv_check_full ***
 *
 * This function is called if we filled the allocated buffer.
 * But are we done yet?
 *
 * toRead = 0 means: Give me everything you have => maybe
 * toRead > 0 means: Yes
 */
static
ERL_NIF_TERM recv_check_full(ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             int              read,
                             int              toRead,
                             ErlNifBinary*    bufP,
                             ERL_NIF_TERM     sockRef,
                             ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM res;

    if (toRead == 0) {

        /* +++ Give us everything you have got =>     *
         *     (maybe) needs to continue          +++ */

        /* Send up each chunk of data for each of the read
         * and let the erlang code assemble it: {ok, false, Bin}
         * (when complete it should return {ok, true, Bin}).
         * We need to read atleast one more time to be sure if its
         * done...
         *
         * Also, we need to check if the rNumCnt has reached its max (rNum),
         * in which case we will assume the read to be done!
         */

        SSDBG( descP,
               ("SOCKET", "recv_check_full -> shall we continue reading"
                "\r\n   read:    %d"
                "\r\n   rNum:    %d"
                "\r\n   rNumCnt: %d"
                "\r\n", read, descP->rNum, descP->rNumCnt) );

        res = recv_check_full_maybe_done(env, descP, read, toRead, bufP,
                                         sockRef, recvRef);

    } else {

        /* +++ We got exactly as much as we requested => We are done +++ */

        SSDBG( descP,
               ("SOCKET",
                "recv_check_full -> [%d] "
                "we got exactly what we could fit\r\n", toRead) );

        res = recv_check_full_done(env, descP, read, bufP, sockRef);

    }

    return res;

}



/* *** recv_check_full_maybe_done ***
 *
 * Send up each chunk of data for each of the read
 * and let the erlang code assemble it: {ok, false, Bin}
 * (when complete it should return {ok, true, Bin}).
 * We need to read atleast one more time to be sure if its
 * done...
 *
 * Also, we need to check if the rNumCnt has reached its max (rNum),
 * in which case we will assume the read to be done!
 */
static
ERL_NIF_TERM recv_check_full_maybe_done(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        int              read,
                                        int              toRead,
                                        ErlNifBinary*    bufP,
                                        ERL_NIF_TERM     sockRef,
                                        ERL_NIF_TERM     recvRef)
{
    char* xres;

    // cnt_inc(&descP->readByteCnt, read);
    SOCK_CNT_INC(env, descP, sockRef, atom_read_byte, &descP->readByteCnt, read);

    if (descP->rNum > 0) {

        descP->rNumCnt++;
        if (descP->rNumCnt >= descP->rNum) {

            descP->rNumCnt = 0;

            // cnt_inc(&descP->readPkgCnt, 1);
            SOCK_CNT_INC(env, descP, sockRef, atom_read_pkg, &descP->readPkgCnt, 1);

            recv_update_current_reader(env, descP, sockRef);

            /* This transfers "ownership" of the *allocated* binary to an
             * erlang term (no need for an explicit free).
             */

            return esock_make_ok3(env, atom_true, MKBIN(env, bufP));

        }
    }

    /* Yes, we *do* need to continue reading */

    if ((xres = recv_init_current_reader(env, descP, recvRef)) != NULL) {
        descP->rNumCnt = 0;
        FREE_BIN(bufP);
        return esock_make_error_str(env, xres);
    }

    /* This transfers "ownership" of the *allocated* binary to an
     * erlang term (no need for an explicit free).
     */

    SSDBG( descP,
           ("SOCKET",
            "recv_check_full_maybe_done -> [%d] "
            "we are done for now - read more\r\n", toRead) );

    return esock_make_ok3(env, atom_false, MKBIN(env, bufP));
}



/* *** recv_check_full_done ***
 *
 * A successful recv and we filled the buffer.
 */
static
ERL_NIF_TERM recv_check_full_done(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  int              read,
                                  ErlNifBinary*    bufP,
                                  ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM data;

    // cnt_inc(&descP->readPkgCnt,  1);
    SOCK_CNT_INC(env, descP, sockRef, atom_read_pkg, &descP->readPkgCnt, 1);
    // cnt_inc(&descP->readByteCnt, read);
    SOCK_CNT_INC(env, descP, sockRef, atom_read_byte, &descP->readByteCnt, read);

    recv_update_current_reader(env, descP, sockRef);

    /* This transfers "ownership" of the *allocated* binary to an
     * erlang term (no need for an explicit free).
     */
    data = MKBIN(env, bufP);

    return esock_make_ok3(env, atom_true, data);
}



/* *** recv_check_fail ***
 *
 * Handle recv failure.
 */
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

    FREE_BIN(buf1P); if (buf2P != NULL) FREE_BIN(buf2P);

    if (saveErrno == ECONNRESET)  {

        /* +++ Oups - closed +++ */

        SSDBG( descP, ("SOCKET", "recv_check_fail -> closed\r\n") );

        // This is a bit overkill (to count here), but just in case...
        // cnt_inc(&descP->readFails, 1);
        SOCK_CNT_INC(env, descP, sockRef, atom_read_fails, &descP->readFails, 1);

        res = recv_check_fail_closed(env, descP, sockRef, recvRef);

    } else if ((saveErrno == ERRNO_BLOCK) ||
               (saveErrno == EAGAIN)) {

        SSDBG( descP, ("SOCKET", "recv_check_fail -> eagain\r\n") );

        res = recv_check_retry(env, descP, sockRef, recvRef);

    } else {

        SSDBG( descP, ("SOCKET", "recv_check_fail -> errno: %d\r\n",
                       saveErrno) );

        // cnt_inc(&descP->readFails, 1);
        SOCK_CNT_INC(env, descP, sockRef, atom_read_fails, &descP->readFails, 1);

        res = recv_check_fail_gen(env, descP, saveErrno, sockRef);
    }

    return res;
}



/* *** recv_check_fail_closed ***
 *
 * We detected that the socket was closed wile reading.
 * Inform current and waiting readers.
 */
static
ERL_NIF_TERM recv_check_fail_closed(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     sockRef,
                                    ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM res = esock_make_error(env, atom_closed);
    int          sres;

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

    descP->closeLocal = FALSE;
    descP->state      = SOCKET_STATE_CLOSING;

    recv_error_current_reader(env, descP, sockRef, res);

    if ((sres = esock_select_stop(env, descP->sock, descP)) < 0) {
        esock_warning_msg("Failed stop select (closed) "
                          "for current reader (%T): %d\r\n",
                          recvRef, sres);
    }

    return res;
}



/* *** recv_check_retry ***
 *
 * The recv call would have blocked, so retry.
 */
static
ERL_NIF_TERM recv_check_retry(ErlNifEnv*       env,
                              ESockDescriptor* descP,
                              ERL_NIF_TERM     sockRef,
                              ERL_NIF_TERM     recvRef)
{
    int          sres;
    char*        xres;
    ERL_NIF_TERM reason;

    descP->rNumCnt = 0;
    if ((xres = recv_init_current_reader(env, descP, recvRef)) != NULL)
        return esock_make_error_str(env, xres);

    SSDBG( descP, ("SOCKET", "recv_check_retry -> SELECT for more\r\n") );

    if ((sres = esock_select_read(env, descP->sock, descP, NULL,
                                  sockRef, recvRef)) < 0) {
        /* Ouch
         * Now what? We have copied ref into *its own* environment!
         */
        reason = MKT2(env, esock_atom_select_failed, MKI(env, sres));
    } else {
        reason = esock_atom_eagain;
    }

    return esock_make_error(env, reason);

}



/* *** recv_check_fail_gen ***
 *
 * The recv call had a "general" failure.
 */
static
ERL_NIF_TERM recv_check_fail_gen(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 int              saveErrno,
                                 ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM res = esock_make_error_errno(env, saveErrno);

    recv_error_current_reader(env, descP, sockRef, res);

    return res;
}



/* *** recv_check_partial ***
 *
 * Handle a sucessful recv which only partly filled the specified buffer.
 */
static
ERL_NIF_TERM recv_check_partial(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                int              read,
                                int              toRead,
                                ErlNifBinary*    bufP,
                                ERL_NIF_TERM     sockRef,
                                ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM res;

    if (toRead == 0) {

        /* +++ We got it all, but since we      +++
         * +++ did not fill the buffer, we      +++
         * +++ must split it into a sub-binary. +++
         */

        SSDBG( descP,
               ("SOCKET",
                "recv_check_partial -> [%d] split buffer\r\n", toRead) );

        res = recv_check_partial_done(env, descP, read, bufP, sockRef);

    } else {

        SSDBG( descP, ("SOCKET", "recv_check_partial -> [%d] "
                       "only part of message - expect more\r\n", toRead) );

        res = recv_check_partial_part(env, descP, read, bufP, sockRef, recvRef);
    }

    return res;
}



/* *** recv_check_partial_done ***
 *
 * A successful but only partial recv, which fulfilled the required read.
 */
static
ERL_NIF_TERM recv_check_partial_done(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     int              read,
                                     ErlNifBinary*    bufP,
                                     ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM data;

    descP->rNumCnt = 0;
    // cnt_inc(&descP->readPkgCnt,  1);
    SOCK_CNT_INC(env, descP, sockRef, atom_read_pkg, &descP->readPkgCnt, 1);
    // cnt_inc(&descP->readByteCnt, read);
    SOCK_CNT_INC(env, descP, sockRef, atom_read_byte, &descP->readByteCnt, read);

    recv_update_current_reader(env, descP, sockRef);

    /* This transfers "ownership" of the *allocated* binary to an
     * erlang term (no need for an explicit free).
     */
    data = MKBIN(env, bufP);
    data = MKSBIN(env, data, 0, read);

    SSDBG( descP,
           ("SOCKET", "recv_check_partial_done -> [%d] done\r\n", read) );

    return esock_make_ok3(env, atom_true, data);
}



/* *** recv_check_partial_part ***
 *
 * A successful but only partial recv, which only partly fulfilled
 * the required read.
 */
static
ERL_NIF_TERM recv_check_partial_part(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     int              read,
                                     ErlNifBinary*    bufP,
                                     ERL_NIF_TERM     sockRef,
                                     ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM res, reason, data;
    char*        xres;
    int          sres;

    if ((xres = recv_init_current_reader(env, descP, recvRef)) != NULL) {
        FREE_BIN(bufP);
        return esock_make_error_str(env, xres);
    }

    data = MKBIN(env, bufP);
    data = MKSBIN(env, data, 0, read);

    // cnt_inc(&descP->readByteCnt, read);
    SOCK_CNT_INC(env, descP, sockRef, atom_read_byte, &descP->readByteCnt, read);

    /* SELECT for more data */

    sres = esock_select_read(env, descP->sock, descP, NULL,
                             sockRef, recvRef);
    if (sres < 0) {
        /* Result: {error, Reason}
         * Reason: {select_failed, sres, data}
         */
        reason = MKT3(env, esock_atom_select_failed, MKI(env, sres), data);
        res    = esock_make_error(env, reason);

    } else {

        res = esock_make_ok3(env, atom_false, data);

    }

    /* This transfers "ownership" of the *allocated* binary to an
     * erlang term (no need for an explicit free).
     */
    return res;
}





/* The recvfrom function delivers one (1) message. If our buffer
 * is to small, the message will be truncated. So, regardless
 * if we filled the buffer or not, we have got what we are going
 * to get regarding this message.
 */
static
ERL_NIF_TERM recvfrom_check_result(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              read,
                                   int              saveErrno,
                                   ErlNifBinary*    bufP,
                                   ESockAddress*    fromAddrP,
                                   unsigned int     fromAddrLen,
                                   ERL_NIF_TERM     sockRef,
                                   ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM data, res;

    SSDBG( descP,
           ("SOCKET", "recvfrom_check_result -> entry with"
            "\r\n   read:      %d"
            "\r\n   saveErrno: %d"
            "\r\n   recvRef:   %T"
            "\r\n", read, saveErrno, recvRef) );

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

        // cnt_inc(&descP->readPkgCnt,  1);
        SOCK_CNT_INC(env, descP, sockRef, atom_read_pkg, &descP->readPkgCnt, 1);
        // cnt_inc(&descP->readByteCnt, read);
        SOCK_CNT_INC(env, descP, sockRef, atom_read_byte,
                     &descP->readByteCnt, read);

        recv_update_current_reader(env, descP, sockRef);
        
        res = esock_make_ok2(env, MKT2(env, eSockAddr, data));

    }

    return res;

}



/* *** recvmsg_check_result ***
 *
 * The recvmsg function delivers one (1) message. If our buffer
 * is to small, the message will be truncated. So, regardless
 * if we filled the buffer or not, we have got what we are going
 * to get regarding this message.
 */
static
ERL_NIF_TERM recvmsg_check_result(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  int              read,
                                  int              saveErrno,
                                  struct msghdr*   msgHdrP,
                                  ErlNifBinary*    dataBufP,
                                  ErlNifBinary*    ctrlBufP,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     recvRef)
{
    ERL_NIF_TERM res;

    SSDBG( descP,
           ("SOCKET", "recvmsg_check_result -> entry with"
            "\r\n   read:      %d"
            "\r\n   saveErrno: %d"
            "\r\n   recvRef:   %T"
            "\r\n", read, saveErrno, recvRef) );


    /* <KOLLA>
     *
     * We need to handle read = 0 for other type(s) (DGRAM) when
     * its actually valid to read 0 bytes.
     *
     * </KOLLA>
     */
    
    if ((read == 0) && (descP->type == SOCK_STREAM)) {
        
        /*
         * When a stream socket peer has performed an orderly shutdown,
         * the return value will be 0 (the traditional "end-of-file" return).
         *
         * *We* do never actually try to read 0 bytes from a stream socket!
         */

        SOCK_CNT_INC(env, descP, sockRef, atom_read_fails, &descP->readFails, 1);

        FREE_BIN(dataBufP); FREE_BIN(ctrlBufP);

        return esock_make_error(env, atom_closed);

    }


    /* There is a special case: If the provided 'to read' value is
     * zero (0). That means that we reads as much as we can, using
     * the default read buffer size.
     */

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



/* *** recvmsg_check_msg ***
 *
 * We successfully read one message. Time to process.
 */
static
ERL_NIF_TERM recvmsg_check_msg(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               int              read,
                               struct msghdr*   msgHdrP,
                               ErlNifBinary*    dataBufP,
                               ErlNifBinary*    ctrlBufP,
                               ERL_NIF_TERM     sockRef)
{
    ERL_NIF_TERM res, eMsgHdr;
    char*        xres;

    /*
     * <KOLLA>
     *
     * The return value of recvmsg is the *total* number of bytes
     * that where successfully read. This data has been put into
     * the *IO vector*.
     *
     * </KOLLA>
     */

    if ((xres = encode_msghdr(env, descP,
                              read, msgHdrP, dataBufP, ctrlBufP,
                              &eMsgHdr)) != NULL) {

        SSDBG( descP,
               ("SOCKET",
                "recvmsg_check_result -> "
                "(msghdr) encode failed: %s\r\n", xres) );

        /* So this is a bit strange. We did "successfully" read 'read' bytes,
         * but then we fail to process the message header. So what counters
         * shall we increment?
         *    Only failure?
         *    Or only success (pkg and byte), since the read was "ok" (or was it?)
         *    Or all of them?
         *
         * For now, we increment all three...
         */
         
        SOCK_CNT_INC(env, descP, sockRef, atom_read_fails, &descP->readFails, 1);
        SOCK_CNT_INC(env, descP, sockRef, atom_read_pkg, &descP->readPkgCnt, 1);
        SOCK_CNT_INC(env, descP, sockRef, atom_read_byte,
                     &descP->readByteCnt, read);

        recv_update_current_reader(env, descP, sockRef);

        FREE_BIN(dataBufP); FREE_BIN(ctrlBufP);

        res = esock_make_error_str(env, xres);

    } else {

        SSDBG( descP,
               ("SOCKET", "recvmsg_check_result -> (msghdr) encode ok\r\n") );

        SOCK_CNT_INC(env, descP, sockRef, atom_read_pkg, &descP->readPkgCnt, 1);
        SOCK_CNT_INC(env, descP, sockRef, atom_read_byte,
                     &descP->readByteCnt, read);

        recv_update_current_reader(env, descP, sockRef);

        res = esock_make_ok2(env, eMsgHdr);
    }

    return res;
}




/* +++ encode_msghdr +++
 *
 * Encode a msghdr (recvmsg). In erlang its represented as
 * a map, which has a specific set of attributes:
 *
 *     addr (source address) - sockaddr()
 *     iov                   - [binary()]
 *     ctrl                  - [cmsghdr()]
 *     flags                 - msghdr_flags()
 */

extern
char* encode_msghdr(ErlNifEnv*       env,
                    ESockDescriptor* descP,
                    int              read,
                    struct msghdr*   msgHdrP,
                    ErlNifBinary*    dataBufP,
                    ErlNifBinary*    ctrlBufP,
                    ERL_NIF_TERM*    eSockAddr)
{
    char*        xres;
    ERL_NIF_TERM addr, iov, ctrl, flags;

    SSDBG( descP,
           ("SOCKET", "encode_msghdr -> entry with"
            "\r\n   read: %d"
            "\r\n", read) );

    /* The address is not used if we are connected (unless, maybe,
     * family is 'local'), so check (length = 0) before we try to encodel
     */
    if (msgHdrP->msg_namelen != 0) {
        if ((xres = esock_encode_sockaddr(env,
                                          (ESockAddress*) msgHdrP->msg_name,
                                          msgHdrP->msg_namelen,
                                          &addr)) != NULL)
            return xres;
    } else {
        addr = esock_atom_undefined;
    }

    SSDBG( descP, ("SOCKET", "encode_msghdr -> try encode iov\r\n") );
    if ((xres = esock_encode_iov(env,
                                 read,
                                 msgHdrP->msg_iov,
                                 msgHdrP->msg_iovlen,
                                 dataBufP,
                                 &iov)) != NULL)
        return xres;

    SSDBG( descP, ("SOCKET", "encode_msghdr -> try encode cmsghdrs\r\n") );
    if ((xres = encode_cmsghdrs(env, descP, ctrlBufP, msgHdrP, &ctrl)) != NULL)
        return xres;

    SSDBG( descP, ("SOCKET", "encode_msghdr -> try encode flags\r\n") );
    if ((xres = encode_msghdr_flags(env, descP, msgHdrP->msg_flags, &flags)) != NULL)
        return xres;

    SSDBG( descP,
           ("SOCKET", "encode_msghdr -> components encoded:"
            "\r\n   addr:  %T"
            "\r\n   ctrl:  %T"
            "\r\n   flags: %T"
           "\r\n", addr, ctrl, flags) );
    {
        ERL_NIF_TERM keys[]  = {esock_atom_addr,
                                esock_atom_iov,
                                esock_atom_ctrl,
                                esock_atom_flags};
        ERL_NIF_TERM vals[]  = {addr, iov, ctrl, flags};
        unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);
        unsigned int numVals = sizeof(vals) / sizeof(ERL_NIF_TERM);
        ERL_NIF_TERM tmp;
        
        ESOCK_ASSERT( (numKeys == numVals) );
        
        SSDBG( descP, ("SOCKET", "encode_msghdr -> create msghdr map\r\n") );
        if (!MKMA(env, keys, vals, numKeys, &tmp))
            return ESOCK_STR_EINVAL;

        SSDBG( descP, ("SOCKET", "encode_msghdr -> msghdr encoded\r\n") );

        *eSockAddr = tmp;
    }

    SSDBG( descP, ("SOCKET", "encode_msghdr -> done\r\n") );

    return NULL;
}




/* +++ encode_cmsghdrs +++
 *
 * Encode a list of cmsghdr(). There can be 0 or more cmsghdr "blocks".
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

extern
char* encode_cmsghdrs(ErlNifEnv*       env,
                      ESockDescriptor* descP,
                      ErlNifBinary*    cmsgBinP,
                      struct msghdr*   msgHdrP,
                      ERL_NIF_TERM*    eCMsgHdr)
{
    ERL_NIF_TERM    ctrlBuf  = MKBIN(env, cmsgBinP); // The *entire* binary
    SocketTArray    cmsghdrs = TARRAY_CREATE(128);
    struct cmsghdr* firstP   = CMSG_FIRSTHDR(msgHdrP);
    struct cmsghdr* currentP;
    
    SSDBG( descP, ("SOCKET", "encode_cmsghdrs -> entry when"
                   "\r\n   msg ctrl len:  %d"
                   "\r\n   (ctrl) firstP: 0x%lX"
                   "\r\n",
                   msgHdrP->msg_controllen, firstP) );

    for (currentP = firstP;
         /*
          * In *old* versions of darwin, the CMSG_FIRSTHDR does not
          * check the msg_controllen, so we do it here.
          * We should really test this stuff during configure,
          * but for now, this will have to do.
          */
#if defined(__DARWIN__)
         (msgHdrP->msg_controllen >= sizeof(struct cmsghdr)) && (currentP != NULL);
#else
         (currentP != NULL);
#endif
         currentP = CMSG_NXTHDR(msgHdrP, currentP)) {

        SSDBG( descP,
               ("SOCKET", "encode_cmsghdrs -> process cmsg header when"
                "\r\n   TArray Size: %d"
                "\r\n", TARRAY_SZ(cmsghdrs)) );

        /* MUST check this since on Linux the returned "cmsg" may actually
         * go too far!
         */
        if (((CHARP(currentP) + currentP->cmsg_len) - CHARP(firstP)) >
            msgHdrP->msg_controllen) {

            /* Ouch, fatal error - give up 
             * We assume we cannot trust any data if this is wrong.
             */

            SSDBG( descP,
                   ("SOCKET", "encode_cmsghdrs -> check failed when: "
                    "\r\n   currentP:           0x%lX"
                    "\r\n   (current) cmsg_len: %d"
                    "\r\n   firstP:             0x%lX"
                    "\r\n   =>                  %d"
                    "\r\n   msg ctrl len:       %d"
                    "\r\n",
                    CHARP(currentP), currentP->cmsg_len, CHARP(firstP),
                    (CHARP(currentP) + currentP->cmsg_len) - CHARP(firstP),
                    msgHdrP->msg_controllen) );

            TARRAY_DELETE(cmsghdrs);
            return ESOCK_STR_EINVAL;
        } else {
            ERL_NIF_TERM   level, type, data;
            unsigned char* dataP   = (unsigned char*) CMSG_DATA(currentP);
            size_t         dataPos = dataP - cmsgBinP->data;
            size_t         dataLen = currentP->cmsg_len - (CHARP(currentP)-CHARP(dataP));

            SSDBG( descP,
                   ("SOCKET", "encode_cmsghdrs -> cmsg header data: "
                    "\r\n   dataPos: %d"
                    "\r\n   dataLen: %d"
                    "\r\n", dataPos, dataLen) );

            /* We can't give up just because its an unknown protocol,
             * so if its a protocol we don't know, we return its integer 
             * value and leave it to the user.
             */
            if (encode_cmsghdr_level(env, currentP->cmsg_level, &level) != NULL)
                level = MKI(env, currentP->cmsg_level);

            if (encode_cmsghdr_type(env,
                                    currentP->cmsg_level, currentP->cmsg_type,
                                    &type) != NULL)
                type = MKI(env, currentP->cmsg_type);

            if (encode_cmsghdr_data(env, ctrlBuf,
                                    currentP->cmsg_level,
                                    currentP->cmsg_type,
                                    dataP, dataPos, dataLen,
                                    &data) != NULL)
                data = MKSBIN(env, ctrlBuf, dataPos, dataLen);

            SSDBG( descP,
                   ("SOCKET", "encode_cmsghdrs -> "
                    "\r\n   level: %T"
                    "\r\n   type:  %T"
                    "\r\n   data:  %T"
                    "\r\n", level, type, data) );

            /* And finally create the 'cmsghdr' map -
             * and if successfull add it to the tarray.
             */
            {
                ERL_NIF_TERM keys[]  = {esock_atom_level,
                                        esock_atom_type,
                                        esock_atom_data};
                ERL_NIF_TERM vals[]  = {level, type, data};
                unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);
                unsigned int numVals = sizeof(vals) / sizeof(ERL_NIF_TERM);
                ERL_NIF_TERM cmsgHdr;

                /* Guard agains cut-and-paste errors */
                ESOCK_ASSERT( (numKeys == numVals) );
            
                if (!MKMA(env, keys, vals, numKeys, &cmsgHdr)) {
                    TARRAY_DELETE(cmsghdrs);
                    return ESOCK_STR_EINVAL;
                }

                /* And finally add it to the list... */
                TARRAY_ADD(cmsghdrs, cmsgHdr);
            }
        }
    }

    SSDBG( descP,
           ("SOCKET", "encode_cmsghdrs -> cmsg headers processed when"
            "\r\n   TArray Size: %d"
            "\r\n", TARRAY_SZ(cmsghdrs)) );

    /* The tarray is populated - convert it to a list */
    TARRAY_TOLIST(cmsghdrs, env, eCMsgHdr);

    return NULL;
}



/* +++ decode_cmsghdrs +++
 *
 * Decode a list of cmsghdr(). There can be 0 or more cmsghdr "blocks".
 *
 * Each element can either be a (erlang) map that needs to be decoded,
 * or a (erlang) binary that just needs to be appended to the control
 * buffer.
 *
 * Our "problem" is that we have no idea much memory we actually need.
 *
 */

extern
char* decode_cmsghdrs(ErlNifEnv*       env,
                      ESockDescriptor* descP,
                      ERL_NIF_TERM     eCMsgHdr,
                      char*            cmsgHdrBufP,
                      size_t           cmsgHdrBufLen,
                      size_t*          cmsgHdrBufUsed)
{
    ERL_NIF_TERM elem, tail, list;
    char*        bufP;
    size_t       rem, used, totUsed = 0;
    unsigned int len;
    int          i;
    char*        xres;

    SSDBG( descP, ("SOCKET", "decode_cmsghdrs -> entry with"
                   "\r\n   cmsgHdrBufP:   0x%lX"
                   "\r\n   cmsgHdrBufLen: %d"
                   "\r\n", cmsgHdrBufP, cmsgHdrBufLen) );

    if (IS_LIST(env, eCMsgHdr) && GET_LIST_LEN(env, eCMsgHdr, &len)) {

        SSDBG( descP, ("SOCKET", "decode_cmsghdrs -> list length: %d\r\n", len) );

        for (i = 0, list = eCMsgHdr, rem  = cmsgHdrBufLen, bufP = cmsgHdrBufP;
             i < len; i++) {
            
            SSDBG( descP, ("SOCKET", "decode_cmsghdrs -> process elem %d:"
                           "\r\n   (buffer) rem:     %u"
                           "\r\n   (buffer) totUsed: %u"
                           "\r\n", i, rem, totUsed) );

            /* Extract the (current) head of the (cmsg hdr) list */
            if (!GET_LIST_ELEM(env, list, &elem, &tail))
                return ESOCK_STR_EINVAL;
            
            used = 0; // Just in case...
            if ((xres = decode_cmsghdr(env, descP, elem, bufP, rem, &used)) != NULL)
                return xres;

            bufP     = CHARP( ULONG(bufP) + used );
            rem      = SZT( rem - used );
            list     = tail;
            totUsed += used;

        }

        SSDBG( descP, ("SOCKET",
                       "decode_cmsghdrs -> all %d ctrl headers processed\r\n",
                       len) );

        xres = NULL;
    } else {
        xres = ESOCK_STR_EINVAL;
    }

    *cmsgHdrBufUsed = totUsed;

    SSDBG( descP, ("SOCKET", "decode_cmsghdrs -> done with %s when"
                   "\r\n   totUsed = %u\r\n",
                   ((xres != NULL) ? xres : "NULL"), totUsed) );

    return xres;
}


/* +++ decode_cmsghdr +++
 *
 * Decode one cmsghdr(). Put the "result" into the buffer and advance the
 * pointer (of the buffer) afterwards. Also update 'rem' accordingly.
 * But before the actual decode, make sure that there is enough room in 
 * the buffer for the cmsg header (sizeof(*hdr) < rem).
 *
 * The eCMsgHdr should be a map with three fields: 
 *
 *     level :: cmsghdr_level()   (socket | protocol() | integer())
 *     type  :: cmsghdr_type()    (atom() | integer())
 *                                What values are valid depend on the level
 *     data  :: cmsghdr_data()    (term() | binary())
 *                                The type of the data depends on
 *                                level and type, but can be a binary,
 *                                which means that the data is already coded.
 */
extern
char* decode_cmsghdr(ErlNifEnv*       env,
                     ESockDescriptor* descP,
                     ERL_NIF_TERM     eCMsgHdr,
                     char*            bufP,
                     size_t           rem,
                     size_t*          used)
{
    SSDBG( descP, ("SOCKET", "decode_cmsghdr -> entry with"
                   "\r\n   eCMsgHdr: %T"
                   "\r\n", eCMsgHdr) );

    if (IS_MAP(env, eCMsgHdr)) {
        ERL_NIF_TERM eLevel, eType, eData;
        int          level, type;
        char*        xres;

        /* First extract all three attributes (as terms) */

        if (!GET_MAP_VAL(env, eCMsgHdr, esock_atom_level, &eLevel))
            return ESOCK_STR_EINVAL;
        
        SSDBG( descP, ("SOCKET", "decode_cmsghdr -> eLevel: %T"
                       "\r\n", eLevel) );

        if (!GET_MAP_VAL(env, eCMsgHdr, esock_atom_type, &eType))
            return ESOCK_STR_EINVAL;
        
        SSDBG( descP, ("SOCKET", "decode_cmsghdr -> eType:  %T"
                       "\r\n", eType) );

        if (!GET_MAP_VAL(env, eCMsgHdr, esock_atom_data, &eData))
            return ESOCK_STR_EINVAL;

        SSDBG( descP, ("SOCKET", "decode_cmsghdr -> eData:  %T"
                       "\r\n", eData) );

        /* Second, decode level */
        if ((xres = decode_cmsghdr_level(env, eLevel, &level)) != NULL)
            return xres;

        SSDBG( descP, ("SOCKET", "decode_cmsghdr -> level:  %d\r\n", level) );

        /* third, decode type */
        if ((xres = decode_cmsghdr_type(env, level, eType, &type)) != NULL)
            return xres;
        
        SSDBG( descP, ("SOCKET", "decode_cmsghdr -> type:   %d\r\n", type) );

        /* And finally data
         * If its a binary, we are done. Otherwise, we need to check
         * level and type to know what kind of data to expect.
         */

        return decode_cmsghdr_data(env, descP, bufP, rem, level, type, eData, used);

    } else {
        *used = 0;
        return ESOCK_STR_EINVAL;
    }

    return NULL;
}


/* *** decode_cmsghdr_data ***
 *
 * For all combinations of level and type we accept a binary as data,
 * so we begin by testing for that. If its not a binary, then we check
 * level (ip) and type (tos or ttl), in which case the data *must* be
 * an integer and ip_tos() respectively.
 */
static
char* decode_cmsghdr_data(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          char*            bufP,
                          size_t           rem,
                          int              level,
                          int              type,
                          ERL_NIF_TERM     eData,
                          size_t*          used)
{
    char* xres;

    SSDBG( descP, ("SOCKET", "decode_cmsghdr_data -> entry with"
                   "\r\n   eData: %T"
                   "\r\n", eData) );

    if (IS_BIN(env, eData)) {
        ErlNifBinary bin;
        
        if (GET_BIN(env, eData, &bin)) {
            SSDBG( descP, ("SOCKET", "decode_cmsghdr_data -> "
                           "do final decode with binary\r\n") );
            return decode_cmsghdr_final(descP, bufP, rem, level, type,
                                        (char*) bin.data, bin.size,
                                        used);
        } else {
            *used = 0;
            xres  = ESOCK_STR_EINVAL;
        }
    } else {

        /* Its *not* a binary so we need to look at what level and type 
         * we have and treat them individually.
         */

        switch (level) {
#if defined(SOL_IP)
        case SOL_IP:
#else
        case IPPROTO_IP:
#endif
            switch (type) {
#if defined(IP_TOS)
            case IP_TOS:
                {
                    int data;
                    if (decode_ip_tos(env, eData, &data)) {
                        SSDBG( descP, ("SOCKET", "decode_cmsghdr_data -> "
                                       "do final decode with tos\r\n") );
                        return decode_cmsghdr_final(descP, bufP, rem, level, type,
                                                    (char*) &data,
                                                    sizeof(data),
                                                    used);
                    } else {
                        *used = 0;
                        xres  = ESOCK_STR_EINVAL;
                    }
                }
                break;
#endif

#if defined(IP_TTL)
            case IP_TTL:
                {
                    int data;
                    if (GET_INT(env, eData, &data)) {
                        SSDBG( descP, ("SOCKET", "decode_cmsghdr_data -> "
                                       "do final decode with ttl\r\n") );
                        return decode_cmsghdr_final(descP, bufP, rem, level, type,
                                                    (char*) &data,
                                                    sizeof(data),
                                                    used);
                    } else {
                        *used = 0;
                        xres  = ESOCK_STR_EINVAL;
                    }
                }
                break;
#endif

            }
            break;

        default:
            *used = 0;
            xres  = ESOCK_STR_EINVAL;
            break;
        }        

    }

    return xres;
}
                              

/* *** decode_cmsghdr_final ***
 *
 * This does the final create of the cmsghdr (including the data copy).
 */
static
char* decode_cmsghdr_final(ESockDescriptor* descP,
                           char*            bufP,
                           size_t           rem,
                           int              level,
                           int              type,
                           char*            data,
                           int              sz,
                           size_t*          used)
{
    int len   = CMSG_LEN(sz);
    int space = CMSG_SPACE(sz);

    SSDBG( descP, ("SOCKET", "decode_cmsghdr_data -> entry when"
                   "\r\n   level: %d"
                   "\r\n   type:  %d"
                   "\r\n   sz:    %d => %d, %d"
                   "\r\n", level, type, sz, len, space) );

    if (rem >= space) {
        struct cmsghdr* cmsgP = (struct cmsghdr*) bufP;
        
        /* The header */
        cmsgP->cmsg_len   = len;
        cmsgP->cmsg_level = level;
        cmsgP->cmsg_type  = type;

        sys_memcpy(CMSG_DATA(cmsgP), data, sz);
        *used = space;
    } else {
        *used = 0;
        return ESOCK_STR_EINVAL;
    }

    SSDBG( descP, ("SOCKET", "decode_cmsghdr_final -> done\r\n") );

    return NULL;
}


/* +++ encode_cmsghdr_level +++
 *
 * Encode the level part of the cmsghdr().
 *
 */

static
char* encode_cmsghdr_level(ErlNifEnv*    env,
                           int           level,
                           ERL_NIF_TERM* eLevel)
{
    char* xres;

    switch (level) {
    case SOL_SOCKET:
        *eLevel = esock_atom_socket;
        xres    = NULL;
        break;

#if defined(SOL_IP)
    case SOL_IP:
#else
    case IPPROTO_IP:
#endif
        *eLevel = esock_atom_ip;
        xres    = NULL;
        break;

#if defined(HAVE_IPV6)
#if defined(SOL_IPV6)
    case SOL_IPV6:
#else
    case IPPROTO_IPV6:
#endif
        *eLevel = esock_atom_ip;
        xres    = NULL;
        break;
#endif

    case IPPROTO_UDP:
        *eLevel = esock_atom_udp;
        xres    = NULL;
        break;

    default:
        *eLevel = MKI(env, level);
        xres    = NULL;
        break;
    }

    return xres;
}



/* +++ decode_cmsghdr_level +++
 *
 * Decode the level part of the cmsghdr().
 *
 */

static
char* decode_cmsghdr_level(ErlNifEnv*   env,
                           ERL_NIF_TERM eLevel,
                           int*         level)
{
    char* xres = NULL;

    if (IS_ATOM(env, eLevel)) {

        if (COMPARE(eLevel, esock_atom_socket) == 0) {
            *level = SOL_SOCKET;
            xres   = NULL;
        } else if (COMPARE(eLevel, esock_atom_ip) == 0) {
#if defined(SOL_IP)
            *level = SOL_IP;
#else
            *level = IPPROTO_IP;
#endif
            xres   = NULL;
#if defined(HAVE_IPV6)
        } else if (COMPARE(eLevel, esock_atom_ipv6) == 0) {
#if defined(SOL_IPV6)
            *level = SOL_IPV6;
#else
            *level = IPPROTO_IPV6;
#endif
            xres   = NULL;
#endif
        } else if (COMPARE(eLevel, esock_atom_udp) == 0) {
            *level = IPPROTO_UDP;
            xres   = NULL;
        } else {
            *level = -1;
            xres   = ESOCK_STR_EINVAL;
        }
    } else if (IS_NUM(env, eLevel)) {
        if (!GET_INT(env, eLevel, level))
            xres = ESOCK_STR_EINVAL;
    } else {
        *level = -1;
        xres   = ESOCK_STR_EINVAL;
    }

    return xres;
}



/* +++ encode_cmsghdr_type +++
 *
 * Encode the type part of the cmsghdr().
 *
 */

static
char* encode_cmsghdr_type(ErlNifEnv*    env,
                          int           level,
                          int           type,
                          ERL_NIF_TERM* eType)
{
    char* xres = NULL;

    switch (level) {
    case SOL_SOCKET:
        switch (type) {
#if defined(SO_TIMESTAMP)
        case SO_TIMESTAMP:
            *eType = esock_atom_timestamp;
            break;
#endif

#if defined(SCM_RIGHTS)
        case SCM_RIGHTS:
            *eType = esock_atom_rights;
            break;
#endif

#if defined(SCM_CREDENTIALS)
        case SCM_CREDENTIALS:
            *eType = esock_atom_credentials;
            break;
#endif

        default:
            xres = ESOCK_STR_EINVAL;
            break;
        }        
        break;

#if defined(SOL_IP)
    case SOL_IP:
#else
    case IPPROTO_IP:
#endif
        switch (type) {
#if defined(IP_TOS)
        case IP_TOS:
            *eType = esock_atom_tos;
            break;
#endif

#if defined(IP_TTL)
        case IP_TTL:
            *eType = esock_atom_ttl;
            break;
#endif

#if defined(IP_PKTINFO)
        case IP_PKTINFO:
            *eType = esock_atom_pktinfo;
            break;
#endif

#if defined(IP_ORIGDSTADDR)
        case IP_ORIGDSTADDR:
            *eType = esock_atom_origdstaddr;
            break;
#endif

        default:
            xres = ESOCK_STR_EINVAL;
            break;
        }
        break;

#if defined(HAVE_IPV6)
#if defined(SOL_IPV6)
        case SOL_IPV6:
#else
        case IPPROTO_IPV6:
#endif
        switch (type) {
#if defined(IPV6_PKTINFO)
        case IPV6_PKTINFO:
            *eType = esock_atom_pktinfo;
            break;
#endif

        default:
            xres = ESOCK_STR_EINVAL;
            break;
        }        
        break;
#endif

    case IPPROTO_TCP:
        switch (type) {
        default:
            xres = ESOCK_STR_EINVAL;
            break;
        }        
        break;

    case IPPROTO_UDP:
        switch (type) {
        default:
            xres = ESOCK_STR_EINVAL;
            break;
        }        
        break;

#if defined(HAVE_SCTP)
    case IPPROTO_SCTP:
        switch (type) {
        default:
            xres = ESOCK_STR_EINVAL;
            break;
        }        
        break;
#endif

    default:
        xres = ESOCK_STR_EINVAL;
        break;
    }

    return xres;
}



/* +++ decode_cmsghdr_type +++
 *
 * Decode the type part of the cmsghdr().
 *
 */

static
char* decode_cmsghdr_type(ErlNifEnv*   env,
                          int          level,
                          ERL_NIF_TERM eType,
                          int*         type)
{
    char* xres = NULL;

    switch (level) {
    case SOL_SOCKET:
        if (IS_NUM(env, eType)) {
            if (!GET_INT(env, eType, type)) {
                *type = -1;
                xres  = ESOCK_STR_EINVAL;
            }
        } else {
            *type = -1;
            xres = ESOCK_STR_EINVAL;
        }
        break;


#if defined(SOL_IP)
    case SOL_IP:
#else
    case IPPROTO_IP:
#endif
        if (IS_ATOM(env, eType)) {
            if (COMPARE(eType, esock_atom_tos) == 0) {
#if defined(IP_TOS)
                *type = IP_TOS;
#else
                xres  = ESOCK_STR_EINVAL;
#endif
            } else if (COMPARE(eType, esock_atom_ttl) == 0) {
#if defined(IP_TTL)
                *type = IP_TTL;
#else
                xres  = ESOCK_STR_EINVAL;
#endif
            } else {
                xres = ESOCK_STR_EINVAL;
            }
        } else if (IS_NUM(env, eType)) {
            if (!GET_INT(env, eType, type)) {
                *type = -1;
                xres  = ESOCK_STR_EINVAL;
            }
        } else {
            *type = -1;
            xres  = ESOCK_STR_EINVAL;
        }
        break;
        
#if defined(HAVE_IPV6)
#if defined(SOL_IPV6)
    case SOL_IPV6:
#else
    case IPPROTO_IPV6:
#endif
        if (IS_NUM(env, eType)) {
            if (!GET_INT(env, eType, type)) {
                *type = -1;
                xres  = ESOCK_STR_EINVAL;
            }
        } else {
            *type = -1;
            xres = ESOCK_STR_EINVAL;
        }
        break;
#endif
        
    case IPPROTO_UDP:
        if (IS_NUM(env, eType)) {
            if (!GET_INT(env, eType, type)) {
                *type = -1;
                xres  = ESOCK_STR_EINVAL;
            }
        } else {
            *type = -1;
            xres = ESOCK_STR_EINVAL;
        }
        break;

    default:
        *type = -1;
        xres  = ESOCK_STR_EINVAL;
        break;
    }

    return xres;
}



/* +++ encode_cmsghdr_data +++
 *
 * Encode the data part of the cmsghdr().
 *
 */

static
char* encode_cmsghdr_data(ErlNifEnv*     env,
                          ERL_NIF_TERM   ctrlBuf,
                          int            level,
                          int            type,
                          unsigned char* dataP,
                          size_t         dataPos,
                          size_t         dataLen,
                          ERL_NIF_TERM*  eCMsgHdrData)
{
    char* xres;

    switch (level) {
#if defined(SOL_SOCKET)
    case SOL_SOCKET:
        xres = encode_cmsghdr_data_socket(env, ctrlBuf, type,
                                          dataP, dataPos, dataLen,
                                          eCMsgHdrData);
        break;
#endif

#if defined(SOL_IP)
    case SOL_IP:
#else
    case IPPROTO_IP:
#endif
        xres = encode_cmsghdr_data_ip(env, ctrlBuf, type,
                                      dataP, dataPos, dataLen,
                                      eCMsgHdrData);
        break;

#if defined(HAVE_IPV6)
#if defined(SOL_IPV6)
    case SOL_IPV6:
#else
    case IPPROTO_IPV6:
#endif        
        xres = encode_cmsghdr_data_ipv6(env, ctrlBuf, type,
                                        dataP, dataPos, dataLen,
                                        eCMsgHdrData);
        break;
#endif

        /*
          case IPPROTO_TCP:
          xres = encode_cmsghdr_data_tcp(env, type, dataP, eCMsgHdrData);
          break;
        */

        /*
          case IPPROTO_UDP:
          xres = encode_cmsghdr_data_udp(env, type, dataP, eCMsgHdrData);
          break;
        */

        /*
          #if defined(HAVE_SCTP)
          case IPPROTO_SCTP:
          xres = encode_cmsghdr_data_sctp(env, type, dataP, eCMsgHdrData);
          break;
          #endif
        */

    default:
        *eCMsgHdrData = MKSBIN(env, ctrlBuf, dataPos, dataLen);
        xres = NULL;
        break;
    }

    return xres;
}



/* +++ encode_cmsghdr_data_socket +++
 *
 * Encode the data part when "protocol" = socket of the cmsghdr().
 *
 */

static
char* encode_cmsghdr_data_socket(ErlNifEnv*     env,
                                 ERL_NIF_TERM   ctrlBuf,
                                 int            type,
                                 unsigned char* dataP,
                                 size_t         dataPos,
                                 size_t         dataLen,
                                 ERL_NIF_TERM*  eCMsgHdrData)
{
    // char* xres;

    switch (type) {
#if defined(SO_TIMESTAMP)
    case SO_TIMESTAMP:
        {
            struct timeval* timeP = (struct timeval*) dataP;

            if (esock_encode_timeval(env, timeP, eCMsgHdrData) != NULL)
                *eCMsgHdrData = MKSBIN(env, ctrlBuf, dataPos, dataLen);
        }
        break;
#endif

    default:
        *eCMsgHdrData = MKSBIN(env, ctrlBuf, dataPos, dataLen);
        break;
    }

    return NULL;
}



/* +++ encode_cmsghdr_data_ip +++
 *
 * Encode the data part when protocol = IP of the cmsghdr().
 *
 */

static
char* encode_cmsghdr_data_ip(ErlNifEnv*     env,
                             ERL_NIF_TERM   ctrlBuf,
                             int            type,
                             unsigned char* dataP,
                             size_t         dataPos,
                             size_t         dataLen,
                             ERL_NIF_TERM*  eCMsgHdrData)
{
    char* xres = NULL;

    switch (type) {
#if defined(IP_TOS)
    case IP_TOS:
        {
            unsigned char tos = *dataP;
            switch (IPTOS_TOS(tos)) {
            case IPTOS_LOWDELAY:
                *eCMsgHdrData = esock_atom_lowdelay;
                break;
            case IPTOS_THROUGHPUT:
                *eCMsgHdrData = esock_atom_throughput;
                break;
            case IPTOS_RELIABILITY:
                *eCMsgHdrData = esock_atom_reliability;
                break;
#if defined(IPTOS_MINCOST)
            case IPTOS_MINCOST:
                *eCMsgHdrData = esock_atom_mincost;
                break;
#endif
            default:
                *eCMsgHdrData = MKUI(env, tos);
                break;
            }
        }
        break;
#endif

#if defined(IP_TTL)
    case IP_TTL:
        {
            int ttl = *((int*) dataP);
            *eCMsgHdrData = MKI(env, ttl);
        }
        break;
#endif

#if defined(IP_PKTINFO)
    case IP_PKTINFO:
        {
            struct in_pktinfo* pktInfoP = (struct in_pktinfo*) dataP;
            ERL_NIF_TERM       ifIndex  = MKUI(env, pktInfoP->ipi_ifindex);
            ERL_NIF_TERM       specDst, addr;

            if ((xres = esock_encode_ip4_address(env,
                                                 &pktInfoP->ipi_spec_dst,
                                                 &specDst)) != NULL) {
                *eCMsgHdrData = esock_atom_undefined;
                return xres;
            }

            if ((xres = esock_encode_ip4_address(env,
                                                 &pktInfoP->ipi_addr,
                                                 &addr)) != NULL) {
                *eCMsgHdrData = esock_atom_undefined;
                return xres;
            }


            {
                ERL_NIF_TERM keys[] = {esock_atom_ifindex,
                                       esock_atom_spec_dst,
                                       esock_atom_addr};
                ERL_NIF_TERM vals[] = {ifIndex, specDst, addr};
                unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);
                unsigned int numVals = sizeof(vals) / sizeof(ERL_NIF_TERM);
    
                ESOCK_ASSERT( (numKeys == numVals) );
                
                if (!MKMA(env, keys, vals, numKeys, eCMsgHdrData)) {
                    *eCMsgHdrData = esock_atom_undefined;
                    return ESOCK_STR_EINVAL;
                }
            }
        }
        break;
#endif

#if defined(IP_ORIGDSTADDR)
    case IP_ORIGDSTADDR:
        if ((xres = esock_encode_sockaddr_in4(env,
                                              (struct sockaddr_in*) dataP,
                                              dataLen,
                                              eCMsgHdrData)) != NULL) {
            *eCMsgHdrData = esock_atom_undefined;
            return xres;            
        }
        break;
#endif

    default:
        *eCMsgHdrData = MKSBIN(env, ctrlBuf, dataPos, dataLen);
        break;
    }

    return xres;
}



/* +++ encode_cmsghdr_data_ipv6 +++
 *
 * Encode the data part when protocol = IPv6 of the cmsghdr().
 *
 */
#if defined(HAVE_IPV6)
static
char* encode_cmsghdr_data_ipv6(ErlNifEnv*     env,
                               ERL_NIF_TERM   ctrlBuf,
                               int            type,
                               unsigned char* dataP,
                               size_t         dataPos,
                               size_t         dataLen,
                               ERL_NIF_TERM*  eCMsgHdrData)
{
    switch (type) {
#if defined(IPV6_PKTINFO)
    case IPV6_PKTINFO:
        {
            struct in6_pktinfo* pktInfoP = (struct in6_pktinfo*) dataP;
            ERL_NIF_TERM        ifIndex  = MKI(env, pktInfoP->ipi6_ifindex);
            ERL_NIF_TERM        addr;
            char*               xres;

            if ((xres = esock_encode_ip6_address(env,
                                                 &pktInfoP->ipi6_addr,
                                                 &addr)) != NULL) {
                *eCMsgHdrData = esock_atom_undefined;
                return xres;
            }

            {
                ERL_NIF_TERM keys[]  = {esock_atom_addr, esock_atom_ifindex};
                ERL_NIF_TERM vals[]  = {addr, ifIndex};
                unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);
                unsigned int numVals = sizeof(vals) / sizeof(ERL_NIF_TERM);
    
                ESOCK_ASSERT( (numKeys == numVals) );
                
                if (!MKMA(env, keys, vals, numKeys, eCMsgHdrData)) {
                    *eCMsgHdrData = esock_atom_undefined;
                    return ESOCK_STR_EINVAL;
                }
            }
        }
        break;
#endif

    default:
        *eCMsgHdrData = MKSBIN(env, ctrlBuf, dataPos, dataLen);
        break;
    }

    return NULL;
}
#endif



/* +++ encode_msghdr_flags +++
 *
 * Encode a list of msghdr_flag().
 *
 * The following flags are handled: eor | trunc | ctrunc | oob | errqueue.
 */

extern
char* encode_msghdr_flags(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          int              msgFlags,
                          ERL_NIF_TERM*    flags)
{
    SSDBG( descP,
           ("SOCKET", "encode_cmsghdrs_flags -> entry with"
            "\r\n   msgFlags: %d (0x%lX)"
            "\r\n", msgFlags, msgFlags) );

    if (msgFlags == 0) {
        *flags = MKEL(env);
        return NULL;
    } else {
        SocketTArray ta = TARRAY_CREATE(10); // Just to be on the safe side

#if defined(MSG_EOR)
        if ((msgFlags & MSG_EOR) == MSG_EOR)
            TARRAY_ADD(ta, esock_atom_eor);
#endif

#if defined(MSG_TRUNC)
        if ((msgFlags & MSG_TRUNC) == MSG_TRUNC)
            TARRAY_ADD(ta, esock_atom_trunc);
#endif
    
#if defined(MSG_CTRUNC)
        if ((msgFlags & MSG_CTRUNC) == MSG_CTRUNC)
            TARRAY_ADD(ta, esock_atom_ctrunc);
#endif
    
#if defined(MSG_OOB)
        if ((msgFlags & MSG_OOB) == MSG_OOB)
            TARRAY_ADD(ta, esock_atom_oob);
#endif
    
#if defined(MSG_ERRQUEUE)
        if ((msgFlags & MSG_ERRQUEUE) == MSG_ERRQUEUE)
            TARRAY_ADD(ta, esock_atom_errqueue);
#endif

        SSDBG( descP,
               ("SOCKET", "esock_encode_cmsghdrs -> flags processed when"
                "\r\n   TArray size: %d"
                "\r\n", TARRAY_SZ(ta)) );

        TARRAY_TOLIST(ta, env, flags);

        return NULL;
    }
}




/* +++ decode the linger value +++
 * The (socket) linger option is provided as a two tuple:
 *
 *       {OnOff :: boolean(), Time :: integer()}
 *
 */
static
BOOLEAN_T decode_sock_linger(ErlNifEnv* env, ERL_NIF_TERM eVal, struct linger* valP)
{
    const ERL_NIF_TERM* lt; // The array of the elements of the tuple
    int                 sz; // The size of the tuple - should be 2
    BOOLEAN_T           onOff;
    int                 secs;

    if (!GET_TUPLE(env, eVal, &sz, &lt))
        return FALSE;

    if (sz != 2)
        return FALSE;


    /* So fas so good - now check the two elements of the tuple. */

    onOff = esock_decode_bool(lt[0]);

    if (!GET_INT(env, lt[1], &secs))
        return FALSE;

    valP->l_onoff  = (onOff) ? 1 : 0;
    valP->l_linger = secs;

    return TRUE;
}



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



/* +++ decode the ip socket option MTU_DISCOVER +++
 * The (ip) option can be provide in two ways:
 *
 *           atom() | integer()
 *
 * When its an atom it can have the values:
 *
 *       want | dont | do | probe
 *
 */
#if defined(IP_MTU_DISCOVER)
static
char* decode_ip_pmtudisc(ErlNifEnv* env, ERL_NIF_TERM eVal, int* val)
{
    char* res = NULL;

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
            *val = -1;
            res  = ESOCK_STR_EINVAL;
        }

    } else if (IS_NUM(env, eVal)) {

        if (!GET_INT(env, eVal, val)) {
            *val = -1;
            res  = ESOCK_STR_EINVAL;
        }

    } else {

        *val   = -1;
        res  = ESOCK_STR_EINVAL;

    }

    return res;
}
#endif



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
#if defined(IPV6_MTU_DISCOVER)
static
char* decode_ipv6_pmtudisc(ErlNifEnv* env, ERL_NIF_TERM eVal, int* val)
{
    char* res = NULL;

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
            *val = -1;
            res  = ESOCK_STR_EINVAL;
        }

    } else if (IS_NUM(env, eVal)) {

        if (!GET_INT(env, eVal, val)) {
            *val = -1;
            res  = ESOCK_STR_EINVAL;
        }

    } else {

        *val   = -1;
        res  = ESOCK_STR_EINVAL;

    }

    return res;
}
#endif



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



/* +++ decocde the native getopt option +++
 * The option is in this case provide in the form of a two tuple:
 *
 *           {NativeOpt, ValueSize}
 *
 * NativeOpt :: integer()
 * ValueSize :: int | bool | non_neg_integer()
 *
 */
static
BOOLEAN_T decode_native_get_opt(ErlNifEnv* env, ERL_NIF_TERM eVal,
                                int* opt, Uint16* valueType, int* valueSz)
{
    const ERL_NIF_TERM* nativeOptT;
    int                 nativeOptTSz;

    /* First, get the tuple and verify its size (2) */

    if (!GET_TUPLE(env, eVal, &nativeOptTSz, &nativeOptT))
        return FALSE;

    if (nativeOptTSz != 2)
        return FALSE;

    /* So far so good.
     * First element is an integer.
     * Second element is an atom or an integer.
     * The only "types" that we support at the moment are:
     *
     *            bool - Which is actually a integer
     *                   (but will be *returned* as a boolean())
     *            int  - Just short for integer
     */

    if (!GET_INT(env, nativeOptT[0], opt))
        return FALSE;

    if (IS_ATOM(env, nativeOptT[1])) {

        if (COMPARE(nativeOptT[1], atom_int) == 0) {
            SGDBG( ("SOCKET", "decode_native_get_opt -> int\r\n") );
            *valueType = SOCKET_OPT_VALUE_TYPE_INT;
            *valueSz   = sizeof(int); // Just to be sure
        } else if (COMPARE(nativeOptT[1], atom_bool) == 0) {
            SGDBG( ("SOCKET", "decode_native_get_opt -> bool\r\n") );
            *valueType = SOCKET_OPT_VALUE_TYPE_BOOL;
            *valueSz   = sizeof(int); // Just to be sure
        } else {
            return FALSE;
        }
    } else if (IS_NUM(env, nativeOptT[1])) {
        if (GET_INT(env, nativeOptT[1], valueSz)) {
            SGDBG( ("SOCKET", "decode_native_get_opt -> unspec\r\n") );
            *valueType = SOCKET_OPT_VALUE_TYPE_UNSPEC;
        } else {
            return FALSE;
        }
    } else {
        return FALSE;
    }

    SGDBG( ("SOCKET", "decode_native_get_opt -> done\r\n") );

    return TRUE;
}



/* +++ encode the ip socket option tos +++
 * The (ip) option can be provide as:
 *
 *       lowdelay |  throughput | reliability | mincost | integer()
 *
 */
static
ERL_NIF_TERM encode_ip_tos(ErlNifEnv* env, int val)
{
    ERL_NIF_TERM result;

    switch (IPTOS_TOS(val)) {
    case IPTOS_LOWDELAY:
        result = esock_make_ok2(env, esock_atom_lowdelay);
        break;

    case IPTOS_THROUGHPUT:
        result = esock_make_ok2(env, esock_atom_throughput);
        break;

    case IPTOS_RELIABILITY:
        result = esock_make_ok2(env, esock_atom_reliability);
        break;

#if defined(IPTOS_MINCOST)
    case IPTOS_MINCOST:
        result = esock_make_ok2(env, esock_atom_mincost);
        break;
#endif

    default:
        result = esock_make_ok2(env, MKI(env, val));
        break;
    }

    return result;
}





/* *** alloc_descriptor ***
 *
 * Allocate and perform basic initialization of a socket descriptor.
 *
 */
static
ESockDescriptor* alloc_descriptor(SOCKET sock, HANDLE event)
{
    ESockDescriptor* descP;

    if ((descP = enif_alloc_resource(sockets, sizeof(ESockDescriptor))) != NULL) {
        char buf[64]; /* Buffer used for building the mutex name */

        descP->pattern        = ESOCK_DESC_PATTERN_CREATED;

        enif_set_pid_undefined(&descP->connPid);
        MON_INIT(&descP->connMon);

        sprintf(buf, "esock[w,%d]", sock);
        descP->writeMtx       = MCREATE(buf);
        enif_set_pid_undefined(&descP->currentWriter.pid);
        MON_INIT(&descP->currentWriter.mon);
        descP->currentWriter.env = NULL;
        descP->currentWriter.ref = esock_atom_undefined;
        descP->currentWriterP = NULL; // currentWriter not used
        descP->writersQ.first = NULL;
        descP->writersQ.last  = NULL;
        descP->isWritable     = FALSE; // TRUE;
        descP->writePkgCnt    = 0;
        descP->writeByteCnt   = 0;
        descP->writeTries     = 0;
        descP->writeWaits     = 0;
        descP->writeFails     = 0;

        sprintf(buf, "esock[r,%d]", sock);
        descP->readMtx        = MCREATE(buf);
        enif_set_pid_undefined(&descP->currentReader.pid);
        MON_INIT(&descP->currentReader.mon);
        descP->currentReader.env = NULL;
        descP->currentReader.ref = esock_atom_undefined;
        descP->currentReaderP = NULL; // currentReader not used
        descP->readersQ.first = NULL;
        descP->readersQ.last  = NULL;
        descP->isReadable     = FALSE; // TRUE;
        descP->readPkgCnt     = 0;
        descP->readByteCnt    = 0;
        descP->readTries      = 0;
        descP->readWaits      = 0;
        descP->readFails      = 0;

        sprintf(buf, "esock[acc,%d]", sock);
        descP->accMtx           = MCREATE(buf);
        enif_set_pid_undefined(&descP->currentAcceptor.pid);
        MON_INIT(&descP->currentAcceptor.mon);
        descP->currentAcceptor.env = NULL;
        descP->currentAcceptor.ref = esock_atom_undefined;
        descP->currentAcceptorP = NULL; // currentAcceptor not used
        descP->acceptorsQ.first = NULL;
        descP->acceptorsQ.last  = NULL;

        sprintf(buf, "esock[close,%d]", sock);
        descP->closeMtx         = MCREATE(buf);
        descP->closeEnv         = NULL;
        descP->closeRef         = esock_atom_undefined;
        enif_set_pid_undefined(&descP->closerPid);
        MON_INIT(&descP->closerMon);

        sprintf(buf, "esock[cfg,%d]", sock);
        descP->cfgMtx           = MCREATE(buf);
        descP->rBufSz           = SOCKET_RECV_BUFFER_SIZE_DEFAULT;
        descP->rNum             = 0;
        descP->rNumCnt          = 0;
        descP->rCtrlSz          = SOCKET_RECV_CTRL_BUFFER_SIZE_DEFAULT;
        descP->wCtrlSz          = SOCKET_SEND_CTRL_BUFFER_SIZE_DEFAULT;
        descP->iow              = FALSE;
        descP->dbg              = SOCKET_DEBUG_DEFAULT;

        descP->sock             = sock;
        descP->event            = event;

        enif_set_pid_undefined(&descP->ctrlPid);
        MON_INIT(&descP->ctrlMon);

    }

    return descP;
}



/* Decrement counters for when a socket is closed
 */
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
#if defined(HAVE_SYS_UN_H)
    else if (domain == AF_UNIX)
        cnt_dec(&data.numDomainInet6, 1);
#endif

    /* *** Type counter *** */
    if (type == SOCK_STREAM)
        cnt_dec(&data.numTypeStreams, 1);
    else if (type == SOCK_DGRAM)
        cnt_dec(&data.numTypeDGrams, 1);
#ifdef HAVE_SCTP
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


/* Increment counters for when a socket is opened
 */
static
void inc_socket(int domain, int type, int protocol)
{
    MLOCK(data.cntMtx);

    cnt_inc(&data.numSockets, 1);
    
    /* *** Domain counter *** */
    if (domain == AF_INET)
        cnt_inc(&data.numDomainInet, 1);
#if defined(HAVE_IN6) && defined(AF_INET6)
    else if (domain == AF_INET6)
        cnt_inc(&data.numDomainInet6, 1);
#endif
#if defined(HAVE_SYS_UN_H)
    else if (domain == AF_UNIX)
        cnt_inc(&data.numDomainInet6, 1);
#endif

    /* *** Type counter *** */
    if (type == SOCK_STREAM)
        cnt_inc(&data.numTypeStreams, 1);
    else if (type == SOCK_DGRAM)
        cnt_inc(&data.numTypeDGrams, 1);
#ifdef HAVE_SCTP
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

    MUNLOCK(data.cntMtx);
}


#endif // if !defined(__WIN32__)



/* ----------------------------------------------------------------------
 *  D e c o d e / E n c o d e   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

/* edomain2domain - convert internal (erlang) domain to (proper) domain
 *
 * Note that only a subset is supported.
 */
#if !defined(__WIN32__)
static
BOOLEAN_T edomain2domain(int edomain, int* domain)
{
    switch (edomain) {
    case SOCKET_DOMAIN_INET:
        *domain = AF_INET;
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case SOCKET_DOMAIN_INET6:
        *domain = AF_INET6;
        break;
#endif
#ifdef HAVE_SYS_UN_H
    case SOCKET_DOMAIN_LOCAL:
        *domain = AF_UNIX;
        break;
#endif

    default:
        *domain = -1;
        return FALSE;
    }

    return TRUE;
}


/* etype2type - convert internal (erlang) type to (proper) type
 *
 * Note that only a subset is supported.
 */
static
BOOLEAN_T etype2type(int etype, int* type)
{
    switch (etype) {
    case SOCKET_TYPE_STREAM:
        *type = SOCK_STREAM;
        break;

    case SOCKET_TYPE_DGRAM:
        *type = SOCK_DGRAM;
        break;

    case SOCKET_TYPE_RAW:
        *type = SOCK_RAW;
        break;

#ifdef HAVE_SCTP    
    case SOCKET_TYPE_SEQPACKET:
        *type = SOCK_SEQPACKET;
        break;
#endif

    default:
        *type = -1;
        return FALSE;
    }

    return TRUE;
}


/* eproto2proto - convert internal (erlang) protocol to (proper) protocol
 *
 * Note that only a subset is supported.
 */
static
BOOLEAN_T eproto2proto(ErlNifEnv*   env,
                       ERL_NIF_TERM eproto,
                       int*         proto)
{
    if (IS_NUM(env, eproto)) {
        int ep;

        if (!GET_INT(env, eproto, &ep)) {
            *proto = -1;
            return FALSE;
        }

        switch (ep) {
        case SOCKET_PROTOCOL_DEFAULT:
            *proto = 0; // default - note that _IP also has the value 0...
            break;
            
        case SOCKET_PROTOCOL_IP:
            *proto = IPPROTO_IP;
            break;
            
        case SOCKET_PROTOCOL_TCP:
            *proto = IPPROTO_TCP;
            break;
            
        case SOCKET_PROTOCOL_UDP:
            *proto = IPPROTO_UDP;
            break;
            
#if defined(HAVE_SCTP)
        case SOCKET_PROTOCOL_SCTP:
            *proto = IPPROTO_SCTP;
            break;
#endif
            
        case SOCKET_PROTOCOL_ICMP:
            *proto = IPPROTO_ICMP;
            break;
            
        case SOCKET_PROTOCOL_IGMP:
            *proto = IPPROTO_IGMP;
            break;
            
        default:
            *proto = -2;
            return FALSE;
        }
    } else {
        const ERL_NIF_TERM* a;
        int                 sz;

        if (!GET_TUPLE(env, eproto, &sz, &a)) {
            *proto = -3;
            return FALSE;
        }
        
        if (sz != 2) {
            *proto = -4;
            return FALSE;
        }

        if (COMPARE(a[0], esock_atom_raw) != 0) {
            *proto = -5;
            return FALSE;
        }

        if (!GET_INT(env, a[1], proto)) {
            *proto = -6;
            return FALSE;
        }
    }

    return TRUE;
}


#ifdef HAVE_SETNS
 /* emap2netns - extract the netns field from the extra map
 *
 * Note that currently we only support one extra option, the netns.
 */
static
BOOLEAN_T emap2netns(ErlNifEnv* env, ERL_NIF_TERM map, char** netns)
{
    size_t       sz;
    ERL_NIF_TERM key;
    ERL_NIF_TERM value;
    unsigned int len;
    char*        buf;
    int          written;

    /* Note that its acceptable that the extra map is empty */
    if (!enif_get_map_size(env, map, &sz) ||
        (sz != 1)) {
        *netns = NULL;
        return TRUE;
    }

    /* The currently only supported extra option is:  netns */
    key = enif_make_atom(env, "netns");
    if (!GET_MAP_VAL(env, map, key, &value)) {
        *netns = NULL; // Just in case...
        return FALSE;
    }

    /* So far so good. The value should be a string, check. */
    if (!enif_is_list(env, value)) {
        *netns = NULL; // Just in case...
        return FALSE;
    }

    if (!enif_get_list_length(env, value, &len)) {
        *netns = NULL; // Just in case...
        return FALSE;
    }

    if ((buf = MALLOC(len+1)) == NULL) {
        *netns = NULL; // Just in case...
        return FALSE;
    }

    written = enif_get_string(env, value, buf, len+1, ERL_NIF_LATIN1);
    if (written == (len+1)) {
        *netns = buf;
        return TRUE;
    } else {
        *netns = NULL; // Just in case...
        return FALSE;
    }
}
#endif


/* esendflags2sendflags - convert internal (erlang) send flags to (proper)
 * send flags.
 */
static
BOOLEAN_T esendflags2sendflags(unsigned int eflags, int* flags)
{
    unsigned int ef;
    int          tmp = 0;

    /* First, check if we have any flags at all */
    if (eflags == 0) {
        *flags = 0;
        return TRUE;
    }
        
    for (ef = SOCKET_SEND_FLAG_LOW; ef <= SOCKET_SEND_FLAG_HIGH; ef++) {

        switch (ef) {
#if defined(MSG_CONFIRM)
        case SOCKET_SEND_FLAG_CONFIRM:
            if ((1 << SOCKET_SEND_FLAG_CONFIRM) & eflags)
                tmp |= MSG_CONFIRM;
            break;
#endif

#if defined(MSG_DONTROUTE)
        case SOCKET_SEND_FLAG_DONTROUTE:
            if ((1 << SOCKET_SEND_FLAG_DONTROUTE) & eflags)
                tmp |= MSG_DONTROUTE;
            break;
#endif

#if defined(MSG_EOR)
        case SOCKET_SEND_FLAG_EOR:
            if ((1 << SOCKET_SEND_FLAG_EOR) & eflags)
                tmp |= MSG_EOR;
            break;
#endif

#if defined(MSG_MORE)
        case SOCKET_SEND_FLAG_MORE:
            if ((1 << SOCKET_SEND_FLAG_MORE) & eflags)
                tmp |= MSG_MORE;
            break;
#endif

#if defined(MSG_NOSIGNAL)
        case SOCKET_SEND_FLAG_NOSIGNAL:
            if ((1 << SOCKET_SEND_FLAG_NOSIGNAL) & eflags)
                tmp |= MSG_NOSIGNAL;
            break;
#endif

#if defined(MSG_OOB)
        case SOCKET_SEND_FLAG_OOB:
            if ((1 << SOCKET_SEND_FLAG_OOB) & eflags)
                tmp |= MSG_OOB;
            break;
#endif

        default:
            return FALSE;
        }

    }

    *flags = tmp;

    return TRUE;
}



/* erecvflags2recvflags - convert internal (erlang) send flags to (proper)
 * send flags.
 */
static
BOOLEAN_T erecvflags2recvflags(unsigned int eflags, int* flags)
{
    unsigned int ef;
    int          tmp = 0;

    SGDBG( ("SOCKET", "erecvflags2recvflags -> entry with"
            "\r\n   eflags: %d"
            "\r\n", eflags) );

    if (eflags == 0) {
        *flags = 0;
        return TRUE;
    }

    for (ef = SOCKET_RECV_FLAG_LOW; ef <= SOCKET_RECV_FLAG_HIGH; ef++) {

        SGDBG( ("SOCKET", "erecvflags2recvflags -> iteration"
                "\r\n   ef:  %d"
                "\r\n   tmp: %d"
                "\r\n", ef, tmp) );

        switch (ef) {
#if defined(MSG_CMSG_CLOEXEC)
        case SOCKET_RECV_FLAG_CMSG_CLOEXEC:
            if ((1 << SOCKET_RECV_FLAG_CMSG_CLOEXEC) & eflags)
                tmp |= MSG_CMSG_CLOEXEC;
            break;
#endif

#if defined(MSG_ERRQUEUE)
        case SOCKET_RECV_FLAG_ERRQUEUE:
            if ((1 << SOCKET_RECV_FLAG_ERRQUEUE) & eflags)
                tmp |= MSG_ERRQUEUE;
            break;
#endif

#if defined(MSG_OOB)
        case SOCKET_RECV_FLAG_OOB:
            if ((1 << SOCKET_RECV_FLAG_OOB) & eflags)
                tmp |= MSG_OOB;
            break;
#endif

            /*
             * <KOLLA>
             *
             * We need to handle this, because it may effect the read algorithm
             *
             * </KOLLA>
             */
#if defined(MSG_PEEK)
        case SOCKET_RECV_FLAG_PEEK:
            if ((1 << SOCKET_RECV_FLAG_PEEK) & eflags)
                tmp |= MSG_PEEK;
            break;
#endif

#if defined(MSG_TRUNC)
        case SOCKET_RECV_FLAG_TRUNC:
            if ((1 << SOCKET_RECV_FLAG_TRUNC) & eflags)
                tmp |= MSG_TRUNC;
            break;
#endif

        default:
            return FALSE;
        }

    }

    *flags = tmp;

    return TRUE;
}



/* eproto2proto - convert internal (erlang) protocol to (proper) protocol
 *
 * Note that only a subset is supported.
 */
static
BOOLEAN_T ehow2how(unsigned int ehow, int* how)
{
     switch (ehow) {
     case SOCKET_SHUTDOWN_HOW_RD:
         *how = SHUT_RD;
         break;

     case SOCKET_SHUTDOWN_HOW_WR:
         *how = SHUT_WR;
         break;

     case SOCKET_SHUTDOWN_HOW_RDWR:
         *how = SHUT_RDWR;
         break;

     default:
         return FALSE;
     }

     return TRUE;
}



/* ecommand2command - convert erlang command to "native" command (and data)
 */
static
BOOLEAN_T ecommand2command(ErlNifEnv*    env,
                           ERL_NIF_TERM  ecommand,
                           Uint16*       command,
                           ERL_NIF_TERM* edata)
{
    size_t       sz;
    ERL_NIF_TERM ecmd;

    if (!IS_MAP(env, ecommand)) {
        SGDBG( ("SOCKET", "ecommand2command -> (e)command not a map\r\n") );
        return FALSE;
    }

    /* The map shall have exactly two attrbutes: 
     *          'command' and 'data'
     */
    if (!enif_get_map_size(env, ecommand, &sz) || (sz != 2)) {
        SGDBG( ("SOCKET", "ecommand2command -> comamnd map size invalid\r\n") );
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
        *command = SOCKET_CMD_DEBUG;
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




#if defined(HAVE_SYS_UN_H) || defined(SO_BINDTODEVICE)
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


/* Send an counter wrap message to the controlling process:
 * A message in the form:
 *
 *     {'$socket', Socket, counter_wrap, Counter :: atom()}
 *
 * This message will only be sent if the iow (Inform On Wrap) is TRUE.
 */
static
char* esock_send_wrap_msg(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     cnt)
{
    ERL_NIF_TERM msg = mk_wrap_msg(env, sockRef, cnt);
    
    return esock_send_msg(env, &descP->ctrlPid, msg, NULL);
}


/* Send an close message to the specified process:
 * A message in the form:
 *
 *     {'$socket', Socket, close, CloseRef}
 *
 * This message is for processes that is waiting in the
 * erlang API (close-) function for the socket to be "closed"
 * (actually that the 'stop' callback function has been called).
 */
static
char* esock_send_close_msg(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ErlNifPid*       pid)
{
    ERL_NIF_TERM sockRef, msg;
    ErlNifEnv*   menv;
    
    if (descP->closeEnv != NULL) {
        sockRef = enif_make_resource(descP->closeEnv, descP);
        msg     = mk_close_msg(descP->closeEnv, sockRef, descP->closeRef);
        menv    = descP->closeEnv;
    } else {
        sockRef = enif_make_resource(env, descP);
        msg     = mk_close_msg(env, sockRef, descP->closeRef);
        menv    = NULL; // This has the effect that the message will be copied
    }
    
    return esock_send_msg(env, pid, msg, menv);
}


/* Send an abort message to the specified process:
 * A message in the form:
 *
 *     {'$socket', Socket, abort, {RecvRef, Reason}}
 *
 * This message is for processes that is waiting in the
 * erlang API functions for a select message.
 */
static
char* esock_send_abort_msg(ErlNifEnv*   env,
                           ERL_NIF_TERM sockRef,
                           ERL_NIF_TERM opRef,
                           ErlNifEnv*   msgEnv,
                           ERL_NIF_TERM reason,
                           ErlNifPid*   pid)
{
    ERL_NIF_TERM msg = mk_abort_msg(msgEnv,
                                    /* sockRef not in msgEnv so copy */
                                    CP_TERM(msgEnv, sockRef),
                                    opRef, reason);

    return esock_send_msg(env, pid, msg, msgEnv);
}


/* Send a message to the specified process.
 */
static
char* esock_send_msg(ErlNifEnv*   env,
                     ErlNifPid*   pid,
                     ERL_NIF_TERM msg,
                     ErlNifEnv*   msgEnv)
{
    int res = enif_send(env, pid, msgEnv, msg);
    if (msgEnv)
        esock_free_env("esock_msg_send - msg-env", msgEnv);

    if (!res)
        return str_exsend;
    else
        return NULL;
}



/* *** mk_abort_msg ***
 *
 * Create the abort message, which has the following form:
 *
 *     {'$socket', Socket, abort, {OpRef, Reason}}
 *
 * This message is for processes that are waiting in the
 * erlang API functions for a select (or this) message.
 */
static
ERL_NIF_TERM mk_abort_msg(ErlNifEnv*   env,
                          ERL_NIF_TERM sockRef,
                          ERL_NIF_TERM opRef,
                          ERL_NIF_TERM reason)
{
    ERL_NIF_TERM info = MKT2(env, opRef, reason);
    
    return mk_socket_msg(env, sockRef, esock_atom_abort, info);
}


/* *** mk_wrap_msg ***
 *
 * Construct a counter wrap (socket) message. It has the form: 
 *
 *         {'$socket', Socket, counter_wrap, Counter}
 *
 */
static
ERL_NIF_TERM mk_wrap_msg(ErlNifEnv*   env,
                         ERL_NIF_TERM sockRef,
                         ERL_NIF_TERM cnt)
{
    return mk_socket_msg(env, sockRef, atom_counter_wrap, cnt);
}


/* *** mk_close_msg ***
 *
 * Construct a close (socket) message. It has the form: 
 *
 *         {'$socket', Socket, close, closeRef}
 *
 */
static
ERL_NIF_TERM mk_close_msg(ErlNifEnv*   env,
                          ERL_NIF_TERM sockRef,
                          ERL_NIF_TERM closeRef)
{
    return mk_socket_msg(env, sockRef, esock_atom_close, closeRef);
}


/* *** mk_select_msg ***
 *
 * Construct a select (socket) message. It has the form: 
 *
 *         {'$socket', Socket, select, selectRef}
 *
 */
static
ERL_NIF_TERM mk_select_msg(ErlNifEnv*   env,
                           ERL_NIF_TERM sockRef,
                           ERL_NIF_TERM selectRef)
{
    return mk_socket_msg(env, sockRef, atom_select, selectRef);
}


/* *** mk_socket_msg ***
 *
 * Construct the socket message:
 *
 *         {'$socket', Socket, Tag, Info}
 *
 * Socket :: socket() (#socket{})
 * Tag    :: atom()
 * Info   :: term()
 *
 */
static
ERL_NIF_TERM mk_socket_msg(ErlNifEnv*   env,
                           ERL_NIF_TERM sockRef,
                           ERL_NIF_TERM tag,
                           ERL_NIF_TERM info)
{
    ERL_NIF_TERM socket = mk_socket(env, sockRef);

    return MKT4(env, esock_atom_socket_tag, socket, tag, info);
}


/* *** mk_socket ***
 *
 * Simple utility function that construct the socket resord:
 *
 *      #socket{ref = SockRef} => {socket, SockRef :: reference()}
 */
static
ERL_NIF_TERM mk_socket(ErlNifEnv*   env,
                       ERL_NIF_TERM sockRef)
{
    return MKT2(env, esock_atom_socket, sockRef);
}

#endif // #if defined(__WIN32__)

                              
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
static
int esock_select_read(ErlNifEnv*       env,
                      ErlNifEvent      event,     // The file descriptor
                      void*            obj,       // The socket descriptor object
                      const ErlNifPid* pid,       // Destination
                      ERL_NIF_TERM     sockRef,   // Socket
                      ERL_NIF_TERM     selectRef) // "ID" of the operation
{
    ERL_NIF_TERM selectMsg = mk_select_msg(env, sockRef, selectRef);

    return enif_select_read(env, event, obj, pid, selectMsg, NULL);

}


/* *** esock_select_write ***
 *
 * Perform a write select. When the select is triggered, a 'select'
 * message (see mk_select_msg) will be sent.
 * The sockRef is copied to the msgEnv when the socket message is created,
 * so no need to do that here, but the selectRef needs to be copied.
 */
static
int esock_select_write(ErlNifEnv*       env,
                       ErlNifEvent      event,     // The file descriptor
                       void*            obj,       // The socket descriptor
                       const ErlNifPid* pid,       // Destination
                       ERL_NIF_TERM     sockRef,   // Socket
                       ERL_NIF_TERM     selectRef) // "ID" of the operation
{
    ERL_NIF_TERM selectMsg = mk_select_msg(env, sockRef, selectRef);

    return enif_select_write(env, event, obj, pid, selectMsg, NULL);
}


static
int esock_select_stop(ErlNifEnv*  env,
                      ErlNifEvent event,
                      void*       obj)
{
    return enif_select(env, event, (ERL_NIF_SELECT_STOP), obj, NULL,
                       esock_atom_undefined);
}

static
int esock_select_cancel(ErlNifEnv*             env,
                        ErlNifEvent            event,
                        enum ErlNifSelectFlags mode,
                        void*                  obj)
{
    return enif_select(env, event, (ERL_NIF_SELECT_CANCEL | mode), obj, NULL,
                       esock_atom_undefined);
}


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

#if !defined(__WIN32__)

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
                SSDBG( descP,                                \
                       ("SOCKET",                                       \
                        "activate_next_" #F " -> new (active) requestor: " \
                        "\r\n   pid: %T"                                \
                        "\r\n   ref: %T"                                \
                        "\r\n", reqP->pid, reqP->ref) );                \
                                                                        \
                /* We need to copy req ref to 'env' */                  \
                if ((sres = esock_select_##S(env, descP->sock, descP,   \
                                             &reqP->pid, sockRef,       \
                                             CP_TERM(env, reqP->ref))) < 0) { \
                                                                        \
                    /* We need to inform this process, reqP->pid,  */   \
                    /* that we failed to select, so we don't leave */   \
                    /* it hanging.                                 */   \
                    /* => send abort                               */   \
                                                                        \
                    reason = MKT2(env,                                  \
                                  esock_atom_select_failed,             \
                                  MKI(env, sres));                      \
                    esock_send_abort_msg(env, sockRef,                  \
                                         reqP->ref, reqP->env,          \
                                         reason, &reqP->pid);           \
                    reqP->env = NULL;                                  \
                                                                        \
                } else {                                                \
                                                                        \
                    /* Success: New requestor selected */               \
                    popped    = TRUE;                                   \
                    activated = FALSE;                                  \
                                                                        \
                }                                                       \
                                                                        \
            } else {                                                    \
                                                                        \
                SSDBG( descP,                                           \
                       ("SOCKET",                                       \
                        "activate_next_" #F " -> no more requestors\r\n") ); \
                                                                        \
                popped    = TRUE;                                       \
                activated = FALSE;                                      \
            }                                                           \
                                                                        \
        } while (!popped);                                              \
                                                                        \
        SSDBG( descP,                                                   \
               ("SOCKET", "activate_next_" #F " -> "                    \
                "done with %s\r\n", B2S(activated)) );                  \
                                                                        \
        return activated;                                               \
    }
ACTIVATE_NEXT_FUNCS
#undef ACTIVATE_NEXT_FUNC_DECL


#endif // if !defined(__WIN32__)


/* ----------------------------------------------------------------------
 *  R e q u e s t o r   Q u e u e   F u n c t i o n s
 * ----------------------------------------------------------------------
 *
 * Since each of these functions (search4pid, push, pop and unqueue
 * are virtually identical for acceptors, writers and readers, 
 * we make use of set of declaration macros.
 */

#if !defined(__WIN32__)

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



/* *** acceptor_push ***
 * *** writer_push   ***
 * *** reader_push   ***
 *
 * Push a requestor (acceptor, writer, or reader) onto its queue.
 * This happens when we already have a current request (of its type).
 *
 */

#define REQ_PUSH_FUNCS                       \
    REQ_PUSH_FUNC_DECL(acceptor, acceptorsQ) \
    REQ_PUSH_FUNC_DECL(writer,   writersQ)   \
    REQ_PUSH_FUNC_DECL(reader,   readersQ)

#define REQ_PUSH_FUNC_DECL(F, Q)                                       \
    static                                                             \
    ERL_NIF_TERM F##_push(ErlNifEnv*       env,                        \
                          ESockDescriptor* descP,                      \
                          ErlNifPid        pid,                        \
                          ERL_NIF_TERM     ref)                        \
    {                                                                  \
        ESockRequestQueueElement* e    = MALLOC(sizeof(ESockRequestQueueElement)); \
        ESockRequestor*           reqP = &e->data;                     \
                                                                       \
        reqP->pid = pid;                                               \
        if (MONP("reader_push -> " #F " request",                      \
                 env, descP, &pid, &reqP->mon) != 0) {                 \
            FREE(e);                                                   \
            return esock_make_error(env, atom_exmon);                  \
        }                                                              \
        reqP->env = esock_alloc_env(#F "_push");                                  \
        reqP->ref = enif_make_copy(reqP->env, ref);                    \
                                                                       \
        qpush(&descP->Q, e);                                           \
                                                                       \
        return esock_make_error(env, esock_atom_eagain);               \
    }
REQ_PUSH_FUNCS
#undef REQ_PUSH_FUNC_DECL



/* *** acceptor_pop ***
 * *** writer_pop   ***
 * *** reader_pop   ***
 *
 * Pop a requestor (acceptor, writer, or reader) from its queue.
 *
 */
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



/* *** acceptor_unqueue ***
 * *** writer_unqueue   ***
 * *** reader_unqueue   ***
 *
 * Remove a requestor (acceptor, writer, or reader) from its queue.
 *
 */

#define REQ_UNQUEUE_FUNCS                       \
    REQ_UNQUEUE_FUNC_DECL(acceptor, acceptorsQ) \
    REQ_UNQUEUE_FUNC_DECL(writer,   writersQ)   \
    REQ_UNQUEUE_FUNC_DECL(reader,   readersQ)

#define REQ_UNQUEUE_FUNC_DECL(F, Q)                            \
    static                                                     \
    BOOLEAN_T F##_unqueue(ErlNifEnv*       env,                \
                          ESockDescriptor* descP,              \
                          const ErlNifPid* pid)                \
    {                                                          \
        return qunqueue(env, descP, "qunqueue -> waiting " #F, \
                        &descP->Q, pid);                       \
    }
REQ_UNQUEUE_FUNCS
#undef REQ_UNQUEUE_FUNC_DECL



/* *** requestor pop ***
 *
 * Pop an requestor from its queue.
 */
static
BOOLEAN_T requestor_pop(ESockRequestQueue* q,
                        ESockRequestor*    reqP)
{
    ESockRequestQueueElement* e = qpop(q);

    if (reqP->env)
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
        enif_set_pid_undefined(&reqP->pid);
        MON_INIT(&reqP->mon);
        reqP->env = NULL;
        reqP->ref = esock_atom_undefined; // Just in case
        return FALSE;
    }
    
}


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



static
BOOLEAN_T qunqueue(ErlNifEnv*         env,
                   ESockDescriptor*   descP,
                   const char*        slogan,
                   ESockRequestQueue* q,
                   const ErlNifPid*   pid)
{
    ESockRequestQueueElement* e = q->first;
    ESockRequestQueueElement* p = NULL;

    /* Check if it was one of the waiting acceptor processes */
    while (e != NULL) {
        if (COMPARE_PIDS(&e->data.pid, pid) == 0) {

            /* We have a match */

            DEMONP(slogan, env, descP, &e->data.mon);
            
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

            if (e->data.env)
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
#endif // if !defined(__WIN32__)



/* ----------------------------------------------------------------------
 *  C o u n t e r   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

#if !defined(__WIN32__)
static
BOOLEAN_T cnt_inc(Uint32* cnt, Uint32 inc)
{
    BOOLEAN_T wrap;
    Uint32    max     = 0xFFFFFFFF;
    Uint32    current = *cnt;

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
void cnt_dec(Uint32* cnt, Uint32 dec)
{
    Uint32 current = *cnt;

    if (dec > current)
        *cnt = 0; // The counter cannot be < 0 so this is the best we can do...
    else
        *cnt -= dec;

    return;
}
#endif // if !defined(__WIN32__)




/* ----------------------------------------------------------------------
 *  M o n i t o r   W r a p p e r   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

#if !defined(__WIN32__)

static
int esock_monitor(const char*      slogan,
                  ErlNifEnv*       env,
                  ESockDescriptor* descP,
                  const ErlNifPid* pid,
                  ESockMonitor*    monP)
{
    int res;

    SSDBG( descP, ("SOCKET", "[%d] %s: try monitor\r\n", descP->sock, slogan) );
    res = enif_monitor_process(env, descP, pid, &monP->mon);

    if (res != 0) {
        monP->isActive = FALSE;
        SSDBG( descP, ("SOCKET", "[%d] monitor failed: %d\r\n", descP->sock, res) );
    } else {
        monP->isActive = TRUE;
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

    if (!monP->isActive)
        return 1;

    SSDBG( descP, ("SOCKET", "[%d] %s: try demonitor\r\n", descP->sock, slogan) );

    res = enif_demonitor_process(env, descP, &monP->mon);

    if (res == 0) {
        esock_monitor_init(monP);
    } else {
        SSDBG( descP,
               ("SOCKET", "[%d] demonitor failed: %d\r\n", descP->sock, res) );
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



#endif // if !defined(__WIN32__)



/* ----------------------------------------------------------------------
 *  C a l l b a c k   F u n c t i o n s
 * ----------------------------------------------------------------------
 */


static void free_request_queue(ESockRequestQueue* q)
{
    while (q->first) {
        ESockRequestQueueElement* free_me = q->first;
        q->first = free_me->nextP;
        if (free_me->data.env)
            esock_free_env("dtor", free_me->data.env);
        FREE(free_me);
    }
}

/* =========================================================================
 * socket_dtor - Callback function for resource destructor
 *
 */
static
void socket_dtor(ErlNifEnv* env, void* obj)
{
#if !defined(__WIN32__)    
  ESockDescriptor* descP = (ESockDescriptor*) obj;

  MDESTROY(descP->writeMtx); descP->writeMtx = NULL;
  MDESTROY(descP->readMtx);  descP->readMtx  = NULL;
  MDESTROY(descP->accMtx);   descP->accMtx   = NULL;
  MDESTROY(descP->closeMtx); descP->closeMtx = NULL;
  MDESTROY(descP->cfgMtx);   descP->cfgMtx   = NULL;

  if (descP->currentReader.env) {
      esock_free_env("dtor reader", descP->currentReader.env);
      descP->currentReader.env = NULL;
  }
  if (descP->currentWriter.env) {
      esock_free_env("dtor writer", descP->currentWriter.env);
      descP->currentWriter.env = NULL;
  }
  if (descP->currentAcceptor.env) {
      esock_free_env("dtor acceptor", descP->currentAcceptor.env);
      descP->currentAcceptor.env = NULL;
  }
  free_request_queue(&descP->readersQ);
  free_request_queue(&descP->writersQ);
  free_request_queue(&descP->acceptorsQ);

  descP->state   = SOCKET_STATE_DTOR;
  descP->pattern = ESOCK_DESC_PATTERN_DTOR;  
#endif
}


/* =========================================================================
 * socket_stop - Callback function for resource stop
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
void socket_stop(ErlNifEnv* env, void* obj, int fd, int is_direct_call)
{
#if !defined(__WIN32__)
    ESockDescriptor* descP = (ESockDescriptor*) obj;
    ERL_NIF_TERM     sockRef;

    SSDBG( descP,
           ("SOCKET", "socket_stop -> entry when %s"
            "\r\n   sock: %d (%d)"
            "\r\n",
            ((is_direct_call) ? "called" : "scheduled"), descP->sock, fd) );

    /* +++ Lock it down +++ */
    
    MLOCK(descP->writeMtx);
    MLOCK(descP->readMtx);
    MLOCK(descP->accMtx);
    MLOCK(descP->cfgMtx);
    if (!is_direct_call) MLOCK(descP->closeMtx);
    
    SSDBG( descP, ("SOCKET", "socket_stop -> "
                   "[%d, %T] all mutex(s) locked when counters:"
                   "\r\n   writePkgCnt:  %u"
                   "\r\n   writeByteCnt: %u"
                   "\r\n   writeTries:   %u"
                   "\r\n   writeWaits:   %u"
                   "\r\n   writeFails:   %u"
                   "\r\n   readPkgCnt:   %u"
                   "\r\n   readByteCnt:  %u"
                   "\r\n   readTries:    %u"
                   "\r\n   readWaits:    %u"
                   "\r\n",
                   descP->sock, descP->ctrlPid,
                   descP->writePkgCnt,
                   descP->writeByteCnt,
                   descP->writeTries,
                   descP->writeWaits,
                   descP->writeFails,
                   descP->readPkgCnt,
                   descP->readByteCnt,
                   descP->readTries,
                   descP->readWaits) );

    sockRef           = enif_make_resource(env, descP);
    descP->state      = SOCKET_STATE_CLOSING; // Just in case...???
    descP->isReadable = FALSE;
    descP->isWritable = FALSE;

    /* We should check that we actually have a monitor.
     * This *should* be done with a "NULL" monitor value,
     * which there currently is none...
     * If we got here because the controlling process died,
     * there is no point to demonitor. Also, we do not actually
     * have a monitor in that case...
     */
    DEMONP("socket_stop -> ctrl", env, descP, &descP->ctrlMon);



    /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     *
     *           Check current and waiting Writers
     *
     * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     */

    if (descP->currentWriterP != NULL) {

        /* We have a (current) writer and *may* therefor also have
         * writers waiting.
         */

        socket_stop_handle_current(env,
                                   "writer",
                                   descP, sockRef, &descP->currentWriter);

        /* And also deal with the waiting writers (in the same way) */
        SSDBG( descP, ("SOCKET", "socket_stop -> handle waiting writer(s)\r\n") );
        inform_waiting_procs(env, "writer",
                             descP, sockRef, &descP->writersQ, TRUE, atom_closed);
    }


    /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     *
     *           Check current and waiting Readers
     *
     * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     */

    if (descP->currentReaderP != NULL) {
        
        /* We have a (current) reader and *may* therefor also have
         * readers waiting.
         */
        
        socket_stop_handle_current(env,
                                   "reader",
                                   descP, sockRef, &descP->currentReader);

        /* And also deal with the waiting readers (in the same way) */
        SSDBG( descP, ("SOCKET", "socket_stop -> handle waiting reader(s)\r\n") );
        inform_waiting_procs(env, "reader",
                             descP, sockRef, &descP->readersQ, TRUE, atom_closed);
    }



    /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     *
     *           Check current and waiting Acceptors
     *
     * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     */

    if (descP->currentAcceptorP != NULL) {

        /* We have a (current) acceptor and *may* therefor also have
         * acceptors waiting.
         */

        socket_stop_handle_current(env,
                                   "acceptor",
                                   descP, sockRef, &descP->currentAcceptor);

        /* And also deal with the waiting acceptors (in the same way) */
        SSDBG( descP, ("SOCKET", "socket_stop -> handle waiting acceptor(s)\r\n") );
        inform_waiting_procs(env, "acceptor",
                             descP, sockRef, &descP->acceptorsQ, TRUE, atom_closed);
    }
    
    

    /* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     *
     *           Maybe inform waiting closer
     *
     * +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     */

    if (descP->sock != INVALID_SOCKET) {

        if (descP->closeLocal) {

            if (!is_direct_call) {
                
                /* +++ send close message to the waiting process +++ */

                esock_send_close_msg(env, descP, &descP->closerPid);

                DEMONP("socket_stop -> closer", env, descP, &descP->closerMon);

            } else {

                /* We only need to explicitly free the environment here
                 * since the message send takes care of it if scheduled.
                 */

                if (descP->closeEnv != NULL)
                    esock_free_env("socket_stop - close-env", descP->closeEnv);

            }
        }
    }
    

    SSDBG( descP, ("SOCKET", "socket_stop -> unlock all mutex(s)\r\n") );

    if (!is_direct_call) MUNLOCK(descP->closeMtx);
    MUNLOCK(descP->cfgMtx);
    MUNLOCK(descP->accMtx);
    MUNLOCK(descP->readMtx);
    MUNLOCK(descP->writeMtx);

    SSDBG( descP,
           ("SOCKET", "socket_stop -> done (%d, %d)\r\n", descP->sock, fd) );

#endif // if !defined(__WIN32__)
}



/* *** socket_stop_handle_current ***
 *
 * Handle current requestor (reader, writer or acceptor) during
 * socket stop.
 */
#if !defined(__WIN32__)
static
void socket_stop_handle_current(ErlNifEnv*       env,
                                const char*      role,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     sockRef,
                                ESockRequestor*  reqP)
{
    SSDBG( descP, ("SOCKET", "socket_stop -> handle current %s\r\n", role) );

    DEMONP("socket_stop_handle_current", env, descP, &reqP->mon);

    if (COMPARE_PIDS(&descP->closerPid, &reqP->pid) != 0) {

        SSDBG( descP, ("SOCKET", "socket_stop_handle_current -> "
                       "send abort message to current %s %T\r\n",
                       role, reqP->pid) );

        if (esock_send_abort_msg(env, sockRef, reqP->ref, reqP->env,
                                 atom_closed, &reqP->pid) != NULL) {

            esock_warning_msg("Failed sending abort (%T) message to "
                              "current %s %T\r\n",
                              reqP->ref, role, reqP->pid);
        }
        reqP->env = NULL;
    }
}



/* This function traverse the queue and sends the specified
 * nif_abort message with the specified reason to each member,
 * and if the 'free' argument is TRUE, the queue will be emptied.
 */
static
void inform_waiting_procs(ErlNifEnv*         env,
                          const char*        role,
                          ESockDescriptor*   descP,
                          ERL_NIF_TERM       sockRef,
                          ESockRequestQueue* q,
                          BOOLEAN_T          free,
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
         * IMPORTANT IN *THIS* CASE, BUT ITS A FUNDAMENTAL OP...
         *
         * </KOLLA>
         */

        SSDBG( descP,
               ("SOCKET",
                "inform_waiting_procs -> abort request %T (from %T)\r\n",
                currentP->data.ref, currentP->data.pid) );

        if (esock_send_abort_msg(env,
                                 sockRef,
                                 currentP->data.ref,
                                 currentP->data.env,
                                 reason,
                                 &currentP->data.pid) != NULL) {

            esock_warning_msg("Failed sending abort (%T) message to "
                              "current %s %T\r\n",
                              currentP->data.ref,
                              role,
                              currentP->data.pid);

        }
        currentP->data.env = NULL,

        DEMONP("inform_waiting_procs -> current 'request'",
               env, descP, &currentP->data.mon);
        nextP = currentP->nextP;
        if (free) FREE(currentP);
        currentP = nextP;
    }

    if (free) {
        q->first = NULL;
        q->last  = NULL;
    }
}
#endif // if !defined(__WIN32__)


/* =========================================================================
 * socket_down - Callback function for resource down (monitored processes)
 *
 */
static
void socket_down(ErlNifEnv*           env,
                 void*                obj,
                 const ErlNifPid*     pid,
                 const ErlNifMonitor* mon)
{
#if !defined(__WIN32__)
    ESockDescriptor* descP = (ESockDescriptor*) obj;
    int              sres;
    ERL_NIF_TERM     sockRef;

    SSDBG( descP, ("SOCKET", "socket_down -> entry with"
                   "\r\n   sock:  %d"
                   "\r\n   pid:   %T"
                   "\r\n   Close: %s (%s)"
                   "\r\n",
                   descP->sock, *pid,
                   B2S(IS_CLOSED(descP)),
                   B2S(IS_CLOSING(descP))) );

    if (!IS_CLOSED(descP)) {

        if (COMPARE_PIDS(&descP->ctrlPid, pid) == 0) {

            /* We don't bother with the queue cleanup here - 
             * we leave it to the stop callback function.
             */

            SSDBG( descP,
                   ("SOCKET", "socket_down -> controlling process exit\r\n") );

            descP->state      = SOCKET_STATE_CLOSING;
            descP->closeLocal = TRUE;
            descP->closerPid  = *pid;
            MON_INIT(&descP->closerMon);

            sres = esock_select_stop(env, descP->sock, descP);

            if (sres & ERL_NIF_SELECT_STOP_CALLED) {

                /* We are done - we can finalize (socket close) directly */
                SSDBG( descP,
                       ("SOCKET", 
                        "socket_down -> [%d] stop called\r\n", descP->sock) );

                dec_socket(descP->domain, descP->type, descP->protocol);
                descP->state = SOCKET_STATE_CLOSED;

                /* And finally close the socket.
                 * Since we close the socket because of an exiting owner,
                 * we do not need to wait for buffers to sync (linger).
                 * If the owner wish to ensure the buffer are written,
                 * it should have closed the socket explicitly...
                 */
                if (sock_close(descP->sock) != 0) {
                    int save_errno = sock_errno();

                    esock_warning_msg("Failed closing socket for terminating "
                                      "controlling process: "
                                      "\r\n   Controlling Process: %T"
                                      "\r\n   Descriptor:          %d"
                                      "\r\n   Errno:               %d"
                                      "\r\n", pid, descP->sock, save_errno);
                }
                sock_close_event(descP->event);

                descP->sock  = INVALID_SOCKET;
                descP->event = INVALID_EVENT;

                descP->state = SOCKET_STATE_CLOSED;

            } else if (sres & ERL_NIF_SELECT_STOP_SCHEDULED) {

                /* The stop callback function has been *scheduled* which means
                 * that "should" wait for it to complete. But since we are in
                 * a callback (down) function, we cannot...
                 * So, we must close the socket
                 */
                SSDBG( descP,
                       ("SOCKET",
                        "socket_down -> [%d] stop scheduled\r\n",
                        descP->sock) );

                dec_socket(descP->domain, descP->type, descP->protocol);

                /* And now what? We can't wait for the stop function here... 
                 * So, we simply close it here and leave the rest of the "close"
                 * for later (when the stop function actually gets called...
                 */

                if (sock_close(descP->sock) != 0) {
                    int save_errno = sock_errno();

                    esock_warning_msg("Failed closing socket for terminating "
                                      "controlling process: "
                                      "\r\n   Controlling Process: %T"
                                      "\r\n   Descriptor:          %d"
                                      "\r\n   Errno:               %d"
                                      "\r\n", pid, descP->sock, save_errno);
                }
                sock_close_event(descP->event);

            } else {

                esock_warning_msg("Failed selecting stop when handling down "
                                  "of controlling process: "
                                  "\r\n   Select Res:          %d"
                                  "\r\n   Controlling Process: %T"
                                  "\r\n   Descriptor:          %d"
                                  "\r\n   Monitor:             %T"
                                  "\r\n", sres, pid, descP->sock,
                                  MON2T(env, mon));
            }

        } else if (COMPARE_PIDS(&descP->connPid, pid) == 0) {

            /* The connPid is only set during the connection.
             * The same goes for the monitor (connMon).
             */

            descP->state = SOCKET_STATE_OPEN;  /* restore state */
            enif_set_pid_undefined(&descP->connPid);
            DEMONP("socket_down -> connector",
                   env, descP, &descP->connMon);

        } else {

            /* check all operation queue(s): acceptor, writer and reader. 
             *
             * Is it really any point in doing this if the socket is closed?
             *
             */

            SSDBG( descP, ("SOCKET", "socket_down -> other process term\r\n") );

            sockRef = enif_make_resource(env, descP);

            MLOCK(descP->accMtx);
            if (descP->currentAcceptorP != NULL)
                socket_down_acceptor(env, descP, sockRef, pid);
            MUNLOCK(descP->accMtx);

            MLOCK(descP->writeMtx);
            if (descP->currentWriterP != NULL)
                socket_down_writer(env, descP, sockRef, pid);
            MUNLOCK(descP->writeMtx);

            MLOCK(descP->readMtx);
            if (descP->currentReaderP != NULL)
                socket_down_reader(env, descP, sockRef, pid);
            MUNLOCK(descP->readMtx);

        }
    }

    SSDBG( descP, ("SOCKET", "socket_down -> done\r\n") );

#endif // if !defined(__WIN32__)
}



/* *** socket_down_acceptor ***
 *
 * Check and then handle a downed acceptor process.
 *
 */
#if !defined(__WIN32__)
static
void socket_down_acceptor(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          const ErlNifPid* pid)
{
    if (COMPARE_PIDS(&descP->currentAcceptor.pid, pid) == 0) {
        
        SSDBG( descP, ("SOCKET",
                       "socket_down_acceptor -> "
                       "current acceptor - try activate next\r\n") );
        
        if (!activate_next_acceptor(env, descP, sockRef)) {

            SSDBG( descP,
                   ("SOCKET", "socket_down_acceptor -> no more writers\r\n") );

            descP->state               = SOCKET_STATE_LISTENING;

            descP->currentAcceptorP    = NULL;
            descP->currentAcceptor.ref = esock_atom_undefined;
            enif_set_pid_undefined(&descP->currentAcceptor.pid);
            esock_monitor_init(&descP->currentAcceptor.mon);
        }

    } else {
        
        /* Maybe unqueue one of the waiting acceptors */
        
        SSDBG( descP, ("SOCKET",
                       "socket_down_acceptor -> "
                       "not current acceptor - maybe a waiting acceptor\r\n") );
        
        acceptor_unqueue(env, descP, pid);
    }
}




/* *** socket_down_writer ***
 *
 * Check and then handle a downed writer process.
 *
 */
static
void socket_down_writer(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ERL_NIF_TERM     sockRef,
                        const ErlNifPid* pid)
{
    if (COMPARE_PIDS(&descP->currentWriter.pid, pid) == 0) {
        
        SSDBG( descP, ("SOCKET",
                       "socket_down_writer -> "
                       "current writer - try activate next\r\n") );
        
        if (!activate_next_writer(env, descP, sockRef)) {
            SSDBG( descP, ("SOCKET",
                           "socket_down_writer -> no active writer\r\n") );
            descP->currentWriterP    = NULL;
            descP->currentWriter.ref = esock_atom_undefined;
            enif_set_pid_undefined(&descP->currentWriter.pid);
            esock_monitor_init(&descP->currentWriter.mon);
        }
        
    } else {
        
        /* Maybe unqueue one of the waiting writer(s) */
        
        SSDBG( descP, ("SOCKET",
                       "socket_down_writer -> "
                       "not current writer - maybe a waiting writer\r\n") );
        
        writer_unqueue(env, descP, pid);
    }
}




/* *** socket_down_reader ***
 *
 * Check and then handle a downed reader process.
 *
 */
static
void socket_down_reader(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ERL_NIF_TERM     sockRef,
                        const ErlNifPid* pid)
{
    if (COMPARE_PIDS(&descP->currentReader.pid, pid) == 0) {
        
        SSDBG( descP, ("SOCKET",
                       "socket_down_reader -> "
                       "current reader - try activate next\r\n") );
        
        if (!activate_next_reader(env, descP, sockRef)) {
            SSDBG( descP,
                   ("SOCKET", "ncancel_recv_current -> no more readers\r\n") );
            descP->currentReaderP    = NULL;
            descP->currentReader.ref = esock_atom_undefined;
            enif_set_pid_undefined(&descP->currentReader.pid);
            esock_monitor_init(&descP->currentReader.mon);
        }

    } else {
        
        /* Maybe unqueue one of the waiting reader(s) */
        
        SSDBG( descP, ("SOCKET",
                       "socket_down_reader -> "
                       "not current reader - maybe a waiting reader\r\n") );
        
        reader_unqueue(env, descP, pid);
    }
}
#endif // if !defined(__WIN32__)



/* ----------------------------------------------------------------------
 *  L o a d / u n l o a d / u p g r a d e   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

static
ErlNifFunc socket_funcs[] =
{
    // Some utility and support functions
    {"nif_info",                0, nif_info, 0},
    {"nif_info",                1, nif_info, 0},
    {"nif_supports",            1, nif_supports, 0},
    {"nif_command",             1, nif_command, 0},

    // The proper "socket" interface
    // nif_open/1 is (supposed to be) used when we already have a file descriptor
    // {"nif_open",                1, nif_open, 0},
    {"nif_open",                4, nif_open, 0},
    {"nif_bind",                2, nif_bind, 0},
    {"nif_connect",             2, nif_connect, 0},
    {"nif_listen",              2, nif_listen, 0},
    {"nif_accept",              2, nif_accept, 0},
    {"nif_send",                4, nif_send, 0},
    {"nif_sendto",              5, nif_sendto, 0},
    {"nif_sendmsg",             4, nif_sendmsg, 0},
    {"nif_recv",                4, nif_recv, 0},
    {"nif_recvfrom",            4, nif_recvfrom, 0},
    {"nif_recvmsg",             5, nif_recvmsg, 0},
    {"nif_close",               1, nif_close, 0},
    {"nif_shutdown",            2, nif_shutdown, 0},
    {"nif_setopt",              5, nif_setopt, 0},
    {"nif_getopt",              4, nif_getopt, 0},
    {"nif_sockname",            1, nif_sockname, 0},
    {"nif_peername",            1, nif_peername, 0},

    /* Misc utility functions */

    /* "Extra" functions to "complete" the socket interface.
     * For instance, the function nif_finalize_connection
     * is called after the connect *select* has "completed".
     */
    {"nif_finalize_connection", 1, nif_finalize_connection, 0},
    {"nif_cancel",              3, nif_cancel, 0},
    {"nif_finalize_close",      1, nif_finalize_close, ERL_NIF_DIRTY_JOB_IO_BOUND}
};


#if !defined(__WIN32__)
static
BOOLEAN_T extract_debug(ErlNifEnv*   env,
                        ERL_NIF_TERM map)
{
    /*
     * We need to do this here since the "proper" atom has not been
     * created when this function is called.
     */
    ERL_NIF_TERM debug = MKA(env, "debug");
    
    return esock_extract_bool_from_map(env, map, debug, SOCKET_GLOBAL_DEBUG_DEFAULT);
}

static
BOOLEAN_T extract_iow(ErlNifEnv*   env,
                      ERL_NIF_TERM map)
{
    /*
     * We need to do this here since the "proper" atom has not been
     * created when this function is called.
     */
    ERL_NIF_TERM iow = MKA(env, "iow");
    
    return esock_extract_bool_from_map(env, map, iow, SOCKET_NIF_IOW_DEFAULT);
}
#endif // if !defined(__WIN32__)



/* =======================================================================
 * load_info - A map of misc info (e.g global debug)
 */

static
int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
#if !defined(__WIN32__)    
    esock_dbg_init(ESOCK_DBGOUT_DEFAULT);
    // esock_dbg_init(ESOCK_DBGOUT_UNIQUE);

    data.dbg = extract_debug(env, load_info);
    data.iow = extract_iow(env, load_info);

    /* +++ Global Counters +++ */
    data.cntMtx         = MCREATE("socket[gcnt]");
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
#endif

    /* +++ Local atoms and error reason atoms +++ */
#define LOCAL_ATOM_DECL(A) atom_##A = MKA(env, #A)
LOCAL_ATOMS
LOCAL_ERROR_REASON_ATOMS
#undef LOCAL_ATOM_DECL

    /* Global atom(s) and error reason atom(s) */
#define GLOBAL_ATOM_DECL(A) esock_atom_##A = MKA(env, #A)
GLOBAL_ATOMS
GLOBAL_ERROR_REASON_ATOMS
#undef GLOBAL_ATOM_DECL
    esock_atom_socket_tag = MKA(env, "$socket");

    sockets = enif_open_resource_type_x(env,
                                        "sockets",
                                        &socketInit,
                                        ERL_NIF_RT_CREATE,
                                        NULL);

    return !sockets;
}

ERL_NIF_INIT(socket, socket_funcs, on_load, NULL, NULL, NULL)
