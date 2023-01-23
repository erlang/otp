/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2018-2023. All Rights Reserved.
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
 * '<io-backend>_<something>', e.g. essio_open (actually
 * essio_open_with_fd or essio_open_plain).
 * ----------------------------------------------------------------------
 *
 *
 * This is just a code snippet example in case there is need of
 * extra debugging:
 *
 * esock_dbg_printf("DEMONP", "[%d] %s: %T\r\n",
 *                  descP->sock, slogan,
 *                  esock_make_monitor_term(env, &mon));
 *
 */

#define STATIC_ERLANG_NIF 1

#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#ifndef ESOCK_ENABLE
#    include <erl_nif.h>

static
ErlNifFunc esock_funcs[] = {};

static
int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void)env;
    (void)priv_data;
    (void)load_info;

    return 1;
}

ERL_NIF_INIT(prim_socket, esock_funcs, on_load, NULL, NULL, NULL)

#else

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
#include "prim_socket_int.h"
#include "socket_io.h"
#include "socket_syncio.h"
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



#define ESOCK_RECV_BUFFER_COUNT_DEFAULT     0
#define ESOCK_RECV_BUFFER_SIZE_DEFAULT      8192
#define ESOCK_RECV_CTRL_BUFFER_SIZE_DEFAULT 1024
#define ESOCK_SEND_CTRL_BUFFER_SIZE_DEFAULT 1024

#define ESOCK_DESC_PATTERN_CREATED 0x03030303
#define ESOCK_DESC_PATTERN_DTOR    0xC0C0C0C0


/*----------------------------------------------------------------------------
 * Interface constants.
 *
 * The set of elements should be the same as for the type
 * msg_flag() in socket.erl.
 */

const ESockMsgFlag msg_flags[] = {

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


static const struct ioctl_flag {
    int           flag;
    ERL_NIF_TERM *name;
} ioctl_flags[] = {

    {
#ifdef IFF_UP
        IFF_UP,
#else
        0,
#endif
        &esock_atom_up},

    {
#ifdef IFF_BROADCAST
        IFF_BROADCAST,
#else
        0,
#endif
        &esock_atom_broadcast},

    {
#ifdef IFF_DEBUG
        IFF_DEBUG,
#else
        0,
#endif
        &esock_atom_debug},

    {
#ifdef IFF_LOOPBACK
        IFF_LOOPBACK,
#else
        0,
#endif
        &esock_atom_loopback},

    {
#ifdef IFF_POINTOPOINT
      IFF_POINTOPOINT,
#else
        0,
#endif
        &esock_atom_pointopoint},

    {
#ifdef IFF_NOTRAILERS
      IFF_NOTRAILERS,
#else
      0,
#endif
      &esock_atom_notrailers},

    /* FreeBSD: Has the same value as (Linux) notrailers */
    {
#ifdef IFF_KNOWSEPOCH
      IFF_KNOWSEPOCH,
#else
      0,
#endif
      &esock_atom_knowsepoch},

    {
#ifdef IFF_RUNNING
        IFF_RUNNING,
#else
        0,
#endif
        &esock_atom_running},

    {
#ifdef IFF_NOARP
        IFF_NOARP,
#else
        0,
#endif
        &esock_atom_noarp},

    {
#ifdef IFF_PROMISC
        IFF_PROMISC,
#else
        0,
#endif
        &esock_atom_promisc},

    {
#ifdef IFF_ALLMULTI
        IFF_ALLMULTI,
#else
        0,
#endif
        &esock_atom_allmulti},

    {
#ifdef IFF_MASTER
      IFF_MASTER,
#else
      0,
#endif
      &esock_atom_master},

    /* FreeBSD: Has the same value as (Linux) master */
    {
#ifdef IFF_OACTIVE
      IFF_OACTIVE,
#else
      0,
#endif
      &esock_atom_oactive},

    {
#ifdef IFF_SLAVE
      IFF_SLAVE,
#else
      0,
#endif
      &esock_atom_slave},

    /* FreeBSD: Has the same value as (Linux) slave */
    {
#ifdef IFF_SIMPLEX
      IFF_SIMPLEX,
#else
      0,
#endif
      &esock_atom_simplex},

    {
#ifdef IFF_MULTICAST
      IFF_MULTICAST,
#else
      0,
#endif
      &esock_atom_multicast},

    // FreeBSD, ...
    {
#ifdef IFF_LINK0
      IFF_LINK0,
#else
      0,
#endif
      &esock_atom_link0},

    // FreeBSD, ...
    {
#ifdef IFF_LINK1
      IFF_LINK1,
#else
      0,
#endif
      &esock_atom_link1},

    // FreeBSD, ...
    {
#ifdef IFF_LINK2
      IFF_LINK2,
#else
      0,
#endif
      &esock_atom_link2},

    {
#ifdef IFF_PORTSEL
      IFF_PORTSEL,
#else
      0,
#endif
      &esock_atom_portsel},

    {
#ifdef IFF_AUTOMEDIA
      IFF_AUTOMEDIA,
#else
      0,
#endif
      &esock_atom_automedia},

    {
#ifdef IFF_DYNAMIC
      IFF_DYNAMIC,
#else
      0,
#endif
      &esock_atom_dynamic},

    // FreeBSD, ...
    {
#ifdef IFF_CANTCONFIG
      IFF_CANTCONFIG,
#else
      0,
#endif
      &esock_atom_cantconfig},

    {
#ifdef IFF_LOWER_UP
      IFF_LOWER_UP,
#else
      0,
#endif
      &esock_atom_lower_up},

    // FreeBSD, ...
    {
#ifdef IFF_PPROMISC
      IFF_PPROMISC,
#else
      0,
#endif
      &esock_atom_ppromisc},

    {
#ifdef IFF_DORMANT
      IFF_DORMANT,
#else
      0,
#endif
      &esock_atom_dormant},

    // FreeBSD, ...
    {
#ifdef IFF_MONITOR
      IFF_MONITOR,
#else
      0,
#endif
      &esock_atom_monitor},

    {
#ifdef IFF_ECHO
      IFF_ECHO,
#else
      0,
#endif
      &esock_atom_echo},

    // FreeBSD, ...
    {
#ifdef IFF_STATICARP
      IFF_STATICARP,
#else
      0,
#endif
      &esock_atom_staticarp},

    // FreeBSD, ...
    {
#ifdef IFF_DYING
      IFF_DYING,
#else
      0,
#endif
      &esock_atom_dying},

    // FreeBSD, ...
    {
#ifdef IFF_RENAMING
      IFF_RENAMING,
#else
      0,
#endif
      &esock_atom_renaming},

    // FreeBSD, ...
    {
#ifdef IFF_NOGROUP
      IFF_NOGROUP,
#else
      0,
#endif
      &esock_atom_nogroup}
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


/* =================================================================== *
 *                                                                     *
 *                        Various esockmacros                          *
 *                                                                     *
 * =================================================================== */

/* Global socket debug */
#define SGDBG( proto )            ESOCK_DBG_PRINTF( data.dbg , proto )


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


// #ifdef HAS_ACCEPT4
// We have to figure out what the flags are...
// #define sock_accept(s, addr, len)       accept4((s), (addr), (len), (SOCK_CLOEXEC))
// #else
// #define sock_accept(s, addr, len)       accept((s), (addr), (len))
// #endif
// #define sock_bind(s, addr, len)         bind((s), (addr), (len))
#define sock_close(s)                   close((s))
#define sock_close_event(e)             /* do nothing */
// #define sock_connect(s, addr, len)      connect((s), (addr), (len))
#define sock_create_event(s)            (s) /* return file descriptor */
#define sock_errno()                    errno
#define sock_getopt(s,t,n,v,l)          getsockopt((s),(t),(n),(v),(l))
#define sock_htons(x)                   htons((x))
#define sock_htonl(x)                   htonl((x))
// #define sock_listen(s, b)               listen((s), (b))
// #define sock_name(s, addr, len)         getsockname((s), (addr), (len))
#define sock_ntohs(x)                   ntohs((x))
// #define sock_open(domain, type, proto)  socket((domain), (type), (proto))
// #define sock_peer(s, addr, len)         getpeername((s), (addr), (len))
// #define sock_recv(s,buf,len,flag)       recv((s),(buf),(len),(flag))
/* #define sock_recvfrom(s,buf,blen,flag,addr,alen) \ */
/*     recvfrom((s),(buf),(blen),(flag),(addr),(alen)) */
// #define sock_recvmsg(s,msghdr,flag)     recvmsg((s),(msghdr),(flag))
// #define sock_send(s,buf,len,flag)       send((s), (buf), (len), (flag))
// #define sock_sendmsg(s,msghdr,flag)     sendmsg((s),(msghdr),(flag))
/* #define sock_sendto(s,buf,blen,flag,addr,alen)                \ */
/*     sendto((s),(buf),(blen),(flag),(addr),(alen)) */
#define sock_setopt(s,l,o,v,ln)         setsockopt((s),(l),(o),(v),(ln))
// #define sock_shutdown(s, how)           shutdown((s), (how))


/* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ *
 *                                                                        *
 * End of non-__WIN32__ section a.k.a UNIX section                        *
 *                                                                        *
 * ---------------------------------------------------------------------- */
#endif /* #ifdef __WIN32__  #else */


#ifdef HAVE_SENDFILE

ESockSendfileCounters initESockSendfileCounters =
    {0, 0, 0, 0, 0, 0, 0, 0, 0};

#endif


/* We can use the IPv4 def for this since the beginning
 * is the same for INET and INET6 */
#define which_address_port(sap)		     \
  ((((sap)->in4.sin_family == AF_INET) ||  \
    ((sap)->in4.sin_family == AF_INET6)) ? \
   ((sap)->in4.sin_port) : -1)


/* ----------------------------------------------------------------------
 *  F o r w a r d s
 * ----------------------------------------------------------------------
 */




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
 * nif_ioctl
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
    ESOCK_NIF_FUNC_DEF(ioctl);                      \
    ESOCK_NIF_FUNC_DEF(finalize_close);             \
    ESOCK_NIF_FUNC_DEF(cancel);

#define ESOCK_NIF_FUNC_DEF(F)                              \
    static ERL_NIF_TERM nif_##F(ErlNifEnv*         env,    \
                                int                argc,   \
                                const ERL_NIF_TERM argv[]);
ESOCK_NIF_FUNCS
#undef ESOCK_NIF_FUNC_DEF


/* =======================================================================
 * Socket specific backend 'synchronicity' functions.
 * This type is used to create 'sync' function table.
 * This table is initiated when the nif is loaded.
 * Initially, its content will be hardcoded to:
 *   * Windows:      async (esaio)
 *   * Other (unix): sync  (essio)
 * When we introduce async I/O for unix (io_uring or something similar)
 * we may make it possible to choose (set a flag when the VM is started;
 * --esock-io=<async|sync>).
 */

typedef struct {
    ESockIOInit                  init;
    ESockIOFinish                finish;

    ESockIOOpenWithFd            open_with_fd;
    ESockIOOpenPlain             open_plain;
    ESockIOBind                  bind;

    ESockIOConnect               connect;
    ESockIOListen                listen;
    ESockIOAccept                accept;

    ESockIOSend                  send;
    ESockIOSendTo                sendto;
    ESockIOSendMsg               sendmsg;
    ESockIOSendFileStart         sendfile_start;
    ESockIOSendFileContinue      sendfile_cont;
    ESockIOSendFileDeferredClose sendfile_dc;

    ESockIORecv                  recv;
    ESockIORecvFrom              recvfrom;
    ESockIORecvMsg               recvmsg;

    ESockIOClose                 close;
    ESockIOFinClose              fin_close;
    ESockIOShutdown              shutdown;

    ESockIOSockName              sockname;
    ESockIOPeerName              peername;
} ESockIoBackend;



#ifndef __WIN32__
/* ---------------------------------------------------------------------- *
 *                                                                        *
 *                                                                        *
 * Start of non-__WIN32__ section a.k.a UNIX section                      *
 *                                                                        *
 *                                                                        *
 * vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv */

/* And here comes the functions that does the actual work (for the most part) */

static ERL_NIF_TERM esock_command(ErlNifEnv*   env,
                                  ERL_NIF_TERM command,
                                  ERL_NIF_TERM cdata);
static ERL_NIF_TERM esock_command_debug(ErlNifEnv*   env,
                                        ERL_NIF_TERM cdata);
static ERL_NIF_TERM esock_command_socket_debug(ErlNifEnv*   env,
                                               ERL_NIF_TERM cdata);
static ERL_NIF_TERM esock_command_use_socket_registry(ErlNifEnv*   env,
                                                      ERL_NIF_TERM cdata);

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
static ERL_NIF_TERM esock_supports_ioctl_requests(ErlNifEnv* env);
static ERL_NIF_TERM esock_supports_ioctl_flags(ErlNifEnv* env);
static ERL_NIF_TERM esock_supports_options(ErlNifEnv* env);

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


static ERL_NIF_TERM esock_ioctl1(ErlNifEnv*       env,
				 ESockDescriptor* descP,
				 unsigned long    req);
static ERL_NIF_TERM esock_ioctl2(ErlNifEnv*       env,
				 ESockDescriptor* descP,
				 unsigned long    req,
				 ERL_NIF_TERM     arg);
static ERL_NIF_TERM esock_ioctl3(ErlNifEnv*       env,
				 ESockDescriptor* descP,
				 unsigned long    req,
				 ERL_NIF_TERM     ename,
				 ERL_NIF_TERM     eval);
static ERL_NIF_TERM esock_ioctl_gifconf(ErlNifEnv*       env,
					ESockDescriptor* descP);
#if defined(SIOCGIFNAME)
static ERL_NIF_TERM esock_ioctl_gifname(ErlNifEnv*       env,
					ESockDescriptor* descP,
					ERL_NIF_TERM     eidx);
#endif

/* esock_ioctl_gifindex */
#if defined(SIOCGIFINDEX)
#define IOCTL_GIFINDEX_FUNC_DEF IOCTL_GET_FUNC_DEF(gifindex)
#else
#define IOCTL_GIFINDEX_FUNC_DEF
#endif

/* esock_ioctl_gifflags */
#if defined(SIOCGIFFLAGS)
#define IOCTL_GIFFLAGS_FUNC_DEF IOCTL_GET_FUNC_DEF(gifflags)
#else
#define IOCTL_GIFFLAGS_FUNC_DEF
#endif

/* esock_ioctl_gifaddr */
#if defined(SIOCGIFADDR)
#define IOCTL_GIFADDR_FUNC_DEF IOCTL_GET_FUNC_DEF(gifaddr)
#else
#define IOCTL_GIFADDR_FUNC_DEF
#endif

/* esock_ioctl_gifdstaddr */
#if defined(SIOCGIFDSTADDR)
#define IOCTL_GIFDSTADDR_FUNC_DEF IOCTL_GET_FUNC_DEF(gifdstaddr)
#else
#define IOCTL_GIFDSTADDR_FUNC_DEF
#endif

/* esock_ioctl_gifbrdaddr */
#if defined(SIOCGIFBRDADDR)
#define IOCTL_GIFBRDADDR_FUNC_DEF IOCTL_GET_FUNC_DEF(gifbrdaddr)
#else
#define IOCTL_GIFBRDADDR_FUNC_DEF
#endif

/* esock_ioctl_gifnetmask */
#if defined(SIOCGIFNETMASK)
#define IOCTL_GIFNETMASK_FUNC_DEF IOCTL_GET_FUNC_DEF(gifnetmask)
#else
#define IOCTL_GIFNETMASK_FUNC_DEF
#endif

/* esock_ioctl_gifmtu */
#if defined(SIOCGIFMTU)
#define IOCTL_GIFMTU_FUNC_DEF IOCTL_GET_FUNC_DEF(gifmtu)
#else
#define IOCTL_GIFMTU_FUNC_DEF
#endif

/* esock_ioctl_gifhwaddr */
#if defined(SIOCGIFHWADDR) && defined(ESOCK_USE_HWADDR)
#define IOCTL_GIFHWADDR_FUNC_DEF IOCTL_GET_FUNC_DEF(gifhwaddr)
#else
#define IOCTL_GIFHWADDR_FUNC_DEF
#endif

/* esock_ioctl_gifmap */
#if defined(SIOCGIFMAP) && defined(ESOCK_USE_IFMAP)
#define IOCTL_GIFMAP_FUNC_DEF IOCTL_GET_FUNC_DEF(gifmap)
#else
#define IOCTL_GIFMAP_FUNC_DEF
#endif

/* esock_ioctl_giftxqlen */
#if defined(SIOCGIFTXQLEN)
#define IOCTL_GIFTXQLEN_FUNC_DEF IOCTL_GET_FUNC_DEF(giftxqlen)
#else
#define IOCTL_GIFTXQLEN_FUNC_DEF
#endif

#define IOCTL_GET_FUNCS_DEF			\
  IOCTL_GIFINDEX_FUNC_DEF;			\
  IOCTL_GIFFLAGS_FUNC_DEF;			\
  IOCTL_GIFADDR_FUNC_DEF;			\
  IOCTL_GIFDSTADDR_FUNC_DEF;			\
  IOCTL_GIFBRDADDR_FUNC_DEF;			\
  IOCTL_GIFNETMASK_FUNC_DEF;			\
  IOCTL_GIFMTU_FUNC_DEF;			\
  IOCTL_GIFHWADDR_FUNC_DEF;			\
  IOCTL_GIFMAP_FUNC_DEF;			\
  IOCTL_GIFTXQLEN_FUNC_DEF;
#define IOCTL_GET_FUNC_DEF(F)					\
  static ERL_NIF_TERM esock_ioctl_##F(ErlNifEnv*       env,	\
				      ESockDescriptor* descP,	\
				      ERL_NIF_TERM     ename)
IOCTL_GET_FUNCS_DEF
#undef IOCTL_GET_FUNC_DEF

/* esock_ioctl_sifflags */
#if defined(SIOCSIFFLAGS)
#define IOCTL_SIFFLAGS_FUNC_DEF IOCTL_SET_FUNC_DEF(sifflags)
#else
#define IOCTL_SIFFLAGS_FUNC_DEF
#endif

/* esock_ioctl_sifaddr */
#if defined(SIOCSIFADDR)
#define IOCTL_SIFADDR_FUNC_DEF IOCTL_SET_FUNC_DEF(sifaddr)
#else
#define IOCTL_SIFADDR_FUNC_DEF
#endif

/* esock_ioctl_sifdstaddr */
#if defined(SIOCSIFDSTADDR)
#define IOCTL_SIFDSTADDR_FUNC_DEF IOCTL_SET_FUNC_DEF(sifdstaddr)
#else
#define IOCTL_SIFDSTADDR_FUNC_DEF
#endif

/* esock_ioctl_sifbrdaddr */
#if defined(SIOCSIFBRDADDR)
#define IOCTL_SIFBRDADDR_FUNC_DEF IOCTL_SET_FUNC_DEF(sifbrdaddr)
#else
#define IOCTL_SIFBRDADDR_FUNC_DEF
#endif

/* esock_ioctl_sifnetmask */
#if defined(SIOCSIFNETMASK)
#define IOCTL_SIFNETMASK_FUNC_DEF IOCTL_SET_FUNC_DEF(sifnetmask)
#else
#define IOCTL_SIFNETMASK_FUNC_DEF
#endif

/* esock_ioctl_sifmtu */
#if defined(SIOCSIFMTU)
#define IOCTL_SIFMTU_FUNC_DEF IOCTL_SET_FUNC_DEF(sifmtu)
#else
#define IOCTL_SIFMTU_FUNC_DEF
#endif

/* esock_ioctl_siftxqlen */
#if defined(SIOCSIFTXQLEN)
#define IOCTL_SIFTXQLEN_FUNC_DEF IOCTL_SET_FUNC_DEF(siftxqlen)
#else
#define IOCTL_SIFTXQLEN_FUNC_DEF
#endif

#define IOCTL_SET_FUNCS_DEF			\
  IOCTL_SIFFLAGS_FUNC_DEF;			\
  IOCTL_SIFADDR_FUNC_DEF;			\
  IOCTL_SIFDSTADDR_FUNC_DEF;			\
  IOCTL_SIFBRDADDR_FUNC_DEF;			\
  IOCTL_SIFNETMASK_FUNC_DEF;			\
  IOCTL_SIFMTU_FUNC_DEF;			\
  IOCTL_SIFTXQLEN_FUNC_DEF;
#define IOCTL_SET_FUNC_DEF(F)					\
  static ERL_NIF_TERM esock_ioctl_##F(ErlNifEnv*       env,	\
				      ESockDescriptor* descP,	\
				      ERL_NIF_TERM     ename,   \
				      ERL_NIF_TERM     evalue)
IOCTL_SET_FUNCS_DEF
#undef IOCTL_SET_FUNC_DEF


static ERL_NIF_TERM encode_ioctl_ifconf(ErlNifEnv*       env,
					ESockDescriptor* descP,
					struct ifconf*   ifcP);
static ERL_NIF_TERM encode_ioctl_ifconf_ifreq(ErlNifEnv*       env,
					      ESockDescriptor* descP,
					      struct ifreq*    ifrP);
static ERL_NIF_TERM encode_ioctl_ifreq_name(ErlNifEnv* env,
					    char*      name);
static ERL_NIF_TERM encode_ioctl_ifreq_sockaddr(ErlNifEnv*       env,
						struct sockaddr* sa);
static ERL_NIF_TERM make_ifreq(ErlNifEnv*   env,
			       ERL_NIF_TERM name,
			       ERL_NIF_TERM key2,
			       ERL_NIF_TERM val2);
#if defined(SIOCGIFMAP) && defined(ESOCK_USE_IFMAP)
static ERL_NIF_TERM encode_ioctl_ifrmap(ErlNifEnv*       env,
					ESockDescriptor* descP,
					struct ifmap*    mapP);
#endif
#if defined(SIOCGIFHWADDR) && defined(ESOCK_USE_HWADDR)
static ERL_NIF_TERM encode_ioctl_hwaddr(ErlNifEnv*       env,
					ESockDescriptor* descP,
					struct sockaddr* addrP);
#endif
static ERL_NIF_TERM encode_ioctl_ifraddr(ErlNifEnv*       env,
					 ESockDescriptor* descP,
					 struct sockaddr* addrP);
static ERL_NIF_TERM encode_ioctl_flags(ErlNifEnv*       env,
				       ESockDescriptor* descP,
				       short            flags);
#if defined(SIOCSIFFLAGS)
static BOOLEAN_T decode_ioctl_flags(ErlNifEnv*       env,
				    ESockDescriptor* descP,
				    ERL_NIF_TERM     eflags,
				    short*           flags);
#endif
static BOOLEAN_T decode_ioctl_sockaddr(ErlNifEnv*       env,
				       ESockDescriptor* descP,
				       ERL_NIF_TERM     eaddr,
				       ESockAddress*    addr);
#if defined(SIOCSIFMTU)
static BOOLEAN_T decode_ioctl_mtu(ErlNifEnv*       env,
				  ESockDescriptor* descP,
				  ERL_NIF_TERM     emtu,
				  int*             mtu);
#endif
#if defined(SIOCSIFTXQLEN)
static BOOLEAN_T decode_ioctl_txqlen(ErlNifEnv*       env,
				     ESockDescriptor* descP,
				     ERL_NIF_TERM     etxqlen,
				     int*             txqlen);
#endif
#if defined(SIOCSIFTXQLEN)
static BOOLEAN_T decode_ioctl_ivalue(ErlNifEnv*       env,
				     ESockDescriptor* descP,
				     ERL_NIF_TERM     eivalue,
				     int*             ivalue);
#endif
static ERL_NIF_TERM encode_ioctl_ivalue(ErlNifEnv*       env,
					ESockDescriptor* descP,
					int              ivalue);

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

static BOOLEAN_T ehow2how(ERL_NIF_TERM ehow, int* how);



/* *** esock_activate_next_acceptor ***
 * *** esock_activate_next_writer   ***
 * *** esock_activate_next_reader   ***
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

#define ACTIVATE_NEXT_FUNC_DEF(F)                                       \
    extern BOOLEAN_T esock_activate_next_##F(ErlNifEnv*       env,      \
                                             ESockDescriptor* descP,    \
                                             ERL_NIF_TERM     sockRef);
ACTIVATE_NEXT_FUNCS_DEFS
#undef ACTIVATE_NEXT_FUNC_DEF

/* esock_acceptor_search4pid | esock_writer_search4pid | esock_reader_search4pid
 * esock_acceptor_push       | esock_writer_push       | esock_reader_push
 * esock_acceptor_pop        | esock_writer_pop        | esock_reader_pop
 * esock_acceptor_unqueue    | esock_writer_unqueue    | esock_reader_unqueue
 *
 * All the queue operator functions (search4pid, push, pop
 * and unqueue) for acceptor, writer and reader has exactly
 * the same API, so we apply some macro magic to simplify.
 */

#define ESOCK_OPERATOR_FUNCS_DEFS      \
    ESOCK_OPERATOR_FUNCS_DEF(acceptor) \
    ESOCK_OPERATOR_FUNCS_DEF(writer)   \
    ESOCK_OPERATOR_FUNCS_DEF(reader)

#define ESOCK_OPERATOR_FUNCS_DEF(O)                                    \
    extern BOOLEAN_T esock_##O##_search4pid(ErlNifEnv*       env,      \
                                            ESockDescriptor* descP,    \
                                            ErlNifPid*       pid);     \
    extern void esock_##O##_push(ErlNifEnv*       env,                 \
                                 ESockDescriptor* descP,               \
                                 ErlNifPid        pid,                 \
                                 ERL_NIF_TERM     ref);                \
    extern BOOLEAN_T esock_##O##_pop(ErlNifEnv*       env,     \
                                     ESockDescriptor* descP,   \
                                     ESockRequestor*  reqP);   \
    extern BOOLEAN_T esock_##O##_unqueue(ErlNifEnv*       env,          \
                                         ESockDescriptor* descP,        \
                                         ERL_NIF_TERM*    refP,         \
                                         const ErlNifPid* pidP);
ESOCK_OPERATOR_FUNCS_DEFS
#undef ESOCK_OPERATOR_FUNCS_DEF

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

#ifdef HAVE_SENDFILE
static void
esock_send_sendfile_deferred_close_msg(ErlNifEnv*       env,
                                       ESockDescriptor* descP);
#endif
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

/*
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
*/

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

static void esock_on_halt(void* priv_data);

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
    GLOBAL_ATOM_DECL(acc_success);                     \
    GLOBAL_ATOM_DECL(acc_fails);                       \
    GLOBAL_ATOM_DECL(acc_tries);                       \
    GLOBAL_ATOM_DECL(acc_waits);                       \
    GLOBAL_ATOM_DECL(adaption_layer);                  \
    GLOBAL_ATOM_DECL(addr);                            \
    GLOBAL_ATOM_DECL(addrform);                        \
    GLOBAL_ATOM_DECL(add_membership);                  \
    GLOBAL_ATOM_DECL(add_source_membership);           \
    GLOBAL_ATOM_DECL(alen);                            \
    GLOBAL_ATOM_DECL(allmulti);                        \
    GLOBAL_ATOM_DECL(already);                         \
    GLOBAL_ATOM_DECL(any);                             \
    GLOBAL_ATOM_DECL(appletlk);                        \
    GLOBAL_ATOM_DECL(arcnet);                          \
    GLOBAL_ATOM_DECL(associnfo);                       \
    GLOBAL_ATOM_DECL(atm);                             \
    GLOBAL_ATOM_DECL(authhdr);                         \
    GLOBAL_ATOM_DECL(auth_active_key);                 \
    GLOBAL_ATOM_DECL(auth_asconf);                     \
    GLOBAL_ATOM_DECL(auth_chunk);                      \
    GLOBAL_ATOM_DECL(auth_delete_key);                 \
    GLOBAL_ATOM_DECL(auth_key);                        \
    GLOBAL_ATOM_DECL(auth_level);                      \
    GLOBAL_ATOM_DECL(autoclose);                       \
    GLOBAL_ATOM_DECL(automedia);                       \
    GLOBAL_ATOM_DECL(ax25);                            \
    GLOBAL_ATOM_DECL(bad_data);                        \
    GLOBAL_ATOM_DECL(bindtodevice);                    \
    GLOBAL_ATOM_DECL(block_source);                    \
    GLOBAL_ATOM_DECL(broadcast);                       \
    GLOBAL_ATOM_DECL(busy_poll);                       \
    GLOBAL_ATOM_DECL(cantconfig);		       \
    GLOBAL_ATOM_DECL(chaos);                           \
    GLOBAL_ATOM_DECL(checksum);                        \
    GLOBAL_ATOM_DECL(close);                           \
    GLOBAL_ATOM_DECL(closed);                          \
    GLOBAL_ATOM_DECL(cmsg_cloexec);                    \
    GLOBAL_ATOM_DECL(command);                         \
    GLOBAL_ATOM_DECL(confirm);                         \
    GLOBAL_ATOM_DECL(congestion);                      \
    GLOBAL_ATOM_DECL(connect);                         \
    GLOBAL_ATOM_DECL(connected);                       \
    GLOBAL_ATOM_DECL(connecting);                      \
    GLOBAL_ATOM_DECL(context);                         \
    GLOBAL_ATOM_DECL(cork);                            \
    GLOBAL_ATOM_DECL(credentials);                     \
    GLOBAL_ATOM_DECL(ctrl);                            \
    GLOBAL_ATOM_DECL(ctrunc);                          \
    GLOBAL_ATOM_DECL(data);                            \
    GLOBAL_ATOM_DECL(data_size);                       \
    GLOBAL_ATOM_DECL(debug);                           \
    GLOBAL_ATOM_DECL(default);                         \
    GLOBAL_ATOM_DECL(default_send_params);             \
    GLOBAL_ATOM_DECL(delayed_ack_time);                \
    GLOBAL_ATOM_DECL(dgram);                           \
    GLOBAL_ATOM_DECL(disable_fragments);               \
    GLOBAL_ATOM_DECL(dlci);                            \
    GLOBAL_ATOM_DECL(domain);                          \
    GLOBAL_ATOM_DECL(dontfrag);                        \
    GLOBAL_ATOM_DECL(dontroute);                       \
    GLOBAL_ATOM_DECL(dormant);                         \
    GLOBAL_ATOM_DECL(drop_membership);                 \
    GLOBAL_ATOM_DECL(drop_source_membership);          \
    GLOBAL_ATOM_DECL(dstopts);                         \
    GLOBAL_ATOM_DECL(dup);                             \
    GLOBAL_ATOM_DECL(dying);			       \
    GLOBAL_ATOM_DECL(dynamic);                         \
    GLOBAL_ATOM_DECL(echo);                            \
    GLOBAL_ATOM_DECL(eether);                          \
    GLOBAL_ATOM_DECL(efile);                           \
    GLOBAL_ATOM_DECL(egp);                             \
    GLOBAL_ATOM_DECL(enotsup);                         \
    GLOBAL_ATOM_DECL(eor);                             \
    GLOBAL_ATOM_DECL(error);                           \
    GLOBAL_ATOM_DECL(errqueue);                        \
    GLOBAL_ATOM_DECL(esp_network_level);               \
    GLOBAL_ATOM_DECL(esp_trans_level);                 \
    GLOBAL_ATOM_DECL(ether);                           \
    GLOBAL_ATOM_DECL(eui64);                           \
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
    GLOBAL_ATOM_DECL(frelay);                          \
    GLOBAL_ATOM_DECL(get_peer_addr_info);              \
    GLOBAL_ATOM_DECL(hatype);                          \
    GLOBAL_ATOM_DECL(hdrincl);                         \
    GLOBAL_ATOM_DECL(hmac_ident);                      \
    GLOBAL_ATOM_DECL(hoplimit);                        \
    GLOBAL_ATOM_DECL(hopopts);                         \
    GLOBAL_ATOM_DECL(host);                            \
    GLOBAL_ATOM_DECL(icmp);                            \
    GLOBAL_ATOM_DECL(icmp6);                           \
    GLOBAL_ATOM_DECL(ieee802);                         \
    GLOBAL_ATOM_DECL(ieee1394);                        \
    GLOBAL_ATOM_DECL(ifindex);                         \
    GLOBAL_ATOM_DECL(igmp);                            \
    GLOBAL_ATOM_DECL(implink);                         \
    GLOBAL_ATOM_DECL(index);                           \
    GLOBAL_ATOM_DECL(inet);                            \
    GLOBAL_ATOM_DECL(inet6);                           \
    GLOBAL_ATOM_DECL(infiniband);                      \
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
    GLOBAL_ATOM_DECL(knowsepoch);		       \
    GLOBAL_ATOM_DECL(leave_group);                     \
    GLOBAL_ATOM_DECL(level);                           \
    GLOBAL_ATOM_DECL(linger);                          \
    GLOBAL_ATOM_DECL(link);                            \
    GLOBAL_ATOM_DECL(link0);                            \
    GLOBAL_ATOM_DECL(link1);                            \
    GLOBAL_ATOM_DECL(link2);                            \
    GLOBAL_ATOM_DECL(local);                           \
    GLOBAL_ATOM_DECL(localtlk);                        \
    GLOBAL_ATOM_DECL(local_auth_chunks);               \
    GLOBAL_ATOM_DECL(loopback);			       \
    GLOBAL_ATOM_DECL(lowdelay);                        \
    GLOBAL_ATOM_DECL(lower_up);                        \
    GLOBAL_ATOM_DECL(mark);                            \
    GLOBAL_ATOM_DECL(master);                          \
    GLOBAL_ATOM_DECL(maxburst);                        \
    GLOBAL_ATOM_DECL(maxseg);                          \
    GLOBAL_ATOM_DECL(md5sig);                          \
    GLOBAL_ATOM_DECL(metricom);                        \
    GLOBAL_ATOM_DECL(mincost);                         \
    GLOBAL_ATOM_DECL(minttl);                          \
    GLOBAL_ATOM_DECL(monitor);			       \
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
    GLOBAL_ATOM_DECL(name);                            \
    GLOBAL_ATOM_DECL(netns);                           \
    GLOBAL_ATOM_DECL(netrom);                          \
    GLOBAL_ATOM_DECL(nlen);                            \
    GLOBAL_ATOM_DECL(noarp);                           \
    GLOBAL_ATOM_DECL(nodelay);                         \
    GLOBAL_ATOM_DECL(nodefrag);                        \
    GLOBAL_ATOM_DECL(nogroup);			       \
    GLOBAL_ATOM_DECL(none);                            \
    GLOBAL_ATOM_DECL(noopt);                           \
    GLOBAL_ATOM_DECL(nopush);                          \
    GLOBAL_ATOM_DECL(nosignal);                        \
    GLOBAL_ATOM_DECL(notrailers);                      \
    GLOBAL_ATOM_DECL(not_found);                       \
    GLOBAL_ATOM_DECL(not_owner);                       \
    GLOBAL_ATOM_DECL(oactive);			       \
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
    GLOBAL_ATOM_DECL(pointopoint);                     \
    GLOBAL_ATOM_DECL(port);                            \
    GLOBAL_ATOM_DECL(portrange);                       \
    GLOBAL_ATOM_DECL(portsel);                         \
    GLOBAL_ATOM_DECL(ppromisc);			       \
    GLOBAL_ATOM_DECL(primary_addr);                    \
    GLOBAL_ATOM_DECL(prim_file);                       \
    GLOBAL_ATOM_DECL(priority);                        \
    GLOBAL_ATOM_DECL(promisc);                         \
    GLOBAL_ATOM_DECL(pronet);                          \
    GLOBAL_ATOM_DECL(protocol);                        \
    GLOBAL_ATOM_DECL(pup);                             \
    GLOBAL_ATOM_DECL(raw);                             \
    GLOBAL_ATOM_DECL(rcvbuf);                          \
    GLOBAL_ATOM_DECL(rcvbufforce);                     \
    GLOBAL_ATOM_DECL(rcvlowat);                        \
    GLOBAL_ATOM_DECL(rcvtimeo);                        \
    GLOBAL_ATOM_DECL(rdm);                             \
    GLOBAL_ATOM_DECL(read_byte);                       \
    GLOBAL_ATOM_DECL(read_fails);                      \
    GLOBAL_ATOM_DECL(read_pkg);                        \
    GLOBAL_ATOM_DECL(read_tries);                      \
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
    GLOBAL_ATOM_DECL(reliability);		       \
    GLOBAL_ATOM_DECL(renaming);			       \
    GLOBAL_ATOM_DECL(reset_streams);                   \
    GLOBAL_ATOM_DECL(retopts);                         \
    GLOBAL_ATOM_DECL(reuseaddr);                       \
    GLOBAL_ATOM_DECL(reuseport);                       \
    GLOBAL_ATOM_DECL(rights);                          \
    GLOBAL_ATOM_DECL(router_alert);                    \
    GLOBAL_ATOM_DECL(rthdr);                           \
    GLOBAL_ATOM_DECL(rtoinfo);                         \
    GLOBAL_ATOM_DECL(running);                         \
    GLOBAL_ATOM_DECL(rxq_ovfl);                        \
    GLOBAL_ATOM_DECL(scope_id);                        \
    GLOBAL_ATOM_DECL(sctp);                            \
    GLOBAL_ATOM_DECL(sec);                             \
    GLOBAL_ATOM_DECL(select);                          \
    GLOBAL_ATOM_DECL(select_failed);                   \
    GLOBAL_ATOM_DECL(select_sent);                     \
    GLOBAL_ATOM_DECL(send);                            \
    GLOBAL_ATOM_DECL(sendfile);                        \
    GLOBAL_ATOM_DECL(sendfile_byte);                   \
    GLOBAL_ATOM_DECL(sendfile_deferred_close);         \
    GLOBAL_ATOM_DECL(sendfile_fails);                  \
    GLOBAL_ATOM_DECL(sendfile_max);                    \
    GLOBAL_ATOM_DECL(sendfile_pkg);                    \
    GLOBAL_ATOM_DECL(sendfile_pkg_max);                \
    GLOBAL_ATOM_DECL(sendfile_tries);                  \
    GLOBAL_ATOM_DECL(sendfile_waits);                  \
    GLOBAL_ATOM_DECL(sendmsg);                         \
    GLOBAL_ATOM_DECL(sendsrcaddr);                     \
    GLOBAL_ATOM_DECL(sendto);                          \
    GLOBAL_ATOM_DECL(seqpacket);                       \
    GLOBAL_ATOM_DECL(setfib);                          \
    GLOBAL_ATOM_DECL(set_peer_primary_addr);           \
    GLOBAL_ATOM_DECL(simplex);			       \
    GLOBAL_ATOM_DECL(slave);                           \
    GLOBAL_ATOM_DECL(slen);                            \
    GLOBAL_ATOM_DECL(sndbuf);                          \
    GLOBAL_ATOM_DECL(sndbufforce);                     \
    GLOBAL_ATOM_DECL(sndlowat);                        \
    GLOBAL_ATOM_DECL(sndtimeo);                        \
    GLOBAL_ATOM_DECL(socket);                          \
    GLOBAL_ATOM_DECL(spec_dst);                        \
    GLOBAL_ATOM_DECL(staticarp);		       \
    GLOBAL_ATOM_DECL(state);                           \
    GLOBAL_ATOM_DECL(status);                          \
    GLOBAL_ATOM_DECL(stream);                          \
    GLOBAL_ATOM_DECL(syncnt);                          \
    GLOBAL_ATOM_DECL(tclass);                          \
    GLOBAL_ATOM_DECL(tcp);                             \
    GLOBAL_ATOM_DECL(throughput);                      \
    GLOBAL_ATOM_DECL(timestamp);                       \
    GLOBAL_ATOM_DECL(tos);                             \
    GLOBAL_ATOM_DECL(transparent);                     \
    GLOBAL_ATOM_DECL(timeout);                         \
    GLOBAL_ATOM_DECL(true);                            \
    GLOBAL_ATOM_DECL(trunc);                           \
    GLOBAL_ATOM_DECL(ttl);                             \
    GLOBAL_ATOM_DECL(tunnel);                          \
    GLOBAL_ATOM_DECL(tunnel6);                         \
    GLOBAL_ATOM_DECL(type);                            \
    GLOBAL_ATOM_DECL(udp);                             \
    GLOBAL_ATOM_DECL(unblock_source);                  \
    GLOBAL_ATOM_DECL(undefined);                       \
    GLOBAL_ATOM_DECL(unicast_hops);                    \
    GLOBAL_ATOM_DECL(unspec);                          \
    GLOBAL_ATOM_DECL(up);                              \
    GLOBAL_ATOM_DECL(usec);                            \
    GLOBAL_ATOM_DECL(user);                            \
    GLOBAL_ATOM_DECL(user_timeout);                    \
    GLOBAL_ATOM_DECL(use_ext_recvinfo);                \
    GLOBAL_ATOM_DECL(use_min_mtu);                     \
    GLOBAL_ATOM_DECL(use_registry);                    \
    GLOBAL_ATOM_DECL(value);                           \
    GLOBAL_ATOM_DECL(void);                            \
    GLOBAL_ATOM_DECL(v6only);                          \
    GLOBAL_ATOM_DECL(write_byte);                      \
    GLOBAL_ATOM_DECL(write_fails);                     \
    GLOBAL_ATOM_DECL(write_pkg);                       \
    GLOBAL_ATOM_DECL(write_tries);                     \
    GLOBAL_ATOM_DECL(write_waits);                     \
    GLOBAL_ATOM_DECL(zero)



/* *** Global error reason atoms *** */
#define GLOBAL_ERROR_REASON_ATOMS     \
    GLOBAL_ATOM_DECL(eagain);         \
    GLOBAL_ATOM_DECL(einval);         \
    GLOBAL_ATOM_DECL(select_read);    \
    GLOBAL_ATOM_DECL(select_write)


#define GLOBAL_ATOM_DECL(A) ERL_NIF_TERM esock_atom_##A
GLOBAL_ATOMS;
GLOBAL_ERROR_REASON_ATOMS;
#undef GLOBAL_ATOM_DECL
ERL_NIF_TERM esock_atom_socket_tag; // This has a "special" name ('$socket')

/* *** Local atoms *** */
#define LOCAL_ATOMS                    \
    LOCAL_ATOM_DECL(accepting);	       \
    LOCAL_ATOM_DECL(adaptation_layer); \
    LOCAL_ATOM_DECL(add);              \
    LOCAL_ATOM_DECL(addr_unreach);     \
    LOCAL_ATOM_DECL(address);          \
    LOCAL_ATOM_DECL(adm_prohibited);   \
    LOCAL_ATOM_DECL(association);      \
    LOCAL_ATOM_DECL(assoc_id);         \
    LOCAL_ATOM_DECL(authentication);   \
    LOCAL_ATOM_DECL(base_addr);        \
    LOCAL_ATOM_DECL(boolean);          \
    LOCAL_ATOM_DECL(bound);	       \
    LOCAL_ATOM_DECL(bufsz);            \
    LOCAL_ATOM_DECL(close);            \
    LOCAL_ATOM_DECL(closing);          \
    LOCAL_ATOM_DECL(code);             \
    LOCAL_ATOM_DECL(cookie_life);      \
    LOCAL_ATOM_DECL(counter_wrap);     \
    LOCAL_ATOM_DECL(counters);         \
    LOCAL_ATOM_DECL(ctype);            \
    LOCAL_ATOM_DECL(data_io);          \
    LOCAL_ATOM_DECL(debug_filename);   \
    LOCAL_ATOM_DECL(del);              \
    LOCAL_ATOM_DECL(dest_unreach);     \
    LOCAL_ATOM_DECL(dma);              \
    LOCAL_ATOM_DECL(do);               \
    LOCAL_ATOM_DECL(dont);             \
    LOCAL_ATOM_DECL(dtor);             \
    LOCAL_ATOM_DECL(exclude);          \
    LOCAL_ATOM_DECL(false);            \
    LOCAL_ATOM_DECL(frag_needed);      \
    LOCAL_ATOM_DECL(gifaddr);          \
    LOCAL_ATOM_DECL(gifbrdaddr);       \
    LOCAL_ATOM_DECL(gifconf);          \
    LOCAL_ATOM_DECL(gifdstaddr);       \
    LOCAL_ATOM_DECL(gifflags);         \
    LOCAL_ATOM_DECL(gifhwaddr);        \
    LOCAL_ATOM_DECL(gifindex);         \
    LOCAL_ATOM_DECL(gifmap);           \
    LOCAL_ATOM_DECL(gifmtu);           \
    LOCAL_ATOM_DECL(gifname);          \
    LOCAL_ATOM_DECL(gifnetmask);       \
    LOCAL_ATOM_DECL(giftxqlen);        \
    LOCAL_ATOM_DECL(host_unknown);     \
    LOCAL_ATOM_DECL(host_unreach);     \
    LOCAL_ATOM_DECL(how);              \
    LOCAL_ATOM_DECL(in4_sockaddr);     \
    LOCAL_ATOM_DECL(in6_sockaddr);     \
    LOCAL_ATOM_DECL(include);          \
    LOCAL_ATOM_DECL(initial);          \
    LOCAL_ATOM_DECL(interface);        \
    LOCAL_ATOM_DECL(integer);          \
    LOCAL_ATOM_DECL(ioctl_flags);      \
    LOCAL_ATOM_DECL(ioctl_requests);   \
    LOCAL_ATOM_DECL(iov_max);          \
    LOCAL_ATOM_DECL(iow);              \
    LOCAL_ATOM_DECL(io_num_threads);   \
    LOCAL_ATOM_DECL(irq);              \
    LOCAL_ATOM_DECL(listening);	       \
    LOCAL_ATOM_DECL(local_rwnd);       \
    LOCAL_ATOM_DECL(map);              \
    LOCAL_ATOM_DECL(max);              \
    LOCAL_ATOM_DECL(max_attempts);     \
    LOCAL_ATOM_DECL(max_init_timeo);   \
    LOCAL_ATOM_DECL(max_instreams);    \
    LOCAL_ATOM_DECL(asocmaxrxt);       \
    LOCAL_ATOM_DECL(mem_end);          \
    LOCAL_ATOM_DECL(mem_start);        \
    LOCAL_ATOM_DECL(min);              \
    LOCAL_ATOM_DECL(missing);          \
    LOCAL_ATOM_DECL(mode);             \
    LOCAL_ATOM_DECL(msg);              \
    LOCAL_ATOM_DECL(msg_flags);        \
    LOCAL_ATOM_DECL(mtu);	       \
    LOCAL_ATOM_DECL(multiaddr);        \
    LOCAL_ATOM_DECL(net_unknown);      \
    LOCAL_ATOM_DECL(net_unreach);      \
    LOCAL_ATOM_DECL(nogroup);	       \
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
    LOCAL_ATOM_DECL(port);             \
    LOCAL_ATOM_DECL(port_unreach);     \
    LOCAL_ATOM_DECL(probe);            \
    LOCAL_ATOM_DECL(protocols);        \
    LOCAL_ATOM_DECL(rcvctrlbuf);       \
    LOCAL_ATOM_DECL(read);             \
    LOCAL_ATOM_DECL(read_pkg_max);     \
    LOCAL_ATOM_DECL(read_waits);       \
    LOCAL_ATOM_DECL(read_write);       \
    LOCAL_ATOM_DECL(registry);         \
    LOCAL_ATOM_DECL(reject_route);     \
    LOCAL_ATOM_DECL(remote);           \
    LOCAL_ATOM_DECL(rstates);          \
    LOCAL_ATOM_DECL(selected);         \
    LOCAL_ATOM_DECL(sender_dry);       \
    LOCAL_ATOM_DECL(send_failure);     \
    LOCAL_ATOM_DECL(shutdown);         \
    LOCAL_ATOM_DECL(sifaddr);          \
    LOCAL_ATOM_DECL(sifbrdaddr);       \
    LOCAL_ATOM_DECL(sifdstaddr);       \
    LOCAL_ATOM_DECL(sifflags);         \
    LOCAL_ATOM_DECL(sifmtu);           \
    LOCAL_ATOM_DECL(sifnetmask);       \
    LOCAL_ATOM_DECL(siftxqlen);        \
    LOCAL_ATOM_DECL(slist);            \
    LOCAL_ATOM_DECL(sndctrlbuf);       \
    LOCAL_ATOM_DECL(sockaddr);         \
    LOCAL_ATOM_DECL(socket_debug);     \
    LOCAL_ATOM_DECL(socket_level);     \
    LOCAL_ATOM_DECL(socket_option);    \
    LOCAL_ATOM_DECL(sourceaddr);       \
    LOCAL_ATOM_DECL(time_exceeded);    \
    LOCAL_ATOM_DECL(true);             \
    LOCAL_ATOM_DECL(txqlen);	       \
    LOCAL_ATOM_DECL(txstatus);         \
    LOCAL_ATOM_DECL(txtime);           \
    LOCAL_ATOM_DECL(want);             \
    LOCAL_ATOM_DECL(write);            \
    LOCAL_ATOM_DECL(write_pkg_max);    \
    LOCAL_ATOM_DECL(wstates);          \
    LOCAL_ATOM_DECL(zerocopy)

/* Local error reason atoms 
 * Keep this (commented) for future use...
 */
/*
#define LOCAL_ERROR_REASON_ATOMS                \
    LOCAL_ATOM_DECL(select_read);               \
    LOCAL_ATOM_DECL(select_write)
*/
#define LOCAL_ATOM_DECL(LA) static ERL_NIF_TERM atom_##LA
LOCAL_ATOMS;
// LOCAL_ERROR_REASON_ATOMS;
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

/* Jump table for the I/O backend (async or sync) */
static ESockIoBackend io_backend = {0};


/* This, the test for NULL), is temporary until we have a win stub */
#define ESOCK_IO_INIT(NUMT)                                     \
    ((io_backend.init != NULL) ?                                \
     io_backend.init((NUMT)) : ESOCK_IO_ERR_UNSUPPORTED)
#define ESOCK_IO_FIN()                                          \
    ((io_backend.finish != NULL) ?                              \
     io_backend.finish() : ESOCK_IO_ERR_UNSUPPORTED)

#define ESOCK_IO_OPEN_WITH_FD(ENV, FD, EOPTS)                   \
    ((io_backend.open_with_fd != NULL) ?                        \
     io_backend.open_with_fd((ENV), (FD), (EOPTS), &data) :     \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_OPEN_PLAIN(ENV, D, T, P, EOPTS)              \
    ((io_backend.open_plain != NULL) ?                        \
     io_backend.open_plain((ENV), (D),                        \
                           (T), (P), (EOPTS), &data) :        \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_BIND(ENV, D, SAP, AL)                  \
    ((io_backend.bind != NULL) ?                        \
     io_backend.bind((ENV), (D), (SAP), (AL)) :         \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_CONNECT(ENV, D, SR, CR, AP, AL)                 \
    ((io_backend.connect != NULL) ?                              \
     io_backend.connect((ENV), (D),                              \
                        (SR), (CR), (AP), (AL)) :                \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_LISTEN(ENV, D, BL)         \
    ((io_backend.listen != NULL) ?          \
     io_backend.listen((ENV), (D), (BL)) :  \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_ACCEPT(ENV, D, SR, AR)                 \
    ((io_backend.accept != NULL) ?                      \
     io_backend.accept((ENV), (D), (SR), (AR)) :        \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_SEND(ENV, D, SR, RF, L, F)                             \
    ((io_backend.send != NULL) ?                                        \
     io_backend.send((ENV), (D),                                        \
                     (SR), (RF), (L), (F)) :                            \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_SENDTO(ENV, D,                           \
                        SOCKR, SENDR,                     \
                        DP, F, TAP, TAL)                  \
    ((io_backend.sendto != NULL) ?                        \
     io_backend.sendto((ENV), (D),                        \
                       (SOCKR), (SENDR),                  \
                       (DP), (F), (TAP), (TAL)) :         \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_SENDMSG(ENV, D,                                \
                         SOCKR, SENDR, EM, F, EIOV)             \
    ((io_backend.sendmsg != NULL) ?                             \
     io_backend.sendmsg((ENV), (D),                             \
                        (SOCKR), (SENDR),                       \
                        (EM), (F), (EIOV), &data) :             \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_SENDFILE_START(ENV, D,                         \
                                SOR, SNR,                       \
                                O, CN, FR)                      \
    ((io_backend.sendfile_start != NULL) ?                      \
     io_backend.sendfile_start((ENV), (D),                      \
                               (SOR), (SNR),                    \
                               (O), (CN), (FR)) :               \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_SENDFILE_CONT(ENV, D,                          \
                               SOR, SNR,                        \
                               O, CP)                           \
    ((io_backend.sendfile_cont != NULL) ?                       \
     io_backend.sendfile_cont((ENV), (D),                       \
                              (SOR), (SNR),                     \
                              (O), (CP)) :                      \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_SENDFILE_DC(ENV, D)                            \
    ((io_backend.sendfile_dc != NULL) ?                         \
     io_backend.sendfile_dc((ENV), (D)) :                       \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_RECV(ENV, D,                         \
                      SR, RR, L, F)                   \
    ((io_backend.recv != NULL) ?                      \
     io_backend.recv((ENV), (D),                      \
                     (SR), (RR), (L), (F)) :          \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_RECVFROM(ENV, D,                         \
                          SR, RR, L, F)                   \
    ((io_backend.recvfrom != NULL) ?                      \
     io_backend.recvfrom((ENV), (D),                      \
                         (SR), (RR), (L), (F)) :          \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_RECVMSG(ENV, D,                          \
                         SR, RR, BL, CL, F)               \
    ((io_backend.recvmsg != NULL) ?                       \
     io_backend.recvmsg((ENV), (D),                       \
                        (SR), (RR),                       \
                        (BL), (CL), (F)) :                \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_CLOSE(ENV, D)                  \
    ((io_backend.close != NULL) ?               \
     io_backend.close((ENV), (D)) :             \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_FIN_CLOSE(ENV, D)                  \
    ((io_backend.fin_close != NULL) ?               \
     io_backend.fin_close((ENV), (D)) :             \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_SHUTDOWN(ENV, D, H)        \
    ((io_backend.shutdown != NULL) ?        \
     io_backend.shutdown((ENV), (D), (H)) : \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_SOCKNAME(ENV, D)               \
    ((io_backend.sockname != NULL) ?            \
     io_backend.sockname((ENV), (D)) :          \
     enif_raise_exception(env, MKA(env, "notsup")))
#define ESOCK_IO_PEERNAME(ENV, D)               \
    ((io_backend.peername != NULL) ?            \
     io_backend.peername((ENV), (D)) :          \
     enif_raise_exception(env, MKA(env, "notsup")))



/* These two (inline) functions are primarily intended for debugging,
 * that is, to make it easy to add debug printouts.
 */
// static ESOCK_INLINE void esock_free_env(const char* slogan, ErlNifEnv* env)
extern void esock_free_env(const char* slogan, ErlNifEnv* env)
{
    SGDBG( ("SOCKET", "env free - %s: 0x%lX\r\n", slogan, env) );
    // esock_dbg_printf("SOCK ENV", "free - %s: 0x%lX\r\n", slogan, env);

    if (env != NULL) enif_free_env(env);
}


// static ESOCK_INLINE ErlNifEnv* esock_alloc_env(const char* slogan)
extern ErlNifEnv* esock_alloc_env(const char* slogan)
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
                          esock_atom_use_registry,
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
      TARRAY_ADD(estate, esock_atom_connecting);
    }

    if ((state & ESOCK_STATE_CONNECTED) != 0) {
      /*
      SSDBG( descP, ("SOCKET", "esock_socket_info_state {%d} -> connected"
		     "\r\n", descP->sock) );
      */
      TARRAY_ADD(estate, esock_atom_connected);
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
      TARRAY_ADD(estate, esock_atom_closed);
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
    ERL_NIF_TERM keys[] = {esock_atom_read_byte,
                           esock_atom_read_fails,
                           esock_atom_read_pkg,
                           atom_read_pkg_max,
                           esock_atom_read_tries,
                           atom_read_waits,
                           esock_atom_write_byte,
                           esock_atom_write_fails,
                           esock_atom_write_pkg,
                           atom_write_pkg_max,
                           esock_atom_write_tries,
                           esock_atom_write_waits,
                           esock_atom_acc_success,
                           esock_atom_acc_fails,
                           esock_atom_acc_tries,
                           esock_atom_acc_waits};
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
            {esock_atom_sendfile,
             esock_atom_sendfile_byte,
             esock_atom_sendfile_fails,
             esock_atom_sendfile_max,
             esock_atom_sendfile_pkg,
             esock_atom_sendfile_pkg_max,
             esock_atom_sendfile_tries,
             esock_atom_sendfile_waits},
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
    ERL_NIF_TERM command, cdata, result;

    ESOCK_ASSERT( argc == 1 );

    SGDBG( ("SOCKET", "nif_command -> entry with %d args\r\n", argc) );

    if (! GET_MAP_VAL(env, argv[0], esock_atom_command, &command)) {
        SGDBG( ("SOCKET",
                "nif_command -> field not found: command\r\n") );
        return enif_make_badarg(env);
    }
    if (! GET_MAP_VAL(env, argv[0], esock_atom_data, &cdata)) {
        SGDBG( ("SOCKET",
                "nif_command -> field not found: data\r\n") );
        return enif_make_badarg(env);
    }

    SGDBG( ("SOCKET", "nif_command -> "
            "\r\n   command:  %T"
            "\r\n   cdata:     %T"
            "\r\n", command, cdata) );

    result = esock_command(env, command, cdata);

    SGDBG( ("SOCKET", "nif_command -> done with result: "
           "\r\n   %T"
           "\r\n", result) );

    return result;

#endif // #ifdef __WIN32__ #else
}


#ifndef __WIN32__
static
ERL_NIF_TERM
esock_command(ErlNifEnv* env, ERL_NIF_TERM command, ERL_NIF_TERM cdata)
{
    int cmp;

    SGDBG( ("SOCKET", "esock_command -> entry with %T\r\n", command) );

    cmp = COMPARE(command, atom_socket_debug);
    if (cmp == 0) {
        return esock_command_socket_debug(env, cdata);
    } else if (cmp < 0) {
        if (COMPARE(command, esock_atom_debug) == 0)
            return esock_command_debug(env, cdata);
    } else { // 0 < cmp
        if (COMPARE(command, esock_atom_use_registry) == 0)
            return esock_command_use_socket_registry(env, cdata);
    }

    SGDBG( ("SOCKET", "esock_command -> invalid command: %T\r\n",
            command) );

    return esock_raise_invalid(env, MKT2(env, esock_atom_command, command));
}
#endif // #ifndef __WIN32__


#ifndef __WIN32__
static
ERL_NIF_TERM esock_command_debug(ErlNifEnv* env, ERL_NIF_TERM cdata)
{
    ERL_NIF_TERM result;

    /* The data *should* be a boolean() */
    if (esock_decode_bool(cdata, &data.dbg))
        result = esock_atom_ok;
    else
        result =
            esock_raise_invalid(env, MKT2(env, esock_atom_data, cdata));

    return result;
}
#endif // #ifndef __WIN32__


#ifndef __WIN32__
static
ERL_NIF_TERM esock_command_socket_debug(ErlNifEnv* env, ERL_NIF_TERM cdata)
{
    BOOLEAN_T dbg;

    /* The data *should* be a boolean() */
    if (! esock_decode_bool(cdata, &dbg))
        return
            esock_raise_invalid(env, MKT2(env, esock_atom_data, cdata));

    MLOCK(data.cntMtx);
    data.sockDbg = dbg;
    MUNLOCK(data.cntMtx);

    return esock_atom_ok;;
}
#endif // #ifndef __WIN32__


#ifndef __WIN32__
static
ERL_NIF_TERM esock_command_use_socket_registry(ErlNifEnv*   env,
                                               ERL_NIF_TERM cdata)
{
    BOOLEAN_T useReg = FALSE;

    /* The data *should* be a boolean() */
    if (! esock_decode_bool(cdata, &useReg))
        return
            esock_raise_invalid(env, MKT2(env, esock_atom_data, cdata));

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
    TARRAY_ADD(opts, MKT2(env, esock_atom_netns, is_supported));

#if defined(HAVE_SENDFILE)
    is_supported = esock_atom_true;
#else
    is_supported = esock_atom_false;
#endif
    TARRAY_ADD(opts, MKT2(env, esock_atom_sendfile, is_supported));

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
    else if (COMPARE(key, atom_ioctl_requests) == 0)
      result = esock_supports_ioctl_requests(env);
    else if (COMPARE(key, atom_ioctl_flags) == 0)
      result = esock_supports_ioctl_flags(env);
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

  protocols = MKEL(env);

#if defined(HAVE_GETPROTOENT) &&		\
  defined(HAVE_SETPROTOENT) &&			\
  defined(HAVE_ENDPROTOENT)

  {
    struct protoent *pe;
    int stayopen;

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
  }
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
ERL_NIF_TERM esock_supports_ioctl_requests(ErlNifEnv* env)
{
  ERL_NIF_TERM requests;

  requests = MKEL(env);

  /* --- GET REQUESTS --- */
#if defined(SIOCGIFNAME)
  requests = MKC(env, MKT2(env, atom_gifname, MKUL(env, SIOCGIFNAME)), requests);
#endif

#if defined(SIOCGIFINDEX)
  requests = MKC(env, MKT2(env, atom_gifindex, MKUL(env, SIOCGIFINDEX)), requests);
#endif

#if defined(SIOCGIFFLAGS)
  requests = MKC(env, MKT2(env, atom_gifflags, MKUL(env, SIOCGIFFLAGS)), requests);
#endif

#if defined(SIOCGIFADDR)
  requests = MKC(env, MKT2(env, atom_gifaddr, MKUL(env, SIOCGIFADDR)), requests);
#endif

#if defined(SIOCGIFDSTADDR)
  requests = MKC(env, MKT2(env, atom_gifdstaddr, MKUL(env, SIOCGIFDSTADDR)), requests);
#endif

#if defined(SIOCGIFBRDADDR)
  requests = MKC(env, MKT2(env, atom_gifbrdaddr, MKUL(env, SIOCGIFBRDADDR)), requests);
#endif

#if defined(SIOCGIFNETMASK)
  requests = MKC(env, MKT2(env, atom_gifnetmask, MKUL(env, SIOCGIFNETMASK)), requests);
#endif

#if defined(SIOCGIFMTU)
  requests = MKC(env, MKT2(env, atom_gifmtu, MKUL(env, SIOCGIFMTU)), requests);
#endif

#if defined(SIOCGIFHWADDR) && defined(ESOCK_USE_HWADDR)
  requests = MKC(env, MKT2(env, atom_gifhwaddr, MKUL(env, SIOCGIFHWADDR)), requests);
#endif

#if defined(SIOCGIFMAP) && defined(ESOCK_USE_IFMAP)
  requests = MKC(env, MKT2(env, atom_gifmap, MKUL(env, SIOCGIFMAP)), requests);
#endif

#if defined(SIOCGIFTXQLEN)
  requests = MKC(env, MKT2(env, atom_giftxqlen, MKUL(env, SIOCGIFTXQLEN)), requests);
#endif

#if defined(SIOCGIFCONF)
  requests = MKC(env, MKT2(env, atom_gifconf, MKUL(env, SIOCGIFCONF)), requests);
#endif

  /* --- SET REQUESTS --- */
#if defined(SIOCSIFFLAGS)
  requests = MKC(env, MKT2(env, atom_sifflags, MKUL(env, SIOCSIFFLAGS)), requests);
#endif

#if defined(SIOCSIFADDR)
  requests = MKC(env, MKT2(env, atom_sifaddr, MKUL(env, SIOCSIFADDR)), requests);
#endif

#if defined(SIOCSIFDSTADDR)
  requests = MKC(env, MKT2(env, atom_sifdstaddr, MKUL(env, SIOCSIFDSTADDR)), requests);
#endif

#if defined(SIOCSIFBRDADDR)
  requests = MKC(env, MKT2(env, atom_sifbrdaddr, MKUL(env, SIOCSIFBRDADDR)), requests);
#endif

#if defined(SIOCSIFMTU)
  requests = MKC(env, MKT2(env, atom_sifmtu, MKUL(env, SIOCSIFMTU)), requests);
#endif

#if defined(SIOCSIFTXQLEN)
  requests = MKC(env, MKT2(env, atom_siftxqlen, MKUL(env, SIOCSIFTXQLEN)), requests);
#endif

  return requests;
}
#endif // #ifndef __WIN32__


#ifndef __WIN32__

static ERL_NIF_TERM esock_supports_ioctl_flags(ErlNifEnv* env)
{
  size_t       n;
  ERL_NIF_TERM result;

  result = MKEL(env);
  for (n = 0;  n < NUM(ioctl_flags);  n++) {
    result =
      MKC(env,
	  MKT2(env,
	       *(ioctl_flags[n].name),
	       MKI(env, ioctl_flags[n].flag)),
	  result);
  }

  return result;
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
	result = ESOCK_IO_OPEN_WITH_FD(env, fd, eopts);
	MUNLOCK(data.cntMtx);

    } else {
        ERL_NIF_TERM edomain, etype, eproto, eopts;
	int          domain, type, proto;

	ESOCK_ASSERT( argc == 4 );

	/* Extract arguments and perform preliminary validation */

	edomain = argv[0];
	etype   = argv[1];
        eproto  = argv[2];
	eopts   = argv[3];

	SGDBG( ("SOCKET", "nif_open -> "
		"\r\n   edomain: %T"
		"\r\n   etype:   %T"
		"\r\n   eproto:  %T"
		"\r\n   eopts:   %T"
		"\r\n", edomain, etype, eproto, eopts) );

	if (! GET_INT(env, eproto, &proto)) {
            if (IS_INTEGER(env, eproto))
                return esock_make_error_integer_range(env, eproto);
            else
                return enif_make_badarg(env);
        }
	if (! IS_MAP(env,  argv[3])) {
	    return enif_make_badarg(env);
	}

	if (esock_decode_domain(env, edomain, &domain) == 0) {
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
	result = ESOCK_IO_OPEN_PLAIN(env, domain, type, proto, eopts);
	MUNLOCK(data.cntMtx);
    }

    SGDBG( ("SOCKET", "nif_open -> done with result: "
            "\r\n   %T"
            "\r\n", result) );

    return result;

#endif // #ifdef __WIN32__  #else
}


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
    SOCKLEN_T        addrLen;

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

    ret = ESOCK_IO_BIND(env, descP, &sockAddr, addrLen);

    SSDBG( descP, ("SOCKET", "nif_bind(%T) -> done with"
                   "\r\n   ret: %T"
                   "\r\n", argv[0], ret) );

    MUNLOCK(descP->readMtx);

    return ret;
#endif // #ifdef __WIN32__  #else
}


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

    res = ESOCK_IO_CONNECT(env, descP, sockRef, connRef, addrP, addrLen);

    SSDBG( descP, ("SOCKET", "nif_connect(%T) -> done with"
                   "\r\n   res: %T"
                   "\r\n", sockRef, res) );

    MUNLOCK(descP->writeMtx);

    return res;

#endif // #ifdef __WIN32__  #else
}



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

    ret = ESOCK_IO_LISTEN(env, descP, backlog);

    SSDBG( descP, ("SOCKET", "nif_listen(%T) -> done with"
                   "\r\n   ret: %T"
                   "\r\n", argv[0], ret) );

    MUNLOCK(descP->readMtx);

    return ret;
#endif // #ifdef __WIN32__  #else
}




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
            ESOCK_MON2TERM(env, &descP->currentAcceptor.mon),
            descP->currentAcceptor.env,
            descP->currentAcceptor.ref) );

    res = ESOCK_IO_ACCEPT(env, descP, sockRef, ref);

    SSDBG( descP, ("SOCKET", "nif_accept(%T) -> done with"
                   "\r\n   res: %T"
                   "\r\n", sockRef, res) );

    MUNLOCK(descP->readMtx);

    return res;

#endif // #ifdef __WIN32__  #else
}


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

    res = ESOCK_IO_SEND(env, descP, sockRef, sendRef, &sndData, flags);

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

    res = ESOCK_IO_SENDTO(env, descP, sockRef, sendRef, &sndData, flags,
                          &remoteAddr, remoteAddrLen);

    SSDBG( descP, ("SOCKET", "nif_sendto(%T) -> done with"
                   "\r\n   res: %T"
                   "\r\n", sockRef, res) );

    MUNLOCK(descP->writeMtx);

    return res;

#endif // if defined(__WIN32__)
}


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

    res = ESOCK_IO_SENDMSG(env, descP, sockRef, sendRef, eMsg, flags, eIOV);

    MUNLOCK(descP->writeMtx);

    SSDBG( descP, ("SOCKET", "nif_sendmsg(%T) -> done with"
                   "\r\n   res: %T"
                   "\r\n", sockRef, res) );

    return res;

#endif // #ifdef __WIN32__  #else
}



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

        res = ESOCK_IO_SENDFILE_DC(env, descP);

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

            res = ESOCK_IO_SENDFILE_CONT(env, descP,
                                         sockRef, sendRef,
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

            res = ESOCK_IO_SENDFILE_START(env, descP,
                                          sockRef, sendRef,
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
        (COMPARE(recvRef, esock_atom_zero) != 0)) {
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

    res = ESOCK_IO_RECV(env, descP, sockRef, recvRef, len, flags);

    SSDBG( descP, ("SOCKET", "nif_recv(%T) -> done"
                   "\r\n", sockRef) );

    MUNLOCK(descP->readMtx);

    return res;

#endif // #ifdef __WIN32__  #else
}


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
        (COMPARE(recvRef, esock_atom_zero) != 0)) {
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

    res = ESOCK_IO_RECVFROM(env, descP, sockRef, recvRef, len, flags);

    SSDBG( descP, ("SOCKET", "nif_recvfrom(%T) -> done"
                   "\r\n", sockRef) );

    MUNLOCK(descP->readMtx);

    return res;
#endif // #ifdef __WIN32__  #else
}



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
        (COMPARE(recvRef, esock_atom_zero) != 0)) {
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

    res = ESOCK_IO_RECVMSG(env, descP, sockRef, recvRef, bufSz, ctrlSz, flags);

    SSDBG( descP, ("SOCKET", "nif_recvmsg(%T) -> done"
                   "\r\n", sockRef) );

    MUNLOCK(descP->readMtx);

    return res;
#endif // #ifdef __WIN32__  #else
}


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
    ERL_NIF_TERM     res;

    ESOCK_ASSERT( argc == 1 );

    SGDBG( ("SOCKET", "nif_close -> entry with argc: %d\r\n", argc) );

    if (! ESOCK_GET_RESOURCE(env, argv[0], (void**) &descP)) {
        return enif_make_badarg(env);
    }

    MLOCK(descP->readMtx);
    MLOCK(descP->writeMtx);

    SSDBG( descP,
           ("SOCKET", "nif_close(%T) ->"
            "\r\n      Socket:      %d"
            "\r\n      Read State:  0x%X"
            "\r\n      Write State: 0x%X"
            "\r\n      Caller:      %T"
            "\r\n",
            argv[0],
            descP->sock,
            descP->readState, descP->writeState,
            esock_self(env)) );

    res = ESOCK_IO_CLOSE(env, descP);

    MUNLOCK(descP->writeMtx);
    MUNLOCK(descP->readMtx);

    SSDBG( descP, ("SOCKET", "nif_close(%T) -> done"
                   "\r\n   res: %T"
                   "\r\n", argv[0], res) );

    return res;
#endif // #ifdef __WIN32__  #else
}


/* Prepare for close - return whether stop is scheduled
 */
#ifndef __WIN32__ // TEMPORARY - check the prim_socket_int.h file!
extern
BOOLEAN_T esock_do_stop(ErlNifEnv*       env,
                        ESockDescriptor* descP)
{
    BOOLEAN_T    ret;
    int          sres;
    ERL_NIF_TERM sockRef;

    sockRef = enif_make_resource(env, descP);

    if (IS_SELECTED(descP)) {
        ESOCK_ASSERT( (sres = esock_select_stop(env,
                                                (ErlNifEvent) descP->sock,
                                                descP))
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
                             descP, sockRef, &descP->writersQ, esock_atom_closed);

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
                             descP, sockRef, &descP->readersQ, esock_atom_closed);

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
                             descP, sockRef, &descP->acceptorsQ, esock_atom_closed);

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

    result = ESOCK_IO_FIN_CLOSE(env, descP);

    SSDBG( descP, ("SOCKET", "nif_finalize_close(%T) -> done with"
                   "\r\n   result: %T"
                   "\r\n", argv[0], result) );

    MUNLOCK(descP->writeMtx);
    MUNLOCK(descP->readMtx);

    return result;
#endif // #ifdef __WIN32__  #else
}


#ifndef __WIN32__  // TEMPORARY - check the prim_socket_int.h file!
extern
int esock_close_socket(ErlNifEnv*       env,
                       ESockDescriptor* descP,
                       BOOLEAN_T        unlock)
{
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
    esock_dec_socket(descP->domain, descP->type, descP->protocol);

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

    res = ESOCK_IO_SHUTDOWN(env, descP, how);

    MUNLOCK(descP->writeMtx);
    MUNLOCK(descP->readMtx);

    SSDBG( descP, ("SOCKET", "nif_shutdown(%T) -> done with"
                   "\r\n   res: %T"
                   "\r\n", argv[0], res) );

    return res;
#endif // #ifdef __WIN32__  #else
}



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
        return esock_make_error_closed(env);
    }

    if (! esock_decode_bool(eVal, &descP->dbg))
        return esock_make_invalid(env, esock_atom_value);

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
        return esock_make_error_closed(env);
    }

    if (! esock_decode_bool(eVal, &descP->iow))
      return esock_make_invalid(env, esock_atom_value);

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
        return esock_make_error_closed(env);
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
        return esock_make_invalid(env, esock_atom_value);
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
        return esock_make_error_closed(env);
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
            return esock_make_invalid(env, esock_atom_value);
        }
    }
    // We do not want a buffer size that does not fit in ssize_t
    z = bufSz;
    if (bufSz != (size_t) z)
        return esock_make_invalid(env, esock_atom_value);

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
        return esock_make_error_closed(env);
    }

    if (! esock_decode_bufsz(env,
                             eVal,
                             ESOCK_RECV_CTRL_BUFFER_SIZE_DEFAULT,
                             &val)) {
        SSDBG( descP,
               ("SOCKET",
                "esock_setopt_otp_rcvctrlbuf {%d} -> done invalid\r\n",
                descP->sock) );
        return esock_make_invalid(env, esock_atom_value);
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
        return esock_make_error_closed(env);
    }

    if (! esock_decode_bufsz(env,
                             eVal,
                             ESOCK_SEND_CTRL_BUFFER_SIZE_DEFAULT,
                             &val)) {
        SSDBG( descP,
               ("SOCKET",
                "esock_setopt_otp_sndctrlbuf {%d} -> done invalid\r\n",
                descP->sock) );
        return esock_make_invalid(env, esock_atom_value);
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
        return esock_make_error_closed(env);
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
        return esock_make_error_closed(env);
    }

    if (! esock_decode_bool(eVal, &useReg))
      return esock_make_invalid(env, esock_atom_value);

    /* We only allow turning this on! */
    if (! useReg)
        return esock_make_invalid(env, esock_atom_value);

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
        return esock_make_error_closed(env);
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
        result = esock_make_error_invalid(env, esock_atom_value);
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
        return esock_make_error_closed(env);
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
            return esock_make_invalid(env, esock_atom_value);
    }

    if ((! esock_decode_bool(eOnOff, &onOff)) ||
        (! GET_INT(env, eLinger, &val.l_linger)) ||
        (val.l_linger < 0)) {
        return esock_make_invalid(env, esock_atom_value);
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
        return esock_make_invalid(env, esock_atom_value);
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
        return esock_make_invalid(env, esock_atom_value);
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
        result = esock_make_invalid(env, esock_atom_value);
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
        result = esock_make_invalid(env, esock_atom_value);
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
    return esock_make_invalid(env, esock_atom_value);
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
    return esock_make_invalid(env, esock_atom_value);
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

    if (esock_decode_domain(env, eVal, &domain) == 0)
        return esock_make_invalid(env, esock_atom_value);

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
        return esock_make_invalid(env, esock_atom_value);

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
        return esock_make_invalid(env, esock_atom_value);

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
    return esock_make_invalid(env, esock_atom_value);
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
    return esock_make_invalid(env, esock_atom_value);
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

    return esock_make_invalid(env, esock_atom_value);
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
    return esock_make_invalid(env, esock_atom_value);
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
    return esock_make_invalid(env, esock_atom_value);
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
        return esock_make_invalid(env, esock_atom_value);

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
        result = esock_make_invalid(env, esock_atom_value);
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
        result = esock_make_invalid(env, esock_atom_value);
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
        return esock_make_invalid(env, esock_atom_value);

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
        return esock_make_error_closed(env);
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
        return esock_make_error_closed(env);
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
        return esock_make_error_closed(env);
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
        return esock_make_error_closed(env);
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
        return esock_make_error_closed(env);
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
        return esock_make_error_closed(env);
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
        return esock_make_error_closed(env);
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
        return esock_make_error_closed(env);
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
        return esock_make_error_closed(env);
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
        return esock_make_error_closed(env);
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
        return esock_make_error_closed(env);
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
        return esock_make_error_closed(env);
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
        return esock_make_error_closed(env);
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
            result = esock_make_invalid(env, esock_atom_value);
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
        result = esock_make_invalid(env, esock_atom_value);
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
        return esock_make_error_closed(env);
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
extern
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
        result = esock_make_error_invalid(env, esock_atom_data_size);
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
			       esock_atom_value};
	ERL_NIF_TERM vals[NUM(keys)];
	size_t       numKeys = NUM(keys);
	BOOLEAN_T    haveValue;
    
	vals[0] = esock_encode_level(env, currentP->cmsg_level);
	vals[2] = MKSBIN(env, ctrlBuf, dataPos, dataLen);

	haveValue = esock_encode_cmsg(env,
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

	/* Guard against cut-and-paste errors */
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

    res = ESOCK_IO_SOCKNAME(env, descP);

    SSDBG( descP,
           ("SOCKET", "nif_sockname(%T) {%d} -> done with res = %T\r\n",
            argv[0], descP->sock, res) );

    MUNLOCK(descP->readMtx);

    return res;
#endif // #ifdef __WIN32__  #else
}



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

  res = ESOCK_IO_PEERNAME(env, descP);

  SSDBG( descP,
	 ("SOCKET", "nif_peername(%T) {%d} -> done with res = %T\r\n",
	  argv[0], descP->sock, res) );

  MUNLOCK(descP->readMtx);

  return res;
#endif // #ifdef __WIN32__  #else
}



/* ----------------------------------------------------------------------
 * nif_ioctl - control device - get
 *
 * Description:
 * Returns whatever info the ioctl returns for the specific (get) request.
 * WHEN SET IS IMPLEMENTED, WE NED ANOTHER ARGUMENT!!
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * Request      - The ioctl get/set request
 * NameOrIdx    - Name or Index of the interface - only for some requests
 *                Currently only one (get) request does not provide this
 *                (and the value) argument; gifconf
 *                Currently, only one (get) request use index; gifname
 *                All other requests (get and set) use Name as "key".
 * Val          - Value to *set*
 *                This argument is *only* provided for set requests.
 */

static
ERL_NIF_TERM nif_ioctl(ErlNifEnv*         env,
		       int                argc,
		       const ERL_NIF_TERM argv[])
{
#ifdef __WIN32__
  return enif_raise_exception(env, MKA(env, "notsup"));
#else
  ESockDescriptor* descP;
  ERL_NIF_TERM     res;
  unsigned long    req;

  SGDBG( ("SOCKET", "nif_ioctl -> entry with argc: %d\r\n", argc) );

  ESOCK_ASSERT( (argc == 2) || (argc == 3) || (argc == 4) );

  if (! ESOCK_GET_RESOURCE(env, argv[0], (void**) &descP)) {
    SGDBG( ("SOCKET", "nif_ioctl -> no resource\r\n") );
    return enif_make_badarg(env);
  }

  if (! GET_ULONG(env, argv[1], &req)) {
    SGDBG( ("SOCKET", "nif_ioctl -> 'request' not 'unsigned long'\r\n") );
    return enif_make_badarg(env);
  }

  SSDBG( descP,
	 ("SOCKET", "nif_ioctl(%T) {%d} -> ioctl request %d"
	  "\r\n", argv[0], descP->sock, req) );

  MLOCK(descP->readMtx);

  if (! IS_OPEN(descP->readState)) {
    res = esock_make_error_closed(env);
  } else {

    if (argc == 2) {

      /* Only one request with this number of arguments: gifconf
       * Socket and request (=gifconf)
       */

      /* Two arguments: Socket and get request */
      res = esock_ioctl1(env, descP, req);

    } else if (argc == 3) {

      /* (Currently) All *other* get requests has 3 arguments
       * Socket, request and name/index
       */

      ERL_NIF_TERM earg = argv[2];

      /* Two arguments: request and arg */
      res = esock_ioctl2(env, descP, req, earg);

    } else if (argc == 4) {

      /* (Currently) Set requests has 4 arguments
       * Socket, request, name and value
       */

      ERL_NIF_TERM earg1 = argv[2]; // (currently) Name
      ERL_NIF_TERM earg2 = argv[3]; // Value

      /* Three arguments: request, arg1 (name) and arg2 (value) */
      res = esock_ioctl3(env, descP, req, earg1, earg2);

    } else {

      res = esock_make_error(env, esock_atom_einval);

    }
  }

  MUNLOCK(descP->readMtx);

  SSDBG( descP,
	 ("SOCKET", "nif_ioctl(%T) {%d} -> done with res = %T\r\n",
	  argv[0], descP->sock, res) );

  return res;
#endif // #ifdef __WIN32__  #else
}



#ifndef __WIN32__

static
ERL_NIF_TERM esock_ioctl1(ErlNifEnv*       env,
			  ESockDescriptor* descP,
			  unsigned long    req)
{
  switch (req) {

#if defined(SIOCGIFCONF)
  case SIOCGIFCONF:
    return esock_ioctl_gifconf(env, descP);
    break;
#endif

  default:
    return esock_make_error(env, esock_atom_enotsup);
    break;
  }

}


/* The type and value of 'arg' depend on the request,
 * which we have not yet "analyzed".
 *
 * Request     arg       arg type
 * -------     -------   --------
 * gifname     ifindex   integer
 * gifindex    name      string
 * gifflags    name      string
 * gifaddr     name      string
 * gifdstaddr  name      string
 * gifbdraddr  name      string
 * gifnetmask  name      string
 * gifmtu      name      string
 * gifhwaddr   name      string
 * gifmap      name      string
 * giftxqlen   name      string
 */
static
ERL_NIF_TERM esock_ioctl2(ErlNifEnv*       env,
			  ESockDescriptor* descP,
			  unsigned long    req,
			  ERL_NIF_TERM     arg)
{
  /* This for *get* requests */

  switch (req) {

#if defined(SIOCGIFNAME)
  case SIOCGIFNAME:
    return esock_ioctl_gifname(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFINDEX)
  case SIOCGIFINDEX:
    return esock_ioctl_gifindex(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFFLAGS)
  case SIOCGIFFLAGS:
    return esock_ioctl_gifflags(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFADDR)
  case SIOCGIFADDR:
    return esock_ioctl_gifaddr(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFDSTADDR)
  case SIOCGIFDSTADDR:
    return esock_ioctl_gifdstaddr(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFBRDADDR)
  case SIOCGIFBRDADDR:
    return esock_ioctl_gifbrdaddr(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFNETMASK)
  case SIOCGIFNETMASK:
    return esock_ioctl_gifnetmask(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFMTU)
  case SIOCGIFMTU:
    return esock_ioctl_gifmtu(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFHWADDR) && defined(ESOCK_USE_HWADDR)
  case SIOCGIFHWADDR:
    return esock_ioctl_gifhwaddr(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFMAP) && defined(ESOCK_USE_IFMAP)
  case SIOCGIFMAP:
    return esock_ioctl_gifmap(env, descP, arg);
    break;
#endif

#if defined(SIOCGIFTXQLEN)
  case SIOCGIFTXQLEN:
    return esock_ioctl_giftxqlen(env, descP, arg);
    break;
#endif

  default:
    return esock_make_error(env, esock_atom_enotsup);
    break;
  }

}


/* The type and value of arg(s) depend on the request,
 * which we have not yet "analyzed".
 *
 * Request     arg1      arg1 type    arg2     arg2 type
 * -------     -------   ---------    ------   ---------
 * sifflags    name      string       Flags    #{IntFlag := boolean()}
 *                                             IntFlag is the native flag
 * sifaddr     name      string       Addr     sockaddr()
 * sifdstaddr  name      string       DstAddr  sockaddr()
 * sifbrdaddr  name      string       BrdAddr  sockaddr()
 * sifnetmask  name      string       NetMask  sockaddr()
 * gifmtu      name      string       MTU      integer()
 * sifhwaddr   name      string       HwAddr   sockaddr()
 * giftxqlen   name      string       Len      integer()
 */
static
ERL_NIF_TERM esock_ioctl3(ErlNifEnv*       env,
			  ESockDescriptor* descP,
			  unsigned long    req,
			  ERL_NIF_TERM     ename,
			  ERL_NIF_TERM     eval)
{

  switch (req) {

#if defined(SIOCSIFFLAGS)
  case SIOCSIFFLAGS:
    return esock_ioctl_sifflags(env, descP, ename, eval);
    break;
#endif

#if defined(SIOCSIFADDR)
  case SIOCSIFADDR:
    return esock_ioctl_sifaddr(env, descP, ename, eval);
    break;
#endif

#if defined(SIOCSIFDSTADDR)
  case SIOCSIFDSTADDR:
    return esock_ioctl_sifdstaddr(env, descP, ename, eval);
    break;
#endif

#if defined(SIOCSIFBRDADDR)
  case SIOCSIFBRDADDR:
    return esock_ioctl_sifbrdaddr(env, descP, ename, eval);
    break;
#endif

#if defined(SIOCSIFNETMASK)
  case SIOCSIFNETMASK:
    return esock_ioctl_sifnetmask(env, descP, ename, eval);
    break;
#endif

#if defined(SIOCSIFMTU)
  case SIOCSIFMTU:
    return esock_ioctl_sifmtu(env, descP, ename, eval);
    break;
#endif

#if defined(SIOCSIFTXQLEN)
  case SIOCSIFTXQLEN:
    return esock_ioctl_siftxqlen(env, descP, ename, eval);
    break;
#endif

  default:
    return esock_make_error(env, esock_atom_enotsup);
    break;
  }

}


/* ===========================================================================
 * The implemented (ioctl) get requests falls into three grops:
 *
 *   1) gifconf - Takes no argument other then the request
 *   2) gifname - Takes the interface index (integer) as an argument
 *   3) other   - All other (get) requests takes the interface name (string)
 *                as the argument.
 *
 * The functions defined using the macros below are all in the third (3)
 * group.
 *
 */

/* *** esock_ioctl_gifindex *** */
#if defined(SIOCGIFINDEX)
#if defined(ESOCK_USE_IFINDEX)
#define IOCTL_GIFINDEX_FUNC_DECL					\
  IOCTL_GET_REQUEST_DECL(gifindex, SIOCGIFINDEX, ivalue, ifreq.ifr_ifindex)
#elif defined(ESOCK_USE_INDEX)
#define IOCTL_GIFINDEX_FUNC_DECL					\
  IOCTL_GET_REQUEST_DECL(gifindex, SIOCGIFINDEX, ivalue, ifreq.ifr_index)
#else
#define IOCTL_GIFINDEX_FUNC_DECL
#endif
#else
#define IOCTL_GIFINDEX_FUNC_DECL
#endif

/* *** esock_ioctl_gifflags *** */
#if defined(SIOCGIFFLAGS)
#define IOCTL_GIFFLAGS_FUNC_DECL					\
  IOCTL_GET_REQUEST_DECL(gifflags, SIOCGIFFLAGS, flags,  ifreq.ifr_flags)
#else
#define IOCTL_GIFFLAGS_FUNC_DECL
#endif

/* *** esock_ioctl_gifaddr *** */
#if defined(SIOCGIFADDR)
#define IOCTL_GIFADDR_FUNC_DECL						\
  IOCTL_GET_REQUEST_DECL(gifaddr, SIOCGIFADDR, ifraddr, &ifreq.ifr_addr)
#else
#define IOCTL_GIFADDR_FUNC_DECL
#endif

/* *** esock_ioctl_gifdstaddr *** */
#if defined(SIOCGIFDSTADDR)
#define IOCTL_GIFDSTADDR_FUNC_DECL					\
  IOCTL_GET_REQUEST_DECL(gifdstaddr, SIOCGIFDSTADDR, ifraddr, &ifreq.ifr_dstaddr)
#else
#define IOCTL_GIFDSTADDR_FUNC_DECL
#endif

/* *** esock_ioctl_gifbrdaddr *** */
#if defined(SIOCGIFBRDADDR)
#define IOCTL_GIFBRDADDR_FUNC_DECL					\
  IOCTL_GET_REQUEST_DECL(gifbrdaddr, SIOCGIFBRDADDR, ifraddr, &ifreq.ifr_broadaddr)
#else
#define IOCTL_GIFBRDADDR_FUNC_DECL
#endif

/* *** esock_ioctl_gifnetmask *** */
#if defined(SIOCGIFNETMASK)
#ifdef __linux__
#define IOCTL_GIFNETMASK_FUNC_DECL					\
  IOCTL_GET_REQUEST_DECL(gifnetmask, SIOCGIFNETMASK, ifraddr, &ifreq.ifr_netmask)
#else
#define IOCTL_GIFNETMASK_FUNC_DECL					\
  IOCTL_GET_REQUEST_DECL(gifnetmask, SIOCGIFNETMASK, ifraddr, &ifreq.ifr_addr)
#endif
#else
#define IOCTL_GIFNETMASK_FUNC_DECL
#endif

/* *** esock_ioctl_gifmtu *** */
#if defined(SIOCGIFMTU)
#define IOCTL_GIFMTU_FUNC_DECL						\
  IOCTL_GET_REQUEST_DECL(gifmtu, SIOCGIFMTU, ivalue,  ifreq.ifr_mtu)
#else
#define IOCTL_GIFMTU_FUNC_DECL
#endif

/* *** esock_ioctl_gifhwaddr *** */
#if defined(SIOCGIFHWADDR) && defined(ESOCK_USE_HWADDR)
#define IOCTL_GIFHWADDR_FUNC_DECL					\
  IOCTL_GET_REQUEST_DECL(gifhwaddr, SIOCGIFHWADDR, hwaddr, &ifreq.ifr_hwaddr)
#else
#define IOCTL_GIFHWADDR_FUNC_DECL
#endif

/* *** esock_ioctl_gifmap *** */
#if defined(SIOCGIFMAP) && defined(ESOCK_USE_IFMAP)
#define IOCTL_GIFMAP_FUNC_DECL						\
  IOCTL_GET_REQUEST_DECL(gifmap, SIOCGIFMAP, ifrmap, &ifreq.ifr_map)
#else
#define IOCTL_GIFMAP_FUNC_DECL
#endif

/* *** esock_ioctl_giftxqlen *** */
#if defined(SIOCGIFTXQLEN)
#define IOCTL_GIFTXQLEN_FUNC_DECL					\
  IOCTL_GET_REQUEST_DECL(giftxqlen, SIOCGIFTXQLEN, ivalue,  ifreq.ifr_qlen)
#else
#define IOCTL_GIFTXQLEN_FUNC_DECL
#endif

#define IOCTL_GET_FUNCS				\
  IOCTL_GIFINDEX_FUNC_DECL			\
  IOCTL_GIFFLAGS_FUNC_DECL			\
  IOCTL_GIFADDR_FUNC_DECL			\
  IOCTL_GIFDSTADDR_FUNC_DECL			\
  IOCTL_GIFBRDADDR_FUNC_DECL			\
  IOCTL_GIFNETMASK_FUNC_DECL			\
  IOCTL_GIFMTU_FUNC_DECL			\
  IOCTL_GIFHWADDR_FUNC_DECL			\
  IOCTL_GIFMAP_FUNC_DECL			\
  IOCTL_GIFTXQLEN_FUNC_DECL

#define IOCTL_GET_REQUEST_DECL(OR, R, EF, UV)				\
  static								\
  ERL_NIF_TERM esock_ioctl_##OR(ErlNifEnv*       env,			\
                                ESockDescriptor* descP,			\
			        ERL_NIF_TERM     ename)			\
  {									\
    ERL_NIF_TERM result;						\
    struct ifreq ifreq;							\
    char*        ifn = NULL;						\
    int          nlen;							\
									\
    SSDBG( descP, ("SOCKET", "esock_ioctl_" #OR " {%d} -> entry with"   \
		   "\r\n      (e)Name: %T"				\
		   "\r\n", descP->sock, ename) );                       \
									\
    if (!esock_decode_string(env, ename, &ifn))                         \
      return enif_make_badarg(env);                                     \
									\
    nlen = esock_strnlen(ifn, IFNAMSIZ);                                \
									\
    sys_memset(ifreq.ifr_name, '\0', IFNAMSIZ);                         \
    sys_memcpy(ifreq.ifr_name, ifn,                                     \
	       (nlen >= IFNAMSIZ) ? IFNAMSIZ-1 : nlen);                 \
									\
    SSDBG( descP,                                                       \
	   ("SOCKET",                                                   \
	    "esock_ioctl_" #OR " {%d} -> try ioctl\r\n",                \
	    descP->sock) );                                             \
									\
    if (ioctl(descP->sock, R, (char *) &ifreq) < 0) {                   \
      int          saveErrno = sock_errno();                            \
      ERL_NIF_TERM reason    = MKA(env, erl_errno_id(saveErrno));       \
									\
      SSDBG( descP,                                                     \
	     ("SOCKET", "esock_ioctl_" #OR " {%d} -> failure: "         \
	      "\r\n      reason: %T (%d)"                               \
	      "\r\n", descP->sock, reason, saveErrno) );                \
									\
      result = esock_make_error(env, reason);                           \
									\
    } else {                                                            \
      SSDBG( descP,                                                     \
	     ("SOCKET", "esock_ioctl_" #OR " {%d} -> encode value\r\n", \
	      descP->sock) );                                           \
      result = encode_ioctl_##EF(env, descP, UV);                       \
    }									\
									\
    FREE(ifn);								\
									\
    return result;                                                      \
									\
  }
IOCTL_GET_FUNCS
#undef IOCTL_GET_FUNCS


/* ===========================================================================
 * The "rest" of the implemented (ioctl) get requests
 *
 * These (get) requests could not be 'generated' by the macros above.
 */

static
ERL_NIF_TERM esock_ioctl_gifconf(ErlNifEnv*       env,
				 ESockDescriptor* descP)
{
  struct ifconf ifc;
  int           ifc_len = 0;
  int           buflen  = 100 * sizeof(struct ifreq);
  char         *buf     = MALLOC(buflen);
  ERL_NIF_TERM  result;

  SSDBG( descP, ("SOCKET", "esock_ioctl_gifconf {%d} -> entry\r\n", descP->sock) );

  for (;;) {
    ifc.ifc_len = buflen;
    ifc.ifc_buf = buf;
    if (ioctl(descP->sock, SIOCGIFCONF, (char *) &ifc) < 0) {
      int saveErrno = sock_errno();

      SSDBG( descP,
	     ("SOCKET", "esock_ioctl_gifconf {%d} -> failure: "
	      "\r\n      errno: %d (%s)"
	      "\r\n", descP->sock, saveErrno, erl_errno_id(saveErrno)) );

      if (saveErrno != EINVAL || ifc_len) {
	ERL_NIF_TERM reason = MKA(env, erl_errno_id(saveErrno));
	FREE(buf);
	return esock_make_error(env, reason);
      }
    } else {
      if (ifc.ifc_len == ifc_len) break; /* buf large enough */
      ifc_len = ifc.ifc_len;
    }
    buflen += 10 * sizeof(struct ifreq);
    buf     = (char *) REALLOC(buf, buflen);
  }

  result = encode_ioctl_ifconf(env, descP, &ifc);

  FREE(ifc.ifc_buf);

  return result;
}


#if defined(SIOCGIFNAME)
static
ERL_NIF_TERM esock_ioctl_gifname(ErlNifEnv*       env,
				 ESockDescriptor* descP,
				 ERL_NIF_TERM     eidx)
{
  ERL_NIF_TERM result;
  struct ifreq ifreq;
  int          index;
  
  SSDBG( descP, ("SOCKET", "esock_ioctl_gifname {%d} -> entry with"
		 "\r\n      (e)Index: %T"
		 "\r\n", descP->sock, eidx) );

  if (!GET_INT(env, eidx, &index))
    return enif_make_badarg(env);

  ifreq.ifr_ifindex = index;

  SSDBG( descP,
	 ("SOCKET", "esock_ioctl_gifname {%d} -> try ioctl\r\n", descP->sock) );

  if (ioctl(descP->sock, SIOCGIFNAME, (char *) &ifreq) < 0) {
    int          saveErrno = sock_errno();
    ERL_NIF_TERM reason    = MKA(env, erl_errno_id(saveErrno));

    SSDBG( descP,
	   ("SOCKET", "esock_ioctl_gifname {%d} -> failure: "
	    "\r\n      reason: %T (%d)"
	    "\r\n", descP->sock, reason, saveErrno) );

    result = esock_make_error(env, reason);

  } else {
    SSDBG( descP,
	   ("SOCKET", "esock_ioctl_gifname {%d} -> encode name\r\n",
	    descP->sock) );
    
    result = esock_make_ok2(env, encode_ioctl_ifreq_name(env, ifreq.ifr_name));
  }

  SSDBG( descP,
	 ("SOCKET", "esock_ioctl_gifname {%d} -> done with"
	  "\r\n      result: %T"
	  "\r\n",
	  descP->sock, result) );
    
  return result;

}
#endif




/* ===========================================================================
 * The implemented (ioctl) set requests:
 *
 */

/* *** esock_ioctl_sifaddr *** */
#if defined(SIOCSIFADDR)
#define IOCTL_SIFADDR_FUNC_DECL					\
  IOCTL_SET_REQUEST_DECL(sifaddr, SIOCSIFADDR, sockaddr,	\
			 ((ESockAddress*) &ifreq.ifr_addr))
#else
#define IOCTL_SIFADDR_FUNC_DECL
#endif

/* *** esock_ioctl_sifdstaddr *** */
#if defined(SIOCSIFDSTADDR)
#define IOCTL_SIFDSTADDR_FUNC_DECL				\
  IOCTL_SET_REQUEST_DECL(sifdstaddr, SIOCSIFDSTADDR, sockaddr,	\
			 ((ESockAddress*) &ifreq.ifr_dstaddr))
#else
#define IOCTL_SIFDSTADDR_FUNC_DECL
#endif

/* *** esock_ioctl_sifbrdaddr *** */
#if defined(SIOCSIFBRDADDR)
#define IOCTL_SIFBRDADDR_FUNC_DECL					\
  IOCTL_SET_REQUEST_DECL(sifbrdaddr, SIOCSIFBRDADDR, sockaddr,		\
			 ((ESockAddress*) &ifreq.ifr_broadaddr))
#else
#define IOCTL_SIFBRDADDR_FUNC_DECL
#endif

/* *** esock_ioctl_sifnetmask *** */
#if defined(SIOCSIFNETMASK)
#ifdef __linux__
#define IOCTL_SIFNETMASK_FUNC_DECL				\
  IOCTL_SET_REQUEST_DECL(sifnetmask, SIOCSIFNETMASK, sockaddr,	\
			 ((ESockAddress*) &ifreq.ifr_netmask))
#else
#define IOCTL_SIFNETMASK_FUNC_DECL				\
  IOCTL_SET_REQUEST_DECL(sifnetmask, SIOCSIFNETMASK, sockaddr,	\
			 ((ESockAddress*) &ifreq.ifr_addr))
#endif
#else
#define IOCTL_SIFNETMASK_FUNC_DECL
#endif

/* *** esock_ioctl_sifmtu ***
 * On some platforms, MTU is an unsigned int
 */
#if defined(SIOCSIFMTU)
#define IOCTL_SIFMTU_FUNC_DECL						\
  IOCTL_SET_REQUEST_DECL(sifmtu, SIOCSIFMTU, mtu, (int*) &ifreq.ifr_mtu)
#else
#define IOCTL_SIFMTU_FUNC_DECL
#endif

/* *** esock_ioctl_siftxqlen *** */
#if defined(SIOCSIFTXQLEN)
#define IOCTL_SIFTXQLEN_FUNC_DECL						\
  IOCTL_SET_REQUEST_DECL(siftxqlen, SIOCSIFTXQLEN, txqlen, &ifreq.ifr_qlen)
#else
#define IOCTL_SIFTXQLEN_FUNC_DECL
#endif

#define IOCTL_SET_FUNCS				\
  IOCTL_SIFADDR_FUNC_DECL			\
  IOCTL_SIFDSTADDR_FUNC_DECL			\
  IOCTL_SIFBRDADDR_FUNC_DECL			\
  IOCTL_SIFNETMASK_FUNC_DECL			\
  IOCTL_SIFMTU_FUNC_DECL			\
  IOCTL_SIFTXQLEN_FUNC_DECL

#define IOCTL_SET_REQUEST_DECL(OR, R, DF, UVP)				\
  static								\
  ERL_NIF_TERM esock_ioctl_##OR(ErlNifEnv*       env,			\
                                ESockDescriptor* descP,			\
				ERL_NIF_TERM     ename,			\
				ERL_NIF_TERM     evalue)		\
  {									\
    ERL_NIF_TERM result;						\
    struct ifreq ifreq;							\
    char*        ifn = NULL;						\
    int          nlen;							\
                                                                        \
    SSDBG( descP, ("SOCKET", "esock_ioctl_" #OR " {%d} -> entry with"	\
		   "\r\n      (e)Name:  %T"				\
		   "\r\n      (e)Value: %T"				\
		   "\r\n", descP->sock, ename, evalue) );		\
									\
    if (!esock_decode_string(env, ename, &ifn)) {			\
									\
      SSDBG( descP,							\
	     ("SOCKET", "esock_ioctl_" #OR " {%d} -> failed decode name" \
	      "\r\n", descP->sock) );					\
									\
      return enif_make_badarg(env);					\
    }									\
									\
    if (! decode_ioctl_##DF(env, descP, evalue, UVP)) {			\
									\
      SSDBG( descP,							\
	     ("SOCKET", "esock_ioctl_" #OR " {%d} -> failed decode addr" \
	      "\r\n", descP->sock) );					\
									\
      return esock_make_invalid(env, atom_##DF);			\
    }									\
									\
    nlen = esock_strnlen(ifn, IFNAMSIZ);				\
									\
    sys_memset(ifreq.ifr_name, '\0', IFNAMSIZ);			        \
    sys_memcpy(ifreq.ifr_name, ifn,					\
	       (nlen >= IFNAMSIZ) ? IFNAMSIZ-1 : nlen);			\
									\
    SSDBG( descP,							\
	   ("SOCKET", "esock_ioctl_" #OR " {%d} -> try ioctl\r\n",	\
	    descP->sock) );						\
									\
    if (ioctl(descP->sock, R, (char *) &ifreq) < 0) {			\
      int          saveErrno = sock_errno();				\
      ERL_NIF_TERM reason    = MKA(env, erl_errno_id(saveErrno));	\
									\
      SSDBG( descP,							\
	     ("SOCKET", "esock_ioctl_" #OR " {%d} -> failure: "		\
	      "\r\n      reason: %T (%d)"				\
	      "\r\n", descP->sock, reason, saveErrno) );		\
									\
      result = esock_make_error(env, reason);				\
									\
    } else {								\
      SSDBG( descP,							\
	     ("SOCKET", "esock_ioctl_" #OR " {%d} -> "			\
	      "addr successfully set\r\n",				\
	      descP->sock) );						\
      result = esock_atom_ok;						\
    }                                                                   \
                                                                        \
    FREE(ifn);                                                          \
                                                                        \
    return result;                                                      \
                                                                        \
  }

IOCTL_SET_FUNCS
#undef IOCTL_SET_FUNCS


/* ===========================================================================
 * The "rest" of the implemented (ioctl) set requests
 *
 * These (set) requests could not be 'generated' by the macros above.
 */

#if defined(SIOCSIFFLAGS)
static
ERL_NIF_TERM esock_ioctl_sifflags(ErlNifEnv*       env,
				  ESockDescriptor* descP,
				  ERL_NIF_TERM     ename,
				  ERL_NIF_TERM     eflags)
{
  ERL_NIF_TERM result;
  struct ifreq ifreq;
  char*        ifn = NULL;
  int          nlen;

  SSDBG( descP, ("SOCKET", "esock_ioctl_sifflags {%d} -> entry with"
		 "\r\n      (e)Name: %T"
		 "\r\n      (e)Flags: %T"
		 "\r\n", descP->sock, ename, eflags) );

  if (!esock_decode_string(env, ename, &ifn)) {

    SSDBG( descP,
	   ("SOCKET", "esock_ioctl_sifflags {%d} -> failed decode name"
	    "\r\n", descP->sock) );

    return enif_make_badarg(env);
  }

  // Make sure the length of the string is valid!
  nlen = esock_strnlen(ifn, IFNAMSIZ);
  
  sys_memset(ifreq.ifr_name, '\0', IFNAMSIZ); // Just in case
  sys_memcpy(ifreq.ifr_name, ifn, 
	     (nlen >= IFNAMSIZ) ? IFNAMSIZ-1 : nlen);
  
  SSDBG( descP,
	 ("SOCKET", "esock_ioctl_sifflags {%d} -> try (get) ioctl\r\n",
	  descP->sock) );

  if (ioctl(descP->sock, SIOCGIFFLAGS, (char *) &ifreq) < 0) {
    int          saveErrno = sock_errno();
    ERL_NIF_TERM reason    = MKA(env, erl_errno_id(saveErrno));

    SSDBG( descP,
	   ("SOCKET", "esock_ioctl_sifflags {%d} -> "
	    "failure: failed reading *current* flags"
	    "\r\n      reason: %T (%d)"
	    "\r\n", descP->sock, reason, saveErrno) );

    result = esock_make_error(env, reason);

  } else {

    SSDBG( descP,
	   ("SOCKET", "esock_ioctl_sifflags {%d} -> (local) update flags\r\n",
	    descP->sock) );

    if (decode_ioctl_flags(env, descP, eflags, &ifreq.ifr_flags)) {

      SSDBG( descP,
	     ("SOCKET", "esock_ioctl_sifflags {%d} -> try (set) ioctl\r\n",
	      descP->sock) );

      if (ioctl(descP->sock, SIOCSIFFLAGS, (char *) &ifreq) < 0) {
	int          saveErrno = sock_errno();
	ERL_NIF_TERM reason    = MKA(env, erl_errno_id(saveErrno));

	SSDBG( descP,
	       ("SOCKET", "esock_ioctl_sifflags {%d} -> failure: "
		"\r\n      reason: %T (%d)"
		"\r\n", descP->sock, reason, saveErrno) );

	result = esock_make_error(env, reason);

      } else {
	SSDBG( descP,
	       ("SOCKET", "esock_ioctl_sifflags {%d} -> "
		"updated flags successfully set\r\n",
		descP->sock) );
	result = esock_atom_ok;
      }

      /* We know that if esock_decode_string is successful,
       * we have "some" form of string, and therefor memory
       * has been allocated (and need to be freed)... */
      FREE(ifn);

    } else {
      result = enif_make_badarg(env);
    }
  }

  SSDBG( descP,
	 ("SOCKET", "esock_ioctl_sifflags {%d} -> done with result: "
	  "\r\n      %T"
	  "\r\n",
	  descP->sock, result) );

  return result;

}
#endif



/* ===========================================================================
 * ioctl utility functions
 *
 */

static
ERL_NIF_TERM encode_ioctl_ifconf(ErlNifEnv*       env,
				 ESockDescriptor* descP,
				 struct ifconf*   ifcP)
{
  ERL_NIF_TERM result;
  unsigned int len = ((ifcP == NULL) ? 0 :
		      (ifcP->ifc_len / sizeof(struct ifreq)));

  SSDBG( descP,
	 ("SOCKET", "encode_ioctl_ifconf -> entry (when len = %d)\r\n", len) );

  if (len > 0) {
    ERL_NIF_TERM* array = MALLOC(len * sizeof(ERL_NIF_TERM));
    unsigned int  i     = 0;
    struct ifreq* p     = ifcP->ifc_req;

    for (i = 0 ; i < len ; i++) {
      SSDBG( descP,
	     ("SOCKET", "encode_ioctl_ifconf -> encode ifreq entry %d\r\n", i) );
      array[i] = encode_ioctl_ifconf_ifreq(env, descP, &p[i]);
    }

    SSDBG( descP,
	   ("SOCKET", "encode_ioctl_ifconf -> all entries encoded\r\n", i) );

    result = esock_make_ok2(env, MKLA(env, array, len));
    FREE(array);

  } else {

    result = esock_make_ok2(env, MKEL(env));

  }

  return result;
}


#if defined(SIOCGIFMAP) && defined(ESOCK_USE_IFMAP)
static
ERL_NIF_TERM encode_ioctl_ifrmap(ErlNifEnv*       env,
				 ESockDescriptor* descP,
				 struct ifmap*    mapP)
{
  ERL_NIF_TERM mapKeys[] = {atom_mem_start,
			    atom_mem_end,
			    atom_base_addr,
			    atom_irq,
			    atom_dma,
			    atom_port};
  ERL_NIF_TERM mapVals[] = {MKUL(env, mapP->mem_start),
			    MKUL(env, mapP->mem_end),
			    MKUI(env, mapP->base_addr),
			    MKUI(env, mapP->irq),
			    MKUI(env, mapP->dma),
			    MKUI(env, mapP->port)};
  unsigned int numMapKeys = NUM(mapKeys);
  unsigned int numMapVals = NUM(mapVals);
  ERL_NIF_TERM emap;

  ESOCK_ASSERT( numMapVals == numMapKeys );
  ESOCK_ASSERT( MKMA(env, mapKeys, mapVals, numMapKeys, &emap) );

  SSDBG( descP, ("SOCKET", "encode_ioctl_ifrmap -> done with"
		 "\r\n    Map: %T"
		 "\r\n", emap) );

  return esock_make_ok2(env, emap);;
}
#endif


#if defined(SIOCGIFHWADDR) && defined(ESOCK_USE_HWADDR)
static
ERL_NIF_TERM encode_ioctl_hwaddr(ErlNifEnv*       env,
				 ESockDescriptor* descP,
				 struct sockaddr* addrP)
{
  ERL_NIF_TERM eaddr;
  SOCKLEN_T    sz = sizeof(struct sockaddr);

  esock_encode_hwsockaddr(env, addrP, sz, &eaddr);

  SSDBG( descP, ("SOCKET", "encode_ioctl_ifraddr -> done with"
		 "\r\n    Sock Addr: %T"
		 "\r\n", eaddr) );

  return esock_make_ok2(env, eaddr);;
}
#endif


static
ERL_NIF_TERM encode_ioctl_ifraddr(ErlNifEnv*       env,
				  ESockDescriptor* descP,
				  struct sockaddr* addrP)
{
  ERL_NIF_TERM eaddr;

  esock_encode_sockaddr(env, (ESockAddress*) addrP, -1, &eaddr);

  SSDBG( descP, ("SOCKET", "encode_ioctl_ifraddr -> done with"
		 "\r\n    Sock Addr: %T"
		 "\r\n", eaddr) );

  return esock_make_ok2(env, eaddr);;
}


static
ERL_NIF_TERM encode_ioctl_flags(ErlNifEnv*       env,
				ESockDescriptor* descP,
				short            flags)
{
  int          i, flag, num = NUM(ioctl_flags);
  ERL_NIF_TERM eflags, eflag;
  SocketTArray ta = TARRAY_CREATE(20); // Just to be on the safe side

  if (flags == 0) {
    eflags = MKEL(env);
  } else {
    for (i = 0; (i < num) && (flags != 0); i++) {
      flag = ioctl_flags[i].flag;
      if ((flag != 0) && ((flags & flag) == flag)) {
	eflag  = *(ioctl_flags[i].name);
	flags &= ~flag;

	SSDBG( descP, ("SOCKET", "encode_ioctl_flags  {%d} -> "
		       "\r\n      i:               %d"
		       "\r\n      found flag:      %T (%d)"
		       "\r\n      remaining flags: %d"
		       "\r\n", descP->sock, i, eflag, flag, flags) );

	TARRAY_ADD(ta, eflag);
      }
    }
    if (flags != 0) {

      SSDBG( descP, ("SOCKET", "encode_ioctl_flags  {%d} -> unknown flag(s): %d"
		     "\r\n", descP->sock, flags) );

      TARRAY_ADD(ta, MKI(env, flags));
    }

    TARRAY_TOLIST(ta, env, &eflags);
  }
  

  SSDBG( descP, ("SOCKET", "encode_ioctl_flags -> done with"
		 "\r\n    Flags: %T (%d)"
		 "\r\n", eflags, flags) );

  return esock_make_ok2(env, eflags);;
}


static
BOOLEAN_T decode_ioctl_sockaddr(ErlNifEnv*       env,
				ESockDescriptor* descP,
				ERL_NIF_TERM     eaddr,
				ESockAddress*    addr)
{
  SOCKLEN_T addrLen;
  BOOLEAN_T result;

  result = esock_decode_sockaddr(env, eaddr, (ESockAddress*) addr, &addrLen);

  VOID(addrLen);

  SSDBG( descP,
	 ("SOCKET", "esock_decode_ioctl_sockaddr {%d} -> decode result: %s"
	  "\r\n", descP->sock, B2S(result)) );

  return result;
}
				 

static
BOOLEAN_T decode_ioctl_mtu(ErlNifEnv*       env,
			   ESockDescriptor* descP,
			   ERL_NIF_TERM     emtu,
			   int*             mtu)
{
  BOOLEAN_T result;

  if (! GET_INT(env, emtu, mtu)) {
    result = FALSE;
  } else {
    result = TRUE;
  }

  SSDBG( descP,
	 ("SOCKET", "esock_decode_ioctl_mtu {%d} -> decode result: %s"
	  "\r\n", descP->sock, B2S(result)) );

  return result;
}
				 

#if defined(SIOCSIFTXQLEN)
static
BOOLEAN_T decode_ioctl_txqlen(ErlNifEnv*       env,
			      ESockDescriptor* descP,
			      ERL_NIF_TERM     etxqlen,
			      int*             txqlen)
{
  return decode_ioctl_ivalue(env, descP, etxqlen, txqlen);
}
#endif

/* All uses of the function should be added. For instance:
 * #if defined(SIOCGIFTXQLEN) || defined(FOOBAR) || defined(YXA)
 */
#if defined(SIOCGIFTXQLEN)
static
BOOLEAN_T decode_ioctl_ivalue(ErlNifEnv*       env,
			      ESockDescriptor* descP,
			      ERL_NIF_TERM     eivalue,
			      int*             ivalue)
{
  BOOLEAN_T result;

  if (! GET_INT(env, eivalue, ivalue)) {
    result = FALSE;
  } else {
    result = TRUE;
  }

  SSDBG( descP,
	 ("SOCKET", "esock_decode_ioctl_ivalue {%d} -> decode result: %s"
	  "\r\n", descP->sock, B2S(result)) );

  return result;
}
#endif
				 

static
BOOLEAN_T decode_ioctl_flags(ErlNifEnv*       env,
			     ESockDescriptor* descP,
			     ERL_NIF_TERM     eflags,
			     short*           flags)
{
  ERL_NIF_TERM      key, value;
  ErlNifMapIterator iter;
  int               tmpFlags = (int) *flags; // Current value
  int               flag;

  SSDBG( descP,
	 ("SOCKET", "decode_ioctl_flags {%d} -> entry with"
	  "\r\n      flags: %d"
	  "\r\n",
	  descP->sock, tmpFlags) );

  enif_map_iterator_create(env, eflags, &iter, ERL_NIF_MAP_ITERATOR_FIRST);

  while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {

    /* Convert key (eflag) to int */
    if (! GET_INT(env, key, &flag)) {
      enif_map_iterator_destroy(env, &iter);
      return FALSE;
    }

    // Update flag
    if (COMPARE(value, esock_atom_true) == 0) {
      SSDBG( descP,
	     ("SOCKET", "decode_ioctl_flags {%d} -> set %d\r\n",
	      descP->sock, flag) );
      tmpFlags |= flag;
    } else {
      SSDBG( descP,
	     ("SOCKET", "decode_ioctl_flags {%d} -> reset %d\r\n",
	      descP->sock, flag) );
      tmpFlags &= ~flag;
    }

    enif_map_iterator_next(env, &iter);
  }

  enif_map_iterator_destroy(env, &iter);

  SSDBG( descP,
	 ("SOCKET", "decode_ioctl_flags {%d} -> done with"
	  "\r\n      (new) flags: %d"
	  "\r\n",
	  descP->sock, tmpFlags) );

  *flags = (short) tmpFlags;

  return TRUE;
}


static
ERL_NIF_TERM encode_ioctl_ivalue(ErlNifEnv*       env,
				 ESockDescriptor* descP,
				 int              ivalue)
{
  ERL_NIF_TERM eivalue = MKI(env, ivalue);

  SSDBG( descP, ("SOCKET", "encode_ioctl_ivalue -> done with"
		 "\r\n    iValue: %T (%d)"
		 "\r\n", eivalue, ivalue) );

  return esock_make_ok2(env, eivalue);;
}

static
ERL_NIF_TERM encode_ioctl_ifconf_ifreq(ErlNifEnv*       env,
				       ESockDescriptor* descP,
				       struct ifreq*    ifrP)
{
  ERL_NIF_TERM ename, eaddr;

  ESOCK_ASSERT( ifrP != NULL );

  SSDBG( descP, ("SOCKET", "encode_ioctl_ifconf_ifreq -> encode name\r\n") );
  ename = encode_ioctl_ifreq_name(env,     ifrP->ifr_name);

  SSDBG( descP, ("SOCKET", "encode_ioctl_ifconf_ifreq -> encode sockaddr\r\n") );
  eaddr = encode_ioctl_ifreq_sockaddr(env, &ifrP->ifr_addr);

  SSDBG( descP, ("SOCKET", "encode_ioctl_ifconf_ifreq -> make ifreq map with"
		 "\r\n    Name:      %T"
		 "\r\n    Sock Addr: %T"
		 "\r\n", ename, eaddr) );
  return make_ifreq(env, ename, esock_atom_addr, eaddr);
}

static
ERL_NIF_TERM encode_ioctl_ifreq_name(ErlNifEnv* env,
				     char*      name)
{
  return ((name == NULL) ? esock_atom_undefined : MKS(env, name));
}

static
ERL_NIF_TERM encode_ioctl_ifreq_sockaddr(ErlNifEnv* env, struct sockaddr* sa)
{
  ERL_NIF_TERM esa;

  if (sa != NULL) {

    esock_encode_sockaddr(env, (ESockAddress*) sa, -1, &esa);

  } else {

      esa = esock_atom_undefined;

  }

  return esa;
}


/* The ifreq structure *always* contain a name
 * and *one* other element. The second element
 * depend on the ioctl request. 
 */
static
ERL_NIF_TERM make_ifreq(ErlNifEnv*   env,
			ERL_NIF_TERM name,
			ERL_NIF_TERM key2,
			ERL_NIF_TERM val2)
{
  ERL_NIF_TERM keys[2];
  ERL_NIF_TERM vals[2];
  ERL_NIF_TERM res;

  keys[0] = esock_atom_name;
  vals[0] = name;

  keys[1] = key2;
  vals[1] = val2;

  ESOCK_ASSERT( MKMA(env, keys, vals, NUM(keys), &res) );

  return res;
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
            if (COMPARE(op, esock_atom_sendfile) == 0)
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
            result = esock_make_error_closed(env);
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

        res = esock_make_error_closed(env);

    } else if ((descP->connectorP == NULL) ||
               (COMPARE_PIDS(&self, &descP->connector.pid) != 0) ||
               (COMPARE(opRef, descP->connector.ref) != 0)) {

        res = esock_atom_not_found;

    } else {

        res = esock_cancel_write_select(env, descP, opRef);
        esock_requestor_release("esock_cancel_connect",
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

        res = esock_make_error_closed(env);

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
    MON_INIT(&descP->currentAcceptor.mon);
    res = esock_cancel_read_select(env, descP, descP->currentAcceptor.ref);

    SSDBG( descP,
           ("SOCKET",
            "esock_cancel_accept_current(%T) {%d} -> cancel res: %T"
            "\r\n", sockRef, descP->sock, res) );

    if (!esock_activate_next_acceptor(env, descP, sockRef)) {

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

    if (esock_acceptor_unqueue(env, descP, &opRef, selfP)) {
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

        res = esock_make_error_closed(env);

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

    if (!esock_activate_next_writer(env, descP, sockRef)) {
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

    if (esock_writer_unqueue(env, descP, &opRef, selfP)) {
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

        res = esock_make_error_closed(env);

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

    if (!esock_activate_next_reader(env, descP, sockRef)) {
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

    if (esock_reader_unqueue(env, descP, &opRef, selfP)) {
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

#ifndef __WIN32__
extern
BOOLEAN_T esock_encode_cmsg(ErlNifEnv*     env,
                            int            level,
                            int            type,
                            unsigned char* dataP,
                            size_t         dataLen,
                            ERL_NIF_TERM*  eType,
                            ERL_NIF_TERM*  eData)
{
    const ESockCmsgSpec *cmsgTable;
    size_t num;

    if ((cmsgTable = esock_lookup_cmsg_table(level, &num)) != NULL) {
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


/* +++ esock_encode_msg_flags +++
 *
 * Encode a list of msg_flag().
 *
 */

#ifndef __WIN32__
extern
void esock_encode_msg_flags(ErlNifEnv*       env,
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
        size_t       n;
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
#endif


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

    if ((timeP = esock_init_cmsghdr(cmsgP, rem, sizeof(*timeP), usedP)) == NULL)
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

    if ((tosP = esock_init_cmsghdr(cmsgP, rem, sizeof(*tosP), usedP)) == NULL)
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

extern
BOOLEAN_T esock_cmsg_decode_int(ErlNifEnv*      env,
                                ERL_NIF_TERM    eValue,
                                struct cmsghdr* cmsgP,
                                size_t          rem,
                                size_t*         usedP)
{
    int value, *valueP;

    if (! GET_INT(env, eValue, &value))
        return FALSE;

    if ((valueP = esock_init_cmsghdr(cmsgP, rem,
                                     sizeof(*valueP), usedP)) == NULL)
        return FALSE;

    *valueP = value;
    return TRUE;
}
#endif
#endif


#ifndef __WIN32__
extern
BOOLEAN_T esock_cmsg_decode_bool(ErlNifEnv*      env,
                                 ERL_NIF_TERM    eValue,
                                 struct cmsghdr* cmsgP,
                                 size_t          rem,
                                 size_t*         usedP)
{
    BOOLEAN_T v;
    int*      valueP;

    if (! esock_decode_bool(eValue, &v))
        return FALSE;

    if ((valueP = esock_init_cmsghdr(cmsgP, rem,
                                     sizeof(*valueP), usedP)) == NULL)
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
                              (CHARP(sock_err) + dataLen) - CHARP(offender),
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

static int cmpESockCmsgSpec(const void *vpa, const void *vpb) {
    ESockCmsgSpec *a, *b;
    a = (ESockCmsgSpec *) vpa;
    b = (ESockCmsgSpec *) vpb;
    return COMPARE(*(a->nameP), *(b->nameP));
}

static ESockCmsgSpec
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
static ESockCmsgSpec cmsgLevelIPv6[] =
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

extern
ESockCmsgSpec* esock_lookup_cmsg_table(int level, size_t *num)
{
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

extern
ESockCmsgSpec* esock_lookup_cmsg_spec(ESockCmsgSpec* table,
                                      size_t         num,
                                      ERL_NIF_TERM   eType)
{
    ESockCmsgSpec key;

    sys_memzero(CHARP(&key), sizeof(key));
    key.nameP = &eType;
    return bsearch(&key, table, num, sizeof(*table), cmpESockCmsgSpec);
}

#endif // #ifdef __WIN32__



/* Clear the CMSG space and init the ->cmsg_len member,
 * return the position for the data, and the total used space
 */
#ifndef __WIN32__
extern
void* esock_init_cmsghdr(struct cmsghdr* cmsgP,
                         size_t          rem,  // Remaining space
                         size_t          size, // Size of data
                         size_t*         usedP)
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


/* *** esock_alloc_descriptor ***
 *
 * Allocate and perform basic initialization of a socket descriptor.
 *
 */
#ifndef __WIN32__
extern
ESockDescriptor* esock_alloc_descriptor(SOCKET sock, ErlNifEvent event)
{
    ESockDescriptor* descP;
    char buf[64]; /* Buffer used for building the mutex name */

    ESOCK_ASSERT( (descP =
                   enif_alloc_resource(esocks, sizeof(ESockDescriptor)))
                  != NULL );

    descP->pattern = ESOCK_DESC_PATTERN_CREATED;

    esock_requestor_init(&descP->connector);
    descP->connectorP = NULL;

    sprintf(buf, "esock.w[%d]", sock);
    descP->writeMtx       = MCREATE(buf);
    descP->writeState     = 0;
    esock_requestor_init(&descP->currentWriter);
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
    esock_requestor_init(&descP->currentReader);
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
    esock_requestor_init(&descP->currentAcceptor);
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
    descP->useReg           = ESOCK_USE_SOCKET_REGISTRY; // Overwritten by caller
    descP->meta.env         = esock_alloc_env("esock_alloc_descriptor - "
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
extern
void esock_dec_socket(int domain, int type, int protocol)
{
    MLOCK(data.cntMtx);

    esock_cnt_dec(&data.numSockets, 1);

    /* *** Domain counter *** */
    if (domain == AF_INET)
        esock_cnt_dec(&data.numDomainInet, 1);
#if defined(HAVE_IN6) && defined(AF_INET6)
    else if (domain == AF_INET6)
        esock_cnt_dec(&data.numDomainInet6, 1);
#endif
#if defined(HAS_AF_LOCAL)
    else if (domain == AF_LOCAL)
        esock_cnt_dec(&data.numDomainInet6, 1);
#endif

    /* *** Type counter *** */
    if (type == SOCK_STREAM)
        esock_cnt_dec(&data.numTypeStreams, 1);
    else if (type == SOCK_DGRAM)
        esock_cnt_dec(&data.numTypeDGrams, 1);
#if defined(SOCK_SEQPACKET)
    else if (type == SOCK_SEQPACKET)
        esock_cnt_dec(&data.numTypeSeqPkgs, 1);
#endif

    /* *** Protocol counter *** */
    if (protocol == IPPROTO_IP)
        esock_cnt_dec(&data.numProtoIP, 1);
    else if (protocol == IPPROTO_TCP)
        esock_cnt_dec(&data.numProtoTCP, 1);
    else if (protocol == IPPROTO_UDP)
        esock_cnt_dec(&data.numProtoUDP, 1);
#if defined(HAVE_SCTP)
    else if (protocol == IPPROTO_SCTP)
        esock_cnt_dec(&data.numProtoSCTP, 1);
#endif

    MUNLOCK(data.cntMtx);
}


/* Increment counters for when a socket is opened
 */
extern
void esock_inc_socket(int domain, int type, int protocol)
{
    esock_cnt_inc(&data.numSockets, 1);
    
    /* *** Domain counter *** */
    if (domain == AF_INET)
        esock_cnt_inc(&data.numDomainInet, 1);
#if defined(HAVE_IN6) && defined(AF_INET6)
    else if (domain == AF_INET6)
        esock_cnt_inc(&data.numDomainInet6, 1);
#endif
#if defined(HAS_AF_LOCAL)
    else if (domain == AF_LOCAL)
        esock_cnt_inc(&data.numDomainInet6, 1);
#endif

    /* *** Type counter *** */
    if (type == SOCK_STREAM)
        esock_cnt_inc(&data.numTypeStreams, 1);
    else if (type == SOCK_DGRAM)
        esock_cnt_inc(&data.numTypeDGrams, 1);
#if defined(SOCK_SEQPACKET)
    else if (type == SOCK_SEQPACKET)
        esock_cnt_inc(&data.numTypeSeqPkgs, 1);
#endif

    /* *** Protocol counter *** */
    if (protocol == IPPROTO_IP)
        esock_cnt_inc(&data.numProtoIP, 1);
    else if (protocol == IPPROTO_TCP)
        esock_cnt_inc(&data.numProtoTCP, 1);
    else if (protocol == IPPROTO_UDP)
        esock_cnt_inc(&data.numProtoUDP, 1);
#if defined(HAVE_SCTP)
    else if (protocol == IPPROTO_SCTP)
        esock_cnt_inc(&data.numProtoSCTP, 1);
#endif
}



/* ----------------------------------------------------------------------
 *  D e c o d e / E n c o d e   F u n c t i o n s
 * ----------------------------------------------------------------------
 */


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
extern
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
extern
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
extern
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
extern
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
    msg = mk_reg_msg(env, esock_atom_sendfile_deferred_close, sockRef);

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
extern
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
    return mk_socket_msg(env, sockRef, esock_atom_select, selectRef);
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
extern
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
extern
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
extern
int esock_select_stop(ErlNifEnv*  env,
                      ErlNifEvent event,
                      void*       obj)
{
    return enif_select(env, event, (ERL_NIF_SELECT_STOP), obj, NULL,
                       esock_atom_undefined);
}

extern
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

/* *** esock_activate_next_acceptor ***
 * *** esock_activate_next_writer   ***
 * *** esock_activate_next_reader   ***
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

#define ACTIVATE_NEXT_FUNC_DECL(F, S, R, Q)                     \
    extern                                                      \
    BOOLEAN_T esock_activate_next_##F(ErlNifEnv*       env,     \
                                      ESockDescriptor* descP,   \
                                      ERL_NIF_TERM     sockRef) \
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
            if (esock_requestor_pop(q, reqP)) {              \
                                                             \
                /* There was another one */                  \
                                                             \
                SSDBG( descP,                                           \
                       ("SOCKET",                                       \
                        "esock_activate_next_" #F "(%T) {%d} ->"        \
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
                        "esock_activate_next_" #F "(%T) {%d} ->"        \
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
               ("SOCKET", "esock_activate_next_" #F "(%T) {%d} -> "     \
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

/* *** esock_acceptor_search4pid ***
 * *** esock_writer_search4pid   ***
 * *** esock_reader_search4pid   ***
 *
 * Search for a pid in the requestor (acceptor, writer, or reader) queue.
 *
 */

#define REQ_SEARCH4PID_FUNCS                       \
    REQ_SEARCH4PID_FUNC_DECL(acceptor, acceptorsQ) \
    REQ_SEARCH4PID_FUNC_DECL(writer,   writersQ)   \
    REQ_SEARCH4PID_FUNC_DECL(reader,   readersQ)

#define REQ_SEARCH4PID_FUNC_DECL(F, Q)                          \
    extern                                                      \
    BOOLEAN_T esock_##F##_search4pid(ErlNifEnv*       env,      \
                                     ESockDescriptor* descP,    \
                                     ErlNifPid*       pid)      \
    {                                                           \
        return qsearch4pid(env, &descP->Q, pid);                \
    }
REQ_SEARCH4PID_FUNCS
#undef REQ_SEARCH4PID_FUNC_DECL

#endif // #ifndef __WIN32__



/* *** esock_acceptor_push ***
 * *** esock_writer_push   ***
 * *** esock_reader_push   ***
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
    extern                                                              \
    void esock_##F##_push(ErlNifEnv*       env,                         \
                        ESockDescriptor* descP,                         \
                        ErlNifPid        pid, /* self() */              \
                        ERL_NIF_TERM     ref)                           \
    {                                                                   \
        ESockRequestQueueElement *e;                                    \
        ESockRequestor           *reqP;                                 \
                                                                        \
        ESOCK_ASSERT( (e = MALLOC(sizeof(ESockRequestQueueElement)))    \
                      != NULL );                                        \
        reqP = &e->data;                                                \
        reqP->pid = pid;                                                \
        ESOCK_ASSERT( MONP("esock_" #F "_push -> " #F " request",       \
                           env, descP, &pid, &reqP->mon) == 0 );        \
        reqP->env = esock_alloc_env("esock_" #F "_push");               \
        reqP->ref = CP_TERM(reqP->env, ref);                            \
                                                                        \
        qpush(&descP->Q, e);                                            \
    }
REQ_PUSH_FUNCS
#undef REQ_PUSH_FUNC_DECL

#endif // #ifndef __WIN32__



/* *** esock_acceptor_pop ***
 * *** esock_writer_pop   ***
 * *** esock_reader_pop   ***
 *
 * Pop a requestor (acceptor, writer, or reader) from its queue.
 *
 */

#ifndef __WIN32__

#define REQ_POP_FUNCS                       \
    REQ_POP_FUNC_DECL(acceptor, acceptorsQ) \
    REQ_POP_FUNC_DECL(writer,   writersQ)   \
    REQ_POP_FUNC_DECL(reader,   readersQ)

#define REQ_POP_FUNC_DECL(F, Q)                         \
    extern                                              \
    BOOLEAN_T esock_##F##_pop(ErlNifEnv*       env,     \
                              ESockDescriptor* descP,   \
                              ESockRequestor*  reqP)    \
    {                                                   \
        return esock_requestor_pop(&descP->Q, reqP);    \
    }
REQ_POP_FUNCS
#undef REQ_POP_FUNC_DECL

#endif // #ifndef __WIN32__



/* *** esock_acceptor_unqueue ***
 * *** esock_writer_unqueue   ***
 * *** esock_reader_unqueue   ***
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
    extern                                                      \
    BOOLEAN_T esock_##F##_unqueue(ErlNifEnv*       env,         \
                                  ESockDescriptor* descP,       \
                                  ERL_NIF_TERM*    refP,        \
                                  const ErlNifPid* pidP)        \
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

extern
BOOLEAN_T esock_requestor_pop(ESockRequestQueue* q,
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
        esock_requestor_init(reqP);
        return FALSE;
    }
    
}

extern
void esock_requestor_init(ESockRequestor* reqP)
{
    enif_set_pid_undefined(&reqP->pid);
    MON_INIT(&reqP->mon);
    reqP->env = NULL;
    reqP->ref = esock_atom_undefined;
}

extern
void esock_requestor_release(const char*      slogan,
                             ErlNifEnv*       env,
                             ESockDescriptor* descP,
                             ESockRequestor*  reqP)
{
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
        /* At least one element in the queue */
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

extern
BOOLEAN_T esock_cnt_inc(ESockCounter* cnt, ESockCounter inc)
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

extern
void esock_cnt_dec(ESockCounter* cnt, ESockCounter dec)
{
    ESockCounter current = *cnt;

    if (dec > current)
        *cnt = 0; // The counter cannot be < 0 so this is the best we can do...
    else
        *cnt -= dec;

    return;
}



/* ----------------------------------------------------------------------
 *  M o n i t o r   W r a p p e r   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

#ifndef __WIN32__

extern
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
                ESOCK_MON2TERM(env, monP)) );
    }

    return res;
}

extern
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
                   ESOCK_MON2TERM(env, monP)) );

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

extern
void esock_monitor_init(ESockMonitor* monP)
{
    monP->isActive = FALSE;
}

extern
ERL_NIF_TERM esock_make_monitor_term(ErlNifEnv* env, const ESockMonitor* monP)
{
    if (monP->isActive)
        return enif_make_monitor_term(env, &monP->mon);
    else
        return esock_atom_undefined;
}

extern
BOOLEAN_T esock_monitor_eq(const ESockMonitor* monP,
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

    esock_send_abort_msg(env, descP, sockRef, reqP, esock_atom_closed);

    enif_set_pid_undefined(&reqP->pid);
    reqP->ref = esock_atom_undefined;
}



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

        esock_requestor_release("esock_down->connector",
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
        
        if (!esock_activate_next_acceptor(env, descP, sockRef)) {

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
        
        esock_acceptor_unqueue(env, descP, NULL, pidP);
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
        
        if (!esock_activate_next_writer(env, descP, sockRef)) {

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
        
        esock_writer_unqueue(env, descP, NULL, pidP);
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
        
        if (! esock_activate_next_reader(env, descP, sockRef)) {

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
        
        esock_reader_unqueue(env, descP, NULL, pidP);
    }
}
#endif // #ifndef __WIN32__


/*
 * The idea with this function is that it should call esock_io_finish
 * and release anything allocated by the I/O backend (just to be a
 * nice citizen).
 * On Unix this currently a void operation, but on Windows it will be
 * more substantial...
 * So, this is currently just a placeholder.
 */
static
void esock_on_halt(void* priv_data)
{
#ifndef __WIN32__
    VOID(priv_data);
#else
    VOIDP(priv_data);
#endif
}


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
    {"nif_ioctl",               2, nif_ioctl, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"nif_ioctl",               3, nif_ioctl, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"nif_ioctl",               4, nif_ioctl, ERL_NIF_DIRTY_JOB_IO_BOUND},

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
    // LOCAL_ERROR_REASON_ATOMS;
#undef LOCAL_ATOM_DECL

    /* Global atom(s) and error reason atom(s) */
#define GLOBAL_ATOM_DECL(A) esock_atom_##A = MKA(env, #A)
    GLOBAL_ATOMS;
    GLOBAL_ERROR_REASON_ATOMS;
#undef GLOBAL_ATOM_DECL

    esock_atom_socket_tag = MKA(env, "$socket");

    /* This is (currently) intended for Windows use */
    {
        ErlNifSysInfo sysInfo;
        unsigned int  ioNumThreadsDef; // Used as "default" value

        enif_system_info(&sysInfo, sizeof(ErlNifSysInfo));

        ioNumThreadsDef =
            (unsigned int) (sysInfo.scheduler_threads > 0) ?
            2*sysInfo.scheduler_threads : 1; // ESOCK_IO_NUM_THREADS);

        data.ioNumThreads =
            esock_get_uint_from_map(env, load_info,
                                    atom_io_num_threads,
                                    ioNumThreadsDef);
    }


#ifdef __WIN32__

    io_backend.init           = NULL;
    io_backend.finish         = NULL;
    io_backend.open_with_fd   = NULL;
    io_backend.open_plain     = NULL;
    io_backend.bind           = NULL;
    io_backend.connect        = NULL;
    io_backend.listen         = NULL;
    io_backend.accept         = NULL;
    io_backend.send           = NULL;
    io_backend.sendto         = NULL;
    io_backend.sendmsg        = NULL;
    io_backend.sendfile_start = NULL;
    io_backend.sendfile_cont  = NULL;
    io_backend.sendfile_dc    = NULL;
    io_backend.recv           = NULL;
    io_backend.recvfrom       = NULL;
    io_backend.recvmsg        = NULL;
    io_backend.close          = NULL;
    io_backend.fin_close      = NULL;
    io_backend.shutdown       = NULL;
    io_backend.sockname       = NULL;
    io_backend.peername       = NULL;

#else

    io_backend.init           = essio_init;
    io_backend.finish         = essio_finish;
    io_backend.open_with_fd   = essio_open_with_fd;
    io_backend.open_plain     = essio_open_plain;
    io_backend.bind           = essio_bind;
    io_backend.connect        = essio_connect;
    io_backend.listen         = essio_listen;
    io_backend.accept         = essio_accept;
    io_backend.send           = essio_send;
    io_backend.sendto         = essio_sendto;
    io_backend.sendmsg        = essio_sendmsg;
    io_backend.sendfile_start = essio_sendfile_start;
    io_backend.sendfile_cont  = essio_sendfile_cont;
    io_backend.sendfile_dc    = essio_sendfile_deferred_close;
    io_backend.recv           = essio_recv;
    io_backend.recvfrom       = essio_recvfrom;
    io_backend.recvmsg        = essio_recvmsg;
    io_backend.close          = essio_close;
    io_backend.fin_close      = essio_fin_close;
    io_backend.shutdown       = essio_shutdown;
    io_backend.sockname       = essio_sockname;
    io_backend.peername       = essio_peername;

#endif

#ifndef __WIN32__
    // This ifndef is only temporary!!
    if (ESOCK_IO_INIT(data.ioNumThreads) != ESOCK_IO_OK) {
        esock_error_msg("Failed initiating I/O backend");
        return 1; // Failure
    }
#endif

#ifndef __WIN32__

    if (! esock_extract_pid_from_map(env, load_info,
                                     atom_registry,
                                     &data.regPid)) {
        enif_set_pid_undefined(&data.regPid);
        return 1; // Failure - no registry pid
    }

    data.useReg =
        esock_get_bool_from_map(env, load_info,
                                esock_atom_use_registry,
                                ESOCK_USE_SOCKET_REGISTRY);

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

    if (esocks != NULL) {
        int ores;

        // *Try* install on-halt (callback) function
        if ((ores = enif_set_option(env,
                                    ERL_NIF_OPT_ON_HALT,
                                    esock_on_halt)) != 0) {
            esock_error_msg("Failed installing 'on-halt' "
                            "callback function (%d)\r\n", ores);
            return 1; // Failure
        }

        // *Try* enable 'delay on halt' (none-fatal)
        if ((ores = enif_set_option(env, ERL_NIF_OPT_DELAY_HALT)) != 0) {
            esock_error_msg("Failed enable 'on-halt' delay (%d)\r\n", ores);
        }

        return 0; // Success
    } else {
        esock_error_msg("Failed open esock resource type\r\n");
        return 1; // Failure
    }
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

#endif
