/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2018-2018. All Rights Reserved.
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
 *  Purpose : The NIF (C) part of the socket interface
 * ----------------------------------------------------------------------
 *
 */

#define STATIC_ERLANG_NIF 1

/* #include <stdio.h> */
/* #include <stdlib.h> */
/* #include <stdarg.h> */
/* #include <string.h> */
/* #include <unistd.h> */
/* #include <errno.h> */
/* #include <netdb.h> */
/* #include <sys/types.h> */
/* #include <sys/wait.h> */
/* #include <sys/socket.h> */
/* #include <netinet/in.h> */
/* #include <arpa/inet.h> */
/* #include <sys/time.h> */
/* #include <fcntl.h> */


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
#include <netinet/ip.h>
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

#else /* !__WIN32__ */

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

#ifndef WANT_NONBLOCKING
#define WANT_NONBLOCKING
#endif
#include "sys.h"

#endif

#include <erl_nif.h>

#include "socket_dbg.h"
#include "socket_int.h"
#include "socket_util.h"

/* All platforms fail on malloc errors. */
#define FATAL_MALLOC


/* Debug stuff... */
#define SOCKET_NIF_DEBUG_DEFAULT TRUE
#define SOCKET_DEBUG_DEFAULT     TRUE

/* Counters and stuff (Don't know where to sent this stuff anyway) */
#define SOCKET_NIF_IOW_DEFAULT FALSE



/* Socket stuff */
#define INVALID_SOCKET -1
#define INVALID_EVENT  -1
#define SOCKET_ERROR   -1

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

#if defined(TCP_CA_NAME_MAX)
#define SOCKET_OPT_TCP_CONGESTION_NAME_MAX TCP_CA_NAME_MAX
#else
/* This is really excessive, but just in case... */
#define SOCKET_OPT_TCP_CONGESTION_NAME_MAX 256
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

#define IS_OPEN(d) \
    (((d)->state & SOCKET_FLAG_OPEN) == SOCKET_FLAG_OPEN)

#define IS_CONNECTED(d)                                                 \
    (((d)->state & SOCKET_STATE_CONNECTED) == SOCKET_STATE_CONNECTED)

#define IS_CONNECTING(d)                                \
    (((d)->state & SOCKET_FLAG_CON) == SOCKET_FLAG_CON)

#define IS_BUSY(d)                                      \
    (((d)->state & SOCKET_FLAG_BUSY) == SOCKET_FLAG_BUSY)


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

#define SOCKET_RECV_BUFFER_SIZE_DEFAULT 2048

#define SOCKET_OPT_VALUE_TYPE_UNSPEC 0
#define SOCKET_OPT_VALUE_TYPE_INT    1
#define SOCKET_OPT_VALUE_TYPE_BOOL   2

typedef union {
    struct {
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
#define SOCKET_PROTOCOL_IP        1
#define SOCKET_PROTOCOL_TCP       2
#define SOCKET_PROTOCOL_UDP       3
#define SOCKET_PROTOCOL_SCTP      4

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

#define SOCKET_OPT_OTP_DEBUG        0
#define SOCKET_OPT_OTP_IOW          1

#define SOCKET_OPT_SOCK_BROADCAST   4
#define SOCKET_OPT_SOCK_DONTROUTE   7
#define SOCKET_OPT_SOCK_KEEPALIVE   9
#define SOCKET_OPT_SOCK_LINGER     10
#define SOCKET_OPT_SOCK_PRIORITY   16
#define SOCKET_OPT_SOCK_RCVBUF     17
#define SOCKET_OPT_SOCK_REUSEADDR  21
#define SOCKET_OPT_SOCK_SNDBUF     27

#define SOCKET_OPT_IP_RECVTOS      25
#define SOCKET_OPT_IP_ROUTER_ALERT 28
#define SOCKET_OPT_IP_TOS          30
#define SOCKET_OPT_IP_TTL          32

#define SOCKET_OPT_IPV6_HOPLIMIT   12

#define SOCKET_OPT_TCP_CONGESTION   0
#define SOCKET_OPT_TCP_CORK         1
#define SOCKET_OPT_TCP_MAXSEG       2
#define SOCKET_OPT_TCP_NODELAY      3

#define SOCKET_OPT_UDP_CORK         0

#define SOCKET_OPT_SCTP_AUTOCLOSE   7
#define SOCKET_OPT_SCTP_NODELAY    22


/* =================================================================== *
 *                                                                     *
 *                        Various enif macros                          *
 *                                                                     *
 * =================================================================== */

/* #define MALLOC(SZ)          enif_alloc((SZ)) */
/* #define FREE(P)             enif_free((P)) */

/* #define MKA(E,S)            enif_make_atom((E), (S)) */
/* #define MKBIN(E,B)          enif_make_binary((E), (B)) */
/* #define MKI(E,I)            enif_make_int((E), (I)) */
/* #define MKLA(E,A,L)         enif_make_list_from_array((E), (A), (L)) */
/* #define MKMA(E,KA,VA,L,M)   enif_make_map_from_arrays((E), (KA), (VA), (L), (M)) */
/* #define MKREF(E)            enif_make_ref((E)) */
/* #define MKS(E,S)            enif_make_string((E), (S), ERL_NIF_LATIN1) */
/* #define MKSL(E,S,L)         enif_make_string_len((E), (S), (L), ERL_NIF_LATIN1) */
/* #define MKSBIN(E,B,ST,SZ)   enif_make_sub_binary((E), (B), (ST), (SZ)) */
/* #define MKT2(E,E1,E2)       enif_make_tuple2((E), (E1), (E2)) */
/* #define MKT3(E,E1,E2,E3)    enif_make_tuple3((E), (E1), (E2), (E3)) */
/* #define MKT4(E,E1,E2,E3,E4) enif_make_tuple4((E), (E1), (E2), (E3), (E4)) */
/* #define MKT8(E,E1,E2,E3,E4,E5,E6,E7,E8) \ */
/*     enif_make_tuple8((E), (E1), (E2), (E3), (E4), (E5), (E6), (E7), (E8)) */

/* #define MCREATE(N)          enif_mutex_create((N)) */
/* #define MDESTROY(M)         enif_mutex_destroy((M)) */
/* #define MLOCK(M)            enif_mutex_lock((M)) */
/* #define MUNLOCK(M)          enif_mutex_unlock((M)) */

/* #define MONP(E,D,P,M)       enif_monitor_process((E), (D), (P), (M)) */
/* #define DEMONP(E,D,M)       enif_demonitor_process((E), (D), (M)) */

/* #define SELECT(E,FD,M,O,P,R)                            \ */
/*     if (enif_select((E), (FD), (M), (O), (P), (R)) < 0) \ */
/*         return enif_make_badarg((E)); */

/* #define COMPARE(A, B)   enif_compare((A), (B)) */

/* #define IS_ATOM(E,  TE) enif_is_atom((E),   (TE)) */
/* #define IS_BIN(E,   TE) enif_is_binary((E), (TE)) */
/* #define IS_MAP(E,   TE) enif_is_map((E), (TE)) */
/* #define IS_NUM(E,   TE) enif_is_number((E), (TE)) */
/* #define IS_TUPLE(E, TE) enif_is_tuple((E),  (TE)) */

/* #define GET_ATOM_LEN(E, TE, LP) \ */
/*     enif_get_atom_length((E), (TE), (LP), ERL_NIF_LATIN1) */
/* #define GET_ATOM(E, TE, BP, MAX) \ */
/*     enif_get_atom((E), (TE), (BP), (MAX), ERL_NIF_LATIN1) */
/* #define GET_BIN(E, TE, BP)        enif_inspect_iolist_as_binary((E), (TE), (BP)) */
/* #define GET_INT(E, TE, IP)        enif_get_int((E), (TE), (IP)) */
/* #define GET_STR(E, L, B, SZ)      \ */
/*     enif_get_string((E), (L), (B), (SZ), ERL_NIF_LATIN1) */
/* #define GET_UINT(E, TE, IP)       enif_get_uint((E), (TE), (IP)) */
/* #define GET_TUPLE(E, TE, TSZ, TA) enif_get_tuple((E), (TE), (TSZ), (TA)) */

/* #define ALLOC_BIN(SZ, BP)         enif_alloc_binary((SZ), (BP)) */
/* #define REALLOC_BIN(SZ, BP)       enif_realloc_binary((SZ), (BP)) */


#define SGDBG( proto )         ESOCK_DBG_PRINTF( data.dbg , proto )
#define SSDBG( __D__ , proto ) ESOCK_DBG_PRINTF( (__D__)->dbg , proto )


/* =================================================================== *
 *                                                                     *
 *                    Basic socket operations                          *
 *                                                                     *
 * =================================================================== */

#ifdef __WIN32__

/* *** Windows macros *** */

#define sock_accept(s, addr, len) \
    make_noninheritable_handle(accept((s), (addr), (len)))
#define sock_bind(s, addr, len)    bind((s), (addr), (len))
#define sock_close(s)              closesocket((s))
#define sock_close_event(e)        WSACloseEvent(e)
#define sock_connect(s, addr, len) connect((s), (addr), (len))
#define sock_create_event(s)       WSACreateEvent()
#define sock_errno()               WSAGetLastError()
#define sock_getopt(s,l,o,v,ln)    getsockopt((s),(l),(o),(v),(ln))
#define sock_htons(x)              htons((x))
#define sock_htonl(x)              htonl((x))
#define sock_listen(s, b)          listen((s), (b))
#define sock_name(s, addr, len)    getsockname((s), (addr), (len))
#define sock_ntohs(x)              ntohs((x))
#define sock_open(domain, type, proto)                             \
    make_noninheritable_handle(socket((domain), (type), (proto)))
#define sock_recv(s,buf,len,flag)  recv((s),(buf),(len),(flag))
#define sock_recvfrom(s,buf,blen,flag,addr,alen) \
    recvfrom((s),(buf),(blen),(flag),(addr),(alen))
#define sock_send(s,buf,len,flag)  send((s),(buf),(len),(flag))
#define sock_sendto(s,buf,blen,flag,addr,alen) \
    sendto((s),(buf),(blen),(flag),(addr),(alen))
#define sock_setopt(s,l,o,v,ln)    setsockopt((s),(l),(o),(v),(ln))
#define sock_shutdown(s, how)      shutdown((s), (how))


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
#define sock_recv(s,buf,len,flag)       recv((s),(buf),(len),(flag))
#define sock_recvfrom(s,buf,blen,flag,addr,alen) \
    recvfrom((s),(buf),(blen),(flag),(addr),(alen))
#define sock_send(s,buf,len,flag)       send((s), (buf), (len), (flag))
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

/* The general purpose sockaddr * /
typedef union {
    struct sockaddr     in;
    struct sockaddr_in  in4;

#ifdef HAVE_IN6
    struct sockaddr_in6 in6;
#endif

#ifdef HAVE_SYS_UN_H
    struct sockaddr_un  un;
#endif

} SocketAddress;
*/

/* We can use the IPv4 def for this since the beginning
 * is the same for INET and INET6 */
#define which_address_port(sap)		     \
  ((((sap)->in4.sin_family == AF_INET) ||  \
    ((sap)->in4.sin_family == AF_INET6)) ? \
   ((sap)->in4.sin_port) : -1)


typedef struct {
    ErlNifPid     pid; // PID of the requesting process
    ErlNifMonitor mon; // Monitor to the requesting process
    ERL_NIF_TERM  ref; // The (unique) reference (ID) of the request
} SocketRequestor;

typedef struct socket_request_queue_element {
    struct socket_request_queue_element* nextP;
    SocketRequestor                      data;
} SocketRequestQueueElement;

typedef struct {
    SocketRequestQueueElement* first;
    SocketRequestQueueElement* last;
} SocketRequestQueue;


typedef struct {
    /* +++ The actual socket +++ */
    SOCKET         sock;
    HANDLE         event;

    /* +++ Stuff "about" the socket +++ */
    int            domain;
    int            type;
    int            protocol;

    unsigned int   state;
    SocketAddress  remote;
    unsigned int   addrLen;


    /* +++ Controller (owner) process +++ */
    ErlNifPid      ctrlPid;
    ErlNifMonitor  ctrlMon;

    /* +++ Write stuff +++ */
    ErlNifMutex*       writeMtx;
    SocketRequestor    currentWriter;
    SocketRequestor*   currentWriterP; // NULL or points to currentWriter
    SocketRequestQueue writersQ;
    BOOLEAN_T          isWritable;
    uint32_t           writePkgCnt;
    uint32_t           writeByteCnt;
    uint32_t           writeTries;
    uint32_t           writeWaits;
    uint32_t           writeFails;

    /* +++ Read stuff +++ */
    ErlNifMutex*       readMtx;
    SocketRequestor    currentReader;
    SocketRequestor*   currentReaderP; // NULL or points to currentReader
    SocketRequestQueue readersQ;
    BOOLEAN_T          isReadable;
    ErlNifBinary       rbuffer;      // DO WE NEED THIS
    uint32_t           readCapacity; // DO WE NEED THIS
    uint32_t           readPkgCnt;
    uint32_t           readByteCnt;
    uint32_t           readTries;
    uint32_t           readWaits;

    /* +++ Accept stuff +++ */
    ErlNifMutex*       accMtx;
    SocketRequestor    currentAcceptor;
    SocketRequestor*   currentAcceptorP; // NULL or points to currentReader
    SocketRequestQueue acceptorsQ;

    /* +++ Config & Misc stuff +++ */
    size_t    rBufSz; // Read buffer size (when data length = 0 is specified)
    BOOLEAN_T iow;    // Inform On Wrap
    BOOLEAN_T dbg;

    /* +++ Close stuff +++ */
    ErlNifMutex*  closeMtx;
    ErlNifPid     closerPid;
    ErlNifMonitor closerMon;
    ERL_NIF_TERM  closeRef;
    BOOLEAN_T     closeLocal;

} SocketDescriptor;


#define SOCKET_OPT_VALUE_UNDEF  0
#define SOCKET_OPT_VALUE_BOOL   1
#define SOCKET_OPT_VALUE_INT    2
#define SOCKET_OPT_VALUE_LINGER 3
#define SOCKET_OPT_VALUE_BIN    4
#define SOCKET_OPT_VALUE_STR    5

typedef struct {
    unsigned int tag;
    union {
        BOOLEAN_T     boolVal;
        int           intVal;
        struct linger lingerVal;
        ErlNifBinary  binVal;
        struct {
            unsigned int len;
            char*        str;
        } strVal;
    } u;
    /*
    void*     optValP;   // Points to the actual data (above)
    socklen_t optValLen; // The size of the option value
    */
} SocketOptValue;


/* Global stuff (do we really need to "collect"
 * these things?)
 */
typedef struct {
    /* These are for debugging, testing and the like */
    ERL_NIF_TERM version;
    ERL_NIF_TERM buildDate;
    BOOLEAN_T    dbg;
    BOOLEAN_T    iow;

    ErlNifMutex* cntMtx;
    uint32_t     numSockets;
    uint32_t     numTypeDGrams;
    uint32_t     numTypeStreams;
    uint32_t     numTypeSeqPkgs;
    uint32_t     numDomainLocal;
    uint32_t     numDomainInet;
    uint32_t     numDomainInet6;
    uint32_t     numProtoIP;
    uint32_t     numProtoTCP;
    uint32_t     numProtoUDP;
    uint32_t     numProtoSCTP;
} SocketData;


/* ----------------------------------------------------------------------
 *  F o r w a r d s
 * ----------------------------------------------------------------------
 */



static ERL_NIF_TERM nif_is_loaded(ErlNifEnv*         env,
                                  int                argc,
                                  const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_info(ErlNifEnv*         env,
                             int                argc,
                             const ERL_NIF_TERM argv[]);
/*
This is a *global* debug function (enable or disable for all
operations and all sockets.
static ERL_NIF_TERM nif_debug(ErlNifEnv*         env,
                              int                argc,
                              const ERL_NIF_TERM argv[]);
*/
static ERL_NIF_TERM nif_open(ErlNifEnv*         env,
                             int                argc,
                             const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_bind(ErlNifEnv*         env,
                             int                argc,
                             const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_connect(ErlNifEnv*         env,
                               int                argc,
                               const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_listen(ErlNifEnv*         env,
                               int                argc,
                               const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_accept(ErlNifEnv*         env,
                               int                argc,
                               const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_send(ErlNifEnv*         env,
                             int                argc,
                             const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_sendto(ErlNifEnv*         env,
                               int                argc,
                               const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_recv(ErlNifEnv*         env,
                             int                argc,
                             const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_recvfrom(ErlNifEnv*         env,
                                 int                argc,
                                 const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_close(ErlNifEnv*         env,
                              int                argc,
                              const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_shutdown(ErlNifEnv*         env,
                                 int                argc,
                                 const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_setopt(ErlNifEnv*         env,
                               int                argc,
                               const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_getopt(ErlNifEnv*         env,
                               int                argc,
                               const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_finalize_connection(ErlNifEnv*         env,
                                            int                argc,
                                            const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_finalize_close(ErlNifEnv*         env,
                                       int                argc,
                                       const ERL_NIF_TERM argv[]);
/*
static ERL_NIF_TERM nif_cancel(ErlNifEnv*         env,
                               int                argc,
                               const ERL_NIF_TERM argv[]);
*/


static ERL_NIF_TERM nopen(ErlNifEnv* env,
                          int        domain,
                          int        type,
                          int        protocol,
                          char*      netns);
static ERL_NIF_TERM nbind(ErlNifEnv*        env,
                          SocketDescriptor* descP,
                          ERL_NIF_TERM      addr);
static ERL_NIF_TERM nconnect(ErlNifEnv*        env,
                             SocketDescriptor* descP);
static ERL_NIF_TERM nlisten(ErlNifEnv*        env,
                            SocketDescriptor* descP,
                            int               backlog);
static ERL_NIF_TERM naccept_listening(ErlNifEnv*        env,
                                      SocketDescriptor* descP,
                                      ERL_NIF_TERM      ref);
static ERL_NIF_TERM naccept_accepting(ErlNifEnv*        env,
                                      SocketDescriptor* descP,
                                      ERL_NIF_TERM      ref);
static ERL_NIF_TERM naccept(ErlNifEnv*        env,
                            SocketDescriptor* descP,
                            ERL_NIF_TERM      ref);
static ERL_NIF_TERM nsend(ErlNifEnv*        env,
                          SocketDescriptor* descP,
                          ERL_NIF_TERM      sendRef,
                          ErlNifBinary*     dataP,
                          int               flags);
static ERL_NIF_TERM nsendto(ErlNifEnv*        env,
                            SocketDescriptor* descP,
                            ERL_NIF_TERM      sendRef,
                            ErlNifBinary*     dataP,
                            int               flags,
                            SocketAddress*    toAddrP,
                            unsigned int      toAddrLen);
static ERL_NIF_TERM nrecv(ErlNifEnv*        env,
                          SocketDescriptor* descP,
                          ERL_NIF_TERM      recvRef,
                          int               len,
                          int               flags);
static ERL_NIF_TERM nrecvfrom(ErlNifEnv*        env,
                              SocketDescriptor* descP,
                              ERL_NIF_TERM      recvRef,
                              uint16_t          bufSz,
                              int               flags);
static ERL_NIF_TERM nclose(ErlNifEnv*        env,
                           SocketDescriptor* descP);
static ERL_NIF_TERM nshutdown(ErlNifEnv*        env,
                              SocketDescriptor* descP,
                              int               how);

static ERL_NIF_TERM nsetopt(ErlNifEnv*        env,
                            SocketDescriptor* descP,
                            BOOLEAN_T         isEncoded,
                            BOOLEAN_T         isOTP,
                            int               level,
                            int               eOpt,
                            ERL_NIF_TERM      eVal);
static ERL_NIF_TERM nsetopt_otp(ErlNifEnv*        env,
                                SocketDescriptor* descP,
                                int               eOpt,
                                ERL_NIF_TERM      eVal);
static ERL_NIF_TERM nsetopt_otp_debug(ErlNifEnv*        env,
                                      SocketDescriptor* descP,
                                      ERL_NIF_TERM      eVal);
static ERL_NIF_TERM nsetopt_otp_iow(ErlNifEnv*        env,
                                    SocketDescriptor* descP,
                                    ERL_NIF_TERM      eVal);
static ERL_NIF_TERM nsetopt_native(ErlNifEnv*        env,
                                   SocketDescriptor* descP,
                                   int               level,
                                   int               eOpt,
                                   ERL_NIF_TERM      eVal);
static ERL_NIF_TERM nsetopt_level(ErlNifEnv*        env,
                                  SocketDescriptor* descP,
                                  int               level,
                                  int               eOpt,
                                  ERL_NIF_TERM      eVal);
static ERL_NIF_TERM nsetopt_lvl_socket(ErlNifEnv*        env,
                                       SocketDescriptor* descP,
                                       int               eOpt,
                                       ERL_NIF_TERM      eVal);
#if defined(SO_BROADCAST)
static ERL_NIF_TERM nsetopt_lvl_sock_broadcast(ErlNifEnv*        env,
                                               SocketDescriptor* descP,
                                               ERL_NIF_TERM      eVal);
#endif
#if defined(SO_DONTROUTE)
static ERL_NIF_TERM nsetopt_lvl_sock_dontroute(ErlNifEnv*        env,
                                               SocketDescriptor* descP,
                                               ERL_NIF_TERM      eVal);
#endif
#if defined(SO_KEEPALIVE)
static ERL_NIF_TERM nsetopt_lvl_sock_keepalive(ErlNifEnv*        env,
                                               SocketDescriptor* descP,
                                               ERL_NIF_TERM      eVal);
#endif
#if defined(SO_LINGER)
static ERL_NIF_TERM nsetopt_lvl_sock_linger(ErlNifEnv*        env,
                                            SocketDescriptor* descP,
                                            ERL_NIF_TERM      eVal);
#endif
#if defined(SO_PRIORITY)
static ERL_NIF_TERM nsetopt_lvl_sock_priority(ErlNifEnv*        env,
                                              SocketDescriptor* descP,
                                              ERL_NIF_TERM      eVal);
#endif
#if defined(SO_RCVBUF)
static ERL_NIF_TERM nsetopt_lvl_sock_rcvbuf(ErlNifEnv*        env,
                                            SocketDescriptor* descP,
                                            ERL_NIF_TERM      eVal);
#endif
#if defined(SO_REUSEADDR)
static ERL_NIF_TERM nsetopt_lvl_sock_reuseaddr(ErlNifEnv*        env,
                                               SocketDescriptor* descP,
                                               ERL_NIF_TERM      eVal);
#endif
#if defined(SO_SNDBUF)
static ERL_NIF_TERM nsetopt_lvl_sock_sndbuf(ErlNifEnv*        env,
                                            SocketDescriptor* descP,
                                            ERL_NIF_TERM      eVal);
#endif
static ERL_NIF_TERM nsetopt_lvl_ip(ErlNifEnv*        env,
                                   SocketDescriptor* descP,
                                   int               eOpt,
                                   ERL_NIF_TERM      eVal);
#if defined(IP_RECVTOS)
static ERL_NIF_TERM nsetopt_lvl_ip_recvtos(ErlNifEnv*        env,
                                           SocketDescriptor* descP,
                                           ERL_NIF_TERM      eVal);
#endif
#if defined(IP_ROUTER_ALERT)
static ERL_NIF_TERM nsetopt_lvl_ip_router_alert(ErlNifEnv*        env,
                                                SocketDescriptor* descP,
                                                ERL_NIF_TERM      eVal);
#endif
#if defined(IP_TOS)
static ERL_NIF_TERM nsetopt_lvl_ip_tos(ErlNifEnv*        env,
                                       SocketDescriptor* descP,
                                       ERL_NIF_TERM      eVal);
#endif
#if defined(IP_TTL)
static ERL_NIF_TERM nsetopt_lvl_ip_ttl(ErlNifEnv*        env,
                                       SocketDescriptor* descP,
                                       ERL_NIF_TERM      eVal);
#endif
#if defined(SOL_IPV6)
static ERL_NIF_TERM nsetopt_lvl_ipv6(ErlNifEnv*        env,
                                     SocketDescriptor* descP,
                                     int               eOpt,
                                     ERL_NIF_TERM      eVal);
#if defined(IPV6_HOPLIMIT)
static ERL_NIF_TERM nsetopt_lvl_ipv6_hoplimit(ErlNifEnv*        env,
                                              SocketDescriptor* descP,
                                              ERL_NIF_TERM      eVal);
#endif
#endif // defined(SOL_IPV6)
static ERL_NIF_TERM nsetopt_lvl_tcp(ErlNifEnv*        env,
                                    SocketDescriptor* descP,
                                    int               eOpt,
                                    ERL_NIF_TERM      eVal);
#if defined(TCP_CONGESTION)
static ERL_NIF_TERM nsetopt_lvl_tcp_congestion(ErlNifEnv*        env,
                                               SocketDescriptor* descP,
                                               ERL_NIF_TERM      eVal);
#endif
#if defined(TCP_MAXSEG)
static ERL_NIF_TERM nsetopt_lvl_tcp_maxseg(ErlNifEnv*        env,
                                           SocketDescriptor* descP,
                                           ERL_NIF_TERM      eVal);
#endif
#if defined(TCP_NODELAY)
static ERL_NIF_TERM nsetopt_lvl_tcp_nodelay(ErlNifEnv*        env,
                                            SocketDescriptor* descP,
                                            ERL_NIF_TERM      eVal);
#endif
static ERL_NIF_TERM nsetopt_lvl_udp(ErlNifEnv*        env,
                                    SocketDescriptor* descP,
                                    int               eOpt,
                                    ERL_NIF_TERM      eVal);
#if defined(UDP_CORK)
static ERL_NIF_TERM nsetopt_lvl_udp_cork(ErlNifEnv*        env,
                                         SocketDescriptor* descP,
                                         ERL_NIF_TERM      eVal);
#endif
#if defined(HAVE_SCTP)
static ERL_NIF_TERM nsetopt_lvl_sctp(ErlNifEnv*        env,
                                     SocketDescriptor* descP,
                                     int               eOpt,
                                     ERL_NIF_TERM      eVal);
#if defined(SCTP_AUTOCLOSE)
static ERL_NIF_TERM nsetopt_lvl_sctp_autoclose(ErlNifEnv*        env,
                                               SocketDescriptor* descP,
                                               ERL_NIF_TERM      eVal);
#endif
#if defined(SCTP_NODELAY)
static ERL_NIF_TERM nsetopt_lvl_sctp_nodelay(ErlNifEnv*        env,
                                             SocketDescriptor* descP,
                                             ERL_NIF_TERM      eVal);
#endif
#endif // defined(HAVE_SCTP)

static ERL_NIF_TERM ngetopt(ErlNifEnv*        env,
                            SocketDescriptor* descP,
                            BOOLEAN_T         isEncoded,
                            BOOLEAN_T         isOTP,
                            int               level,
                            int               eOpt);
static ERL_NIF_TERM ngetopt_otp(ErlNifEnv*        env,
                                SocketDescriptor* descP,
                                int               eOpt);
static ERL_NIF_TERM ngetopt_otp_debug(ErlNifEnv*        env,
                                      SocketDescriptor* descP);
static ERL_NIF_TERM ngetopt_otp_iow(ErlNifEnv*        env,
                                    SocketDescriptor* descP);
static ERL_NIF_TERM ngetopt_native(ErlNifEnv*        env,
                                   SocketDescriptor* descP,
                                   int               level,
                                   int               eOpt);
static ERL_NIF_TERM ngetopt_native_unspec(ErlNifEnv*        env,
                                          SocketDescriptor* descP,
                                          int               level,
                                          int               opt,
                                          SOCKOPTLEN_T      valueSz);
static ERL_NIF_TERM ngetopt_level(ErlNifEnv*        env,
                                  SocketDescriptor* descP,
                                  int               level,
                                  int               eOpt);
static ERL_NIF_TERM ngetopt_lvl_socket(ErlNifEnv*        env,
                                       SocketDescriptor* descP,
                                       int               eOpt);
#if defined(SO_BROADCAST)
static ERL_NIF_TERM ngetopt_lvl_sock_broadcast(ErlNifEnv*        env,
                                               SocketDescriptor* descP);
#endif
#if defined(SO_DONTROUTE)
static ERL_NIF_TERM ngetopt_lvl_sock_dontroute(ErlNifEnv*        env,
                                               SocketDescriptor* descP);
#endif
#if defined(SO_KEEPALIVE)
static ERL_NIF_TERM ngetopt_lvl_sock_keepalive(ErlNifEnv*        env,
                                               SocketDescriptor* descP);
#endif
#if defined(SO_LINGER)
static ERL_NIF_TERM ngetopt_lvl_sock_linger(ErlNifEnv*        env,
                                            SocketDescriptor* descP);
#endif
#if defined(SO_PRIORITY)
static ERL_NIF_TERM ngetopt_lvl_sock_priority(ErlNifEnv*        env,
                                              SocketDescriptor* descP);
#endif
#if defined(SO_RCVBUF)
static ERL_NIF_TERM ngetopt_lvl_sock_rcvbuf(ErlNifEnv*        env,
                                            SocketDescriptor* descP);
#endif
#if defined(SO_REUSEADDR)
static ERL_NIF_TERM ngetopt_lvl_sock_reuseaddr(ErlNifEnv*        env,
                                               SocketDescriptor* descP);
#endif
#if defined(SO_SNDBUF)
static ERL_NIF_TERM ngetopt_lvl_sock_sndbuf(ErlNifEnv*        env,
                                            SocketDescriptor* descP);
#endif
static ERL_NIF_TERM ngetopt_lvl_ip(ErlNifEnv*        env,
                                   SocketDescriptor* descP,
                                   int               eOpt);
#if defined(IP_RECVTOS)
static ERL_NIF_TERM ngetopt_lvl_ip_recvtos(ErlNifEnv*        env,
                                           SocketDescriptor* descP);
#endif
#if defined(IP_ROUTER_ALERT)
static ERL_NIF_TERM ngetopt_lvl_ip_router_alert(ErlNifEnv*        env,
                                                SocketDescriptor* descP);
#endif
#if defined(IP_TOS)
static ERL_NIF_TERM ngetopt_lvl_ip_tos(ErlNifEnv*        env,
                                       SocketDescriptor* descP);
#endif
#if defined(IP_TTL)
static ERL_NIF_TERM ngetopt_lvl_ip_ttl(ErlNifEnv*        env,
                                       SocketDescriptor* descP);
#endif
#if defined(SOL_IPV6)
static ERL_NIF_TERM ngetopt_lvl_ipv6(ErlNifEnv*        env,
                                     SocketDescriptor* descP,
                                     int               eOpt);
#if defined(IPV6_HOPLIMIT)
static ERL_NIF_TERM ngetopt_lvl_ipv6_hoplimit(ErlNifEnv*        env,
                                              SocketDescriptor* descP);
#endif
#endif // defined(SOL_IPV6)
static ERL_NIF_TERM ngetopt_lvl_tcp(ErlNifEnv*        env,
                                    SocketDescriptor* descP,
                                    int               eOpt);
#if defined(TCP_CONGESTION)
static ERL_NIF_TERM ngetopt_lvl_tcp_congestion(ErlNifEnv*        env,
                                               SocketDescriptor* descP);
#endif
#if defined(TCP_MAXSEG)
static ERL_NIF_TERM ngetopt_lvl_tcp_maxseg(ErlNifEnv*        env,
                                           SocketDescriptor* descP);
#endif
#if defined(TCP_NODELAY)
static ERL_NIF_TERM ngetopt_lvl_tcp_nodelay(ErlNifEnv*        env,
                                            SocketDescriptor* descP);
#endif
static ERL_NIF_TERM ngetopt_lvl_udp(ErlNifEnv*        env,
                                    SocketDescriptor* descP,
                                    int               eOpt);
#if defined(UDP_CORK)
static ERL_NIF_TERM ngetopt_lvl_udp_cork(ErlNifEnv*        env,
                                         SocketDescriptor* descP);
#endif
#if defined(HAVE_SCTP)
static ERL_NIF_TERM ngetopt_lvl_sctp(ErlNifEnv*        env,
                                     SocketDescriptor* descP,
                                     int               eOpt);
#if defined(SCTP_AUTOCLOSE)
static ERL_NIF_TERM ngetopt_lvl_sctp_autoclose(ErlNifEnv*        env,
                                               SocketDescriptor* descP);
#endif
#if defined(SCTP_NODELAY)
static ERL_NIF_TERM ngetopt_lvl_sctp_nodelay(ErlNifEnv*        env,
                                             SocketDescriptor* descP);
#endif
#endif // defined(HAVE_SCTP)

static ERL_NIF_TERM nsetopt_str_opt(ErlNifEnv*        env,
                                    SocketDescriptor* descP,
                                    int               level,
                                    int               opt,
                                    int               max,
                                    ERL_NIF_TERM      eVal);
static ERL_NIF_TERM nsetopt_bool_opt(ErlNifEnv*        env,
                                     SocketDescriptor* descP,
                                     int               level,
                                     int               opt,
                                     ERL_NIF_TERM      eVal);
static ERL_NIF_TERM nsetopt_int_opt(ErlNifEnv*        env,
                                    SocketDescriptor* descP,
                                    int               level,
                                    int               opt,
                                    ERL_NIF_TERM      eVal);

static ERL_NIF_TERM ngetopt_str_opt(ErlNifEnv*        env,
                                    SocketDescriptor* descP,
                                    int               level,
                                    int               opt,
                                    int               max);
static ERL_NIF_TERM ngetopt_bool_opt(ErlNifEnv*        env,
                                     SocketDescriptor* descP,
                                     int               level,
                                     int               opt);
static ERL_NIF_TERM ngetopt_int_opt(ErlNifEnv*        env,
                                    SocketDescriptor* descP,
                                    int               level,
                                    int               opt);

static ERL_NIF_TERM send_check_result(ErlNifEnv*        env,
                                      SocketDescriptor* descP,
                                      ssize_t           written,
                                      ssize_t           dataSize,
                                      ERL_NIF_TERM      sendRef);
static ERL_NIF_TERM recv_check_result(ErlNifEnv*        env,
                                      SocketDescriptor* descP,
                                      int               read,
                                      int               toRead,
                                      ErlNifBinary*     bufP,
                                      ERL_NIF_TERM      recvRef);
static ERL_NIF_TERM recvfrom_check_result(ErlNifEnv*        env,
                                          SocketDescriptor* descP,
                                          int               read,
                                          ErlNifBinary*     bufP,
                                          SocketAddress*    fromAddrP,
                                          unsigned int      fromAddrLen,
                                          ERL_NIF_TERM      recvRef);

static ERL_NIF_TERM nfinalize_connection(ErlNifEnv*        env,
                                         SocketDescriptor* descP);
static ERL_NIF_TERM nfinalize_close(ErlNifEnv*        env,
                                    SocketDescriptor* descP);


static char* decode_in_sockaddr(ErlNifEnv*     env,
                                ERL_NIF_TERM   eSockAddr,
                                SocketAddress* sockAddrP,
                                unsigned int*  addrLenP);
static char* decode_in4_sockaddr(ErlNifEnv*          env,
                                 const ERL_NIF_TERM* eIn4SockAddr,
                                 SocketAddress*      sockAddrP,
                                 unsigned int*       addrLenP);
static char* decode_in4_sockaddr_atomaddr(ErlNifEnv*     env,
                                          ERL_NIF_TERM   eAddr,
                                          int            port,
                                          SocketAddress* sockAddrP,
                                          unsigned int*  addrLenP);
static char* decode_in4_sockaddr_addr(ErlNifEnv*     env,
                                      ERL_NIF_TERM   eAddr,
                                      int            port,
                                      SocketAddress* sockAddrP,
                                      unsigned int*  addrLenP);
#if defined(HAVE_IN6) && defined(AF_INET6)
static char* decode_in6_sockaddr(ErlNifEnv*          env,
                                 const ERL_NIF_TERM* eIn6SockAddr,
                                 SocketAddress*      sockAddrP,
                                 unsigned int*       addrLenP);
static char* decode_in6_sockaddr_atomaddr(ErlNifEnv*     env,
                                          ERL_NIF_TERM   eAddr,
                                          int            port,
                                          unsigned int   flowInfo,
                                          unsigned int   scopeId,
                                          SocketAddress* sockAddrP,
                                          unsigned int*  addrLenP);
/* Decode an in6_sockaddr where the address field is a tuple */
static char* decode_in6_sockaddr_addr(ErlNifEnv*     env,
                                      ERL_NIF_TERM   eAddr,
                                      int            port,
                                      unsigned int   flowInfo,
                                      unsigned int   scopeId,
                                      SocketAddress* sockAddrP,
                                      unsigned int*  addrLenP);
#endif
static char* decode_laddress(ErlNifEnv*     env,
                             int            domain,
                             ERL_NIF_TERM   localAddr,
                             SocketAddress* localP,
                             unsigned int*  addrLenP);
static char* decode_laddress_binary(ErlNifEnv*     env,
                                    int            domain,
                                    ERL_NIF_TERM   localAddr,
                                    SocketAddress* localP,
                                    unsigned int*  addrLenP);
/*
static char* decode_address_tuple(ErlNifEnv*          env,
                                  int                 domain,
                                  const ERL_NIF_TERM* addrt,
                                  int                 port,
                                  SocketAddress*      localP,
                                  unsigned int*  addrLenP);
static char* decode_address_atom(ErlNifEnv*     env,
                                 int            domain,
                                 char*          addr,
                                 int            addrLen,
                                 int            port,
                                 SocketAddress* localP,
                                 unsigned int*  addrLenP);
*/
/*
static char* decode_send_addr(ErlNifEnv*      env,
                              int             domain,
                              ERL_NIF_TERM    addr,
                              int             port,
                              SocketAddress** toAddrP,
                              unsigned int*   addrLenP);
*/
/*
static char* decode_send_addr_tuple(ErlNifEnv*     env,
                                    int            domain,
                                    ERL_NIF_TERM   addr,
                                    int            port,
                                    SocketAddress* toAddrP,
                                    unsigned int*  addrLenP);
*/
static void encode_address(ErlNifEnv*     env,
                           SocketAddress* fromAddrP,
                           unsigned int   fromAddrLen,
                           ERL_NIF_TERM*  fromDomainT,
                           ERL_NIF_TERM*  fromSourceT);
static BOOLEAN_T decode_sock_linger(ErlNifEnv*     env,
                                    ERL_NIF_TERM   eVal,
                                    struct linger* valP);
static BOOLEAN_T decode_ip_tos(ErlNifEnv*   env,
                               ERL_NIF_TERM eVal,
                               int*         val);
/*
static BOOLEAN_T decode_bool(ErlNifEnv*   env,
                             ERL_NIF_TERM eVal,
                             BOOLEAN_T*   val);
*/
static BOOLEAN_T decode_native_get_opt(ErlNifEnv*   env,
                                       ERL_NIF_TERM eVal,
                                       int*         opt,
                                       uint16_t*    valueType,
                                       int*         valueSz);
// static void encode_bool(BOOLEAN_T val, ERL_NIF_TERM* eVal);
static ERL_NIF_TERM encode_ip_tos(ErlNifEnv* env, int val);

static void inform_waiting_procs(ErlNifEnv*          env,
                                 SocketDescriptor*   descP,
                                 SocketRequestQueue* q,
                                 BOOLEAN_T           free,
                                 ERL_NIF_TERM        reason);

static int socket_setopt(int             sock,
                         int             level,
                         int             opt,
                         const void*     optVal,
                         const socklen_t optLen);

static BOOLEAN_T verify_is_connected(SocketDescriptor* descP, int* err);

static SocketDescriptor* alloc_descriptor(SOCKET sock, HANDLE event);

static int compare_pids(ErlNifEnv*       env,
                        const ErlNifPid* pid1,
                        const ErlNifPid* pid2);



static BOOLEAN_T edomain2domain(int edomain, int* domain);
static BOOLEAN_T etype2type(int etype, int* type);
static BOOLEAN_T eproto2proto(int eproto, int* proto);
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

static BOOLEAN_T cnt_inc(uint32_t* cnt, uint32_t inc);

#if defined(HAVE_SYS_UN_H) || defined(SO_BINDTODEVICE)
static size_t my_strnlen(const char *s, size_t maxlen);
#endif

static void socket_dtor(ErlNifEnv* env, void* obj);
static void socket_stop(ErlNifEnv* env,
			void*      obj,
			int        fd,
			int        is_direct_call);
static void socket_down(ErlNifEnv*           env,
			void*                obj,
			const ErlNifPid*     pid,
			const ErlNifMonitor* mon);

/*
static char* send_msg_error_closed(ErlNifEnv*   env,
                                   ErlNifPid*   pid);
*/
/*
static char* send_msg_error(ErlNifEnv*   env,
                            ERL_NIF_TERM reason,
                            ErlNifPid*   pid);
*/
static char* send_msg_nif_abort(ErlNifEnv*   env,
                                ERL_NIF_TERM ref,
                                ERL_NIF_TERM reason,
                                ErlNifPid*   pid);
static char* send_msg(ErlNifEnv*   env,
                      ERL_NIF_TERM msg,
                      ErlNifPid*   pid);

static BOOLEAN_T extract_item_on_load(ErlNifEnv*    env,
                                      ERL_NIF_TERM  map,
                                      ERL_NIF_TERM  key,
                                      ERL_NIF_TERM* val);

static BOOLEAN_T extract_debug_on_load(ErlNifEnv*   env,
                                       ERL_NIF_TERM map,
                                       BOOLEAN_T    def);
static BOOLEAN_T extract_iow_on_load(ErlNifEnv*   env,
                                     ERL_NIF_TERM map,
                                     BOOLEAN_T    def);

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



/* *** String constants *** */
// static char str_any[]          = "any";
static char str_close[]        = "close";
static char str_closed[]       = "closed";
static char str_closing[]      = "closing";
static char str_debug[]        = "debug";
static char str_false[]        = "false";
static char str_global_counters[] = "global_counters";
static char str_in4_sockaddr[] = "in4_sockaddr";
static char str_in6_sockaddr[] = "in6_sockaddr";
static char str_iow[]          = "iow";
// static char str_loopback[]     = "loopback";
static char str_nif_abort[]    = "nif_abort";
static char str_select[]       = "select";
static char str_num_dlocal[]   = "num_domain_local";
static char str_num_dinet[]    = "num_domain_inet";
static char str_num_dinet6[]   = "num_domain_inet6";
static char str_num_pip[]      = "num_proto_ip";
static char str_num_psctp[]    = "num_proto_sctp";
static char str_num_ptcp[]     = "num_proto_tcp";
static char str_num_pudp[]     = "num_proto_udp";
static char str_num_sockets[]  = "num_sockets";
static char str_num_tdgrams[]  = "num_type_dgram";
static char str_num_tseqpkgs[] = "num_type_seqpacket";
static char str_num_tstreams[] = "num_type_stream";
static char str_timeout[]      = "timeout";
static char str_true[]         = "true";

static char str_lowdelay[]     = "lowdelay";
static char str_throughput[]   = "throughput";
static char str_reliability[]  = "reliability";
static char str_mincost[]      = "mincost";

/* (special) error string constants */
static char str_eisconn[]        = "eisconn";
static char str_enotclosing[]    = "enotclosing";
static char str_enotconn[]       = "enotconn";
static char str_exalloc[]        = "exalloc";
static char str_exbadstate[]     = "exbadstate";
static char str_exbusy[]         = "exbusy";
static char str_exmon[]          = "exmonitor";  // failed monitor
static char str_exself[]         = "exself";     // failed self
static char str_exsend[]         = "exsend";     // failed send


/* *** "Global" Atoms *** */
ERL_NIF_TERM esock_atom_addr;
ERL_NIF_TERM esock_atom_any;
ERL_NIF_TERM esock_atom_dgram;
ERL_NIF_TERM esock_atom_error;
ERL_NIF_TERM esock_atom_false;
ERL_NIF_TERM esock_atom_family;
ERL_NIF_TERM esock_atom_flowinfo;
ERL_NIF_TERM esock_atom_inet;
ERL_NIF_TERM esock_atom_inet6;
ERL_NIF_TERM esock_atom_ip;
ERL_NIF_TERM esock_atom_ipv6;
ERL_NIF_TERM esock_atom_local;
ERL_NIF_TERM esock_atom_loopback;
ERL_NIF_TERM esock_atom_ok;
ERL_NIF_TERM esock_atom_path;
ERL_NIF_TERM esock_atom_port;
ERL_NIF_TERM esock_atom_raw;
ERL_NIF_TERM esock_atom_rdm;
ERL_NIF_TERM esock_atom_scope_id;
ERL_NIF_TERM esock_atom_sctp;
ERL_NIF_TERM esock_atom_seqpacket;
ERL_NIF_TERM esock_atom_stream;
ERL_NIF_TERM esock_atom_tcp;
ERL_NIF_TERM esock_atom_true;
ERL_NIF_TERM esock_atom_udp;
ERL_NIF_TERM esock_atom_undefined;

/* *** "Global" error (=reason) atoms *** */
ERL_NIF_TERM esock_atom_eagain;
ERL_NIF_TERM esock_atom_eafnosupport;
ERL_NIF_TERM esock_atom_einval;

/* *** Atoms *** */
static ERL_NIF_TERM atom_close;
static ERL_NIF_TERM atom_closed;
static ERL_NIF_TERM atom_closing;
static ERL_NIF_TERM atom_debug;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_global_counters;
static ERL_NIF_TERM atom_in4_sockaddr;
static ERL_NIF_TERM atom_in6_sockaddr;
static ERL_NIF_TERM atom_iow;
static ERL_NIF_TERM atom_nif_abort;
static ERL_NIF_TERM atom_num_dinet;
static ERL_NIF_TERM atom_num_dinet6;
static ERL_NIF_TERM atom_num_dlocal;
static ERL_NIF_TERM atom_num_pip;
static ERL_NIF_TERM atom_num_psctp;
static ERL_NIF_TERM atom_num_ptcp;
static ERL_NIF_TERM atom_num_pudp;
static ERL_NIF_TERM atom_num_sockets;
static ERL_NIF_TERM atom_num_tdgrams;
static ERL_NIF_TERM atom_num_tseqpkgs;
static ERL_NIF_TERM atom_num_tstreams;
static ERL_NIF_TERM atom_select;
static ERL_NIF_TERM atom_timeout;
static ERL_NIF_TERM atom_true;

static ERL_NIF_TERM atom_lowdelay;
static ERL_NIF_TERM atom_throughput;
static ERL_NIF_TERM atom_reliability;
static ERL_NIF_TERM atom_mincost;

static ERL_NIF_TERM atom_eisconn;
static ERL_NIF_TERM atom_enotclosing;
static ERL_NIF_TERM atom_enotconn;
static ERL_NIF_TERM atom_exalloc;
static ERL_NIF_TERM atom_exbadstate;
static ERL_NIF_TERM atom_exbusy;
static ERL_NIF_TERM atom_exmon;
static ERL_NIF_TERM atom_exself;
static ERL_NIF_TERM atom_exsend;


/* *** Sockets *** */
static ErlNifResourceType*    sockets;
static ErlNifResourceTypeInit socketInit = {
   socket_dtor,
   socket_stop,
   (ErlNifResourceDown*) socket_down
};

// Initiated when the nif is loaded
static SocketData data;


/* ----------------------------------------------------------------------
 *  N I F   F u n c t i o n s
 * ----------------------------------------------------------------------
 *
 * Utility and admin functions:
 * ----------------------------
 * nif_is_loaded/0
 * nif_info/0
 * (nif_debug/1)
 *
 * The "proper" socket functions:
 * ------------------------------
 * nif_open(Domain, Type, Protocol, Extra)
 * nif_bind(Sock, LocalAddr)
 * nif_connect(Sock, SockAddr)
 * nif_listen(Sock, Backlog)
 * nif_accept(LSock, Ref)
 * nif_send(Sock, SendRef, Data, Flags)
 * nif_sendto(Sock, SendRef, Data, Flags, DstSockAddr)
 * nif_recv(Sock, RecvRef, Length, Flags)
 * nif_recvfrom(Sock, Flags)
 * nif_close(Sock)
 * nif_shutdown(Sock, How)
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
 * nif_is_loaded
 *
 * Description:
 * This functions only purpose is to return the atom 'true'.
 * This will happen *if* the (socket) nif library is loaded.
 * If its not, the erlang (nif_is_loaded) will instead return
 * 'false'.
 */
static
ERL_NIF_TERM nif_is_loaded(ErlNifEnv*         env,
                           int                argc,
                           const ERL_NIF_TERM argv[])
{
    if (argc != 0)
        return enif_make_badarg(env);

    return atom_true;
}


/* ----------------------------------------------------------------------
 * nif_info
 *
 * Description:
 * This is currently just a placeholder...
 */
#define MKCT(E, T, C) MKT2((E), (T), MKI((E), (C)))

static
ERL_NIF_TERM nif_info(ErlNifEnv*         env,
                      int                argc,
                      const ERL_NIF_TERM argv[])
{
    if (argc != 0) {
        return enif_make_badarg(env);
    } else {
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
        ERL_NIF_TERM keys[]  = {atom_debug, atom_iow, atom_global_counters};
        ERL_NIF_TERM vals[]  = {BOOL2ATOM(data.dbg), BOOL2ATOM(data.iow), lgcnt};
        ERL_NIF_TERM info;
        unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);
        unsigned int numVals = sizeof(keys) / sizeof(ERL_NIF_TERM);

        ESOCK_ASSERT( (numKeys == numVals) );

        if (!MKMA(env, keys, vals, numKeys, &info))
            return enif_make_badarg(env);
    
        return info;
    }
}


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
 */
static
ERL_NIF_TERM nif_open(ErlNifEnv*         env,
                      int                argc,
                      const ERL_NIF_TERM argv[])
{
    int          edomain, etype, eproto;
    int          domain,  type,  proto;
    char*        netns;
    ERL_NIF_TERM emap;
    ERL_NIF_TERM result;

    SGDBG( ("SOCKET", "nif_open -> entry with %d args\r\n", argc) );
    
    /* Extract arguments and perform preliminary validation */

    if ((argc != 4) ||
        !GET_INT(env, argv[0], &edomain) ||
        !GET_INT(env, argv[1], &etype) ||
        !GET_INT(env, argv[2], &eproto) ||
        !IS_MAP(env,  argv[3])) {
        return enif_make_badarg(env);
    }
    emap = argv[3];

    SGDBG( ("SOCKET", "nif_open -> "
            "\r\n   edomain: %T"
            "\r\n   etype:   %T"
            "\r\n   eproto:  %T"
            "\r\n   extra:   %T"
            "\r\n", argv[0], argv[1], argv[2], argv[3]) );

    if (!edomain2domain(edomain, &domain))
        return enif_make_badarg(env);

    if (!etype2type(etype, &type))
        return enif_make_badarg(env);

    if (!eproto2proto(eproto, &proto))
        return enif_make_badarg(env);

#ifdef HAVE_SETNS
    /* We *currently* only support one extra option: netns */
    if (!emap2netns(env, emap, &netns))
        return enif_make_badarg(env);
#else
    netns = NULL;
#endif

    result = nopen(env, domain, type, proto, netns);

    SGDBG( ("SOCKET", "nif_open -> done with result: "
           "\r\n   %T"
           "\r\n", result) );

    return result;
}


/* nopen - create an endpoint for communication
 *
 * Assumes the input has been validated.
 *
 * Normally we want debugging on (individual) sockets to be controlled
 * by the sockets own debug flag. But since we don't even have a socket
 * yet, we must use the global debug flag.
 */
static
ERL_NIF_TERM nopen(ErlNifEnv* env,
                   int domain, int type, int protocol,
                   char* netns)
{
    SocketDescriptor* descP;
    ERL_NIF_TERM      res;
    int               save_errno = 0;
    SOCKET            sock;
    HANDLE            event;
#ifdef HAVE_SETNS
    int               current_ns;
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

    if ((sock = sock_open(domain, type, protocol)) == INVALID_SOCKET)
        return esock_make_error_errno(env, sock_errno());

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
    descP->protocol = protocol;

    res = enif_make_resource(env, descP);
    enif_release_resource(descP); // We should really store a reference ...


    /* Keep track of the creator
     * This should not be a problem but just in case
     * the *open* function is used with the wrong kind
     * of environment...
     */
    if (enif_self(env, &descP->ctrlPid) == NULL)
        return esock_make_error(env, atom_exself);

    if (MONP(env, descP,
             &descP->ctrlPid,
             &descP->ctrlMon) > 0)
        return esock_make_error(env, atom_exmon);


#ifdef __WIN32__
    /* <KOLLA
     *
     * What is the point of this?
     * And how do we handle it?
     * Since the select message will be delivered to the controlling
     * process, which has no idea what to do with this...
     *
     * TODO!
     *
     * </KOLLA>
     */
     SELECT(env,
            event,
            (ERL_NIF_SELECT_READ),
            descP, NULL, esock_atom_undefined);
#endif


    return esock_make_ok2(env, res);
}



#ifdef HAVE_SETNS
/* We should really have another API, so that we can return errno... */

/* *** change network namespace ***
 * Retreive the current namespace and set the new.
 * Return result and previous namespace if successfull.
 */
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


/* *** restore network namespace ***
 * Restore the previous namespace (see above).
 */
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
#endif



/* ----------------------------------------------------------------------
 * nif_bind
 *
 * Description:
 * Bind a name to a socket.
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * LocalAddr    - Local address is either:
 *                - a binary - when the socket domain = local
 *                - a tuple of size 2 with
 *                  - first element is the address
 *                  - second element is the port number
 *                The address can be in the form of either:
 *                - A tuple of size 4 or 8 (depending on domain)
 *                - The atom 'any'
 *                - The atom 'loopback'
 */
static
ERL_NIF_TERM nif_bind(ErlNifEnv*         env,
                      int                argc,
                      const ERL_NIF_TERM argv[])
{
    SocketDescriptor* descP;

    /* Extract arguments and perform preliminary validation */

    if ((argc != 2) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }

    SSDBG( descP,
           ("SOCKET", "nif_bind -> "
            "\r\n   Socket: %T"
            "\r\n   Addr:   %T"
            "\r\n", argv[0], argv[1]) );

    /* Basic arg validation:
     *  - if binary domain must be local (unix)
     *  - if tuple domain must be either inet or inet6
     */
    if (IS_BIN(env, argv[1]) && (descP->domain != AF_UNIX)) {
        return enif_make_badarg(env);
    } else if (IS_TUPLE(env, argv[1]) &&
               (descP->domain != AF_INET) &&
               (descP->domain != AF_INET6)) {
        return enif_make_badarg(env);
    }


    /* Make sure we are ready
     * Not sure how this would even happen, but...
     */
    /* WHY NOT !IS_OPEN(...) */
    if (descP->state != SOCKET_STATE_OPEN)
        return esock_make_error(env, atom_exbadstate);

    return nbind(env, descP, argv[1]);
}


static
ERL_NIF_TERM nbind(ErlNifEnv*        env,
                   SocketDescriptor* descP,
                   ERL_NIF_TERM      addr)
{
    SocketAddress local;
    unsigned int  addrLen = 0;
    char*         xerr;
    int           port;

    SSDBG( descP,
           ("SOCKET", "nbind -> entry with"
            "\r\n   addr: %T"
            "\r\n", addr) );

    if ((xerr = decode_laddress(env,
                                descP->domain, addr, &local, &addrLen)) != NULL)
        return esock_make_error_str(env, xerr);

    SSDBG( descP, ("SOCKET", "nbind -> try bind\r\n") );

    if (IS_SOCKET_ERROR(sock_bind(descP->sock,
                                  (struct sockaddr*) &local, addrLen))) {
        return esock_make_error_errno(env, sock_errno());
    }

    port = which_address_port(&local);
    if (port == 0) {
        SOCKLEN_T len = sizeof(local);
        sys_memzero((char *) &local, len);
        sock_name(descP->sock, &local.sa, &len);
        port = which_address_port(&local);
    } else if (port == -1) {
        port = 0;
    }

    SSDBG( descP, ("SOCKET", "nbind -> done with port = %d\r\n", port) );

    return esock_make_ok2(env, MKI(env, port));

}


/* Decode the (local) address. The format of the address should
 * either be an binary (domain = local) or an in_sockaddr(), which
 * is either a in4_sockaddr record or a int6_sockaddr record 
 * (if domain is either inet or inet6).
 */
static
char* decode_laddress(ErlNifEnv*     env,
                      int            domain,
                      ERL_NIF_TERM   localAddr,
                      SocketAddress* localP,
                      unsigned int*  addrLenP)
{
    if (IS_BIN(env, localAddr)) {
        return decode_laddress_binary(env, domain, localAddr, localP, addrLenP);
    } else if (IS_TUPLE(env, localAddr)) {
        return decode_in_sockaddr(env, localAddr, localP, addrLenP);
    }  else {
        return ESOCK_STR_EINVAL;
    }

}


/* Only for domain = local (unix)
 * The erlang interface module (socket) ensures that the size of the
 * binary is > 0, so we need not do that here.
 */
static
char* decode_laddress_binary(ErlNifEnv*     env,
                             int            domain,
                             ERL_NIF_TERM   localAddr,
                             SocketAddress* localP,
                             unsigned int*  addrLenP)
{
    unsigned int addrLen;

#ifdef HAVE_SYS_UN_H
    ErlNifBinary bin;

    if (domain != AF_UNIX)
        return ESOCK_STR_EINVAL;

    if (!GET_BIN(env, localAddr, &bin))
        return ESOCK_STR_EINVAL;

    if ((bin.size +
#ifdef __linux__
    /* Make sure the address gets zero terminated
     * except when the first byte is \0 because then it is
     * sort of zero terminated although the zero termination
     * comes before the address...
     * This fix handles Linux's nonportable
     * abstract socket address extension.
     */
    (bin.data[0] == '\0' ? 0 : 1)
#else
        1
#endif
         ) > sizeof(localP->un.sun_path))
        return ESOCK_STR_EINVAL;

    sys_memzero((char*)localP, sizeof(struct sockaddr_un));
    localP->un.sun_family = domain;
    sys_memcpy(localP->un.sun_path, bin.data, bin.size);
    addrLen = offsetof(struct sockaddr_un, sun_path) + bin.size;
#ifndef NO_SA_LEN
    localP->un.sun_len = addrLen;
#endif
    *addrLenP = addrLen;
    return NULL;

#else // HAVE_SYS_UN_H
    return str_eafnosupport;
#endif

}



/* ----------------------------------------------------------------------
 * nif_connect
 *
 * Description:
 * Initiate a connection on a socket
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * SockAddr     - Socket Address of "remote" host.
 *                This is in_sockaddr(), which is either
 *                in4_sockaddr (#in4_sockaddr{}) or 
 *                in6_sockaddr (#in6_sockaddr{}).
 */
static
ERL_NIF_TERM nif_connect(ErlNifEnv*         env,
                         int                argc,
                         const ERL_NIF_TERM argv[])
{
    SocketDescriptor* descP;
    ERL_NIF_TERM      eSockAddr;
    char*             xres;

    /* Extract arguments and perform preliminary validation */

    if ((argc != 2) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }
    eSockAddr = argv[1];

    if ((xres = decode_in_sockaddr(env, eSockAddr,
                                   &descP->remote, &descP->addrLen)) != NULL) {
        return esock_make_error_str(env, xres);
    }

    return nconnect(env, descP);
}


static
ERL_NIF_TERM nconnect(ErlNifEnv*        env,
                      SocketDescriptor* descP)
{
    int code;

    /* Verify that we are where in the proper state */

    if (!IS_OPEN(descP))
        return esock_make_error(env, atom_exbadstate);

    if (IS_CONNECTED(descP))
        return esock_make_error(env, atom_eisconn);

    if (IS_CONNECTING(descP))
        return esock_make_error(env, esock_atom_einval);

    code = sock_connect(descP->sock,
                        (struct sockaddr*) &descP->remote,
                        descP->addrLen);

    if (IS_SOCKET_ERROR(code) &&
        ((sock_errno() == ERRNO_BLOCK) ||   /* Winsock2            */
         (sock_errno() == EINPROGRESS))) {  /* Unix & OSE!!        */
        ERL_NIF_TERM ref = MKREF(env);
        descP->state = SOCKET_STATE_CONNECTING;
        SELECT(env,
               descP->sock,
               (ERL_NIF_SELECT_WRITE),
               descP, NULL, ref);
        return esock_make_ok2(env, ref);
    } else if (code == 0) {                 /* ok we are connected */
        descP->state = SOCKET_STATE_CONNECTED;
        /* Do we need to do somthing for "active" mode?
         * Is there even such a thing *here*?
         */
        return esock_atom_ok;
    } else {
        return esock_make_error_errno(env, sock_errno());
    }

}


/* ----------------------------------------------------------------------
 * nif_finalize_connection
 *
 * Description:
 * Make socket ready for input and output.
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 */
static
ERL_NIF_TERM nif_finalize_connection(ErlNifEnv*         env,
                                     int                argc,
                                     const ERL_NIF_TERM argv[])
{
    SocketDescriptor* descP;

    /* Extract arguments and perform preliminary validation */

    if ((argc != 1) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }

    return nfinalize_connection(env, descP);

}


/* *** nfinalize_connection ***
 * Perform the final check to verify a connection.
 */
static
ERL_NIF_TERM nfinalize_connection(ErlNifEnv*        env,
                                  SocketDescriptor* descP)
{
    int error;

    if (descP->state != SOCKET_STATE_CONNECTING)
        return esock_make_error(env, atom_enotconn);

    if (!verify_is_connected(descP, &error)) {
        descP->state = SOCKET_STATE_OPEN;  /* restore state */
        return esock_make_error_errno(env, error);
    }

    descP->state = SOCKET_STATE_CONNECTED;

    return esock_atom_ok;
}


/* *** verify_is_connected ***
 * Check if a connection has been established.
 */
static
BOOLEAN_T verify_is_connected(SocketDescriptor* descP, int* err)
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
    SocketDescriptor* descP;
    int               backlog;

    /* Extract arguments and perform preliminary validation */

    if ((argc != 2) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP) ||
        !GET_INT(env, argv[1], &backlog)) {
        return enif_make_badarg(env);
    }

    return nlisten(env, descP, backlog);
}



static
ERL_NIF_TERM nlisten(ErlNifEnv*        env,
                     SocketDescriptor* descP,
                     int               backlog)
{
    if (descP->state == SOCKET_STATE_CLOSED)
        return esock_make_error(env, atom_exbadstate);

    if (!IS_OPEN(descP))
        return esock_make_error(env, atom_exbadstate);

    if (IS_SOCKET_ERROR(sock_listen(descP->sock, backlog)))
        return esock_make_error_errno(env, sock_errno());

    descP->state = SOCKET_STATE_LISTENING;

    return esock_atom_ok;
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
    SocketDescriptor* descP;
    ERL_NIF_TERM      ref;

    /* Extract arguments and perform preliminary validation */

    if ((argc != 2) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }
    ref = argv[1];

    return naccept(env, descP, ref);
}


static
ERL_NIF_TERM naccept(ErlNifEnv*        env,
                     SocketDescriptor* descP,
                     ERL_NIF_TERM      ref)
{
    ERL_NIF_TERM res;

    switch (descP->state) {
    case SOCKET_STATE_LISTENING:
        MLOCK(descP->accMtx);
        res = naccept_listening(env, descP, ref);
        MUNLOCK(descP->accMtx);
        break;

    case SOCKET_STATE_ACCEPTING:
        MLOCK(descP->accMtx);
        res = naccept_accepting(env, descP, ref);
        MUNLOCK(descP->accMtx);
        break;

    default:
        res = esock_make_error(env, esock_atom_einval);
        break;
    }

    return res;
}


/* *** naccept_listening ***
 * We have no active acceptor and no acceptors in queue.
 */
static
ERL_NIF_TERM naccept_listening(ErlNifEnv*        env,
                               SocketDescriptor* descP,
                               ERL_NIF_TERM      ref)
{
    SocketAddress remote;
    unsigned int  n;
    SOCKET        accSock;
    HANDLE        accEvent;
    int           save_errno;
    ErlNifPid     caller;

    if (enif_self(env, &caller) == NULL)
        return esock_make_error(env, atom_exself);

    n = sizeof(remote);
    sys_memzero((char *) &remote, n);
    accSock = sock_accept(descP->sock, (struct sockaddr*) &remote, &n);
    if (accSock == INVALID_SOCKET) {
        save_errno = sock_errno();
        if (save_errno == ERRNO_BLOCK) {

            /* *** Try again later *** */

            descP->currentAcceptor.pid = caller;
            if (MONP(env, descP,
                     &descP->currentAcceptor.pid,
                     &descP->currentAcceptor.mon) > 0)
                return esock_make_error(env, atom_exmon);

            descP->currentAcceptor.ref = ref;

            SELECT(env,
                   descP->sock,
                   (ERL_NIF_SELECT_READ),
                   descP, NULL, ref);

            /* Shall we really change state?
             * The ready event is sent directly to the calling
             * process, which simply calls this function again.
             * Basically, state accepting means that we have
             * an "outstanding" accept.
             * Shall we store the pid of the calling process?
             * And if someone else calls accept, return with ebusy?
             * Can any process call accept or just the controlling
             * process?
             * We also need a monitor it case the calling process is
             * called before we are done!
             *
             * Change state (to accepting) and store pid of the acceptor
             * (current process). Only accept calls from the acceptor
             * process (ebusy) and once we have a successful accept,
             * change state back to listening. If cancel is called instead
             * (only accepted from the acceptor process), we reset
             * state to listening and also resets the pid to "null"
             * (is there such a value?).
             * Need a mutex to secure that we don't test and change the
             * pid at the same time.
             */

            descP->state = SOCKET_STATE_ACCEPTING;

            return esock_make_error(env, esock_atom_eagain);

        } else {
            return esock_make_error_errno(env, save_errno);
        }

    } else {
        SocketDescriptor* accDescP;
        ERL_NIF_TERM      accRef;

        /*
         * We got one
         */

        if ((accEvent = sock_create_event(accSock)) == INVALID_EVENT) {
            save_errno = sock_errno();
            while ((sock_close(accSock) == INVALID_SOCKET) &&
                   (sock_errno() == EINTR));
            return esock_make_error_errno(env, save_errno);
        }

        if ((accDescP = alloc_descriptor(accSock, accEvent)) == NULL) {
            sock_close(accSock);
            return enif_make_badarg(env);
        }

        accDescP->domain   = descP->domain;
        accDescP->type     = descP->type;
        accDescP->protocol = descP->protocol;

        accRef = enif_make_resource(env, accDescP);
        enif_release_resource(accDescP); // We should really store a reference ...

        accDescP->ctrlPid = caller;
        if (MONP(env, accDescP,
                 &accDescP->ctrlPid,
                 &accDescP->ctrlMon) > 0) {
            sock_close(accSock);
            return esock_make_error(env, atom_exmon);
        }

        accDescP->remote = remote;
        SET_NONBLOCKING(accDescP->sock);

#ifdef __WIN32__
        /* See 'What is the point of this?' above */
        SELECT(env,
               descP->sock,
               (ERL_NIF_SELECT_READ),
               descP, NULL, esock_atom_undefined);
#endif

        accDescP->state = SOCKET_STATE_CONNECTED;

        return esock_make_ok2(env, accRef);
    }
}


/* *** naccept_accepting ***
 * We have an active acceptor and possibly acceptors waiting in queue.
 * At the moment the queue is *not* implemented.
 */
static
ERL_NIF_TERM naccept_accepting(ErlNifEnv*        env,
                               SocketDescriptor* descP,
                               ERL_NIF_TERM      ref)
{
    SocketAddress remote;
    unsigned int  n;
    SOCKET        accSock;
    HANDLE        accEvent;
    ErlNifPid     caller;
    int           save_errno;

    if (enif_self(env, &caller) == NULL)
        return esock_make_error(env, atom_exself);

    if (compare_pids(env, &descP->currentAcceptor.pid, &caller) != 0) {
        /* This will have to do until we implement the queue.
         * When we have the queue, we should simply push this request,
         * and instead return with eagain (the caller will then wait
         * for the select message).
         */
        return esock_make_error(env, atom_exbusy);
    }

    n = sizeof(descP->remote);
    sys_memzero((char *) &remote, n);
    accSock = sock_accept(descP->sock, (struct sockaddr*) &remote, &n);
    if (accSock == INVALID_SOCKET) {
        save_errno = sock_errno();
        if (save_errno == ERRNO_BLOCK) {

            /*
             * Just try again, no real error, just a ghost trigger from poll,
             */

            SELECT(env,
                   descP->sock,
                   (ERL_NIF_SELECT_READ),
                   descP, NULL, ref);

            return esock_make_error(env, esock_atom_eagain);
        } else {
            return esock_make_error_errno(env, save_errno);
        }
    } else {
        SocketDescriptor* accDescP;
        ERL_NIF_TERM      accRef;

        /*
         * We got one
         */

        if ((accEvent = sock_create_event(accSock)) == INVALID_EVENT) {
            save_errno = sock_errno();
            while ((sock_close(accSock) == INVALID_SOCKET) &&
                   (sock_errno() == EINTR));
            return esock_make_error_errno(env, save_errno);
        }

        if ((accDescP = alloc_descriptor(accSock, accEvent)) == NULL) {
            sock_close(accSock);
            return enif_make_badarg(env);
        }

        accDescP->domain   = descP->domain;
        accDescP->type     = descP->type;
        accDescP->protocol = descP->protocol;

        accRef = enif_make_resource(env, accDescP);
        enif_release_resource(accDescP); // We should really store a reference ...

        accDescP->ctrlPid = caller;
        if (MONP(env, accDescP,
                 &accDescP->ctrlPid,
                 &accDescP->ctrlMon) > 0) {
            sock_close(accSock);
            return esock_make_error(env, atom_exmon);
        }

        accDescP->remote  = remote;
        SET_NONBLOCKING(accDescP->sock);

#ifdef __WIN32__
        /* See 'What is the point of this?' above */
        SELECT(env,
               descP->sock,
               (ERL_NIF_SELECT_READ),
               descP, NULL, esock_atom_undefined);
#endif

        accDescP->state = SOCKET_STATE_CONNECTED;


        /* Here we should have the test if we have something in the queue.
         * And if so, pop it and copy the (waiting) acceptor, and then
         * make a new select with that info).
         */
        descP->state = SOCKET_STATE_LISTENING;

        return esock_make_ok2(env, accRef);
    }
}



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
    SocketDescriptor* descP;
    ERL_NIF_TERM      sendRef;
    ErlNifBinary      data;
    unsigned int      eflags;
    int               flags;
    ERL_NIF_TERM      res;

    /* Extract arguments and perform preliminary validation */

    if ((argc != 4) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP) ||
        !GET_BIN(env, argv[2], &data) ||
        !GET_UINT(env, argv[3], &eflags)) {
        return enif_make_badarg(env);
    }
    sendRef = argv[1];

    if (!IS_CONNECTED(descP))
        return esock_make_error(env, atom_enotconn);

    if (!esendflags2sendflags(eflags, &flags))
        return enif_make_badarg(env);

    MLOCK(descP->writeMtx);

    /* We need to handle the case when another process tries
     * to write at the same time.
     * If the current write could not write its entire package
     * this time (resulting in an select). The write of the
     * other process must be made to wait until current
     * is done!
     * Basically, we need a write queue!
     *
     * A 'writing' field (boolean), which is set if we did
     * not manage to write the entire message and reset every
     * time we do.
     */

    res = nsend(env, descP, sendRef, &data, flags);

    MUNLOCK(descP->writeMtx);

    return res;
}


/* What do we do when another process tries to write
 * when the current writer has a select already waiting?
 * Queue it? And what about simultaneous read and write?
 * Queue up all operations towards the socket?
 *
 * We (may) need a currentOp field and an ops queue field.
 */
static
ERL_NIF_TERM nsend(ErlNifEnv*        env,
                   SocketDescriptor* descP,
                   ERL_NIF_TERM      sendRef,
                   ErlNifBinary*     dataP,
                   int               flags)
{
    ssize_t written;

    if (!descP->isWritable)
        return enif_make_badarg(env);

    /* We ignore the wrap for the moment.
     * Maybe we should issue a wrap-message to controlling process...
     */
    cnt_inc(&descP->writeTries, 1);

    written = sock_send(descP->sock, dataP->data, dataP->size, flags);

    return send_check_result(env, descP, written, dataP->size, sendRef);

}


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
 * Flags        - Send flags.
 * DestSockAddr - Destination (socket) address.
 */

static
ERL_NIF_TERM nif_sendto(ErlNifEnv*         env,
                        int                argc,
                        const ERL_NIF_TERM argv[])
{
    SocketDescriptor* descP;
    ERL_NIF_TERM      sendRef;
    ErlNifBinary      data;
    unsigned int      eflags;
    int               flags;
    ERL_NIF_TERM      eSockAddr;
    SocketAddress     remoteAddr;
    unsigned int      remoteAddrLen;
    char*             xres;

    /* Extract arguments and perform preliminary validation */

    if ((argc != 6) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP) ||
        !GET_BIN(env, argv[2], &data) ||
        !GET_UINT(env, argv[3], &eflags)) {
        return enif_make_badarg(env);
    }
    sendRef   = argv[1];
    eSockAddr = argv[4];

    /* THIS TEST IS NOT CORRECT!!! */
    if (!IS_OPEN(descP))
        return esock_make_error(env, esock_atom_einval);

    if (!esendflags2sendflags(eflags, &flags))
        return esock_make_error(env, esock_atom_einval);

    if ((xres = decode_in_sockaddr(env, eSockAddr,
                                   &remoteAddr,
                                   &remoteAddrLen)) != NULL)
        return esock_make_error_str(env, xres);

    return nsendto(env, descP, sendRef, &data, flags, &remoteAddr, remoteAddrLen);
}


static
ERL_NIF_TERM nsendto(ErlNifEnv*        env,
                     SocketDescriptor* descP,
                     ERL_NIF_TERM      sendRef,
                     ErlNifBinary*     dataP,
                     int               flags,
                     SocketAddress*    toAddrP,
                     unsigned int      toAddrLen)
{
    ssize_t written;

    if (!descP->isWritable)
        return enif_make_badarg(env);

    /* We ignore the wrap for the moment.
     * Maybe we should issue a wrap-message to controlling process...
     */
    cnt_inc(&descP->writeTries, 1);

    if (toAddrP != NULL) {
        written = sock_sendto(descP->sock,
                              dataP->data, dataP->size, flags,
                              &toAddrP->sa, toAddrLen);
    } else {
        written = sock_sendto(descP->sock,
                              dataP->data, dataP->size, flags,
                              NULL, 0);
    }

    return send_check_result(env, descP, written, dataP->size, sendRef);
}



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
ERL_NIF_TERM nwritev(ErlNifEnv*        env,
                     SocketDescriptor* descP,
                     ERL_NIF_TERM      sendRef,
                     ERL_NIF_TERM      data)
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
    SocketDescriptor* descP;
    ERL_NIF_TERM      recvRef;
    int               len;
    unsigned int      eflags;
    int               flags;
    ERL_NIF_TERM      res;

    if ((argc != 4) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP) ||
        !GET_INT(env, argv[2], &len) ||
        !GET_UINT(env, argv[3], &eflags)) {
        return enif_make_badarg(env);
    }
    recvRef  = argv[1];

    if (!IS_CONNECTED(descP))
        return esock_make_error(env, atom_enotconn);

    if (!erecvflags2recvflags(eflags, &flags))
        return enif_make_badarg(env);

    MLOCK(descP->readMtx);

    /* We need to handle the case when another process tries
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
     */

    res = nrecv(env, descP, recvRef, len, flags);

    MUNLOCK(descP->readMtx);

    return res;

}


/* The (read) buffer handling *must* be optimized!
 * But for now we make it easy for ourselves by
 * allocating a binary (of the specified or default
 * size) and then throwing it away...
 */
static
ERL_NIF_TERM nrecv(ErlNifEnv*        env,
                   SocketDescriptor* descP,
                   ERL_NIF_TERM      recvRef,
                   int               len,
                   int               flags)
{
    ssize_t      read;
    ErlNifBinary buf;

    if (!descP->isReadable)
        return enif_make_badarg(env);

    /* Allocate a buffer:
     * Either as much as we want to read or (if zero (0)) use the "default"
     * size (what has been configured).
     */
    if (!ALLOC_BIN((len ? len : descP->rBufSz), &buf))
        return esock_make_error(env, atom_exalloc);

    /* We ignore the wrap for the moment.
     * Maybe we should issue a wrap-message to controlling process...
     */
    cnt_inc(&descP->readTries, 1);

    read = sock_recv(descP->sock, buf.data, buf.size, flags);

    return recv_check_result(env, descP,
                             read, len,
                             &buf,
                             recvRef);
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
 * Socket (ref) - Points to the socket descriptor.
 * RecvRef      - A unique id for this (send) request.
 * BufSz        - Size of the buffer into which we put the received message.
 * Flags        - Receive flags.
 */

static
ERL_NIF_TERM nif_recvfrom(ErlNifEnv*         env,
                          int                argc,
                          const ERL_NIF_TERM argv[])
{
    SocketDescriptor* descP;
    ERL_NIF_TERM      recvRef;
    unsigned int      bufSz;
    unsigned int      eflags;
    int               flags;
    ERL_NIF_TERM      res;

    if ((argc != 4) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP) ||
        !GET_UINT(env, argv[2], &bufSz) ||
        !GET_UINT(env, argv[3], &eflags)) {
        return enif_make_badarg(env);
    }
    recvRef  = argv[1];

    /* if (IS_OPEN(descP)) */
    /*     return esock_make_error(env, atom_enotconn); */

    if (!erecvflags2recvflags(eflags, &flags))
        return enif_make_badarg(env);

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

    res = nrecvfrom(env, descP, recvRef, bufSz, flags);

    MUNLOCK(descP->readMtx);

    return res;

}


/* The (read) buffer handling *must* be optimized!
 * But for now we make it easy for ourselves by
 * allocating a binary (of the specified or default
 * size) and then throwing it away...
 */
static
ERL_NIF_TERM nrecvfrom(ErlNifEnv*        env,
                       SocketDescriptor* descP,
                       ERL_NIF_TERM      recvRef,
                       uint16_t          bufSz,
                       int               flags)
{
    SocketAddress fromAddr;
    unsigned int  addrLen;
    ssize_t       read;
    ErlNifBinary  buf;

    if (!descP->isReadable)
        return enif_make_badarg(env);

    /* Allocate a buffer:
     * Either as much as we want to read or (if zero (0)) use the "default"
     * size (what has been configured).
     */
    if (!ALLOC_BIN((bufSz ? bufSz : descP->rBufSz), &buf))
        return esock_make_error(env, atom_exalloc);

    /* We ignore the wrap for the moment.
     * Maybe we should issue a wrap-message to controlling process...
     */
    cnt_inc(&descP->readTries, 1);

    addrLen = sizeof(fromAddr);
    sys_memzero((char*) &fromAddr, addrLen);

    read = sock_recvfrom(descP->sock, buf.data, buf.size, flags,
                         &fromAddr.sa, &addrLen);

    return recvfrom_check_result(env, descP,
                                 read,
                                 &buf,
                                 &fromAddr, addrLen,
                                 recvRef);
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
    SocketDescriptor* descP;

    if ((argc != 1) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }

    return nclose(env, descP);
}


static
ERL_NIF_TERM nclose(ErlNifEnv*        env,
                    SocketDescriptor* descP)
{
    ERL_NIF_TERM reply, reason;
    BOOLEAN_T    doClose;
    int          selectRes;

    MLOCK(descP->closeMtx);

    if (descP->state == SOCKET_STATE_CLOSED) {
        reason  = atom_closed;
        doClose = FALSE;
    } else if (descP->state == SOCKET_STATE_CLOSING) {
        reason  = atom_closing;
        doClose = FALSE;
    } else {

        /* Store the PID of the caller,
         * since we need to inform it when we
         * (that is, the stop callback function)
         * completes.
         */

        if (enif_self(env, &descP->closerPid) == NULL) {
            MUNLOCK(descP->closeMtx);
            return esock_make_error(env, atom_exself);
        }

        /* Monitor the caller, since we should complete this operation even if
         * the caller dies (for whatever reason).
         */

        if (MONP(env, descP,
             &descP->closerPid,
                 &descP->closerMon) > 0) {
            MUNLOCK(descP->closeMtx);
            return esock_make_error(env, atom_exmon);
        }

        descP->closeLocal = TRUE;
        descP->state      = SOCKET_STATE_CLOSING;
        doClose           = TRUE;
    }

    MUNLOCK(descP->closeMtx);

    if (doClose) {
        descP->closeRef = MKREF(env);
        selectRes       = enif_select(env, descP->sock, (ERL_NIF_SELECT_STOP),
                                      descP, NULL, descP->closeRef);
        if (selectRes & ERL_NIF_SELECT_STOP_CALLED) {
            /* Prep done - inform the caller it can finalize (close) directly */
            reply = esock_atom_ok;
        } else if (selectRes & ERL_NIF_SELECT_STOP_SCHEDULED) {
            /* The stop callback function has been *scheduled* which means that we
             * have to wait for it to complete. */
            reply = esock_make_ok2(env, descP->closeRef);
        } else {
            /* <KOLLA>
             *
             * WE SHOULD REALLY HAVE A WAY TO CLOBBER THE SOCKET,
             * SO WE DON'T LET STUFF LEAK.
             * NOW, BECAUSE WE FAILED TO SELECT, WE CANNOT FINISH
             * THE CLOSE, WHAT TO DO? ABORT?
             *
             * </KOLLA>
             */
            reason = MKT2(env, atom_select, MKI(env, selectRes));
            reply  = esock_make_error(env, reason);
        }
    } else {
        reply = esock_make_error(env, reason);
    }

    return reply;
}



/* ----------------------------------------------------------------------
 * nif_finalize_close
 *
 * Description:
 * Perform the actual socket close!
 * Note that this function is executed in a dirfty scheduler.
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 */
static
ERL_NIF_TERM nif_finalize_close(ErlNifEnv*         env,
                                int                argc,
                                const ERL_NIF_TERM argv[])
{
    SocketDescriptor* descP;

    /* Extract arguments and perform preliminary validation */

    if ((argc != 1) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP)) {
        return enif_make_badarg(env);
    }

    return nfinalize_close(env, descP);

}


/* *** nfinalize_close ***
 * Perform the final step in the socket close.
 */
static
ERL_NIF_TERM nfinalize_close(ErlNifEnv*        env,
                             SocketDescriptor* descP)
{
    ERL_NIF_TERM reply;

    if (descP->state == SOCKET_STATE_CLOSED)
        return esock_atom_ok;

    if (descP->state != SOCKET_STATE_CLOSING)
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



/* ----------------------------------------------------------------------
 * nif_shutdown
 *
 * Description:
 * Disable sends and/or receives on a socket.
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * How          - What will be shutdown.
 */

static
ERL_NIF_TERM nif_shutdown(ErlNifEnv*         env,
                          int                argc,
                          const ERL_NIF_TERM argv[])
{
    SocketDescriptor* descP;
    unsigned int      ehow;
    int               how;

    if ((argc != 2) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP) ||
        !GET_UINT(env, argv[1], &ehow)) {
        return enif_make_badarg(env);
    }

    if (!ehow2how(ehow, &how))
        return enif_make_badarg(env);

    return nshutdown(env, descP, how);
}



static
ERL_NIF_TERM nshutdown(ErlNifEnv*        env,
                       SocketDescriptor* descP,
                       int               how)
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
    SocketDescriptor* descP;
    int               eLevel, level = -1;
    int               eOpt;
    ERL_NIF_TERM      eIsEncoded;
    ERL_NIF_TERM      eVal;
    BOOLEAN_T         isEncoded, isOTP;

    if ((argc != 5) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP) ||
        !GET_INT(env, argv[2], &eLevel) ||
        !GET_INT(env, argv[3], &eOpt)) {
        return enif_make_badarg(env);
    }
    eIsEncoded = argv[1];
    eVal       = argv[4];

    isEncoded = esock_decode_bool(eIsEncoded);

    if (!elevel2level(isEncoded, eLevel, &isOTP, &level))
        return esock_make_error(env, esock_atom_einval);

    return nsetopt(env, descP, isEncoded, isOTP, level, eOpt, eVal);
}


static
ERL_NIF_TERM nsetopt(ErlNifEnv*        env,
                     SocketDescriptor* descP,
                     BOOLEAN_T         isEncoded,
                     BOOLEAN_T         isOTP,
                     int               level,
                     int               eOpt,
                     ERL_NIF_TERM      eVal)
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
ERL_NIF_TERM nsetopt_otp(ErlNifEnv*        env,
                         SocketDescriptor* descP,
                         int               eOpt,
                         ERL_NIF_TERM      eVal)
{
    ERL_NIF_TERM result;

    switch (eOpt) {
    case SOCKET_OPT_OTP_DEBUG:
        result = nsetopt_otp_debug(env, descP, eVal);
        break;

    case SOCKET_OPT_OTP_IOW:
        result = nsetopt_otp_iow(env, descP, eVal);
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
ERL_NIF_TERM nsetopt_otp_debug(ErlNifEnv*        env,
                               SocketDescriptor* descP,
                               ERL_NIF_TERM      eVal)
{
    descP->dbg = esock_decode_bool(eVal);

    return esock_atom_ok;
}


/* nsetopt_otp_iow - Handle the OTP (level) iow options
 */
static
ERL_NIF_TERM nsetopt_otp_iow(ErlNifEnv*        env,
                             SocketDescriptor* descP,
                             ERL_NIF_TERM      eVal)
{
    descP->iow = esock_decode_bool(eVal);

    return esock_atom_ok;
}



/* The option has *not* been encoded. Instead it has been provided
 * in "native mode" (option is provided as is and value as a binary).
 */
static
ERL_NIF_TERM nsetopt_native(ErlNifEnv*        env,
                            SocketDescriptor* descP,
                            int               level,
                            int               opt,
                            ERL_NIF_TERM      eVal)
{
    ErlNifBinary val;
    ERL_NIF_TERM result;

    if (GET_BIN(env, eVal, &val)) {
        int res = socket_setopt(descP->sock, level, opt,
                                val.data, val.size);
        if (res != 0)
            result = esock_make_error_errno(env, res);
        else
            result = esock_atom_ok;
    } else {
        result = esock_make_error(env, esock_atom_einval);
    }

    return result;
}



/* nsetopt_level - A "proper" level (option) has been specified
 */
static
ERL_NIF_TERM nsetopt_level(ErlNifEnv*        env,
                           SocketDescriptor* descP,
                           int               level,
                           int               eOpt,
                           ERL_NIF_TERM      eVal)
{
    ERL_NIF_TERM result;

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

#if defined(SOL_IPV6)
    case SOL_IPV6:
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
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}



/* nsetopt_lvl_socket - Level *SOCKET* option
 */
static
ERL_NIF_TERM nsetopt_lvl_socket(ErlNifEnv*        env,
                                SocketDescriptor* descP,
                                int               eOpt,
                                ERL_NIF_TERM      eVal)
{
    ERL_NIF_TERM result;

    switch (eOpt) {
#if defined(SO_BROADCAST)
    case SOCKET_OPT_SOCK_BROADCAST:
        result = nsetopt_lvl_sock_broadcast(env, descP, eVal);
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

#if defined(SO_REUSEADDR)
    case SOCKET_OPT_SOCK_REUSEADDR:
        result = nsetopt_lvl_sock_reuseaddr(env, descP, eVal);
        break;
#endif

#if defined(SO_SNDBUF)
    case SOCKET_OPT_SOCK_SNDBUF:
        result = nsetopt_lvl_sock_sndbuf(env, descP, eVal);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}


#if defined(SO_BROADCAST)
static
ERL_NIF_TERM nsetopt_lvl_sock_broadcast(ErlNifEnv*        env,
                                        SocketDescriptor* descP,
                                        ERL_NIF_TERM      eVal)
{
    return nsetopt_bool_opt(env, descP, SOL_SOCKET, SO_BROADCAST, eVal);
}
#endif


#if defined(SO_DONTROUTE)
static
ERL_NIF_TERM nsetopt_lvl_sock_dontroute(ErlNifEnv*        env,
                                        SocketDescriptor* descP,
                                        ERL_NIF_TERM      eVal)
{
    return nsetopt_bool_opt(env, descP, SOL_SOCKET, SO_DONTROUTE, eVal);
}
#endif


#if defined(SO_KEEPALIVE)
static
ERL_NIF_TERM nsetopt_lvl_sock_keepalive(ErlNifEnv*        env,
                                        SocketDescriptor* descP,
                                        ERL_NIF_TERM      eVal)
{
    return nsetopt_bool_opt(env, descP, SOL_SOCKET, SO_KEEPALIVE, eVal);
}
#endif


#if defined(SO_LINGER)
static
ERL_NIF_TERM nsetopt_lvl_sock_linger(ErlNifEnv*        env,
                                     SocketDescriptor* descP,
                                     ERL_NIF_TERM      eVal)
{
    ERL_NIF_TERM  result;
    struct linger val;

    if (decode_sock_linger(env, eVal, &val)) {
        int optLen = sizeof(val);
        int res    = socket_setopt(descP->sock, SOL_SOCKET, SO_LINGER,
                                   (void*) &val, optLen);
        if (res != 0)
            result = esock_make_error_errno(env, res);
        else
            result = esock_atom_ok;
    } else {
        result = esock_make_error(env, esock_atom_einval);
    }

    return result;
}
#endif


#if defined(SO_PRIORITY)
static
ERL_NIF_TERM nsetopt_lvl_sock_priority(ErlNifEnv*        env,
                                       SocketDescriptor* descP,
                                       ERL_NIF_TERM      eVal)
{
    return nsetopt_int_opt(env, descP, SOL_SOCKET, SO_PRIORITY, eVal);
}
#endif


#if defined(SO_RCVBUF)
static
ERL_NIF_TERM nsetopt_lvl_sock_rcvbuf(ErlNifEnv*        env,
                                     SocketDescriptor* descP,
                                     ERL_NIF_TERM      eVal)
{
    return nsetopt_int_opt(env, descP, SOL_SOCKET, SO_RCVBUF, eVal);
}
#endif


#if defined(SO_REUSEADDR)
static
ERL_NIF_TERM nsetopt_lvl_sock_reuseaddr(ErlNifEnv*        env,
                                        SocketDescriptor* descP,
                                        ERL_NIF_TERM      eVal)
{
    return nsetopt_bool_opt(env, descP, SOL_SOCKET, SO_REUSEADDR, eVal);
}
#endif


#if defined(SO_SNDBUF)
static
ERL_NIF_TERM nsetopt_lvl_sock_sndbuf(ErlNifEnv*        env,
                                     SocketDescriptor* descP,
                                     ERL_NIF_TERM      eVal)
{
    return nsetopt_int_opt(env, descP, SOL_SOCKET, SO_SNDBUF, eVal);
}
#endif



/* nsetopt_lvl_ip - Level *IP* option(s)
 */
static
ERL_NIF_TERM nsetopt_lvl_ip(ErlNifEnv*        env,
                            SocketDescriptor* descP,
                            int               eOpt,
                            ERL_NIF_TERM      eVal)
{
    ERL_NIF_TERM result;

    switch (eOpt) {
#if defined(IP_RECVTOS)
    case SOCKET_OPT_IP_RECVTOS:
        result = nsetopt_lvl_ip_recvtos(env, descP, eVal);
        break;
#endif

#if defined(IP_ROUTER_ALERT)
    case SOCKET_OPT_IP_ROUTER_ALERT:
        result = nsetopt_lvl_ip_router_alert(env, descP, eVal);
        break;
#endif

#if defined(IP_TOS)
    case SOCKET_OPT_IP_TOS:
        result = nsetopt_lvl_ip_tos(env, descP, eVal);
        break;
#endif

#if defined(IP_TTL)
    case SOCKET_OPT_IP_TTL:
        result = nsetopt_lvl_ip_ttl(env, descP, eVal);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}


/* nsetopt_lvl_ip_recvtos - Level IP RECVTOS option
 */
#if defined(IP_RECVTOS)
static
ERL_NIF_TERM nsetopt_lvl_ip_recvtos(ErlNifEnv*        env,
                                    SocketDescriptor* descP,
                                    ERL_NIF_TERM      eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_bool_opt(env, descP, level, IP_RECVTOS, eVal);
}
#endif


/* nsetopt_lvl_ip_router_alert - Level IP ROUTER_ALERT option
 */
#if defined(IP_ROUTER_ALERT)
static
ERL_NIF_TERM nsetopt_lvl_ip_router_alert(ErlNifEnv*        env,
                                         SocketDescriptor* descP,
                                         ERL_NIF_TERM      eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_int_opt(env, descP, level, IP_ROUTER_ALERT, eVal);
}
#endif


/* nsetopt_lvl_ip_tos - Level IP TOS option
 */
#if defined(IP_TOS)
static
ERL_NIF_TERM nsetopt_lvl_ip_tos(ErlNifEnv*        env,
                                SocketDescriptor* descP,
                                ERL_NIF_TERM      eVal)
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
            result = esock_make_error_errno(env, res);
        else
            result = esock_atom_ok;

    } else {
        result = esock_make_error(env, esock_atom_einval);
    }

    return result;
}
#endif


/* nsetopt_lvl_ip_ttl - Level IP TTL option
 */
#if defined(IP_TTL)
static
ERL_NIF_TERM nsetopt_lvl_ip_ttl(ErlNifEnv*        env,
                                SocketDescriptor* descP,
                                ERL_NIF_TERM      eVal)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return nsetopt_int_opt(env, descP, level, IP_TTL, eVal);
}
#endif



/* nsetopt_lvl_ipv6 - Level *IPv6* option(s)
 */
#if defined(SOL_IPV6)
static
ERL_NIF_TERM nsetopt_lvl_ipv6(ErlNifEnv*        env,
                              SocketDescriptor* descP,
                              int               eOpt,
                              ERL_NIF_TERM      eVal)
{
    ERL_NIF_TERM result;

    switch (eOpt) {
#if defined(IPV6_HOPLIMIT)
    case SOCKET_OPT_IPV6_HOPLIMIT:
        result = nsetopt_lvl_ipv6_hoplimit(env, descP, eVal);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}


#if defined(IPV6_HOPLIMIT)
static
ERL_NIF_TERM nsetopt_lvl_ipv6_hoplimit(ErlNifEnv*        env,
                                       SocketDescriptor* descP,
                                       ERL_NIF_TERM      eVal)
{
    return nsetopt_bool_opt(env, descP, SOL_IPV6, IPV6_HOPLIMIT, eVal);
}
#endif


#endif // defined(SOL_IPV6)



/* nsetopt_lvl_tcp - Level *TCP* option(s)
 */
static
ERL_NIF_TERM nsetopt_lvl_tcp(ErlNifEnv*        env,
                             SocketDescriptor* descP,
                             int               eOpt,
                             ERL_NIF_TERM      eVal)
{
    ERL_NIF_TERM result;

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
ERL_NIF_TERM nsetopt_lvl_tcp_congestion(ErlNifEnv*        env,
                                        SocketDescriptor* descP,
                                        ERL_NIF_TERM      eVal)
{
    int max = SOCKET_OPT_TCP_CONGESTION_NAME_MAX+1;

    return nsetopt_str_opt(env, descP, IPPROTO_TCP, TCP_CONGESTION, max, eVal);
}
#endif


/* nsetopt_lvl_tcp_maxseg - Level TCP MAXSEG option
 */
#if defined(TCP_MAXSEG)
static
ERL_NIF_TERM nsetopt_lvl_tcp_maxseg(ErlNifEnv*        env,
                                    SocketDescriptor* descP,
                                    ERL_NIF_TERM      eVal)
{
    return nsetopt_int_opt(env, descP, IPPROTO_TCP, TCP_MAXSEG, eVal);
}
#endif


/* nsetopt_lvl_tcp_nodelay - Level TCP NODELAY option
 */
#if defined(TCP_NODELAY)
static
ERL_NIF_TERM nsetopt_lvl_tcp_nodelay(ErlNifEnv*        env,
                                     SocketDescriptor* descP,
                                     ERL_NIF_TERM      eVal)
{
    return nsetopt_bool_opt(env, descP, IPPROTO_TCP, TCP_NODELAY, eVal);
}
#endif



/* nsetopt_lvl_udp - Level *UDP* option(s)
 */
static
ERL_NIF_TERM nsetopt_lvl_udp(ErlNifEnv*        env,
                             SocketDescriptor* descP,
                             int               eOpt,
                             ERL_NIF_TERM      eVal)
{
    ERL_NIF_TERM result;

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
ERL_NIF_TERM nsetopt_lvl_udp_cork(ErlNifEnv*        env,
                                  SocketDescriptor* descP,
                                  ERL_NIF_TERM      eVal)
{
    return nsetopt_bool_opt(env, descP, IPPROTO_UDP, UDP_CORK, eVal);
}
#endif




/* nsetopt_lvl_sctp - Level *SCTP* option(s)
 */
#if defined(HAVE_SCTP)
static
ERL_NIF_TERM nsetopt_lvl_sctp(ErlNifEnv*        env,
                              SocketDescriptor* descP,
                              int               eOpt,
                              ERL_NIF_TERM      eVal)
{
    ERL_NIF_TERM result;

    switch (eOpt) {
#if defined(SCTP_AUTOCLOSE)
    case SOCKET_OPT_SCTP_AUTOCLOSE:
        result = nsetopt_lvl_sctp_autoclose(env, descP, eVal);
        break;
#endif

#if defined(SCTP_NODELAY)
    case SOCKET_OPT_SCTP_NODELAY:
        result = nsetopt_lvl_sctp_nodelay(env, descP, eVal);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}


/* nsetopt_lvl_sctp_autoclose - Level SCTP AUTOCLOSE option
 */
#if defined(SCTP_AUTOCLOSE)
static
ERL_NIF_TERM nsetopt_lvl_sctp_autoclose(ErlNifEnv*        env,
                                        SocketDescriptor* descP,
                                        ERL_NIF_TERM      eVal)
{
    return nsetopt_int_opt(env, descP, IPPROTO_SCTP, SCTP_AUTOCLOSE, eVal);
}
#endif


/* nsetopt_lvl_sctp_nodelay - Level SCTP NODELAY option
 */
#if defined(SCTP_NODELAY)
static
ERL_NIF_TERM nsetopt_lvl_sctp_nodelay(ErlNifEnv*        env,
                                      SocketDescriptor* descP,
                                      ERL_NIF_TERM      eVal)
{
    return nsetopt_bool_opt(env, descP, IPPROTO_SCTP, SCTP_NODELAY, eVal);
}
#endif



#endif // defined(HAVE_SCTP)




/* nsetopt_str_opt - set an option that has an string value
 */
static
ERL_NIF_TERM nsetopt_str_opt(ErlNifEnv*        env,
                             SocketDescriptor* descP,
                             int               level,
                             int               opt,
                             int               max,
                             ERL_NIF_TERM      eVal)
{
    ERL_NIF_TERM result;
    char*        val = MALLOC(max);

    if (GET_STR(env, eVal, val, max) > 0) {
        int optLen = strlen(val);
        int res    = socket_setopt(descP->sock, level, opt, &val, optLen);

        if (res != 0)
            result = esock_make_error_errno(env, res);
        else
            result = esock_atom_ok;

    } else {
        result = esock_make_error(env, esock_atom_einval);
    }

    FREE(val);

    return result;
}


/* nsetopt_bool_opt - set an option that has an (integer) bool value
 */
static
ERL_NIF_TERM nsetopt_bool_opt(ErlNifEnv*        env,
                              SocketDescriptor* descP,
                              int               level,
                              int               opt,
                              ERL_NIF_TERM      eVal)
{
    ERL_NIF_TERM result;
    BOOLEAN_T    val;
    int          ival, res;

    val = esock_decode_bool(eVal);
    
    ival = (val) ? 1 : 0;
    res  = socket_setopt(descP->sock, level, opt, &ival, sizeof(ival));

    if (res != 0)
        result = esock_make_error_errno(env, res);
    else
        result = esock_atom_ok;

    return result;
}


/* nsetopt_int_opt - set an option that has an integer value
 */
static
ERL_NIF_TERM nsetopt_int_opt(ErlNifEnv*        env,
                             SocketDescriptor* descP,
                             int               level,
                             int               opt,
                             ERL_NIF_TERM      eVal)
{
    ERL_NIF_TERM result;
    int          val;

    if (GET_INT(env, eVal, &val)) {
        int res = socket_setopt(descP->sock, level, opt, &val, sizeof(val));

        if (res != 0)
            result = esock_make_error_errno(env, res);
        else
            result = esock_atom_ok;

    } else {
        result = esock_make_error(env, esock_atom_einval);
    }

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
            *level = IPROTO_IP;
#endif
            result = TRUE;
            break;

#if defined(SOL_IPV6)
        case SOCKET_OPT_LEVEL_IPV6:
            *isOTP = FALSE;
            *level = SOL_IPV6;
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
    SocketDescriptor* descP;
    int               eLevel, level = -1;
    int               eOpt;
    ERL_NIF_TERM      eIsEncoded;
    BOOLEAN_T         isEncoded, isOTP;

    if ((argc != 4) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP) ||
        !GET_INT(env, argv[2], &eLevel) ||
        !GET_INT(env, argv[3], &eOpt)) {
        return enif_make_badarg(env);
    }
    eIsEncoded = argv[1];

    isEncoded = esock_decode_bool(eIsEncoded);

    if (!elevel2level(isEncoded, eLevel, &isOTP, &level))
        return esock_make_error(env, esock_atom_einval);

    return ngetopt(env, descP, isEncoded, isOTP, level, eOpt);
}



static
ERL_NIF_TERM ngetopt(ErlNifEnv*        env,
                     SocketDescriptor* descP,
                     BOOLEAN_T         isEncoded,
                     BOOLEAN_T         isOTP,
                     int               level,
                     int               eOpt)
{
    ERL_NIF_TERM result;

    if (isOTP) {
        /* These are not actual socket options,
         * but options for our implementation.
         */
        result = ngetopt_otp(env, descP, eOpt);
    } else if (!isEncoded) {
        result = ngetopt_native(env, descP, level, eOpt);
    } else {
        result = ngetopt_level(env, descP, level, eOpt);
    }

    return result;
}



/* ngetopt_otp - Handle OTP (level) options
 */
static
ERL_NIF_TERM ngetopt_otp(ErlNifEnv*        env,
                         SocketDescriptor* descP,
                         int               eOpt)
{
    ERL_NIF_TERM result;

    switch (eOpt) {
    case SOCKET_OPT_OTP_DEBUG:
        result = ngetopt_otp_debug(env, descP);
        break;

    case SOCKET_OPT_OTP_IOW:
        result = ngetopt_otp_iow(env, descP);
        break;

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}


/* ngetopt_otp_debug - Handle the OTP (level) debug options
 */
static
ERL_NIF_TERM ngetopt_otp_debug(ErlNifEnv*        env,
                               SocketDescriptor* descP)
{
    ERL_NIF_TERM eVal = esock_encode_bool(descP->dbg);

    return esock_make_ok2(env, eVal);
}


/* ngetopt_otp_iow - Handle the OTP (level) iow options
 */
static
ERL_NIF_TERM ngetopt_otp_iow(ErlNifEnv*        env,
                             SocketDescriptor* descP)
{
    ERL_NIF_TERM eVal = esock_encode_bool(descP->iow);

    return esock_make_ok2(env, eVal);
}



/* The option has *not* been encoded. Instead it has been provided
 * in "native mode" (option is provided as is). In this case it will have the
 * format: {NativeOpt :: integer(), ValueSize :: non_neg_integer()}
 */
static
ERL_NIF_TERM ngetopt_native(ErlNifEnv*        env,
                            SocketDescriptor* descP,
                            int               level,
                            int               eOpt)
{
    ERL_NIF_TERM result = enif_make_badarg(env);
    int          opt;
    uint16_t     valueType;
    SOCKOPTLEN_T valueSz;

    /* <KOLLA>
     * We should really make it possible to specify common specific types,
     * such as integer or boolean (instead of the size)...
     * </KOLLA>
     */

    if (decode_native_get_opt(env, eOpt, &opt, &valueType, (int*) &valueSz)) {
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

    return result;
}


static
ERL_NIF_TERM ngetopt_native_unspec(ErlNifEnv*        env,
                                   SocketDescriptor* descP,
                                   int               level,
                                   int               opt,
                                   SOCKOPTLEN_T      valueSz)
{
    ERL_NIF_TERM result = enif_make_badarg(env);
    int          res;

    if (valueSz == 0) {
        res = sock_getopt(descP->sock, level, opt, NULL, NULL);
        if (res != 0)
            result = esock_make_error_errno(env, res);
        else
            result = esock_atom_ok;
    } else {
        ErlNifBinary val;

        if (ALLOC_BIN(valueSz, &val)) {
            res = sock_getopt(descP->sock, level, opt, val.data, &valueSz);
            if (res != 0) {
                result = esock_make_error_errno(env, res);
            } else {
                if (valueSz < val.size) {
                    if (REALLOC_BIN(&val, valueSz)) {
                        result = esock_make_ok2(env, MKBIN(env, &val));
                    } else {
                        result = enif_make_badarg(env);
                    }
                }
            }
        } else {
            result = enif_make_badarg(env);
        }
    }

    return result;
}



/* ngetopt_level - A "proper" level (option) has been specified
 */
static
ERL_NIF_TERM ngetopt_level(ErlNifEnv*        env,
                           SocketDescriptor* descP,
                           int               level,
                           int               eOpt)
{
    ERL_NIF_TERM result;

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

#if defined(SOL_IPV6)
    case SOL_IPV6:
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

    return result;
}


/* ngetopt_lvl_socket - Level *SOCKET* option
 */
static
ERL_NIF_TERM ngetopt_lvl_socket(ErlNifEnv*        env,
                                SocketDescriptor* descP,
                                int               eOpt)
{
    ERL_NIF_TERM result;

    switch (eOpt) {
#if defined(SO_BROADCAST)
    case SOCKET_OPT_SOCK_BROADCAST:
        result = ngetopt_lvl_sock_broadcast(env, descP);
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

#if defined(SO_PRIORITY)
    case SOCKET_OPT_SOCK_PRIORITY:
        result = ngetopt_lvl_sock_priority(env, descP);
        break;
#endif

#if defined(SO_RCVBUF)
    case SOCKET_OPT_SOCK_RCVBUF:
        result = ngetopt_lvl_sock_rcvbuf(env, descP);
        break;
#endif

#if defined(SO_REUSEADDR)
    case SOCKET_OPT_SOCK_REUSEADDR:
        result = ngetopt_lvl_sock_reuseaddr(env, descP);
        break;
#endif

#if defined(SO_SNDBUF)
    case SOCKET_OPT_SOCK_SNDBUF:
        result = ngetopt_lvl_sock_sndbuf(env, descP);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}


#if defined(SO_BROADCAST)
static
ERL_NIF_TERM ngetopt_lvl_sock_broadcast(ErlNifEnv*        env,
                                        SocketDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, SOL_SOCKET, SO_BROADCAST);
}
#endif


#if defined(SO_DONTROUTE)
static
ERL_NIF_TERM ngetopt_lvl_sock_dontroute(ErlNifEnv*        env,
                                        SocketDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, SOL_SOCKET, SO_DONTROUTE);
}
#endif


#if defined(SO_KEEPALIVE)
static
ERL_NIF_TERM ngetopt_lvl_sock_keepalive(ErlNifEnv*        env,
                                        SocketDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, SOL_SOCKET, SO_KEEPALIVE);
}
#endif


#if defined(SO_LINGER)
static
ERL_NIF_TERM ngetopt_lvl_sock_linger(ErlNifEnv*        env,
                                     SocketDescriptor* descP)
{
    ERL_NIF_TERM  result;
    struct linger val;
    SOCKOPTLEN_T  valSz = sizeof(val);
    int           res;

    sys_memzero((void *) &val, sizeof(val));

    res = sock_getopt(descP->sock, SOL_SOCKET, SO_LINGER,
                      &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, res);
    } else {
        ERL_NIF_TERM lOnOff = MKI(env, val.l_onoff);
        ERL_NIF_TERM lSecs  = MKI(env, val.l_linger);
        ERL_NIF_TERM linger = MKT2(env, lOnOff, lSecs);

        result = esock_make_ok2(env, linger);
    }

    return result;
}
#endif


#if defined(SO_PRIORITY)
static
ERL_NIF_TERM ngetopt_lvl_sock_priority(ErlNifEnv*        env,
                                       SocketDescriptor* descP)
{
    return ngetopt_int_opt(env, descP, SOL_SOCKET, SO_PRIORITY);
}
#endif


#if defined(SO_RCVBUF)
static
ERL_NIF_TERM ngetopt_lvl_sock_rcvbuf(ErlNifEnv*        env,
                                     SocketDescriptor* descP)
{
    return ngetopt_int_opt(env, descP, SOL_SOCKET, SO_RCVBUF);
}
#endif


#if defined(SO_REUSEADDR)
static
ERL_NIF_TERM ngetopt_lvl_sock_reuseaddr(ErlNifEnv*        env,
                                        SocketDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, SOL_SOCKET, SO_REUSEADDR);
}
#endif


#if defined(SO_SNDBUF)
static
ERL_NIF_TERM ngetopt_lvl_sock_sndbuf(ErlNifEnv*        env,
                                     SocketDescriptor* descP)
{
    return ngetopt_int_opt(env, descP, SOL_SOCKET, SO_SNDBUF);
}
#endif


/* ngetopt_lvl_ip - Level *IP* option(s)
 */
static
ERL_NIF_TERM ngetopt_lvl_ip(ErlNifEnv*        env,
                            SocketDescriptor* descP,
                            int               eOpt)
{
    ERL_NIF_TERM result;

    switch (eOpt) {
#if defined(IP_RECVTOS)
    case SOCKET_OPT_IP_RECVTOS:
        result = ngetopt_lvl_ip_recvtos(env, descP);
        break;
#endif

#if defined(IP_ROUTER_ALERT)
    case SOCKET_OPT_IP_ROUTER_ALERT:
        result = ngetopt_lvl_ip_router_alert(env, descP);
        break;
#endif

#if defined(IP_TOS)
    case SOCKET_OPT_IP_TOS:
        result = ngetopt_lvl_ip_tos(env, descP);
        break;
#endif

#if defined(IP_TTL)
    case SOCKET_OPT_IP_TTL:
        result = ngetopt_lvl_ip_ttl(env, descP);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}


/* ngetopt_lvl_ip_recvtos - Level IP RECVTOS option
 */
#if defined(IP_RECVTOS)
static
ERL_NIF_TERM ngetopt_lvl_ip_recvtos(ErlNifEnv*        env,
                                    SocketDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_bool_opt(env, descP, level, IP_RECVTOS);
}
#endif


/* ngetopt_lvl_ip_router_alert - Level IP ROUTER_ALERT option
 */
#if defined(IP_ROUTER_ALERT)
static
ERL_NIF_TERM ngetopt_lvl_ip_router_alert(ErlNifEnv*        env,
                                         SocketDescriptor* descP)
{
#if defined(SOL_IP)
    int level = SOL_IP;
#else
    int level = IPPROTO_IP;
#endif

    return ngetopt_int_opt(env, descP, level, IP_ROUTER_ALERT);
}
#endif


/* ngetopt_lvl_ip_tos - Level IP TOS option
 */
#if defined(IP_TOS)
static
ERL_NIF_TERM ngetopt_lvl_ip_tos(ErlNifEnv*        env,
                                SocketDescriptor* descP)
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
        result = esock_make_error_errno(env, res);
    } else {
        result = encode_ip_tos(env, val);
    }

    return result;
}
#endif


/* ngetopt_lvl_ip_ttl - Level IP TTL option
 */
#if defined(IP_TTL)
static
ERL_NIF_TERM ngetopt_lvl_ip_ttl(ErlNifEnv*        env,
                                SocketDescriptor* descP)
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
#if defined(SOL_IPV6)
static
ERL_NIF_TERM ngetopt_lvl_ipv6(ErlNifEnv*        env,
                              SocketDescriptor* descP,
                              int               eOpt)
{
    ERL_NIF_TERM result;

    switch (eOpt) {
#if defined(IPV6_HOPLIMIT)
    case SOCKET_OPT_IPV6_HOPLIMIT:
        result = ngetopt_lvl_ipv6_hoplimit(env, descP);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}


#if defined(IPV6_HOPLIMIT)
static
ERL_NIF_TERM ngetopt_lvl_ipv6_hoplimit(ErlNifEnv*        env,
                                       SocketDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, SOL_IPV6, IPV6_HOPLIMIT);
}
#endif


#endif // defined(SOL_IPV6)



/* ngetopt_lvl_tcp - Level *TCP* option(s)
 */
static
ERL_NIF_TERM ngetopt_lvl_tcp(ErlNifEnv*        env,
                             SocketDescriptor* descP,
                             int               eOpt)
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
ERL_NIF_TERM ngetopt_lvl_tcp_congestion(ErlNifEnv*        env,
                                        SocketDescriptor* descP)
{
    int max = SOCKET_OPT_TCP_CONGESTION_NAME_MAX+1;

    return ngetopt_str_opt(env, descP, IPPROTO_TCP, TCP_CONGESTION, max);
}
#endif


/* ngetopt_lvl_tcp_maxseg - Level TCP MAXSEG option
 */
#if defined(TCP_MAXSEG)
static
ERL_NIF_TERM ngetopt_lvl_tcp_maxseg(ErlNifEnv*        env,
                                    SocketDescriptor* descP)
{
    return ngetopt_int_opt(env, descP, IPPROTO_TCP, TCP_MAXSEG);
}
#endif


/* ngetopt_lvl_tcp_nodelay - Level TCP NODELAY option
 */
#if defined(TCP_NODELAY)
static
ERL_NIF_TERM ngetopt_lvl_tcp_nodelay(ErlNifEnv*        env,
                                     SocketDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, IPPROTO_TCP, TCP_NODELAY);
}
#endif



/* ngetopt_lvl_udp - Level *UDP* option(s)
 */
static
ERL_NIF_TERM ngetopt_lvl_udp(ErlNifEnv*        env,
                             SocketDescriptor* descP,
                             int               eOpt)
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
ERL_NIF_TERM ngetopt_lvl_udp_cork(ErlNifEnv*        env,
                                  SocketDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, IPPROTO_UDP, UDP_CORK);
}
#endif



/* ngetopt_lvl_sctp - Level *SCTP* option(s)
 */
#if defined(HAVE_SCTP)
static
ERL_NIF_TERM ngetopt_lvl_sctp(ErlNifEnv*        env,
                              SocketDescriptor* descP,
                              int               eOpt)
{
    ERL_NIF_TERM result;

    switch (eOpt) {
#if defined(SCTP_AUTOCLOSE)
    case SOCKET_OPT_SCTP_AUTOCLOSE:
        result = ngetopt_lvl_sctp_autoclose(env, descP);
        break;
#endif

#if defined(SCTP_NODELAY)
    case SOCKET_OPT_SCTP_NODELAY:
        result = ngetopt_lvl_sctp_nodelay(env, descP);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}


/* ngetopt_lvl_sctp_autoclose - Level SCTP AUTOCLOSE option
 */
#if defined(SCTP_AUTOCLOSE)
static
ERL_NIF_TERM ngetopt_lvl_sctp_autoclose(ErlNifEnv*        env,
                                        SocketDescriptor* descP)
{
    return ngetopt_int_opt(env, descP, IPPROTO_SCTP, SCTP_AUTOCLOSE);
}
#endif


/* ngetopt_lvl_sctp_nodelay - Level SCTP NODELAY option
 */
#if defined(SCTP_NODELAY)
static
ERL_NIF_TERM ngetopt_lvl_sctp_nodelay(ErlNifEnv*        env,
                                      SocketDescriptor* descP)
{
    return ngetopt_bool_opt(env, descP, IPPROTO_SCTP, SCTP_NODELAY);
}
#endif


#endif // defined(HAVE_SCTP)



/* ngetopt_str_opt - get an string option
 */
static
ERL_NIF_TERM ngetopt_str_opt(ErlNifEnv*        env,
                             SocketDescriptor* descP,
                             int               level,
                             int               opt,
                             int               max)
{
    ERL_NIF_TERM result;
    char*        val   = MALLOC(max);
    SOCKOPTLEN_T valSz = max;
    int          res;

    res = sock_getopt(descP->sock, level, opt, &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, res);
    } else {
        ERL_NIF_TERM sval = MKSL(env, val, valSz);

        result = esock_make_ok2(env, sval);
    }

    FREE(val);

    return result;
}


/* nsetopt_bool_opt - get an (integer) bool option
 */
static
ERL_NIF_TERM ngetopt_bool_opt(ErlNifEnv*        env,
                              SocketDescriptor* descP,
                              int               level,
                              int               opt)
{
    ERL_NIF_TERM result;
    int          val;
    SOCKOPTLEN_T valSz = sizeof(val);
    int          res;

    res = sock_getopt(descP->sock, level, opt, &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, res);
    } else {
        ERL_NIF_TERM bval = ((val) ? atom_true : atom_false);

        result = esock_make_ok2(env, bval);
    }

    return result;
}


/* nsetopt_int_opt - get an integer option
 */
static
ERL_NIF_TERM ngetopt_int_opt(ErlNifEnv*        env,
                             SocketDescriptor* descP,
                             int               level,
                             int               opt)
{
    ERL_NIF_TERM result;
    int          val;
    SOCKOPTLEN_T valSz = sizeof(val);
    int          res;

    res = sock_getopt(descP->sock, level, opt, &val, &valSz);

    if (res != 0) {
        result = esock_make_error_errno(env, res);
    } else {
        result = esock_make_ok2(env, MKI(env, val));
    }

    return result;
}



/* ----------------------------------------------------------------------
 *  U t i l i t y   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

static
ERL_NIF_TERM send_check_result(ErlNifEnv*        env,
                               SocketDescriptor* descP,
                               ssize_t           written,
                               ssize_t           dataSize,
                               ERL_NIF_TERM      sendRef)
{
    if (written == dataSize) {

        cnt_inc(&descP->writePkgCnt,  1);
        cnt_inc(&descP->writeByteCnt, written);

        return esock_atom_ok;

    } else if (written < 0) {

        /* Ouch, check what kind of failure */
        int save_errno = sock_errno();
        if ((save_errno != EAGAIN) &&
            (save_errno != EINTR)) {

            cnt_inc(&descP->writeFails, 1);

            return esock_make_error_errno(env, save_errno);

        } else {

            /* Ok, try again later */

            /* <KOLLA>
             * SHOULD RESULT IN {error, eagain}!!!!
             * </KOLLA>
             */
            written = 0;

        }
    }

    /* We failed to write the *entire* packet (anything less then size
     * of the packet, which is 0 <= written < sizeof packet),
     * so schedule the rest for later.
     */

    cnt_inc(&descP->writeWaits, 1);

    SELECT(env, descP->sock, (ERL_NIF_SELECT_WRITE),
           descP, NULL, sendRef);

    return esock_make_ok2(env, enif_make_int(env, written));

}


static
ERL_NIF_TERM recv_check_result(ErlNifEnv*        env,
                               SocketDescriptor* descP,
                               int               read,
                               int               toRead,
                               ErlNifBinary*     bufP,
                               ERL_NIF_TERM      recvRef)
{
    ERL_NIF_TERM data;

    /* There is a special case: If the provided 'to read' value is
     * zero (0). That means that we reads as much as we can, using
     * the default read buffer size.
     */

    if (bufP->size == read) {

        /* +++ We filled the buffer +++ */

        if (toRead == 0) {

            /* +++ Give us everything you have got => needs to continue +++ */

            /* How do we do this?
             * Either:
             * 1) Send up each chunk of data for each of the read
             *    and let the erlang code assemble it: {ok, false, Bin}
             *    (when complete it should return {ok, true, Bin}).
             *    We need to read atleast one more time to be sure if its
             *    done...
             * 2) Or put it in a buffer here, and then let the erlang code
             *    know that it should call again (special return value)
             *    (continuous binary realloc "here").
             *
             * => We choose alt 1 for now.
             */

            data = MKBIN(env, bufP);

            return esock_make_ok3(env, atom_false, data);

        } else {

            /* +++ We got exactly as much as we requested +++ */

            /* <KOLLA>
             * WE NEED TO INFORM ANY WAITING READERS
             * </KOLLA>
             */

            data = MKBIN(env, bufP);

            return esock_make_ok3(env, atom_true, data);

        }

    } else if (read < 0) {

        /* +++ Error handling +++ */

        int save_errno = sock_errno();

        if (save_errno == ECONNRESET)  {

            /* +++ Oups - closed +++ */

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

            SELECT(env,
                   descP->sock,
                   (ERL_NIF_SELECT_STOP),
                   descP, NULL, recvRef);

            return esock_make_error(env, atom_closed);

        } else if ((save_errno == ERRNO_BLOCK) ||
                   (save_errno == EAGAIN)) {
            return esock_make_error(env, esock_atom_eagain);
        } else {
            return esock_make_error_errno(env, save_errno);
        }

    } else {

        /* +++ We did not fill the buffer +++ */

        if (toRead == 0) {

            /* +++ We got a chunk of data but +++
             * +++ since we did not fill the  +++
             * +++ buffer, we must split it   +++
             * +++ into a sub-binary.         +++
             */

            data = MKBIN(env, bufP);
            data = MKSBIN(env, data, 0, read);

            return esock_make_ok3(env, atom_true, data);

        } else {

            /* +++ We got only a part of what was expected +++
             * +++ => receive more later.                  +++ */

            return esock_make_ok3(env, atom_false, MKBIN(env, bufP));
        }
    }
}


/* The recvfrom function delivers one (1) message. If our buffer
 * is to small, the message will be truncated. So, regardless
 * if we filled the buffer or not, we have got what we are going
 * to get regarding this message.
 */
static
ERL_NIF_TERM recvfrom_check_result(ErlNifEnv*        env,
                                   SocketDescriptor* descP,
                                   int               read,
                                   ErlNifBinary*     bufP,
                                   SocketAddress*    fromAddrP,
                                   unsigned int      fromAddrLen,
                                   ERL_NIF_TERM      recvRef)
{
    ERL_NIF_TERM data;

    /* There is a special case: If the provided 'to read' value is
     * zero (0). That means that we reads as much as we can, using
     * the default read buffer size.
     */

    if (read < 0) {

        /* +++ Error handling +++ */

        int save_errno = sock_errno();

        if (save_errno == ECONNRESET)  {

            /* +++ Oups - closed +++ */

            /* <KOLLA>
             * IF THE CURRENT PROCESS IS *NOT* THE CONTROLLING
             * PROCESS, WE NEED TO INFORM IT!!!
             *
             * ALL WAITING PROCESSES MUST ALSO GET THE ERROR!!
             *
             * </KOLLA>
             */

            descP->closeLocal = FALSE;
            descP->state      = SOCKET_STATE_CLOSING;

            SELECT(env,
                   descP->sock,
                   (ERL_NIF_SELECT_STOP),
                   descP, NULL, recvRef);

            return esock_make_error(env, atom_closed);

        } else if ((save_errno == ERRNO_BLOCK) ||
                   (save_errno == EAGAIN)) {
            return esock_make_error(env, esock_atom_eagain);
        } else {
            return esock_make_error_errno(env, save_errno);
        }

    } else {

        /* +++ We sucessfully got a message - time to encode the address +++ */

        ERL_NIF_TERM fromDomainT, fromSourceT;

        encode_address(env,
                       fromAddrP, fromAddrLen,
                       &fromDomainT, &fromSourceT);

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

        return esock_make_ok2(env, MKT3(env, fromDomainT, fromSourceT, data));

    }
}


/* *** decode_send_addr ***
 *
 * The rather odd thing about the 'toAddrP' (the **) is
 * because we need to be able to return a NULL pointer,
 * in the case of the dest address is the atom 'null'.
 * Its possible to call the sendto function with the
 * args NULL (address) and 0 (port number).
 *
 * This function whouls really have a char* return value
 * type!!
 */
/*
static
char* decode_send_addr(ErlNifEnv*      env,
                       int             domain,
                       ERL_NIF_TERM    addr,
                       int             port,
                       SocketAddress** toAddrP,
                       unsigned int*   toAddrLenP)
{
    if (IS_ATOM(env, addr)) {
        unsigned int len;
        char         a[16]; // Just in case...

        / * The only acceptable value is the atom 'null' * /

        if (!(GET_ATOM_LEN(env, addr, &len) &&
              (len > 0) &&
              (len <= (sizeof("null")))))
            return ESOCK_STR_EINVAL;

        if (!GET_ATOM(env, addr, a, sizeof(a)))
            return ESOCK_STR_EINVAL;

        *toAddrP = NULL;
        if (strncmp(a, "null", len) == 0)
            return NULL;
        else
            return ESOCK_STR_EINVAL;

    } else if (IS_TUPLE(env, addr)) {
        / * We now know that the we have a proper address. * /
        return decode_send_addr_tuple(env, domain, addr, port,
                                      *toAddrP, toAddrLenP);
    } else {
        return ESOCK_STR_EINVAL;
    }
}
*/

/*
static
char* decode_send_addr_tuple(ErlNifEnv*     env,
                             int            domain,
                             ERL_NIF_TERM   addr,
                             int            port,
                             SocketAddress* toAddrP,
                             unsigned int*  toAddrLenP)
{
    / * We handle two different tuples:
     *    - size 4 (INET)
     *    - size 8 (INET6)
     * /

    const ERL_NIF_TERM* addrt;
    int                 addrtSz;

    if (!GET_TUPLE(env, addr, &addrtSz, &addrt))
        return ESOCK_STR_EINVAL; // PLACEHOLDER

    switch (domain) {
    case AF_INET:
        if (addrtSz != 4)
            return ESOCK_STR_EINVAL;
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        if (addrtSz != 8)
            return ESOCK_STR_EINVAL;
        break;
#endif

    default:
        return str_eafnosupport;
        break;
    }

    return decode_address_tuple(env, domain,
                                addrt, port,
                                toAddrP, toAddrLenP);

}
*/


/* Decode an in_sockaddr(). This is either a: 
 *    in4_sockaddr: #in4_sockaddr{} = 3 tuple =>
 *        1: The tag in4_sockaddr
 *        2: Port number (an integer())
 *        3: The address: any | loopback | ip4_address()
 *    in6_sockaddr: #in6_sockaddr{} = 5 tuple =>
 *        1: The tag in6_sockaddr
 *        2: Port number: integer()
 *        3: The address: any | loopback | ip6_address()
 *        4: Flow info: integer()
 *        5: Scope Id: integer()
 *
 */
static
char* decode_in_sockaddr(ErlNifEnv*     env,
                         ERL_NIF_TERM   eSockAddr,
                         SocketAddress* sockAddrP,
                         unsigned int*  addrLenP)
{
    const ERL_NIF_TERM* addrt;
    int                 addrtSz;
    char*               result = NULL;
    
    if (!GET_TUPLE(env, eSockAddr, &addrtSz, &addrt))
        return ESOCK_STR_EINVAL;

    /*
     * We use the tuple size to figure out which
     * of the records this is.
     */
    switch (addrtSz) {
    case 3:
        result = decode_in4_sockaddr(env, addrt, sockAddrP, addrLenP);
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case 5:
        result = decode_in6_sockaddr(env, addrt, sockAddrP, addrLenP);
        break;
#endif

    default:
        result = ESOCK_STR_EAFNOSUPPORT;
        break;
    }

    return result;
}



/* Decode an in4_sockaddr().
 * The first element should be the atom in4_sockaddr
 * The second, the port number integer .
 * The third and final, the ip4_address tuple.
 */
static
char* decode_in4_sockaddr(ErlNifEnv*          env,
                          const ERL_NIF_TERM* eIn4SockAddr,
                          SocketAddress*      sockAddrP,
                          unsigned int*       addrLenP)
{
    int port;

    /* 1: Ensure that the tuple has the correct tag: in4_sockaddr */
    if (COMPARE(atom_in4_sockaddr, eIn4SockAddr[0]) != 0)
        return ESOCK_STR_EINVAL;

    /* 2: Get the port number */
    if (!GET_INT(env, eIn4SockAddr[1], &port))
        return ESOCK_STR_EINVAL;

    /* 3: Get the address.
     *    It can either be the atoms: any | loopback, 
     *    or the IPv4 address tuple (size 4).
     */
    if (IS_ATOM(env, eIn4SockAddr[2])) {
        return decode_in4_sockaddr_atomaddr(env, eIn4SockAddr[2], port,
                                            sockAddrP, addrLenP);
    } else if (IS_TUPLE(env, eIn4SockAddr[2])) {
        return decode_in4_sockaddr_addr(env, eIn4SockAddr[2], port,
                                        sockAddrP, addrLenP);
    } else {
        return ESOCK_STR_EINVAL;
    }
}



static
char* decode_in4_sockaddr_atomaddr(ErlNifEnv*     env,
                                   ERL_NIF_TERM   eAddr,
                                   int            port,
                                   SocketAddress* sockAddrP,
                                   unsigned int*  addrLenP)
{
    struct in_addr addr;

    if (COMPARE(esock_atom_loopback, eAddr) == 0) {
        addr.s_addr = sock_htonl(INADDR_LOOPBACK);
    } else if (COMPARE(esock_atom_any, eAddr) == 0) {
        addr.s_addr = sock_htonl(INADDR_ANY);
    } else {
        return ESOCK_STR_EINVAL;
    }

    sys_memzero((char*) sockAddrP, sizeof(struct sockaddr_in));
#ifndef NO_SA_LEN
    sockAddrP->sai.sin_len         = sizeof(struct sockaddr_in);
#endif
    sockAddrP->in4.sin_family      = AF_INET;
    sockAddrP->in4.sin_port        = sock_htons(port);
    sockAddrP->in4.sin_addr.s_addr = addr.s_addr;
    *addrLenP                      = sizeof(struct sockaddr_in);
    
    return NULL;
}


/* Decode an in4_sockaddr where the address field is a tuple.
 * Its *supposed* to be an ip4_address (tuple).
 */
static
char* decode_in4_sockaddr_addr(ErlNifEnv*     env,
                               ERL_NIF_TERM   eAddr,
                               int            port,
                               SocketAddress* sockAddrP,
                               unsigned int*  addrLenP)
{
    const ERL_NIF_TERM* ip4AddrT;
    int                 ip4AddrTSz;
    int                 a, v;
    char                addr[4];

    /* This shall be a 4 tuple */
    if (!GET_TUPLE(env, eAddr, &ip4AddrTSz, &ip4AddrT))
        return ESOCK_STR_EINVAL;

    if (ip4AddrTSz != 4)
        return ESOCK_STR_EINVAL;

    sys_memzero((char*)sockAddrP, sizeof(struct sockaddr_in));
#ifndef NO_SA_LEN
    sockAddrP->in4.sin_len    = sizeof(struct sockaddr_in);
#endif
    sockAddrP->in4.sin_family = AF_INET;
    sockAddrP->in4.sin_port   = sock_htons(port);
    for (a = 0; a < 4; a++) {
        if (!GET_INT(env, ip4AddrT[a], &v))
            return ESOCK_STR_EINVAL;
        addr[a] = v;
    }
    sys_memcpy(&sockAddrP->in4.sin_addr, &addr, sizeof(addr));
    *addrLenP = sizeof(struct sockaddr_in);
    
    return NULL;
}



/* Decode an in6_sockaddr().
 * The first element should be the atom in4_sockaddr
 * The second, the port number integer .
 * The third, the ip4_address tuple.
 * The forth, the flowinfo integer.
 * The fifth and final, the scope_id integer.
 */
#if defined(HAVE_IN6) && defined(AF_INET6)
static
char* decode_in6_sockaddr(ErlNifEnv*          env,
                          const ERL_NIF_TERM* eIn6SockAddr,
                          SocketAddress*      sockAddrP,
                          unsigned int*       addrLenP)
{
    int          port;
    unsigned int flowInfo, scopeId;
    
    /* 1: Ensure that the tuple has the correct tag: in6_sockaddr */
    if (COMPARE(atom_in6_sockaddr, eIn6SockAddr[0]) != 0)
        return ESOCK_STR_EINVAL;

    /* 2: Get the port number */
    if (!GET_INT(env, eIn6SockAddr[1], &port))
        return ESOCK_STR_EINVAL;

    /* 4: Get the flowinfo */
    if (!GET_UINT(env, eIn6SockAddr[3], &flowInfo))
        return ESOCK_STR_EINVAL;

    /* 5: Get the scope_id */
    if (!GET_UINT(env, eIn6SockAddr[4], &scopeId))
        return ESOCK_STR_EINVAL;

    /* 3: Get the address.
     *    It can either be the atoms: any | loopback, 
     *    or the IPv6 address tuple (size 8).
     */
    if (IS_ATOM(env, eIn6SockAddr[2])) {
        return decode_in6_sockaddr_atomaddr(env, eIn6SockAddr[2], port,
                                            flowInfo, scopeId,
                                            sockAddrP, addrLenP);
    } else if (IS_TUPLE(env, eIn6SockAddr[2])) {
        return decode_in6_sockaddr_addr(env, eIn6SockAddr[2], port,
                                        flowInfo, scopeId,
                                        sockAddrP, addrLenP);
    } else {
        return ESOCK_STR_EINVAL;
    }
}
#endif


#if defined(HAVE_IN6) && defined(AF_INET6)
static
char* decode_in6_sockaddr_atomaddr(ErlNifEnv*     env,
                                   ERL_NIF_TERM   eAddr,
                                   int            port,
                                   unsigned int   flowInfo,
                                   unsigned int   scopeId,
                                   SocketAddress* sockAddrP,
                                   unsigned int*  addrLenP)
{
    const struct in6_addr* addr;

    if (COMPARE(esock_atom_loopback, eAddr) == 0) {
        addr = &in6addr_loopback;
    } else if (COMPARE(esock_atom_any, eAddr) == 0) {
        addr = &in6addr_any;
    } else {
        return ESOCK_STR_EINVAL;
    }

    sys_memzero((char*)sockAddrP, sizeof(struct sockaddr_in6));
#ifndef NO_SA_LEN
    sockAddrP->in6.sin6_len      = sizeof(struct sockaddr_in6);
#endif
    sockAddrP->in6.sin6_family   = AF_INET6;
    sockAddrP->in6.sin6_port     = sock_htons(port);
    sockAddrP->in6.sin6_flowinfo = flowInfo;
    sockAddrP->in6.sin6_scope_id = scopeId;
    sockAddrP->in6.sin6_addr     = *addr;
    *addrLenP                     = sizeof(struct sockaddr_in6);
    
    return NULL;
}
#endif



#if defined(HAVE_IN6) && defined(AF_INET6)
/* Decode an in6_sockaddr where the address field is a tuple */
static
char* decode_in6_sockaddr_addr(ErlNifEnv*     env,
                               ERL_NIF_TERM   eAddr,
                               int            port,
                               unsigned int   flowInfo,
                               unsigned int   scopeId,
                               SocketAddress* sockAddrP,
                               unsigned int*  addrLenP)
{
    const ERL_NIF_TERM* ip6AddrT;
    int                 ip6AddrTSz;
    int                 a, v;
    char                addr[16];

    /* This shall be a 8 tuple */
    if (!GET_TUPLE(env, eAddr, &ip6AddrTSz, &ip6AddrT))
        return ESOCK_STR_EINVAL;

    if (ip6AddrTSz != 8)
        return ESOCK_STR_EINVAL;

    sys_memzero((char*)sockAddrP, sizeof(struct sockaddr_in6));
#ifndef NO_SA_LEN
    sockAddrP->in6.sin6_len      = sizeof(struct sockaddr_in6);
#endif
    sockAddrP->in6.sin6_family   = AF_INET6;
    sockAddrP->in6.sin6_port     = sock_htons(port);
    sockAddrP->in6.sin6_flowinfo = flowInfo;
    sockAddrP->in6.sin6_scope_id = scopeId;
    /* The address tuple is of size 8
     * and each element is a two byte integer
     */
    for (a = 0; a < 8; a++) {
        if (!GET_INT(env, ip6AddrT[a], &v))
            return ESOCK_STR_EINVAL;
        addr[a*2  ] = ((v >> 8) & 0xFF);
        addr[a*2+1] = (v & 0xFF);
    }
    sys_memcpy(&sockAddrP->in6.sin6_addr, &addr, sizeof(addr));
    *addrLenP = sizeof(struct sockaddr_in6);
    
    return NULL;
}
#endif


/* Decode the 4- or 8-element address tuple
 * and initiate the socket address structure.
 */
/*
static
char* decode_address_tuple(ErlNifEnv*          env,
                           int                 domain,
                           const ERL_NIF_TERM* addrt,
                           int                 port,
                           SocketAddress*      addrP,
                           unsigned int*       addrLenP)
{

    / * We now *know* that the size of the tuple is correct,
     * so we don't need to check anything here, just unpack.
     * /

    switch (domain) {
    case AF_INET:
        {
            int  a, v;
            char laddr[4];

            sys_memzero((char*)addrP, sizeof(struct sockaddr_in));
#ifndef NO_SA_LEN
            addrP->sai.sin_len    = sizeof(struct sockaddr_in);
#endif
            addrP->sai.sin_family = domain;
            addrP->sai.sin_port   = sock_htons(port);
            for (a = 0; a < 4; a++) {
                if (!GET_INT(env, addrt[a], &v))
                    return ESOCK_STR_EINVAL;
                laddr[a] = v;
            }
            sys_memcpy(&addrP->sai.sin_addr, &laddr, sizeof(laddr));
            *addrLenP = sizeof(struct sockaddr_in);
            return NULL;
        }
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        {
            int  a, v;
            char laddr[16];

            sys_memzero((char*)addrP, sizeof(struct sockaddr_in6));
#ifndef NO_SA_LEN
            addrP->sai6.sin6_len      = sizeof(struct sockaddr_in6);
#endif
            addrP->sai6.sin6_family   = domain;
            addrP->sai6.sin6_port     = sock_htons(port);
            addrP->sai6.sin6_flowinfo = 0;
            / * The address tuple is of size 8
             * and each element is a two byte integer
             * /
            for (a = 0; a < 8; a++) {
                if (!GET_INT(env, addrt[a], &v))
                    return ESOCK_STR_EINVAL;
                laddr[a*2  ] = ((v >> 8) & 0xFF);
                laddr[a*2+1] = (v & 0xFF);
            }
            sys_memcpy(&addrP->sai6.sin6_addr, &laddr, sizeof(laddr));
            *addrLenP = sizeof(struct sockaddr_in6);
            return NULL;
        }
        break;
#endif

    } / * switch (domain) * /

    return ESOCK_STR_EAFNOSUPPORT;

}
*/

/* Encode the 4- or 8-element address tuple from the socket address structure.
 *
 * This function is called when we have received a message. So, if we for some
 * reason fail to decode the address or parts of it, it makes more sense to
 * return with "undefined" for the values rather then fail completely (and not
 * deliver the received message).
 *
 * Returns two things (assuming the encode works):
 *
 * Domain: inet | inet6 | local
 * Source: {Address, Port} | string()
 *
 */
static
void encode_address(ErlNifEnv*     env,
                    SocketAddress* sockAddrP,
                    unsigned int   addrLen,
                    ERL_NIF_TERM*  domainT,
                    ERL_NIF_TERM*  sourceT)
{
    short port;

    switch (sockAddrP->sa.sa_family) {

        /* +++ inet (IPv4) +++ */

    case AF_INET:
        if (addrLen >= sizeof(struct sockaddr_in)) {
            ERL_NIF_TERM addrT, portT;
            unsigned int i;
            ERL_NIF_TERM at4[4];
            char*        a4 = (char*) &sockAddrP->in4.sin_addr;

            port = sock_ntohs(sockAddrP->in4.sin_port);
            for (i = 0; i < 4; i++) {
                at4[i] = MKI(env, a4[i]);
            }

            *domainT = MKA(env, "inet"); // Shall we encode these? See decode
            addrT    = MKT4(env, at4[0], at4[1], at4[2], at4[3]);
            portT    = MKI(env, port);
            *sourceT = MKT2(env, addrT, portT);
        } else {
            *domainT = esock_atom_undefined;
            *sourceT = esock_atom_undefined;
        }
        break;


        /* +++ inet6 (IPv6) +++ */

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        if (addrLen >= sizeof(struct sockaddr_in6)) {
            ERL_NIF_TERM addrT, portT;
            unsigned int i;
            ERL_NIF_TERM at6[8];
            char*        a16 = (char*) &sockAddrP->in6.sin6_addr;

            port = sock_ntohs(sockAddrP->in6.sin6_port);
            /* The address tuple is of size 8
             * and each element is a two byte integer
             */
            for (i = 0; i < 8; i++) {
                // at6[i] = MKI(env, get_int16(a16[i*2]));
                at6[i] = MKI(env, get_int16(a16 + i*2));
            }

            *domainT = MKA(env, "inet6"); // Shall we encode these? See decode
            addrT    = MKT8(env,
                            at6[0], at6[1], at6[2], at6[3],
                            at6[4], at6[5], at6[6], at6[7]);
            portT    = MKI(env, port);
            *sourceT = MKT2(env, addrT, portT);
        } else {
            *domainT = esock_atom_undefined;
            *sourceT = esock_atom_undefined;
        }
        break;
#endif

        /* +++ local (Unix Domain Sockets) +++ */

#ifdef HAVE_SYS_UN_H
    case AF_UNIX:
        {
            size_t n, m;

            *domainT = MKA(env, "local");
            if (addrLen < offsetof(struct sockaddr_un, sun_path)) {
                *sourceT = esock_atom_undefined;
            } else {
                n = addrLen - offsetof(struct sockaddr_un, sun_path);
                if (255 < n) {
                    *sourceT = esock_atom_undefined;
                } else {
                    m = my_strnlen(sockAddrP->un.sun_path, n);
#ifdef __linux__
                    /* Assume that the address is a zero terminated string,
                     * except when the first byte is \0 i.e the string length is 0,
                     * then use the reported length instead.
                     * This fix handles Linux's nonportable
                     * abstract socket address extension.
                     */
                    if (m == 0) {
                        m = n;
                    }
#endif

                    *sourceT = MKSL(env, sockAddrP->un.sun_path, m);
                }
            }
        }
        break;
#endif

    default:
        *domainT = esock_atom_undefined;
        *sourceT = esock_atom_undefined;
        break;

    } /* switch (addrP->sa.sa_family) */

}


/* Decode the address when its an atom.
 * Currently we only accept two atoms: 'any' and 'loopback'
 */
/*
static
char* decode_address_atom(ErlNifEnv*     env,
                          int            domain,
                          char*          addr,
                          int            addrLen,
                          int            port,
                          SocketAddress* addrP,
                          unsigned int*  addrLenP)
{
    BOOLEAN_T any;

    if (strncmp(addr, "any", addrLen) == 0) {
        any = TRUE;
    } if (strncmp(addr, "loopback", addrLen) == 0) {
        any = FALSE;
    } else {
        return ESOCK_STR_EINVAL;
    }

    / * If we get this far, we *know* its either 'any' or 'loopback' * /

    switch (domain) {
    case AF_INET:
        {
            struct in_addr addr;
            if (any) {
                addr.s_addr = sock_htonl(INADDR_ANY);
            } else {
                addr.s_addr = sock_htonl(INADDR_LOOPBACK);
            }
            sys_memzero((char*) addrP, sizeof(struct sockaddr_in));
#ifndef NO_SA_LEN
            addrP->sai.sin_len         = sizeof(struct sockaddr_in6);
#endif
            addrP->sai.sin_family      = domain;
            addrP->sai.sin_port        = sock_htons(port);
            addrP->sai.sin_addr.s_addr = addr.s_addr;
            *addrLenP                  = sizeof(struct sockaddr_in);
        }
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        {
            const struct in6_addr* paddr;
            if (any) {
                paddr = &in6addr_any;
            } else {
                paddr = &in6addr_loopback;
            }
            sys_memzero((char*)addrP, sizeof(struct sockaddr_in6));
#ifndef NO_SA_LEN
            addrP->sai6.sin6_len      = sizeof(struct sockaddr_in6);
#endif
            addrP->sai6.sin6_family   = domain;
            addrP->sai6.sin6_port     = sock_htons(port);
            addrP->sai6.sin6_flowinfo = 0;
            addrP->sai6.sin6_addr     = *paddr;
            *addrLenP                 = sizeof(struct sockaddr_in6);
        }
        break;
#endif

    default:
        return ESOCK_STR_EINVAL;
        break;
    }

    return NULL;
}
*/

/*
static
BOOLEAN_T decode_bool(ErlNifEnv* env, ERL_NIF_TERM eVal, BOOLEAN_T* val)
{
     unsigned int len;
     char         b[16]; // Just in case...

     / * Verify that the value is actually an atom * /
     if (!IS_ATOM(env, eVal))
         return FALSE;

     / * Verify that the value is of acceptable length * /
     if (!(GET_ATOM_LEN(env, eVal, &len) &&
           (len > 0) &&
           (len <= sizeof("false"))))
         return FALSE;

     / * And finally try to extract the value * /
     if (!GET_ATOM(env, eVal, b, sizeof(b)))
         return FALSE;

     if (strncmp(b, "true", len) == 0)
         *val = TRUE;
     else
         *val = FALSE;

     return TRUE;
}
*/


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



/* +++ decode the ip socket option tos +++
 * The (ip) option can be provide in two ways:
 *
 *           atom() | integer()
 *
 * When its an atom it can have the values:
 *
 *       lowdelay |  throughput | reliability | mincost
 *
 */
static
BOOLEAN_T decode_ip_tos(ErlNifEnv* env, ERL_NIF_TERM eVal, int* val)
{
    BOOLEAN_T result = FALSE;

    if (IS_ATOM(env, eVal)) {
        unsigned int len;
        char         b[sizeof(str_reliability)+1]; // Just in case...

        if (!(GET_ATOM_LEN(env, eVal, &len) &&
              (len > 0) &&
              (len <= (sizeof(str_reliability))))) {
            *val = -1;
            return FALSE;
        }

        if (!GET_ATOM(env, eVal, b, sizeof(b))) {
            *val = -1;
            return FALSE;
        }

        if (strncmp(b, str_lowdelay, len) == 0) {
            *val = IPTOS_LOWDELAY;
            result = TRUE;
        } else if (strncmp(b, str_throughput, len) == 0) {
            *val = IPTOS_THROUGHPUT;
            result = TRUE;
        } else if (strncmp(b, str_reliability, len) == 0) {
            *val = IPTOS_RELIABILITY;
            result = TRUE;
        } else if (strncmp(b, str_mincost, len) == 0) {
            *val = IPTOS_MINCOST;
            result = TRUE;
        } else {
            *val = -1;
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
                                int* opt, uint16_t* valueType, int* valueSz)
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
        unsigned int len;
        char         t[16]; // Just in case

        if (!(GET_ATOM_LEN(env, nativeOptT[1], &len) &&
              (len > 0) &&
              (len <= (sizeof("bool")))))
            return FALSE;

        if (!GET_ATOM(env, nativeOptT[1], t, sizeof(t)))
            return FALSE;

        if (strncmp(t, "bool", len) == 0) {
            *valueType = SOCKET_OPT_VALUE_TYPE_BOOL;
            *valueSz   = sizeof(int); // Just to be sure
        } else if (strncmp(t, "int", len) == 0) {
            *valueType = SOCKET_OPT_VALUE_TYPE_INT;
            *valueSz   = sizeof(int); // Just to be sure
        } else {
            return FALSE;
        }

    } else if (IS_NUM(env, nativeOptT[1])) {
        if (GET_INT(env, nativeOptT[1], valueSz)) {
            *valueType = SOCKET_OPT_VALUE_TYPE_UNSPEC;
        } else {
            return FALSE;
        }
    } else {
        return FALSE;
    }

    return TRUE;
}


/*
static
void encode_bool(BOOLEAN_T val, ERL_NIF_TERM* eVal)
{
    if (val)
        *eVal = esock_atom_true;
    else
        *eVal = esock_atom_false;
}
*/


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

    switch (val) {
    case IPTOS_LOWDELAY:
        result = esock_make_ok2(env, atom_lowdelay);
        break;

    case IPTOS_THROUGHPUT:
        result = esock_make_ok2(env, atom_throughput);
        break;

    case IPTOS_RELIABILITY:
        result = esock_make_ok2(env, atom_reliability);
        break;

    case IPTOS_MINCOST:
        result = esock_make_ok2(env, atom_mincost);
        break;

    default:
        result = esock_make_ok2(env, MKI(env, val));
        break;
    }

    return result;
}





/* *** alloc_descriptor ***
 * Allocate and perform basic initialization of a socket descriptor.
 *
 */
static
SocketDescriptor* alloc_descriptor(SOCKET sock, HANDLE event)
{
    SocketDescriptor* descP;

    if ((descP = enif_alloc_resource(sockets, sizeof(SocketDescriptor))) != NULL) {
        char buf[64]; /* Buffer used for building the mutex name */

        sprintf(buf, "socket[w,%d]", sock);
        descP->writeMtx       = MCREATE(buf);
        descP->currentWriterP = NULL; // currentWriter not used
        descP->writersQ.first = NULL;
        descP->writersQ.last  = NULL;
        descP->isWritable     = TRUE;
        descP->writePkgCnt    = 0;
        descP->writeByteCnt   = 0;
        descP->writeTries     = 0;
        descP->writeWaits     = 0;
        descP->writeFails     = 0;

        sprintf(buf, "socket[r,%d]", sock);
        descP->readMtx        = MCREATE(buf);
        descP->currentReaderP = NULL; // currentReader not used
        descP->readersQ.first = NULL;
        descP->readersQ.last  = NULL;
        descP->isReadable     = TRUE;
        descP->readPkgCnt     = 0;
        descP->readByteCnt    = 0;
        descP->readTries      = 0;
        descP->readWaits      = 0;

        sprintf(buf, "socket[acc,%d]", sock);
        descP->accMtx           = MCREATE(buf);
        descP->currentAcceptorP = NULL; // currentAcceptor not used
        descP->acceptorsQ.first = NULL;
        descP->acceptorsQ.last  = NULL;

        sprintf(buf, "socket[close,%d]", sock);
        descP->closeMtx         = MCREATE(buf);

        descP->rBufSz           = SOCKET_RECV_BUFFER_SIZE_DEFAULT;
        descP->iow              = FALSE;
        descP->dbg              = SOCKET_DEBUG_DEFAULT;

        descP->sock             = sock;
        descP->event            = event;

    }

    return descP;
}


/* compare_pids - Test if two pids are equal
 *
 */
static
int compare_pids(ErlNifEnv*       env,
                 const ErlNifPid* pid1,
                 const ErlNifPid* pid2)
{
    ERL_NIF_TERM p1 = enif_make_pid(env, pid1);
    ERL_NIF_TERM p2 = enif_make_pid(env, pid2);

    return enif_is_identical(p1, p2);
}


/* ----------------------------------------------------------------------
 *  D e c o d e / E n c o d e   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

/* edomain2domain - convert internal (erlang) domain to (proper) domain
 *
 * Note that only a subset is supported.
 */
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

    case SOCKET_TYPE_SEQPACKET:
        *type = SOCK_SEQPACKET;
        break;

    default:
        return FALSE;
    }

    return TRUE;
}


/* eproto2proto - convert internal (erlang) protocol to (proper) protocol
 *
 * Note that only a subset is supported.
 */
static
BOOLEAN_T eproto2proto(int eproto, int* proto)
{
    switch (eproto) {
    case SOCKET_PROTOCOL_IP:
        *proto = IPPROTO_IP;
        break;

    case SOCKET_PROTOCOL_TCP:
        *proto = IPPROTO_TCP;
        break;

    case SOCKET_PROTOCOL_UDP:
        *proto = IPPROTO_UDP;
        break;

    case SOCKET_PROTOCOL_SCTP:
        *proto = IPPROTO_SCTP;
        break;

    default:
        return FALSE;
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
BOOLEAN_T esendflags2sendflags(unsigned int esendflags, int* sendflags)
{
    unsigned int ef;
    int          tmp = 0;

    for (ef = SOCKET_SEND_FLAG_LOW; ef <= SOCKET_SEND_FLAG_HIGH; ef++) {
        switch (ef) {
        case SOCKET_SEND_FLAG_CONFIRM:
            tmp |= MSG_CONFIRM;
            break;

        case SOCKET_SEND_FLAG_DONTROUTE:
            tmp |= MSG_DONTROUTE;
            break;

        case SOCKET_SEND_FLAG_EOR:
            tmp |= MSG_EOR;
            break;

        case SOCKET_SEND_FLAG_MORE:
            tmp |= MSG_MORE;
            break;

        case SOCKET_SEND_FLAG_NOSIGNAL:
            tmp |= MSG_NOSIGNAL;
            break;

        case SOCKET_SEND_FLAG_OOB:
            tmp |= MSG_OOB;
            break;

        default:
            return FALSE;
        }

    }

    *sendflags = tmp;

    return TRUE;
}



/* erecvflags2recvflags - convert internal (erlang) send flags to (proper)
 * send flags.
 */
static
BOOLEAN_T erecvflags2recvflags(unsigned int erecvflags, int* recvflags)
{
    unsigned int ef;
    int          tmp = 0;

    for (ef = SOCKET_RECV_FLAG_LOW; ef <= SOCKET_RECV_FLAG_HIGH; ef++) {
        switch (ef) {
        case SOCKET_RECV_FLAG_CMSG_CLOEXEC:
            tmp |= MSG_CMSG_CLOEXEC;
            break;

        case SOCKET_RECV_FLAG_ERRQUEUE:
            tmp |= MSG_ERRQUEUE;
            break;

        case SOCKET_RECV_FLAG_OOB:
            tmp |= MSG_OOB;
            break;

        case SOCKET_RECV_FLAG_PEEK:
            tmp |= MSG_PEEK;
            break;

        case SOCKET_RECV_FLAG_TRUNC:
            tmp |= MSG_TRUNC;
            break;

        default:
            return FALSE;
        }

    }

    *recvflags = tmp;

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



/* +++ decode_sockaddr +++
 *
 * Decode a socket address - sockaddr. In erlang its represented by
 * a map, which has a specific set of attributes, depending on one
 * mandatory attribute; family. So depending on the value of the family
 * attribute: 
 *
 *    local - sockaddr_un:  path
 *    inet  - sockaddr_in4: port, addr
 *    inet6 - sockaddr_in6: port, addr, flowinfo, scope_id
 */
/*
static
char* decode_sockaddr(ErlNifEnv*     env,
                      ERL_NIF_TERM   eSockAddr,
                      SocketAddress* sockAddrP,
                      unsigned int*  addrLen)
{
    ERL_NIF_TERM efam;
    int          fam;
    char*        res;

    if (!IS_MAP(env, eSockAddr))
        return ESOCK_STR_EINVAL;

    if (!GET_MAP_VAL(env, eSockAddr, esock_atom_family, &efam))
        return ESOCK_STR_EINVAL;

    if (!decode_domain(env, efam, &fam))
        return ESOCK_STR_EINVAL;

    switch (fam) {
    case AF_INET:
        res = decode_sockaddr_in4(env, eSockAddr, &sockAddrP->in6, addrLen);
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        res = decode_sockaddr_in6(env, eSockAddr, &sockAddrP->in6, addrLen);
        break;
#endif

#ifdef HAVE_SYS_UN_H
    case AF_UNIX:
        res = decode_sockaddr_un(env, eSockAddr, &sockAddrP->un, addrLen);
        break;
#endif

    default:
        result = ESOCK_STR_EAFNOSUPPORT;
        break;

    }

    return res;
}
*/



/* +++ decode_sockaddr_in4 +++
 *
 * Decode a IPv4 socket address - sockaddr_in4. In erlang its represented by
 * a map, which has a specific set of attributes: 
 *
 *    port :: port_numbber()
 *    addr :: ip4_address()
 *
 * The erlang module ensures that both of these has values exist, so there 
 * is no need for any elaborate error handling.
 */
/*
static
char* decode_sockaddr_in4(ErlNifEnv*          env,
                          ERL_NIF_TERM        eSockAddr,
                          struct sockaddr_in* sockAddrP,
                          unsigned int*       addrLen)
{
    ERL_NIF_TERM eport, eaddr;
    short        port;

    / * Basic init * /
    sys_memzero((char*) sockAddrP, sizeof(struct sockaddr_in));

#ifndef NO_SA_LEN
    sockAddrP->sin_len    = sizeof(struct sockaddr_in);
#endif

    sockAddrP->sin_family = AF_INET;

    / * Extract (e) port number from map * /
    if (!GET_MAP_VAL(env, eSockAddr, atom_port, &eport))
        return ESOCK_STR_EINVAL;

    / * Decode port number * /
    if (!GET_INT(env, eport, &port))
        return ESOCK_STR_EINVAL;
    sockAddrP->sin_port = sock_htons(port);

    / * Extract (e) address from map * /
    if (!GET_MAP_VAL(env, eSockAddr, atom_addr, &eaddr))
        return ESOCK_STR_EINVAL;

    / * Decode address * /
    if (!decode_ip4_address(env, eaddr, sockAddrP, addrLen))
        return ESOCK_STR_EINVAL;

    return NULL;
}
*/



/* +++ decode_sockaddr_in6 +++
 *
 * Decode a IPv6 socket address - sockaddr_in6. In erlang its represented by
 * a map, which has a specific set of attributes: 
 *
 *    port     :: port_numbber()  (integer)
 *    addr     :: ip6_address()   (tuple)
 *    flowinfo :: in6_flow_info() (integer)
 *    scope_id :: in6_scope_id()  (integer)
 *
 * The erlang module ensures that all of these has values exist, so there
 * is no need for any elaborate error handling.
 */
/*
#if defined(HAVE_IN6) && defined(AF_INET6)
static
char* decode_sockaddr_in6(ErlNifEnv*           env,
                          ERL_NIF_TERM         eSockAddr,
                          struct sockaddr_in6* sockAddrP,
                          unsigned int*        addrLen)
{
    ERL_NIF_TERM eport, eaddr;
    short        port;

    / * Basic init * /
    sys_memzero((char*) sockAddrP, sizeof(struct sockaddr_in6));
#ifndef NO_SA_LEN
    sockAddrP->sin6_len = sizeof(struct sockaddr_in);
#endif

    sockAddrP->sin6_family = AF_INET6;

    / * *** Extract (e) port number from map *** * /
    if (!GET_MAP_VAL(env, eSockAddr, atom_port, &eport))
        return ESOCK_STR_EINVAL;

    / * Decode port number * /
    if (!GET_INT(env, eport, &port))
        return ESOCK_STR_EINVAL;

    sockAddrP->sin6_port = sock_htons(port);

    / * *** Extract (e) flowinfo from map *** * /
    if (!GET_MAP_VAL(env, eSockAddr, atom_flowinfo, &eflowInfo))
        return ESOCK_STR_EINVAL;

    / * 4: Get the flowinfo * /
    if (!GET_UINT(env, eflowInfo, &flowInfo))
        return ESOCK_STR_EINVAL;

    sockAddrP->sin6_flowinfo = flowInfo;
    
    / * *** Extract (e) scope_id from map *** * /
    if (!GET_MAP_VAL(env, eSockAddr, atom_scope_id, &escopeId))
        return ESOCK_STR_EINVAL;

    / * *** Get the scope_id *** * /
    if (!GET_UINT(env, escopeId, &scopeId))
        return ESOCK_STR_EINVAL;

    sockAddrP->sin6_scope_id = scopeId;

    / * *** Extract (e) address from map *** * /
    if (!GET_MAP_VAL(env, eSockAddr, atom_addr, &eaddr))
        return ESOCK_STR_EINVAL;

    / * Decode address * /
    if (!decode_ip6_address(env, eaddr, sockAddrP, addrLen))
        return ESOCK_STR_EINVAL;

    return NULL;
}
#endif
*/




/* +++ decode_sockaddr_un +++
 *
 * Decode a Unix Domain socket address - sockaddr_un. In erlang its represented by
 * a map, which has a specific set of attributes: 
 *
 *    path :: binary()
 *
 * The erlang module ensures that this value exist, so there 
 * is no need for any elaborate error handling.
 */
/*
#ifdef HAVE_SYS_UN_H
static
char* decode_sockaddr_un(ErlNifEnv*          env,
                         ERL_NIF_TERM        eSockAddr,
                         struct sockaddr_un* sockAddrP,
                         unsigned int*       addrLen)
{
    ErlNifBinary bin;
    ERL_NIF_TERM epath;
    unsigned int len;

    / * *** Extract (e) path (a binary) from map *** * /
    if (!GET_MAP_VAL(env, eSockAddr, atom_port, &epath))
        return ESOCK_STR_EINVAL;

    / * Get the path * /
    if (!GET_BIN(env, epath, &bin))
        return ESOCK_STR_EINVAL;
    
    if ((bin.size +
#ifdef __linux__
    / * Make sure the address gets zero terminated
     * except when the first byte is \0 because then it is
     * sort of zero terminated although the zero termination
     * comes before the address...
     * This fix handles Linux's nonportable
     * abstract socket address extension.
     * /
    (bin.data[0] == '\0' ? 0 : 1)
#else
        1
#endif
         ) > sizeof(sockaAddrP->sun_path))
        return ESOCK_STR_EINVAL;


    sys_memzero((char*) sockAddrP, sizeof(struct sockaddr_un));
    sockAddrP->sun_family = AF_UNIX;

    sys_memcpy(sockAddrP->sun_path, bin.data, bin.size);
    len = offsetof(struct sockaddr_un, sun_path) + bin.size;

#ifndef NO_SA_LEN
    sockAddrP->sun_len = len;
#endif
    *addrLen = len:

    return NULL;
}
#endif
*/



/* +++ decode_ip4_address +++
 *
 * Decode a IPv4 address. This can be three things:
 *
 *    + Then atom 'any'
 *    + Then atom 'loopback'
 *    + An ip4_address() (4 tuple)
 *
 * Note that this *only* decodes the "address" part of a
 * (IPv4) socket address. There are several other things (port).
 */
/*
static
char* decode_ip4_address(ErlNifEnv*          env,
                         ERL_NIF_TERM        eAddr,
                         struct sockaddr_in* sockAddrP,
                         unsigned int*       addrLen)
{
    if (IS_ATOM(env, eAddr)) {
        / * This is either 'any' or 'loopback' * /
        struct in_addr addr;

        if (COMPARE(esock_atom_loopback, eAddr) == 0) {
            addr.s_addr = sock_htonl(INADDR_LOOPBACK);
        } else if (COMPARE(esock_atom_any, eAddr) == 0) {
            addr.s_addr = sock_htonl(INADDR_ANY);
        } else {
            return ESOCK_STR_EINVAL;
        }

        sockAddrP->sin_addr.s_addr = addr.s_addr;
        *addrLen                   = sizeof(struct sockaddr_in);

    } else {
        / * This is a 4-tuple * /

        const ERL_NIF_TERM* addrt;
        int                 addrtSz;
        int                 a, v;
        char                addr[4];
        
        if (!GET_TUPLE(env, eAddr, &addrtSz, &addrt))
            return ESOCK_STR_EINVAL;
        
        if (addrtSz != 4)
            return ESOCK_STR_EINVAL;

        for (a = 0; a < 4; a++) {
            if (!GET_INT(env, addrt[a], &v))
                return ESOCK_STR_EINVAL;
            addr[a] = v;
        }

        sys_memcpy(&sockAddrP->sin_addr, &addr, sizeof(addr));
        *addrLenP = sizeof(struct sockaddr_in);
        
    }

    return NULL;
}
*/



/* +++ decode_ip6_address +++
 *
 * Decode a IPv6 address. This can be three things:
 *
 *    + Then atom 'any'
 *    + Then atom 'loopback'
 *    + An ip6_address() (8 tuple)
 *
 * Note that this *only* decodes the "address" part of a
 * (IPv6) socket address. There are several other things
 * (port, flowinfo and scope_id) that are handled elsewhere).
 */
/*
#if defined(HAVE_IN6) && defined(AF_INET6)
static
char* decode_ip6_address(ErlNifEnv*           env,
                         ERL_NIF_TERM         eAddr,
                         struct sockaddr_in6* sockAddrP,
                         unsigned int*        addrLen)
{
    if (IS_ATOM(env, eAddr)) {
        / * This is either 'any' or 'loopback' * /
        const struct in6_addr* addr;

        if (COMPARE(esock_atom_loopback, eAddr) == 0) {
            addr = &in6addr_loopback;
        } else if (COMPARE(esock_atom_any, eAddr) == 0) {
            addr = &in6addr_any;
        } else {
            return ESOCK_STR_EINVAL;
        }

        sockAddrP->sin6_addr = *addr;
        *addrLen             = sizeof(struct sockaddr_in6);

    } else {
        / * This is a 8-tuple * /

        const ERL_NIF_TERM* addrt;
        int                 addrtSz;
        int                 a, v;
        char                addr[16];
        
        if (!GET_TUPLE(env, eAddr, &addrtSz, &addrt))
            return ESOCK_STR_EINVAL;
        
        if (addrtSz != 8)
            return ESOCK_STR_EINVAL;

        for (a = 0; a < 8; a++) {
            if (!GET_INT(env, addrt[a], &v))
                return ESOCK_STR_EINVAL;
            addr[a*2  ] = ((v >> 8) & 0xFF);
            addr[a*2+1] = (v & 0xFF);
        }

        sys_memcpy(&sockAddrP->sin6_addr, &addr, sizeof(addr));
        *addrLen = sizeof(struct sockaddr_in6);
        
    }

    return NULL;
}
#endif
*/



#if defined(HAVE_SYS_UN_H) || defined(SO_BINDTODEVICE)
/* strnlen doesn't exist everywhere */
static
size_t my_strnlen(const char *s, size_t maxlen)
{
    size_t i = 0;
    while (i < maxlen && s[i] != '\0')
        i++;
    return i;
}
#endif


/* Send an error closed message to the specified process:
 *
 * This message is for processes that are waiting in the
 * erlang API functions for a select message.
 */
/*
static
char* send_msg_error_closed(ErlNifEnv* env,
                            ErlNifPid* pid)
{
  return send_msg_error(env, atom_closed, pid);
}
*/

/* Send an error message to the specified process:
 * A message in the form:
 *
 *     {error, Reason}
 *
 * This message is for processes that are waiting in the
 * erlang API functions for a select message.
 */
/*
static
char* send_msg_error(ErlNifEnv*   env,
                     ERL_NIF_TERM reason,
                     ErlNifPid*   pid)
{
    ERL_NIF_TERM msg = enif_make_tuple2(env, atom_error, reason);

    return send_msg(env, msg, pid);
}
*/


/* Send an (nif-) abort message to the specified process:
 * A message in the form:
 *
 *     {nif_abort, Ref, Reason}
 *
 * This message is for processes that are waiting in the
 * erlang API functions for a select message.
 */
static
char* send_msg_nif_abort(ErlNifEnv*   env,
                         ERL_NIF_TERM ref,
                         ERL_NIF_TERM reason,
                         ErlNifPid*   pid)
{
    ERL_NIF_TERM msg = MKT3(env, atom_nif_abort, ref, reason);

    return send_msg(env, msg, pid);
}


/* Send a message to the specified process.
 */
static
char* send_msg(ErlNifEnv*   env,
	       ERL_NIF_TERM msg,
	       ErlNifPid*   pid)
{
  if (!enif_send(env, pid, NULL, msg))
    return str_exsend;
  else
    return NULL;
}



/* ----------------------------------------------------------------------
 *  C o u n t e r   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

static
BOOLEAN_T cnt_inc(uint32_t* cnt, uint32_t inc)
{
  BOOLEAN_T wrap;
  uint32_t  max     = 0xFFFFFFFF;
  uint32_t  current = *cnt;

  if ((max - inc) >= current) {
    *cnt += inc;
    wrap  = FALSE;
  } else {
    *cnt = inc - (max - current) - 1;
    wrap = TRUE;
  }

  return (wrap);
}



/* ----------------------------------------------------------------------
 *  C a l l b a c k   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

/* =========================================================================
 * socket_dtor - Callback function for resource destructor
 *
 */
static
void socket_dtor(ErlNifEnv* env, void* obj)
{
  SocketDescriptor* descP = (SocketDescriptor*) obj;

  MDESTROY(descP->writeMtx);
  MDESTROY(descP->readMtx);
  MDESTROY(descP->accMtx);
  MDESTROY(descP->closeMtx);
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
 * <KOLLA>
 *
 * We do not handle linger-issues yet! So anything in the out
 * buffers will be left for the OS to solve...
 * Do we need a special "close"-thread? Dirty scheduler?
 *
 * What happens if we are "stopped" for another reason then 'close'?
 * For instance, down?
 *
 * </KOLLA>
 */
static
void socket_stop(ErlNifEnv* env, void* obj, int fd, int is_direct_call)
{
  SocketDescriptor* descP = (SocketDescriptor*) obj;

  MLOCK(descP->writeMtx);
  MLOCK(descP->readMtx);
  MLOCK(descP->accMtx);
  MLOCK(descP->closeMtx);


  descP->state      = SOCKET_STATE_CLOSING; // Just in case...???
  descP->isReadable = FALSE;
  descP->isWritable = FALSE;


  /* We should check that we actually have a monitor.
   * This *should* be done with a "NULL" monitor value,
   * which there currently is none...
   */
  DEMONP(env, descP, &descP->ctrlMon);

  if (descP->currentWriterP != NULL) {
      /* We have a (current) writer and *may* therefor also have
       * writers waiting.
       */

      ESOCK_ASSERT( (NULL == send_msg_nif_abort(env,
                                                descP->currentWriter.ref,
                                                atom_closed,
                                                &descP->currentWriter.pid)) );

      /* And also deal with the waiting writers (in the same way) */
      inform_waiting_procs(env, descP, &descP->writersQ, TRUE, atom_closed);
  }

  if (descP->currentReaderP != NULL) {

      /* We have a (current) reader and *may* therefor also have
       * readers waiting.
       */

      ESOCK_ASSERT( (NULL == send_msg_nif_abort(env,
                                                descP->currentReader.ref,
                                                atom_closed,
                                                &descP->currentReader.pid)) );

      /* And also deal with the waiting readers (in the same way) */
      inform_waiting_procs(env, descP, &descP->readersQ, TRUE, atom_closed);
  }

  if (descP->currentAcceptorP != NULL) {
      /* We have a (current) acceptor and *may* therefor also have
       * acceptors waiting.
       */

      ESOCK_ASSERT( (NULL == send_msg_nif_abort(env,
                                                descP->currentAcceptor.ref,
                                                atom_closed,
                                                &descP->currentAcceptor.pid)) );

      /* And also deal with the waiting acceptors (in the same way) */
      inform_waiting_procs(env, descP, &descP->acceptorsQ, TRUE, atom_closed);
  }


  if (descP->sock != INVALID_SOCKET) {

      /*
       * <KOLLA>
       *
       * WE NEED TO CHECK IF THIS OPERATION IS TRIGGERED
       * LOCALLY (VIA A CALL TO CLOSE) OR REMOTELLY
       * (VIA I.E. ECONSRESET).
       *
       * </KOLLA>
       */

      if (descP->closeLocal) {

          /* +++ send close message to the waiting process +++
           *
           *           {close, CloseRef}
           *
           * <KOLLA>
           *
           * WHAT HAPPENS IF THE RECEIVER HAS DIED IN THE MEANTIME????
           *
           * </KOLLA>
           */

          send_msg(env, MKT2(env, atom_close, descP->closeRef), &descP->closerPid);

          DEMONP(env, descP, &descP->closerMon);

      } else {

          /*
           * <KOLLA>
           *
           * ABORT?
           *
           * </KOLLA>
           */
      }
  }


  MUNLOCK(descP->closeMtx);
  MUNLOCK(descP->accMtx);
  MUNLOCK(descP->readMtx);
  MUNLOCK(descP->writeMtx);

}


/* This function traverse the queue and sends the specified
 * nif_abort message with the specified reason to each member,
 * and if the 'free' argument is TRUE, the queue will be emptied.
 */
static
void inform_waiting_procs(ErlNifEnv*          env,
                          SocketDescriptor*   descP,
                          SocketRequestQueue* q,
                          BOOLEAN_T           free,
                          ERL_NIF_TERM        reason)
{
    SocketRequestQueueElement* currentP = q->first;
    SocketRequestQueueElement* nextP;

    while (currentP != NULL) {

        /* <KOLLA>
         *
         * Should we inform anyone if we fail to demonitor?
         * NOT SURE WHAT THAT WOULD REPRESENT AND IT IS NOT
         * IMPORTANT IN *THIS* CASE, BUT ITS A FUNDAMENTAL OP...
         *
         * </KOLLA>
         */

        ESOCK_ASSERT( (NULL == send_msg_nif_abort(env,
                                                  currentP->data.ref,
                                                  reason,
                                                  &currentP->data.pid)) );
        DEMONP(env, descP, &currentP->data.mon);
        nextP = currentP->nextP;
        if (free) FREE(currentP);
        currentP = nextP;
    }

    if (free) {
        q->first = NULL;
        q->last  = NULL;
    }
}



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
}



/* ----------------------------------------------------------------------
 *  L o a d / u n l o a d / u p g r a d e   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

static
ErlNifFunc socket_funcs[] =
{
    // Some utility functions
    {"nif_is_loaded", 0, nif_is_loaded, 0},
    {"nif_info",      0, nif_info, 0},
    // {"nif_debug",      1, nif_debug_, 0},

    // The proper "socket" interface
    // This is used when we already have a file descriptor
    // {"nif_open",                1, nif_open, 0},
    {"nif_open",                4, nif_open, 0},
    {"nif_bind",                2, nif_bind, 0},
    {"nif_connect",             2, nif_connect, 0},
    {"nif_listen",              2, nif_listen, 0},
    {"nif_accept",              2, nif_accept, 0},
    {"nif_send",                4, nif_send, 0},
    {"nif_sendto",              5, nif_sendto, 0},
    {"nif_recv",                4, nif_recv, 0},
    {"nif_recvfrom",            4, nif_recvfrom, 0},
    {"nif_close",               1, nif_close, 0},
    {"nif_shutdown",            2, nif_shutdown, 0},
    {"nif_setopt",              5, nif_setopt, 0},
    {"nif_getopt",              4, nif_getopt, 0},

    /* Misc utility functions */

    /* "Extra" functions to "complete" the socket interface.
     * For instance, the function nif_finalize_connection
     * is called after the connect *select* has "completed".
     */
    {"nif_finalize_connection", 1, nif_finalize_connection, 0},
    // {"nif_cancel",              2, nif_cancel, 0},
    {"nif_finalize_close",      1, nif_finalize_close, ERL_NIF_DIRTY_JOB_IO_BOUND}
};


static
BOOLEAN_T extract_item_on_load(ErlNifEnv*    env,
                               ERL_NIF_TERM  map,
                               ERL_NIF_TERM  key,
                               ERL_NIF_TERM* val)
{
    if (!enif_is_map(env, map))
        return FALSE;

    if (!GET_MAP_VAL(env, map, key, val))
        return FALSE;

    return TRUE;
}

static
BOOLEAN_T extract_debug_on_load(ErlNifEnv* env, ERL_NIF_TERM map, BOOLEAN_T def)
{
    ERL_NIF_TERM dbgKey = enif_make_atom(env, "debug");
    ERL_NIF_TERM dbgVal;
    unsigned int len;
    char         d[16]; // Just in case...

    /* Extra the value of the debug property */
    if (!extract_item_on_load(env, map, dbgKey, &dbgVal))
        return def;

    /* Verify that the value is actually an atom */
    if (!enif_is_atom(env, dbgVal))
        return def;

    /* Verify that the value is of acceptable length */
    if (!(GET_ATOM_LEN(env, dbgVal, &len) &&
          (len > 0) &&
          (len <= sizeof("false"))))
        return def;

    /* And finally try to extract the value */
    if (!GET_ATOM(env, dbgVal, d, sizeof(d)))
        return def;

    if (strncmp(d, "true", len) == 0)
        return TRUE;
    else
        return FALSE;

}


static
BOOLEAN_T extract_iow_on_load(ErlNifEnv* env, ERL_NIF_TERM map, BOOLEAN_T def)
{
     ERL_NIF_TERM iowKey = enif_make_atom(env, "iow");
     ERL_NIF_TERM iowVal;
     unsigned int len;
     char         b[16]; // Just in case...

     /* Extra the value of the debug property */
     if (!extract_item_on_load(env, map, iowKey, &iowVal))
         return def;

     /* Verify that the value is actually an atom */
     if (!enif_is_atom(env, iowVal))
         return def;

     /* Verify that the value is of acceptable length */
     if (!(GET_ATOM_LEN(env, iowVal, &len) &&
           (len > 0) &&
           (len <= sizeof("false"))))
         return def;

     /* And finally try to extract the value */
     if (!GET_ATOM(env, iowVal, b, sizeof(b)))
         return def;

     if (strncmp(b, "true", len) == 0)
         return TRUE;
     else
         return FALSE;

 }



/* =======================================================================
 * load_info - A map of misc info (e.g global debug)
 */

static
int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    data.dbg = extract_debug_on_load(env, load_info,
                                     SOCKET_NIF_DEBUG_DEFAULT);
    data.iow = extract_iow_on_load(env, load_info,
                                   SOCKET_NIF_IOW_DEFAULT);

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

    /* +++ Misc atoms +++ */
    atom_close        = MKA(env, str_close);
    atom_closed       = MKA(env, str_closed);
    atom_closing      = MKA(env, str_closing);
    atom_debug        = MKA(env, str_debug);
    atom_false        = MKA(env, str_false);
    atom_global_counters = MKA(env, str_global_counters);
    atom_in4_sockaddr = MKA(env, str_in4_sockaddr);
    atom_in6_sockaddr = MKA(env, str_in6_sockaddr);
    atom_iow          = MKA(env, str_iow);
    atom_nif_abort    = MKA(env, str_nif_abort);
    atom_num_dinet    = MKA(env, str_num_dinet);
    atom_num_dinet6   = MKA(env, str_num_dinet6);
    atom_num_dlocal   = MKA(env, str_num_dlocal);
    atom_num_pip      = MKA(env, str_num_pip);
    atom_num_psctp    = MKA(env, str_num_psctp);
    atom_num_ptcp     = MKA(env, str_num_ptcp);
    atom_num_pudp     = MKA(env, str_num_pudp);
    atom_num_sockets  = MKA(env, str_num_sockets);
    atom_num_tdgrams  = MKA(env, str_num_tdgrams);
    atom_num_tseqpkgs = MKA(env, str_num_tseqpkgs);
    atom_num_tstreams = MKA(env, str_num_tstreams);
    atom_select       = MKA(env, str_select);
    atom_timeout      = MKA(env, str_timeout);
    atom_true         = MKA(env, str_true);

    /* Global atom(s) */
    esock_atom_addr      = MKA(env, "addr");
    esock_atom_any       = MKA(env, "any");
    esock_atom_dgram     = MKA(env, "dgram");
    esock_atom_error     = MKA(env, "error");
    esock_atom_false     = MKA(env, "famlse");
    esock_atom_family    = MKA(env, "family");
    esock_atom_flowinfo  = MKA(env, "flowinfo");
    esock_atom_inet      = MKA(env, "inet");
    esock_atom_inet6     = MKA(env, "inet6");
    esock_atom_ip        = MKA(env, "ip");
    esock_atom_ipv6      = MKA(env, "ipvp");
    esock_atom_local     = MKA(env, "local");
    esock_atom_loopback  = MKA(env, "loopback");
    esock_atom_ok        = MKA(env, "ok");
    esock_atom_path      = MKA(env, "path");
    esock_atom_port      = MKA(env, "port");
    esock_atom_raw       = MKA(env, "raw");
    esock_atom_rdm       = MKA(env, "rdm");
    esock_atom_scope_id  = MKA(env, "scope_id");
    esock_atom_sctp      = MKA(env, "sctp");
    esock_atom_seqpacket = MKA(env, "seqpacket");
    esock_atom_stream    = MKA(env, "stream");
    esock_atom_tcp       = MKA(env, "tcp");
    esock_atom_true      = MKA(env, "true");
    esock_atom_udp       = MKA(env, "udp");
    esock_atom_undefined = MKA(env, "undefined");

    /* Global error codes */
    esock_atom_eafnosupport = MKA(env, ESOCK_STR_EAFNOSUPPORT);
    esock_atom_eagain       = MKA(env, ESOCK_STR_EAGAIN);
    esock_atom_einval       = MKA(env, ESOCK_STR_EINVAL);

    atom_lowdelay     = MKA(env, str_lowdelay);
    atom_throughput   = MKA(env, str_throughput);
    atom_reliability  = MKA(env, str_reliability);
    atom_mincost      = MKA(env, str_mincost);

    /* Error codes */
    atom_eisconn      = MKA(env, str_eisconn);
    atom_enotclosing  = MKA(env, str_enotclosing);
    atom_enotconn     = MKA(env, str_enotconn);
    atom_exalloc      = MKA(env, str_exalloc);
    atom_exbadstate   = MKA(env, str_exbadstate);
    atom_exbusy       = MKA(env, str_exbusy);
    atom_exmon        = MKA(env, str_exmon);
    atom_exself       = MKA(env, str_exself);
    atom_exsend       = MKA(env, str_exsend);

    // For storing "global" things...
    // data.env       = enif_alloc_env(); // We should really check
    // data.version   = MKA(env, ERTS_VERSION);
    // data.buildDate = MKA(env, ERTS_BUILD_DATE);

    sockets = enif_open_resource_type_x(env,
                                        "sockets",
                                        &socketInit,
                                        ERL_NIF_RT_CREATE,
                                        NULL);

    return !sockets;
}

ERL_NIF_INIT(socket, socket_funcs, on_load, NULL, NULL, NULL)
