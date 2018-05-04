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


/* All platforms fail on malloc errors. */
#define FATAL_MALLOC



/* *** Boolean *type* stuff... *** */
typedef unsigned int BOOLEAN_T;
#define TRUE  1
#define FALSE 0
#define BOOL2STR(__B__)  ((__B__) ? "true"    : "false")
#define BOOL2ATOM(__B__) ((__B__) ? atom_true : atom_false)

/* Two byte integer decoding */
#define get_int16(s) ((((unsigned char*) (s))[0] << 8)  | \
                      (((unsigned char*) (s))[1]))

#define SASSERT(e) \
  ((void) ((e) ? 1 : (xabort(#e, __func__, __FILE__, __LINE__), 0)))


/* Debug stuff... */
#define SOCKET_NIF_DEBUG_DEFAULT TRUE
#define SOCKET_DEBUG_DEFAULT     TRUE

/* Counters and stuff (Don't know where to sent this stuff anyway) */
#define SOCKET_NIF_IOW_DEFAULT FALSE


/* Used in debug printouts */
#ifdef __WIN32__
#define LLU "%I64u"
#else
#define LLU "%llu"
#endif
typedef unsigned long long llu_t;



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


#define SOCKET_OPT_LEVEL_OTP      0
#define SOCKET_OPT_LEVEL_SOCKET   1
#define SOCKET_OPT_LEVEL_IP       2
#define SOCKET_OPT_LEVEL_IPV6     3
#define SOCKET_OPT_LEVEL_TCP      4
#define SOCKET_OPT_LEVEL_UDP      5
#define SOCKET_OPT_LEVEL_SCTP     6

#define SOCKET_OPT_OTP_DEBUG       0
#define SOCKET_OPT_OTP_IOW         1
#define SOCKET_OPT_SOCK_KEEPALIVE  0
#define SOCKET_OPT_SOCK_LINGER     1
#define SOCKET_OPT_IP_RECVTOS      0
#define SOCKET_OPT_IP_ROUTER_ALERT 1
#define SOCKET_OPT_IP_TOS          2
#define SOCKET_OPT_IP_TTL          3
#define SOCKET_OPT_IPV6_HOPLIMIT   0
#define SOCKET_OPT_TCP_MAXSEG      0


/* =================================================================== *
 *                                                                     *
 *                        Various enif macros                          *
 *                                                                     *
 * =================================================================== */

#define MALLOC(SZ)          enif_alloc((SZ))
#define FREE(P)             enif_free((P))

#define MKA(E,S)            enif_make_atom((E), (S))
#define MKBIN(E,B)          enif_make_binary((E), (B))
#define MKI(E,I)            enif_make_int((E), (I))
#define MKREF(E)            enif_make_ref((E))
#define MKS(E,S)            enif_make_string((E), (S), ERL_NIF_LATIN1)
#define MKSL(E,S,L)         enif_make_string_len((E), (S), (L), ERL_NIF_LATIN1)
#define MKSBIN(E,B,ST,SZ)   enif_make_sub_binary((E), (B), (ST), (SZ))
#define MKT2(E,E1,E2)       enif_make_tuple2((E), (E1), (E2))
#define MKT3(E,E1,E2,E3)    enif_make_tuple3((E), (E1), (E2), (E3))
#define MKT4(E,E1,E2,E3,E4) enif_make_tuple4((E), (E1), (E2), (E3), (E4))
#define MKT8(E,E1,E2,E3,E4,E5,E6,E7,E8) \
    enif_make_tuple8((E), (E1), (E2), (E3), (E4), (E5), (E6), (E7), (E8))

#define MCREATE(N)          enif_mutex_create((N))
#define MDESTROY(M)         enif_mutex_destroy((M))
#define MLOCK(M)            enif_mutex_lock((M))
#define MUNLOCK(M)          enif_mutex_unlock((M))

#define MONP(E,D,P,M)       enif_monitor_process((E), (D), (P), (M))
#define DEMONP(E,D,M)       enif_demonitor_process((E), (D), (M))

#define SELECT(E,FD,M,O,P,R)                            \
    if (enif_select((E), (FD), (M), (O), (P), (R)) < 0) \
        return enif_make_badarg((E));

#define IS_ATOM(E,  TE) enif_is_atom((E),   (TE))
#define IS_BIN(E,   TE) enif_is_binary((E), (TE))
#define IS_NUM(E,   TE) enif_is_number((E), (TE))
#define IS_TUPLE(E, TE) enif_is_tuple((E),  (TE))

#define GET_ATOM_LEN(E, TE, LP) \
    enif_get_atom_length((E), (TE), (LP), ERL_NIF_LATIN1)
#define GET_ATOM(E, TE, BP, MAX) \
    enif_get_atom((E), (TE), (BP), (MAX), ERL_NIF_LATIN1)
#define GET_BIN(E, TE, BP)        enif_inspect_iolist_as_binary((E), (TE), (BP))
#define GET_INT(E, TE, IP)        enif_get_int((E), (TE), (IP))
#define GET_UINT(E, TE, IP)       enif_get_uint((E), (TE), (IP))
#define GET_TUPLE(E, TE, TSZ, TA) enif_get_tuple((E), (TE), (TSZ), (TA))

#define ALLOC_BIN(SZ, BP)         enif_alloc_binary((SZ), (BP))


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


/* The general purpose sockaddr */
typedef union {
    struct sockaddr     sa;
    struct sockaddr_in  sai;

#ifdef HAVE_IN6
    struct sockaddr_in6 sai6;
#endif

#ifdef HAVE_SYS_UN_H
    struct sockaddr_un  sal;
#endif
} SocketAddress;

#define which_address_port(sap)		     \
  ((((sap)->sai.sin_family == AF_INET) ||  \
    ((sap)->sai.sin_family == AF_INET6)) ? \
   ((sap)->sai.sin_port) : -1)


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

typedef struct {
    unsigned int tag;
    union {
        BOOLEAN_T     boolVal;
        int           intVal;
        struct linger lingerVal;
        ErlNifBinary  binVal;
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

    ErlNifMutex* cntMtx;
    BOOLEAN_T    iow;
    uint32_t     numSockets;
    uint32_t     numTypeDGrams;
    uint32_t     numTypeStreams;
    uint32_t     numTypeSeqPkg;
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

/* THIS IS JUST TEMPORARY */
extern char* erl_errno_id(int error);


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
static ERL_NIF_TERM nif_cancel(ErlNifEnv*         env,
                               int                argc,
                               const ERL_NIF_TERM argv[]);


static ERL_NIF_TERM nopen(ErlNifEnv* env,
                          int        domain,
                          int        type,
                          int        protocol,
                          char*      netns);
static ERL_NIF_TERM nbind(ErlNifEnv*        env,
                          SocketDescriptor* descP,
                          ERL_NIF_TERM      addr);
static ERL_NIF_TERM nconnect(ErlNifEnv*          env,
                             SocketDescriptor*   descP,
                             const ERL_NIF_TERM* addr,
                             int                 port);
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
                            int               opt,
                            SocketOptValue*   valP);
static ERL_NIF_TERM nsetopt_otp(ErlNifEnv*        env,
                                SocketDescriptor* descP,
                                int               opt,
                                SocketOptValue*   valP);
static ERL_NIF_TERM nsetopt_gen(ErlNifEnv*        env,
                                SocketDescriptor* descP,
                                int               level,
                                int               opt,
                                SocketOptValue*   valP);

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
static char* decode_laddress_tuple(ErlNifEnv*     env,
                                   int            domain,
                                   ERL_NIF_TERM   laddr,
                                   SocketAddress* localP,
                                   unsigned int*  addrLenP);
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
static char* decode_send_addr(ErlNifEnv*      env,
                              int             domain,
                              ERL_NIF_TERM    addr,
                              int             port,
                              SocketAddress** toAddrP,
                              unsigned int*   addrLenP);
static char* decode_send_addr_tuple(ErlNifEnv*     env,
                                    int            domain,
                                    ERL_NIF_TERM   addr,
                                    int            port,
                                    SocketAddress* toAddrP,
                                    unsigned int*  addrLenP);
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
static BOOLEAN_T decode_bool(ErlNifEnv*   env,
                             ERL_NIF_TERM eVal,
                             BOOLEAN_T*   val);

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
static BOOLEAN_T eoptval2optval(ErlNifEnv*      env,
                                BOOLEAN_T       isEncoded,
                                BOOLEAN_T       isOTP,
                                int             level,
                                int             eOpt,
                                ERL_NIF_TERM    eVal,
                                int*            opt,
                                SocketOptValue* val);
static BOOLEAN_T eoptval2optval_otp(ErlNifEnv*      env,
                                    int             eOpt,
                                    ERL_NIF_TERM    eVal,
                                    int*            opt,
                                    SocketOptValue* valP);
static BOOLEAN_T eoptval2optval_plain(ErlNifEnv*      env,
                                      int             eOpt,
                                      ERL_NIF_TERM    eVal,
                                      int*            opt,
                                      SocketOptValue* valP);
static BOOLEAN_T eoptval2optval_socket(ErlNifEnv*      env,
                                       int             eOpt,
                                       ERL_NIF_TERM    eVal,
                                       int*            opt,
                                       SocketOptValue* valP);
static BOOLEAN_T eoptval2optval_ip(ErlNifEnv*      env,
                                   int             eOpt,
                                   ERL_NIF_TERM    eVal,
                                   int*            opt,
                                   SocketOptValue* valP);
#if defined(SOL_IPV6)
static BOOLEAN_T eoptval2optval_ipv6(ErlNifEnv*      env,
                                     int             eOpt,
                                     ERL_NIF_TERM    eVal,
                                     int*            opt,
                                     SocketOptValue* valP);
#endif
static BOOLEAN_T eoptval2optval_tcp(ErlNifEnv*      env,
                                    int             eOpt,
                                    ERL_NIF_TERM    eVal,
                                    int*            opt,
                                    SocketOptValue* valP);
static BOOLEAN_T eoptval2optval_udp(ErlNifEnv*      env,
                                    int             eOpt,
                                    ERL_NIF_TERM    eVal,
                                    int*            opt,
                                    SocketOptValue* valP);
#ifdef HAVE_SCTP
static BOOLEAN_T eoptval2optval_sctp(ErlNifEnv*      env,
                                     int             eOpt,
                                     ERL_NIF_TERM    eVal,
                                     int*            opt,
                                     SocketOptValue* valP);
#endif
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

static ERL_NIF_TERM make_ok2(ErlNifEnv* env, ERL_NIF_TERM val);
static ERL_NIF_TERM make_ok3(ErlNifEnv* env, ERL_NIF_TERM val1, ERL_NIF_TERM val2);
static ERL_NIF_TERM make_error(ErlNifEnv* env, ERL_NIF_TERM reason);
static ERL_NIF_TERM make_error1(ErlNifEnv* env, char* reason);
static ERL_NIF_TERM make_error2(ErlNifEnv* env, int err);

static char* send_msg_error_closed(ErlNifEnv*   env,
                                   ErlNifPid*   pid);
static char* send_msg_error(ErlNifEnv*   env,
                            ERL_NIF_TERM reason,
                            ErlNifPid*   pid);
static char* send_msg_nif_abort(ErlNifEnv*   env,
                                ERL_NIF_TERM ref,
                                ERL_NIF_TERM reason,
                                ErlNifPid*   pid);
static char* send_msg(ErlNifEnv*   env,
                      ERL_NIF_TERM msg,
                      ErlNifPid*   pid);

static void xabort(const char* expr,
                   const char* func,
                   const char* file,
                   int         line);

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
static char str_close[]       = "close";
static char str_closed[]      = "closed";
static char str_closing[]     = "closing";
static char str_error[]       = "error";
static char str_false[]       = "false";
static char str_nif_abort[]   = "nif_abort";
static char str_ok[]          = "ok";
static char str_select[]      = "select";
static char str_timeout[]     = "timeout";
static char str_true[]        = "true";
static char str_undefined[]   = "undefined";

/* (special) error string constants */
static char str_eagain[]         = "eagain";
static char str_eafnosupport[]   = "eafnosupport";
static char str_einval[]         = "einval";
static char str_eisconn[]        = "eisconn";
static char str_enotclosing[]    = "enotclosing";
static char str_enotconn[]       = "enotconn";
static char str_exalloc[]        = "exalloc";
static char str_exbadstate[]     = "exbadstate";
static char str_exbusy[]         = "exbusy";
static char str_exmon[]          = "exmonitor";  // failed monitor
static char str_exself[]         = "exself";     // failed self
static char str_exsend[]         = "exsend";     // failed send


/* *** Atoms *** */
static ERL_NIF_TERM atom_close;
static ERL_NIF_TERM atom_closed;
static ERL_NIF_TERM atom_closing;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_nif_abort;
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_select;
static ERL_NIF_TERM atom_timeout;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_undefined;

static ERL_NIF_TERM atom_eagain;
static ERL_NIF_TERM atom_eafnosupport;
static ERL_NIF_TERM atom_einval;
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
static SocketData socketData;


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
 * nif_connect(Sock, Addr, Port)
 * nif_listen(Sock, Backlog)
 * nif_accept(LSock, Ref)
 * nif_send(Sock, SendRef, Data, Flags)
 * nif_sendto(Sock, SendRef, Data, Flags, DstAddr, DstPort)
 * nif_recv(Sock, RecvRef, Length, Flags)
 * nif_recvfrom(Sock, Flags)
 * nif_close(Sock)
 * nif_shutdown(Sock, How)
 *
 * And some functions to manipulate and retrieve socket options:
 * -------------------------------------------------------------
 * nif_setopt/3
 * nif_getopt/2
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
static
ERL_NIF_TERM nif_info(ErlNifEnv*         env,
                      int                argc,
                      const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM info = enif_make_new_map(env);
    return info;
}


/* ----------------------------------------------------------------------
 * nif_open
 *
 * Description:
 * Create an endpoint for communication.
 *
 * Arguments:
 * Domain
 * Type
 * Protocol
 * Extra    - A map with obscure options.
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

    /* Extract arguments and perform preliminary validation */

    if ((argc != 4) ||
        !enif_get_int(env, argv[0], &edomain) ||
        !enif_get_int(env, argv[1], &etype) ||
        !enif_get_int(env, argv[2], &eproto) ||
        !enif_is_map(env,  argv[3])) {
        return enif_make_badarg(env);
    }
    emap = argv[3];

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

    return nopen(env, domain, type, proto, netns);
}


/* nopen - create an endpoint for communication
 *
 * Assumes the input has been validated.
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


#ifdef HAVE_SETNS
    if (!change_network_namespace(netns, &current_ns, &save_errno))
        return make_error2(env, save_errno);
#endif

    if ((sock = sock_open(domain, type, protocol)) == INVALID_SOCKET)
        return make_error2(env, sock_errno());

#ifdef HAVE_SETNS
    if (!restore_network_namespace(current_ns, sock, &save_errno))
        return make_error2(env, save_errno);

    if (netns != NULL)
        FREE(netns);
#endif


    if ((event = sock_create_event(sock)) == INVALID_EVENT) {
        save_errno = sock_errno();
        while ((sock_close(sock) == INVALID_SOCKET) && (sock_errno() == EINTR));
        return make_error2(env, save_errno);
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
        return make_error(env, atom_exself);

    if (MONP(env, descP,
             &descP->ctrlPid,
             &descP->ctrlMon) > 0)
        return make_error(env, atom_exmon);


#ifdef __WIN32__
    /* What is the point of this?
     * And how do we handle it?
     * Since the select message will be delivered to the controlling
     * process, which has no idea what to do with this...
     *
     * TODO!
     */
     SELECT(env,
            event,
            (ERL_NIF_SELECT_READ),
            descP, NULL, atom_undefined);
#endif


    return make_ok2(env, res);
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
        return make_error(env, atom_exbadstate);

    return nbind(env, descP, argv[1]);
}


static
ERL_NIF_TERM nbind(ErlNifEnv*        env,
                   SocketDescriptor* descP,
                   ERL_NIF_TERM      addr)
{
    SocketAddress local;
    unsigned int  addrLen;
    char*         err;
    int           port;

    if ((err = decode_laddress(env, descP->domain, addr, &local, &addrLen)) != NULL)
        return make_error1(env, err);

    if (IS_SOCKET_ERROR(sock_bind(descP->sock,
                                  (struct sockaddr*) &local, addrLen))) {
        return make_error2(env, sock_errno());
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

    return make_ok2(env, enif_make_int(env, port));

}


/* Decode the (local) address. The format of the address should
 * either be an binary (domain = local) or a two tuple (first
 * part the actual address tuple and the second part the port
 * number) if the domain is either INET or INET6.
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
        return decode_laddress_tuple(env, domain, localAddr, localP, addrLenP);
    }  else {
        return str_einval;
    }

}


/* Only for domain = local (unix)
 * The erlang interface module ensures that the size of the
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
        return str_einval;

    if (!GET_BIN(env, localAddr, &bin))
        return str_einval;

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
         ) > sizeof(localP->sal.sun_path))
        return str_einval;

    sys_memzero((char*)localP, sizeof(struct sockaddr_un));
    localP->sal.sun_family = domain;
    sys_memcpy(localP->sal.sun_path, bin.data, bin.size);
    addrLen = offsetof(struct sockaddr_un, sun_path) + bin.size;
#ifndef NO_SA_LEN
    localP->u.sal.sun_len = addrLen;
#endif
    *addrLenP = addrLen;
    return NULL;

#else // HAVE_SYS_UN_H
    return str_eafnosupport;
#endif

}


/* Only for domain = INET and INET6
 * The (local) address here is a two tuple:
 *    {Addr, Port}
 * where
 *    Addr:
 *      - a tuple of size 4 (INET) or size 8 (INET6)
 *      - the atoms 'any' or 'loopback'
 *   Port:
 *      - the port number (int)
 *
 */
static
char* decode_laddress_tuple(ErlNifEnv*     env,
                            int            domain,
                            ERL_NIF_TERM   laddr,
                            SocketAddress* localP,
                            unsigned int*  addrLenP)
{
    const ERL_NIF_TERM* laddrt;
    int                 laddrtSz;
    int                 port;

    /* First, get the tuple and verify its size (2) */

    if (!GET_TUPLE(env, laddr, &laddrtSz, &laddrt))
        return str_einval;

    if (laddrtSz != 2)
        return str_einval;

    /* So far so good. The first element is either a tuple or an atom */

    if (IS_TUPLE(env, laddrt[0]) &&
        IS_NUM(env, laddrt[1])) {

        /* We handle two different tuples:
         *    - size 4 (INET)
         *    - size 8 (INET6)
         */

        const ERL_NIF_TERM* addrt;
        int                 addrtSz;

        if (!GET_TUPLE(env, laddrt[0], &addrtSz, &addrt))
            return str_einval; // PLACEHOLDER

        if (!GET_INT(env, laddrt[1], &port))
            return str_einval; // PLACEHOLDER

        switch (domain) {
        case AF_INET:
            if (addrtSz != 4)
                return str_einval;
            break;

#if defined(HAVE_IN6) && defined(AF_INET6)
        case AF_INET6:
            if (addrtSz != 8)
                return str_einval;
            break;
#endif

        default:
            return str_eafnosupport;
            break;
        }

        return decode_address_tuple(env,
                                    domain, addrt, port,
                                    localP, addrLenP);

    } else if (IS_ATOM(env, laddrt[0]) &&
               IS_NUM(env, laddrt[1])) {

        /* There are only two atoms we handle:
         *    - any
         *    - loopback
         */

        unsigned int len;
        char         a[16]; // Just in case...

        if (!(GET_ATOM_LEN(env, laddrt[0], &len) &&
              (len > 0) &&
              (len <= (sizeof("loopback")))))
            return str_einval;

        if (!GET_ATOM(env, laddrt[0], a, sizeof(a)))
            return str_einval;

        return decode_address_atom(env,
                                   domain, a, len, port,
                                   localP, addrLenP);

    } else {
        return str_einval;
    }

}



/* ----------------------------------------------------------------------
 * nif_connect
 *
 * Description:
 * Initiate a connection on a socket
 *
 * Arguments:
 * Socket (ref) - Points to the socket descriptor.
 * Addr         - Address of "remote" host.
 *                A tuple of size 4 or 8 (depending on domain)
 * Port         - Port number of "remote" host.
 */
static
ERL_NIF_TERM nif_connect(ErlNifEnv*         env,
                         int                argc,
                         const ERL_NIF_TERM argv[])
{
    SocketDescriptor*   descP;
    int                 addrSz;
    const ERL_NIF_TERM* addr;
    int                 port;

    /* Extract arguments and perform preliminary validation */

    if ((argc != 3) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP) ||
        !GET_TUPLE(env, argv[1], &addrSz, &addr) ||
        !GET_INT(env, argv[2], &port)) {
        return enif_make_badarg(env);
    }

    switch (descP->domain) {
    case AF_INET:
        if (addrSz != 4)
            return make_error(env, atom_einval);
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        if (addrSz != 8)
            return make_error(env, atom_einval);
        break;
#endif

    default:
        return make_error(env, atom_eafnosupport);
        break;
    } // switch (descP->domain)

    return nconnect(env, descP, addr, port);
}


static
ERL_NIF_TERM nconnect(ErlNifEnv*          env,
                      SocketDescriptor*   descP,
                      const ERL_NIF_TERM* addr,
                      int                 port)
{
    unsigned int addrLen;
    int          code;
    char*        xerr;

    /* Verify that we are where in the proper state */

    if (!IS_OPEN(descP))
        return make_error(env, atom_exbadstate);

    if (IS_CONNECTED(descP))
        return make_error(env, atom_eisconn);

    if (IS_CONNECTING(descP))
        return make_error(env, atom_einval);

    if ((xerr = decode_address_tuple(env,
                                     descP->domain, addr, port,
                                     &descP->remote, &addrLen)) != NULL)
        return make_error1(env, xerr);

    code = sock_connect(descP->sock,
                        (struct sockaddr*) &descP->remote, addrLen);

    if (IS_SOCKET_ERROR(code) &&
        ((sock_errno() == ERRNO_BLOCK) ||   /* Winsock2            */
         (sock_errno() == EINPROGRESS))) {  /* Unix & OSE!!        */
        ERL_NIF_TERM ref = MKREF(env);
        descP->state = SOCKET_STATE_CONNECTING;
        SELECT(env,
               descP->sock,
               (ERL_NIF_SELECT_WRITE),
               descP, NULL, ref);
        return make_ok2(env, ref);
    } else if (code == 0) {                 /* ok we are connected */
        descP->state = SOCKET_STATE_CONNECTED;
        /* Do we need to do somthing for "active" mode?
         * Is there even such a thing *here*?
         */
        return atom_ok;
    } else {
        return make_error2(env, sock_errno());
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
        return make_error(env, atom_enotconn);

    if (!verify_is_connected(descP, &error)) {
        descP->state = SOCKET_STATE_OPEN;  /* restore state */
        return make_error2(env, error);
    }

    descP->state = SOCKET_STATE_CONNECTED;

    return atom_ok;
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
        return make_error(env, atom_exbadstate);

    if (!IS_OPEN(descP))
        return make_error(env, atom_exbadstate);

    if (IS_SOCKET_ERROR(sock_listen(descP->sock, backlog)))
        return make_error2(env, sock_errno());

    descP->state = SOCKET_STATE_LISTENING;

    return atom_ok;
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
        res = make_error(env, atom_einval);
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
        return make_error(env, atom_exself);

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
                return make_error(env, atom_exmon);

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

            return make_error(env, atom_eagain);

        } else {
            return make_error2(env, save_errno);
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
            return make_error2(env, save_errno);
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
            return make_error(env, atom_exmon);
        }

        accDescP->remote = remote;
        SET_NONBLOCKING(accDescP->sock);

#ifdef __WIN32__
        /* See 'What is the point of this?' above */
        SELECT(env,
               descP->sock,
               (ERL_NIF_SELECT_READ),
               descP, NULL, atom_undefined);
#endif

        accDescP->state = SOCKET_STATE_CONNECTED;

        return make_ok2(env, accRef);
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
        return make_error(env, atom_exself);

    if (compare_pids(env, &descP->currentAcceptor.pid, &caller) != 0) {
        /* This will have to do until we implement the queue.
         * When we have the queue, we should simply push this request,
         * and instead return with eagain (the caller will then wait
         * for the select message).
         */
        return make_error(env, atom_exbusy);
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

            return make_error(env, atom_eagain);
        } else {
            return make_error2(env, save_errno);
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
            return make_error2(env, save_errno);
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
            return make_error(env, atom_exmon);
        }

        accDescP->remote  = remote;
        SET_NONBLOCKING(accDescP->sock);

#ifdef __WIN32__
        /* See 'What is the point of this?' above */
        SELECT(env,
               descP->sock,
               (ERL_NIF_SELECT_READ),
               descP, NULL, atom_undefined);
#endif

        accDescP->state = SOCKET_STATE_CONNECTED;


        /* Here we should have the test if we have something in the queue.
         * And if so, pop it and copy the (waiting) acceptor, and then
         * make a new select with that info).
         */
        descP->state = SOCKET_STATE_LISTENING;

        return make_ok2(env, accRef);
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
        return make_error(env, atom_enotconn);

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
 * DestAddr     - Destination address.
 * DestPort     - Destination Port.
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
    ERL_NIF_TERM      addr;
    int               port;
    SocketAddress     remoteAddr;
    SocketAddress*    remoteAddrP = &remoteAddr;
    unsigned int      remoteAddrLen;
    char*             xerr;
    // ERL_NIF_TERM      res;

    /* Extract arguments and perform preliminary validation */

    if ((argc != 6) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP) ||
        !GET_BIN(env, argv[2], &data) ||
        !GET_UINT(env, argv[3], &eflags) ||
        !GET_INT(env, argv[5], &port)) {
        return enif_make_badarg(env);
    }
    sendRef = argv[1];
    addr    = argv[4];

    /* THIS TEST IS NOT CORRECT!!! */
    if (!IS_OPEN(descP))
        return make_error(env, atom_einval);

    if (!esendflags2sendflags(eflags, &flags))
        return enif_make_badarg(env);

    if ((xerr = decode_send_addr(env, descP->domain,
                                 addr, port,
                                 &remoteAddrP,
                                 &remoteAddrLen)) != NULL)
        return make_error1(env, xerr);

    return nsendto(env, descP, sendRef, &data, flags, remoteAddrP, remoteAddrLen);
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
        return make_error(env, atom_enotconn);

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
        return make_error(env, atom_exalloc);

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
    /*     return make_error(env, atom_enotconn); */

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
        return make_error(env, atom_exalloc);

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
            return make_error(env, atom_exself);
        }

        /* Monitor the caller, since we should complete this operation even if
         * the caller dies (for whatever reason).
         */

        if (MONP(env, descP,
             &descP->closerPid,
                 &descP->closerMon) > 0) {
            MUNLOCK(descP->closeMtx);
            return make_error(env, atom_exmon);
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
            reply = atom_ok;
        } else if (selectRes & ERL_NIF_SELECT_STOP_SCHEDULED) {
            /* The stop callback function has been *scheduled* which means that we
             * have to wait for it to complete. */
            reply = make_ok2(env, descP->closeRef);
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
            reply  = make_error(env, reason);
        }
    } else {
        reply = make_error(env, reason);
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
        return atom_ok;

    if (descP->state != SOCKET_STATE_CLOSING)
        return make_error(env, atom_enotclosing);

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
            reply = make_error(env, atom_timeout);
        } else {
            reply = make_error2(env, save_errno);
        }
    } else {
        reply = atom_ok;
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
        reply = atom_ok;
    } else {
        reply = make_error2(env, sock_errno());
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
    unsigned int      eIsEncoded;
    BOOLEAN_T         isEncoded, isOTP;
    int               eLevel, level = -1;
    int               eOpt, opt = -1;
    ERL_NIF_TERM      eVal;
    SocketOptValue    val;

    if ((argc != 5) ||
        !enif_get_resource(env, argv[0], sockets, (void**) &descP) ||
        !GET_UINT(env, argv[1], &eIsEncoded) ||
        !GET_INT(env, argv[2], &eLevel) ||
        !GET_INT(env, argv[3], &eOpt)) {
        return enif_make_badarg(env);
    }
    eVal = argv[4];

    isEncoded = ((eIsEncoded == 0) ? FALSE : TRUE);

    if (!elevel2level(isEncoded, eLevel, &isOTP, &level))
        return make_error(env, atom_einval);

    if (!eoptval2optval(env, isEncoded, isOTP, level, eOpt, eVal, &opt, &val))
        return make_error(env, atom_einval);

    return nsetopt(env, descP, isEncoded, isOTP, level, opt, &val);
}


static
ERL_NIF_TERM nsetopt(ErlNifEnv*        env,
                     SocketDescriptor* descP,
                     BOOLEAN_T         isEncoded,
                     BOOLEAN_T         isOTP,
                     int               level,
                     int               opt,
                     SocketOptValue*   valP)
{
    ERL_NIF_TERM result;
    int          res;

    if (!isEncoded) {
        res = socket_setopt(descP->sock, level, opt,
                            valP->u.binVal.data, valP->u.binVal.size);
        if (res != 0)
            result = make_error2(env, res);
        else
            result = atom_ok;
    } else {
        if (isOTP) {
            /* These are not actual socket options,
             * but options for our implementation.
             */
            result = nsetopt_otp(env, descP, opt, valP);
        } else {
            /* Basically, call setsockopt(...)
             * <KOLLA>
             * How do we know what type each option have? tag in value type?
             * </KOLLA>
             */
            result = nsetopt_gen(env, descP, level, opt, valP);
        }
    }

    return result;
}


static
ERL_NIF_TERM nsetopt_otp(ErlNifEnv*        env,
                         SocketDescriptor* descP,
                         int               opt,
                         SocketOptValue*   valP)
{
    ERL_NIF_TERM result;

    /* Make an idiot check just to be on the safe side... */
    if (valP->tag == SOCKET_OPT_VALUE_UNDEF)
        return make_error(env, atom_einval);

    switch (opt) {
    case SOCKET_OPT_OTP_DEBUG:
        descP->dbg = valP->u.boolVal;
        result = atom_ok;
        break;

    case SOCKET_OPT_OTP_IOW:
        descP->iow = valP->u.boolVal;
        result = atom_ok;
        break;

    default:
        result = make_error(env, atom_einval);
        break;
    }

    return result;
}


static
ERL_NIF_TERM nsetopt_gen(ErlNifEnv*        env,
                         SocketDescriptor* descP,
                         int               level,
                         int               opt,
                         SocketOptValue*   valP)
{
    socklen_t    optLen;
    int          res;
    ERL_NIF_TERM result;

    switch (valP->tag) {
    case SOCKET_OPT_VALUE_INT:
        {
            optLen = sizeof(valP->u.intVal);
            res    = socket_setopt(descP->sock, level, opt,
                                   (void*) &valP->u.intVal, optLen);
            if (res != 0)
                result = make_error2(env, res);
            else
                result = atom_ok;
        }
        break;

    case SOCKET_OPT_VALUE_BIN:
        {
            optLen = valP->u.binVal.size;
            res    = socket_setopt(descP->sock, level, opt,
                                   &valP->u.binVal.data, optLen);
            if (res != 0)
                result = make_error2(env, res);
            else
                result = atom_ok;
        }
        break;

    case SOCKET_OPT_VALUE_LINGER:
        {
            optLen = sizeof(valP->u.lingerVal);
            res    = socket_setopt(descP->sock, level, opt,
                                   &valP->u.lingerVal, optLen);
            if (res != 0)
                result = make_error2(env, res);
            else
                result = atom_ok;
        }
        break;

    default:
        result = make_error(env, atom_einval);
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


static
BOOLEAN_T eoptval2optval(ErlNifEnv*      env,
                         BOOLEAN_T       isEncoded,
                         BOOLEAN_T       isOTP,
                         int             level,
                         int             eOpt,
                         ERL_NIF_TERM    eVal,
                         int*            opt,
                         SocketOptValue* valP)
{
    if (isOTP) {
        return eoptval2optval_otp(env, eOpt, eVal, opt, valP);
    } else if (!isEncoded) {
        return eoptval2optval_plain(env, eOpt, eVal, opt, valP);
    } else {
        switch (level) {
        case SOL_SOCKET:
            return eoptval2optval_socket(env, eOpt, eVal, opt, valP);
            break;

#if defined(SOL_IP)
        case SOL_IP:
#else
        case IPPROTO_IP:
#endif
            return eoptval2optval_ip(env, eOpt, eVal, opt, valP);
            break;

#if defined(SOL_IPV6)
        case SOL_IPV6:
            return eoptval2optval_ipv6(env, eOpt, eVal, opt, valP);
            break;
#endif

        case IPPROTO_TCP:
            return eoptval2optval_tcp(env, eOpt, eVal, opt, valP);
            break;

        case IPPROTO_UDP:
            return eoptval2optval_udp(env, eOpt, eVal, opt, valP);
            break;

#ifdef HAVE_SCTP
        case IPPROTO_SCTP:
            return eoptval2optval_sctp(env, eOpt, eVal, opt, valP);
            break;
#endif

        default:
            *opt = -1;
            return FALSE;
        }
    }
}



static
BOOLEAN_T eoptval2optval_otp(ErlNifEnv*      env,
                             int             eOpt,
                             ERL_NIF_TERM    eVal,
                             int*            opt,
                             SocketOptValue* valP)
{
    BOOLEAN_T result = FALSE;

    switch (eOpt) {
        case SOCKET_OPT_OTP_IOW:
        case SOCKET_OPT_OTP_DEBUG:
            {
                if (decode_bool(env, eVal, &valP->u.boolVal)) {
                    valP->tag = SOCKET_OPT_VALUE_BOOL;
                    result = TRUE;
                } else {
                    result = FALSE;
                }
                *opt = eOpt;
            }
            break;

    default:
        *opt = -1;
        valP->tag = SOCKET_OPT_VALUE_UNDEF;
    }

    return result;
}


static
BOOLEAN_T eoptval2optval_plain(ErlNifEnv*      env,
                               int             eOpt,
                               ERL_NIF_TERM    eVal,
                               int*            opt,
                               SocketOptValue* valP)
{
    if (!GET_BIN(env, eVal, &valP->u.binVal))
        return FALSE;
    valP->tag = SOCKET_OPT_VALUE_BIN;
    *opt      = eOpt;

    return TRUE;
}



static
BOOLEAN_T eoptval2optval_socket(ErlNifEnv*      env,
                                int             eOpt,
                                ERL_NIF_TERM    eVal,
                                int*            opt,
                                SocketOptValue* valP)
{
    switch (eOpt) {
#if defined(SO_KEEPALIVE)
        case SOCKET_OPT_SOCK_KEEPALIVE:
            {
                BOOLEAN_T val;

                if (decode_bool(env, eVal, &val)) {
                    *opt           = SO_KEEPALIVE;
                    valP->tag      = SOCKET_OPT_VALUE_INT;
                    valP->u.intVal = (val) ? 1 : 0;
                    return TRUE;
                } else {
                    *opt      = -1;
                    valP->tag = SOCKET_OPT_VALUE_UNDEF;
                    return FALSE;
                }
            }
            break;
#endif

#if defined(SO_LINGER)
    case SOCKET_OPT_SOCK_LINGER:
        {
            if (decode_sock_linger(env, eVal, &valP->u.lingerVal)) {
                *opt      = SO_LINGER;
                valP->tag = SOCKET_OPT_VALUE_LINGER;
                return TRUE;
            } else {
                *opt      = -1;
                valP->tag = SOCKET_OPT_VALUE_UNDEF;
                return FALSE;
            }
        }
        break;
#endif

    default:
        *opt      = -1;
        valP->tag = SOCKET_OPT_VALUE_UNDEF;
        return FALSE;
    }
}



static
BOOLEAN_T eoptval2optval_ip(ErlNifEnv*      env,
                            int             eOpt,
                            ERL_NIF_TERM    eVal,
                            int*            opt,
                            SocketOptValue* valP)
{
    switch (eOpt) {
#if defined(IP_RECVTOS)
    case SOCKET_OPT_IP_RECVTOS:
        {
            BOOLEAN_T val;

            if (decode_bool(env, eVal, &val)) {
                *opt           = IP_RECVTOS;
                valP->tag      = SOCKET_OPT_VALUE_INT;
                valP->u.intVal = (val) ? 1 : 0;
                return TRUE;
            } else {
                *opt      = -1;
                valP->tag = SOCKET_OPT_VALUE_UNDEF;
                return TRUE;
            }
        }
        break;
#endif

#if defined(IP_ROUTER_ALERT)
    case SOCKET_OPT_IP_ROUTER_ALERT:
        if (GET_INT(env, eVal, &valP->u.intVal)) {
            valP->tag = SOCKET_OPT_VALUE_INT;
            *opt      = IP_ROUTER_ALERT;
            return TRUE;
        } else {
            *opt      = -1;
            valP->tag = SOCKET_OPT_VALUE_UNDEF;
            return FALSE;
        }
        break;
#endif

#if defined(IP_TOS)
    case SOCKET_OPT_IP_TOS:
        {
            if (decode_ip_tos(env, eVal, &valP->u.intVal)) {
                valP->tag = SOCKET_OPT_VALUE_INT;
                *opt      = IP_TOS;
                return TRUE;
            } else {
                *opt      = -1;
                return FALSE;
            }
        }
        break;
#endif

#if defined(IP_TTL)
    case SOCKET_OPT_IP_TTL:
        /* <KOLLA>
         * Should we care about the value? That is, if it is valid?
         * And what is the valid range anyway for ttl? 0 - 255?
         * </KOLLA>
         */
        if (!GET_INT(env, eVal, &valP->u.intVal))
            return FALSE; // PLACEHOLDER - We should really be more informative
        valP->tag = SOCKET_OPT_VALUE_INT;
        *opt      = IP_TTL;
        return TRUE;
        break;
#endif

    default:
        *opt      = -1;
        valP->tag = SOCKET_OPT_VALUE_UNDEF;
        return FALSE;
    }

}



#if defined(SOL_IPV6)
static
BOOLEAN_T eoptval2optval_ipv6(ErlNifEnv*      env,
                              int             eOpt,
                              ERL_NIF_TERM    eVal,
                              int*            opt,
                              SocketOptValue* valP)
{
    BOOLEAN_T result = FALSE;

    switch (eOpt) {
#if defined(IPV6_HOPLIMIT)
    case SOCKET_OPT_IPV6_HOPLIMIT:
        {
            BOOLEAN_T val;

            if (decode_bool(env, eVal, &val)) {
                valP->tag = SOCKET_OPT_VALUE_INT;
                valP->u.intVal = (val) ? 1 : 0;
                *opt   = IPV6_HOPLIMIT;
                result = TRUE;
            } else {
                *opt   = -1;
                result = FALSE;
            }
        }
        break;
#endif

    default:
        *opt      = -1;
        valP->tag = SOCKET_OPT_VALUE_UNDEF;
        result    = FALSE;
        break;
    }

    return result;
}
#endif



static
BOOLEAN_T eoptval2optval_tcp(ErlNifEnv*      env,
                             int             eOpt,
                             ERL_NIF_TERM    eVal,
                             int*            opt,
                             SocketOptValue* valP)
{
    switch (eOpt) {
#if defined(TCP_MAXSEG)
    case SOCKET_OPT_TCP_MAXSEG:
        if (!GET_INT(env, eVal, &valP->u.intVal)) {
            valP->tag = SOCKET_OPT_VALUE_INT;
            *opt      = TCP_MAXSEG;
            return TRUE;
        } else {
            return FALSE;
        }
        break;
#endif

    default:
        *opt      = -1;
        valP->tag = SOCKET_OPT_VALUE_UNDEF;
        return FALSE;
    }
}



/* +++ decode UDP socket options +++
 * Currently there are no such options, so this function
 * is just a placeholder!
 */
static
BOOLEAN_T eoptval2optval_udp(ErlNifEnv*      env,
                             int             eOpt,
                             ERL_NIF_TERM    eVal,
                             int*            opt,
                             SocketOptValue* valP)
{
    switch (eOpt) {
    default:
        *opt      = -1;
        valP->tag = SOCKET_OPT_VALUE_UNDEF;
        return FALSE;
    }
}



#ifdef HAVE_SCTP
static
BOOLEAN_T eoptval2optval_sctp(ErlNifEnv*      env,
                              int             eOpt,
                              ERL_NIF_TERM    eVal,
                              int*            opt,
                              SocketOptValue* valP)
{
    switch (eOpt) {
#if defined(SCTP_AUTOCLOSE)
    case SOCKET_OPT_SCTP_AUTOCLOSE:
        if (!GET_INT(env, eVal, &valP->u.intVal))
            return FALSE; // PLACEHOLDER - We should really be more informative
        valP->tag = SOCKET_OPT_VALUE_INT;
        *opt      = SCTP_AUTOCLOSE;
        return TRUE;
        break;
#endif

    default:
        *opt      = -1;
        valP->tag = SOCKET_OPT_VALUE_UNDEF;
        return FALSE;
    }
}
#endif



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
    int       tmpIValPRIO;
    int       tmpIValTOS;
    int       resPRIO;
    int       resTOS;
    SOCKLEN_T tmpArgSzPRIO = sizeof(tmpIValPRIO);
    SOCKLEN_T tmpArgSzTOS  = sizeof(tmpIValTOS);

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

        return atom_ok;

    } else if (written < 0) {

        /* Ouch, check what kind of failure */
        int save_errno = sock_errno();
        if ((save_errno != EAGAIN) &&
            (save_errno != EINTR)) {

            cnt_inc(&descP->writeFails, 1);

            return make_error2(env, save_errno);

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

    return make_ok2(env, enif_make_int(env, written));

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

            return make_ok3(env, atom_false, data);

        } else {

            /* +++ We got exactly as much as we requested +++ */

            /* <KOLLA>
             * WE NEED TO INFORM ANY WAITING READERS
             * </KOLLA>
             */

            data = MKBIN(env, bufP);

            return make_ok3(env, atom_true, data);

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

            return make_error(env, atom_closed);

        } else if ((save_errno == ERRNO_BLOCK) ||
                   (save_errno == EAGAIN)) {
            return make_error(env, atom_eagain);
        } else {
            return make_error2(env, save_errno);
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

            return make_ok3(env, atom_true, data);

        } else {

            /* +++ We got only a part of what was expected +++
             * +++ => receive more later.                  +++ */

            return make_ok3(env, atom_false, MKBIN(env, bufP));
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

            return make_error(env, atom_closed);

        } else if ((save_errno == ERRNO_BLOCK) ||
                   (save_errno == EAGAIN)) {
            return make_error(env, atom_eagain);
        } else {
            return make_error2(env, save_errno);
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

        return make_ok2(env, MKT3(env, fromDomainT, fromSourceT, data));

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

        /* The only acceptable value is the atom 'null' */

        if (!(GET_ATOM_LEN(env, addr, &len) &&
              (len > 0) &&
              (len <= (sizeof("null")))))
            return str_einval;

        if (!GET_ATOM(env, addr, a, sizeof(a)))
            return str_einval;

        *toAddrP = NULL;
        if (strncmp(a, "null", len) == 0)
            return NULL;
        else
            return str_einval;

    } else if (IS_TUPLE(env, addr)) {
        /* We now know that the we have a proper address. */
        return decode_send_addr_tuple(env, domain, addr, port,
                                      *toAddrP, toAddrLenP);
    } else {
        return str_einval;
    }
}


static
char* decode_send_addr_tuple(ErlNifEnv*     env,
                             int            domain,
                             ERL_NIF_TERM   addr,
                             int            port,
                             SocketAddress* toAddrP,
                             unsigned int*  toAddrLenP)
{
    /* We handle two different tuples:
     *    - size 4 (INET)
     *    - size 8 (INET6)
     */

    const ERL_NIF_TERM* addrt;
    int                 addrtSz;

    if (!GET_TUPLE(env, addr, &addrtSz, &addrt))
        return str_einval; // PLACEHOLDER

    switch (domain) {
    case AF_INET:
        if (addrtSz != 4)
            return str_einval;
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        if (addrtSz != 8)
            return str_einval;
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


/* Decode the 4- or 8-element address tuple
 * and initiate the socket address structure.
 */
static
char* decode_address_tuple(ErlNifEnv*          env,
                           int                 domain,
                           const ERL_NIF_TERM* addrt,
                           int                 port,
                           SocketAddress*      addrP,
                           unsigned int*       addrLenP)
{

    /* We now *know* that the size of the tuple is correct,
     * so we don't need to check anything here, just unpack.
     */

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
                    return str_einval;
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
            /* The address tuple is of size 8
             * and each element is a two byte integer
             */
            for (a = 0; a < 8; a++) {
                if (!GET_INT(env, addrt[a], &v))
                    return str_einval;
                laddr[a*2  ] = ((v >> 8) & 0xFF);
                laddr[a*2+1] = (v & 0xFF);
            }
            sys_memcpy(&addrP->sai6.sin6_addr, &laddr, sizeof(laddr));
            *addrLenP = sizeof(struct sockaddr_in6);
            return NULL;
        }
        break;
#endif

    } /* switch (domain) */

    return str_eafnosupport;

}

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
                    SocketAddress* addrP,
                    unsigned int   addrLen,
                    ERL_NIF_TERM*  domainT,
                    ERL_NIF_TERM*  sourceT)
{
    short port;

    switch (addrP->sa.sa_family) {

        /* +++ inet (IPv4) +++ */

    case AF_INET:
        if (addrLen >= sizeof(struct sockaddr_in)) {
            ERL_NIF_TERM addrT, portT;
            unsigned int i;
            ERL_NIF_TERM at4[4];
            char*        a4 = (char*) &addrP->sai.sin_addr;

            port = sock_ntohs(addrP->sai.sin_port);
            for (i = 0; i < 4; i++) {
                at4[i] = MKI(env, a4[i]);
            }

            *domainT = MKA(env, "inet"); // Shall we encode these? See decode
            addrT    = MKT4(env, at4[0], at4[1], at4[2], at4[3]);
            portT    = MKI(env, port);
            *sourceT = MKT2(env, addrT, portT);
        } else {
            *domainT = atom_undefined;
            *sourceT = atom_undefined;
        }
        break;


        /* +++ inet6 (IPv6) +++ */

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        if (addrLen >= sizeof(struct sockaddr_in6)) {
            ERL_NIF_TERM addrT, portT;
            unsigned int i;
            ERL_NIF_TERM at6[8];
            char*        a16 = (char*) &addrP->sai6.sin6_addr;

            port = sock_ntohs(addrP->sai6.sin6_port);
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
            *domainT = atom_undefined;
            *sourceT = atom_undefined;
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
                *sourceT = atom_undefined;
            } else {
                n = addrLen - offsetof(struct sockaddr_un, sun_path);
                if (255 < n) {
                    *sourceT = atom_undefined;
                } else {
                    m = my_strnlen(addrP->sal.sun_path, n);
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

                    *sourceT = MKSL(env, addrP->sal.sun_path, m);
                }
            }
        }
        break;
#endif

    default:
        *domainT = atom_undefined;
        *sourceT = atom_undefined;
        break;

    } /* switch (addrP->sa.sa_family) */

}


/* Decode the address when its an atom.
 * Currently we only accept two atoms: 'any' and 'loopback'
 */
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
        return str_einval;
    }

    /* If we get this far, we *know* its either 'any' or 'loopback' */

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
        return str_einval;
        break;
    }

    return NULL;
}


static
BOOLEAN_T decode_bool(ErlNifEnv* env, ERL_NIF_TERM eVal, BOOLEAN_T* val)
{
     unsigned int len;
     char         b[16]; // Just in case...

     /* Verify that the value is actually an atom */
     if (!IS_ATOM(env, eVal))
         return FALSE;

     /* Verify that the value is of acceptable length */
     if (!(GET_ATOM_LEN(env, eVal, &len) &&
           (len > 0) &&
           (len <= sizeof("false"))))
         return FALSE;

     /* And finally try to extract the value */
     if (!GET_ATOM(env, eVal, b, sizeof(b)))
         return FALSE;

     if (strncmp(b, "true", len) == 0)
         *val = TRUE;
     else
         *val = FALSE;

     return TRUE;
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

    if (!decode_bool(env, lt[0], &onOff))
        return FALSE;

    if (!GET_INT(env, lt[1], &secs))
        return FALSE;

    valP->l_onoff  = (onOff) ? 1 : 0;
    valP->l_linger = secs;

    return TRUE;
}



/* +++ decocde the ip socket option tos +++
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
        char         b[sizeof("reliability")+1]; // Just in case...

        if (!(GET_ATOM_LEN(env, eVal, &len) &&
              (len > 0) &&
              (len <= (sizeof("reliability"))))) {
            *val = -1;
            return FALSE;
        }

        if (!GET_ATOM(env, eVal, b, sizeof(b))) {
            *val = -1;
            return FALSE;
        }

        if (strncmp(b, "lowdelay", len) == 0) {
            *val = IPTOS_LOWDELAY;
            result = TRUE;
        } else if (strncmp(b, "throughput", len) == 0) {
            *val = IPTOS_THROUGHPUT;
            result = TRUE;
        } else if (strncmp(b, "reliability", len) == 0) {
            *val = IPTOS_RELIABILITY;
            result = TRUE;
        } else if (strncmp(b, "mincost", len) == 0) {
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
    if (!enif_get_map_value(env, map, key, &value)) {
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


/* Create an ok two (2) tuple in the form: {ok, Any}.
 * The second element (Any) is already in the form of an
 * ERL_NIF_TERM so all we have to do is create the tuple.
 */
static
ERL_NIF_TERM make_ok2(ErlNifEnv* env, ERL_NIF_TERM any)
{
    return MKT2(env, atom_ok, any);
}


/* Create an ok three (3) tuple in the form: {ok, Val1, Val2}.
 * The second (Val1) and third (Val2) elements are already in
 * the form of an ERL_NIF_TERM so all we have to do is create
 * the tuple.
 */
static
ERL_NIF_TERM make_ok3(ErlNifEnv* env, ERL_NIF_TERM val1, ERL_NIF_TERM val2)
{
  return MKT3(env, atom_ok, val1, val2);
}


/* Create an error two (2) tuple in the form: {error, Reason}.
 * The second element (Reason) is already in the form of an
 * ERL_NIF_TERM so all we have to do is create the tuple.
 */
static
ERL_NIF_TERM make_error(ErlNifEnv* env, ERL_NIF_TERM reason)
{
    return MKT2(env, atom_error, reason);
}


/* Create an error two (2) tuple in the form: {error, Reason}.
 * The second element, Reason, is a string to be converted into
 * an atom.
 */
static
ERL_NIF_TERM make_error1(ErlNifEnv* env, char* reason)
{
    return make_error(env, MKA(env, reason));
}


/* Create an error two (2) tuple in the form: {error, Reason}.
 * The second element, Reason, is the errno value in its
 * basic form (integer) which will (eventually) be converted
 * into an atom.
 */
static
ERL_NIF_TERM make_error2(ErlNifEnv* env, int err)
{
    return make_error1(env, erl_errno_id(err));
}


/* Send an error closed message to the specified process:
 *
 * This message is for processes that are waiting in the
 * erlang API functions for a select message.
 */
static
char* send_msg_error_closed(ErlNifEnv* env,
                            ErlNifPid* pid)
{
  return send_msg_error(env, atom_closed, pid);
}


/* Send an error message to the specified process:
 * A message in the form:
 *
 *     {error, Reason}
 *
 * This message is for processes that are waiting in the
 * erlang API functions for a select message.
 */
static
char* send_msg_error(ErlNifEnv*   env,
                     ERL_NIF_TERM reason,
                     ErlNifPid*   pid)
{
    ERL_NIF_TERM msg = enif_make_tuple2(env, atom_error, reason);

    return send_msg(env, msg, pid);
}


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


static
void xabort(const char* expr,
	    const char* func,
	    const char* file,
	    int         line)
{
  fflush(stdout);
  fprintf(stderr, "%s:%d:%s() Assertion failed: %s\n",
	  file, line, func, expr);
  fflush(stderr);
  abort();
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

      SASSERT( (NULL == send_msg_nif_abort(env,
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

      SASSERT( (NULL == send_msg_nif_abort(env,
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

      SASSERT( (NULL == send_msg_nif_abort(env,
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

        SASSERT( (NULL == send_msg_nif_abort(env,
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
    {"nif_open",                4, nif_open, 0},
    {"nif_bind",                3, nif_bind, 0},
    {"nif_connect",             3, nif_connect, 0},
    {"nif_listen",              2, nif_listen, 0},
    {"nif_accept",              2, nif_accept, 0},
    {"nif_send",                4, nif_send, 0},
    {"nif_sendto",              6, nif_sendto, 0},
    {"nif_recv",                4, nif_recv, 0},
    {"nif_recvfrom",            2, nif_recvfrom, 0},
    {"nif_close",               1, nif_close, 0},
    {"nif_shutdown",            2, nif_shutdown, 0},
    {"nif_setopt",              3, nif_setopt, 0},
    {"nif_getopt",              2, nif_getopt, 0},

    /* "Extra" functions to "complete" the socket interface.
     * For instance, the function nif_finalize_connection
     * is called after the connect *select* has "completed".
     */
    {"nif_finalize_connection", 1, nif_finalize_connection, 0},
    {"nif_cancel",              2, nif_cancel, 0},
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

    if (!enif_get_map_value(env, map, key, val))
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
    socketData.dbg = extract_debug_on_load(env, load_info,
                                           SOCKET_NIF_DEBUG_DEFAULT);

    /* +++ Global Counters +++ */
    socketData.cntMtx         = MCREATE("socket[gcnt]");
    socketData.iow            = extract_iow_on_load(env,
                                                    load_info,
                                                    SOCKET_NIF_IOW_DEFAULT);
    socketData.numSockets     = 0;
    socketData.numTypeDGrams  = 0;
    socketData.numTypeStreams = 0;
    socketData.numTypeSeqPkg  = 0;
    socketData.numDomainLocal = 0;
    socketData.numDomainInet  = 0;
    socketData.numDomainInet6 = 0;
    socketData.numProtoIP     = 0;
    socketData.numProtoTCP    = 0;
    socketData.numProtoUDP    = 0;
    socketData.numProtoSCTP   = 0;

    /* +++ Misc atoms +++ */
    // atom_active       = MKA(env, str_active);
    // atom_active_n     = MKA(env, str_active_n);
    // atom_active_once  = MKA(env, str_active_once);
    // atom_binary       = MKA(env, str_binary);
    // atom_buildDate    = MKA(env, str_buildDate);
    atom_close        = MKA(env, str_close);
    atom_closed       = MKA(env, str_closed);
    atom_closing      = MKA(env, str_closing);
    atom_error        = MKA(env, str_error);
    atom_false        = MKA(env, str_false);
    // atom_list         = MKA(env, str_list);
    // atom_mode         = MKA(env, str_mode);
    atom_nif_abort    = MKA(env, str_nif_abort);
    atom_ok           = MKA(env, str_ok);
    // atom_once         = MKA(env, str_once);
    // atom_passive      = MKA(env, str_passive);
    // atom_receiver     = MKA(env, str_receiver);
    atom_select       = MKA(env, str_select);
    // atom_tcp_closed   = MKA(env, str_tcp_closed);
    atom_timeout      = MKA(env, str_timeout);
    atom_true         = MKA(env, str_true);
    atom_undefined    = MKA(env, str_undefined);
    // atom_version      = MKA(env, str_version);

    /* Error codes */
    atom_eagain       = MKA(env, str_eagain);
    atom_eafnosupport = MKA(env, str_eafnosupport);
    atom_einval       = MKA(env, str_einval);
    atom_eisconn      = MKA(env, str_eisconn);
    atom_enotclosing  = MKA(env, str_enotclosing);
    atom_enotconn     = MKA(env, str_enotconn);
    atom_exalloc      = MKA(env, str_exalloc);
    atom_exbadstate   = MKA(env, str_exbadstate);
    atom_exbusy       = MKA(env, str_exbusy);
    // atom_exnotopen    = MKA(env, str_exnotopen);
    atom_exmon        = MKA(env, str_exmon);
    atom_exself       = MKA(env, str_exself);
    atom_exsend       = MKA(env, str_exsend);

    // For storing "global" things...
    // socketData.env       = enif_alloc_env(); // We should really check
    // socketData.version   = MKA(env, ERTS_VERSION);
    // socketData.buildDate = MKA(env, ERTS_BUILD_DATE);

    sockets = enif_open_resource_type_x(env,
                                        "sockets",
                                        &socketInit,
                                        ERL_NIF_RT_CREATE,
                                        NULL);

    return !sockets;
}

ERL_NIF_INIT(socket, socket_funcs, on_load, NULL, NULL, NULL)
