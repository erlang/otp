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


/* Debug stuff... */
#define SOCKET_NIF_DEBUG_DEFAULT TRUE
#define SOCKET_DEBUG_DEFAULT     TRUE

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
#define MALLOC(SZ)    enif_alloc(SZ)
#define FREE(P)       enif_free(P)
#define MKA(E,S)      enif_make_atom(E, S)
#define MKREF(E)      enif_make_ref(E)
#define MKT2(E,E1,E2) enif_make_tuple2(E, E1, E2)
#define MCREATE(N)    enif_mutex_create(N)


/* *** Socket state defs *** */

#define SOCKET_FLAG_OPEN         0x0001
#define SOCKET_FLAG_ACTIVE       0x0004
#define SOCKET_FLAG_LISTEN       0x0008
#define SOCKET_FLAG_CON          0x0010
#define SOCKET_FLAG_ACC          0x0020
#define SOCKET_FLAG_BUSY         0x0040
#define SOCKET_FLAG_MULTI_CLIENT 0x0100 /* Multiple clients for one descriptor, *
                                         * i.e. multi-accept                    */
#define SOCKET_STATE_CLOSED          (0)
#define SOCKET_STATE_OPEN            (SOCKET_FLAG_OPEN)
#define SOCKET_STATE_CONNECTED       (SOCKET_STATE_OPEN      | SOCKET_FLAG_ACTIVE)
#define SOCKET_STATE_LISTENING       (SOCKET_STATE_OPEN      | SOCKET_FLAG_LISTEN)
#define SOCKET_STATE_CONNECTING      (SOCKET_STATE_OPEN      | SOCKET_FLAG_CON)
#define SOCKET_STATE_ACCEPTING       (SOCKET_STATE_LISTENING | SOCKET_FLAG_ACC)
#define SOCKET_STATE_MULTI_ACCEPTING (SOCKET_STATE_ACCEPTING | SOCKET_FLAG_MULTI_CLIENT)

#define IS_OPEN(d) \
    (((d)->state & SOCKET_FLAG_OPEN) == SOCKET_FLAG_OPEN)

#define IS_CONNECTED(d)                                                 \
    (((d)->state & SOCKET_STATE_CONNECTED) == SOCKET_STATE_CONNECTED)

#define IS_CONNECTING(d)                                \
    (((d)->state & SOCKET_FLAG_CON) == SOCKET_FLAG_CON)

#define IS_BUSY(d)                                      \
    (((d)->state & SOCKET_FLAG_BUSY) == SOCKET_FLAG_BUSY)


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


/* =================================================================== *
 *                                                                     *
 *                        Various enif macros                          *
 *                                                                     *
 * =================================================================== */

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
#define GET_TUPLE(E, TE, TSZ, TA) enif_get_tuple((E), (TE), (TSZ), (TA))


/* =================================================================== *
 *                                                                     *
 *                    Basic socket operations                          *
 *                                                                     *
 * =================================================================== */

#ifdef __WIN32__

/* *** Windown macros *** */

#define sock_bind(s, addr, len)    bind((s), (addr), (len))
#define sock_connect(s, addr, len) connect((s), (addr), (len))
#define sock_getopt(s,t,n,v,l)     getsockopt((s),(t),(n),(v),(l))
#define sock_htons(x)              htons((x))
#define sock_htonl(x)              htonl((x))
#define sock_listen(s, b)          listen((s), (b))
#define sock_name(s, addr, len)    getsockname((s), (addr), (len))
#define sock_open(domain, type, proto)                             \
    make_noninheritable_handle(socket((domain), (type), (proto)))

#define sock_errno()               WSAGetLastError()
#define sock_create_event(s)       WSACreateEvent()

#define SET_BLOCKING(s)            ioctlsocket(s, FIONBIO, &zero_value)
#define SET_NONBLOCKING(s)         ioctlsocket(s, FIONBIO, &one_value)
static unsigned long zero_value = 0;
static unsigned long one_value  = 1;

#else /* !__WIN32__ */

#define sock_bind(s, addr, len)         bind((s), (addr), (len))
#define sock_connect(s, addr, len)      connect((s), (addr), (len))
#define sock_getopt(s,t,n,v,l)          getsockopt((s),(t),(n),(v),(l))
#define sock_htons(x)                   htons((x))
#define sock_htonl(x)                   htonl((x))
#define sock_listen(s, b)               listen((s), (b))
#define sock_name(s, addr, len)         getsockname((s), (addr), (len))
#define sock_open(domain, type, proto)  socket((domain), (type), (proto))

#define sock_errno()                errno
#define sock_create_event(s)        (s) /* return file descriptor */


#endif /* !__WIN32__ */

#ifdef HAVE_SOCKLEN_T
#  define SOCKLEN_T socklen_t
#else
#  define SOCKLEN_T size_t
#endif


/* The general purpose sockaddr */
typedef struct {
    union {
        struct sockaddr     sa;
        struct sockaddr_in  sai;

#ifdef HAVE_IN6
        struct sockaddr_in6 sai6;
#endif

#ifdef HAVE_SYS_UN_H
        struct sockaddr_un  sal;
#endif

    } u;
    unsigned int len;
} SocketAddress;

#define which_address_port(sap)		     \
  ((((sap)->u.sai.sin_family == AF_INET) ||  \
    ((sap)->u.sai.sin_family == AF_INET6)) ? \
   ((sap)->u.sai.sin_port) : -1)


typedef struct {
    // The actual socket
    SOCKET         sock;
    HANDLE         event;

    /* "Stuff" about the socket */
    int            domain;
    int            type;
    int            protocol;

    unsigned int   state;
    SocketAddress  remote;


    // Controller (owner) process
    ErlNifPid      ctrlPid;
    ErlNifMonitor  ctrlMon;

    // Write
    ErlNifMutex*   writeMtx;
    BOOLEAN_T      isWritable;
    unsigned int   writePkgCnt;
    unsigned int   writeByteCnt;
    unsigned int   writeTries;
    unsigned int   writeWaits;

    // Read
    ErlNifMutex*   readMtx;
    BOOLEAN_T      isReadable;
    ErlNifBinary   rbuffer;
    unsigned int   readCapacity;
    unsigned int   readPkgCnt;
    unsigned int   readByteCnt;
    unsigned int   readTries;
    unsigned int   readWaits;


    /* We need to keep track of the "request(s)" we have pending.
     * If for instance an accept takes to long, the issuer may
     * decide to "cancel" the accept (actually the select). This
     * is done by calling the *nif_cancel* function with the request
     * ref as argument.
     * We also need to keep track of requests so that if a new
     * request is issued before the current has completed, we
     * reply with e.g. ebusy (or something to that effect).
     * Or do we? Can the caller actually do that?
     */


    /* Misc stuff */
    BOOLEAN_T      dbg;
} SocketDescriptor;


/* Global stuff (do we really need to "collect"
 * these things?)
 */
typedef struct {
  /* These are for debugging, testing and the like */
  ERL_NIF_TERM version;
  ERL_NIF_TERM buildDate;
  BOOLEAN_T    dbg;
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
static ERL_NIF_TERM nif_accept4(ErlNifEnv*         env,
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
static ERL_NIF_TERM nif_setopt(ErlNifEnv*         env,
                               int                argc,
                               const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_getopt(ErlNifEnv*         env,
                               int                argc,
                               const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_finalize_connection(ErlNifEnv*         env,
                                            int                argc,
                                            const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_cancel(ErlNifEnv*         env,
                               int                argc,
                               const ERL_NIF_TERM argv[]);


static char* decode_laddress(ErlNifEnv*     env,
                             int            domain,
                             ERL_NIF_TERM   localAddr,
                             SocketAddress* localP);
static char* decode_laddress_binary(ErlNifEnv*     env,
                                    int            domain,
                                    ERL_NIF_TERM   localAddr,
                                    SocketAddress* localP);
static char* decode_laddress_tuple(ErlNifEnv*     env,
                                   int            domain,
                                   ERL_NIF_TERM   laddr,
                                   SocketAddress* localP);
static char* decode_address_tuple(ErlNifEnv*          env,
                                  int                 domain,
                                  const ERL_NIF_TERM* addrt,
                                  int                 port,
                                  SocketAddress*      localP);
static char* decode_address_atom(ErlNifEnv*     env,
                                 int            domain,
                                 char*          addr,
                                 int            addrLen,
                                 int            port,
                                 SocketAddress* localP);

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
static ERL_NIF_TERM nfinalize_connection(ErlNifEnv*        env,
                                         SocketDescriptor* descP);

static BOOLEAN_T verify_is_connected(SocketDescriptor* descP, int* err);

static BOOLEAN_T edomain2domain(int edomain, int* domain);
static BOOLEAN_T etype2type(int etype, int* type);
static BOOLEAN_T eproto2proto(int eproto, int* proto);
#ifdef HAVE_SETNS
static BOOLEAN_T emap2netns(ErlNifEnv* env, ERL_NIF_TERM map, char** netns);
static BOOLEAN_T change_network_namespace(char* netns, int* cns, int* err);
static BOOLEAN_T restore_network_namespace(int ns, SOCKET sock, int* err);
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

static ERL_NIF_TERM make_ok(ErlNifEnv* env, ERL_NIF_TERM any);
static ERL_NIF_TERM make_error(ErlNifEnv* env, ERL_NIF_TERM reason);
static ERL_NIF_TERM make_error1(ErlNifEnv* env, char* reason);
static ERL_NIF_TERM make_error2(ErlNifEnv* env, int err);


static BOOLEAN_T extract_item_on_load(ErlNifEnv*    env,
                                      ERL_NIF_TERM  map,
                                      ERL_NIF_TERM  key,
                                      ERL_NIF_TERM* val);

static BOOLEAN_T extract_debug_on_load(ErlNifEnv*   env,
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
static char str_false[]       = "false";
static char str_error[]       = "error";
static char str_ok[]          = "ok";
static char str_true[]        = "true";
static char str_undefined[]   = "undefined";

/* (special) error string constants */
static char str_eagain[]         = "eagain";
static char str_eafnosupport[]   = "eafnosupport";
static char str_einval[]         = "einval";
static char str_eisconn[]        = "eisconn";
static char str_eisnconn[]       = "eisnconn";
static char str_exbadstate[]     = "exbadstate";
static char str_exmon[]          = "exmonitor";  // failed monitor
static char str_exself[]         = "exself";     // failed self


/* *** Atoms *** */
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_undefined;

static ERL_NIF_TERM atom_eagain;
static ERL_NIF_TERM atom_eafnosupport;
static ERL_NIF_TERM atom_einval;
static ERL_NIF_TERM atom_eisconn;
static ERL_NIF_TERM atom_eisnconn;
static ERL_NIF_TERM atom_exbadstate;
static ERL_NIF_TERM atom_exmon;
static ERL_NIF_TERM atom_exself;


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
 * nif_accept4(LSock, Ref)
 * nif_send(Sock, Data, Flags)
 * nif_sendto(Sock, Data, Flags, DstAddr, DstPort)
 * nif_recv(Sock, Flags)
 * nif_recvfrom(Sock, Flags)
 * nif_close(Sock)
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
        while ((close(sock) == INVALID_SOCKET) && (sock_errno() == EINTR));
        return make_error2(env, save_errno);
    }


    SET_NONBLOCKING(sock);


    /* Create and initiate the socket "descriptor" */
    descP = enif_alloc_resource(sockets, sizeof(SocketDescriptor));
    {
        char buf[64]; /* Buffer used for building the mutex name */
        sprintf(buf, "socket[w,%d]", sock);
        descP->writeMtx = MCREATE(buf);
        sprintf(buf, "socket[r,%d]", sock);
        descP->readMtx  = MCREATE(buf);
    }
    descP->isWritable   = TRUE;
    descP->isReadable   = TRUE;
    descP->writePkgCnt  = 0;
    descP->writeByteCnt = 0;
    descP->writeTries   = 0;
    descP->writeWaits   = 0;
    descP->readPkgCnt   = 0;
    descP->readByteCnt  = 0;
    descP->readTries    = 0;
    descP->readWaits    = 0;
    descP->dbg          = SOCKET_DEBUG_DEFAULT;
    descP->state        = SOCKET_STATE_OPEN;
    descP->domain       = domain;
    descP->type         = type;
    descP->protocol     = protocol;
    descP->sock         = sock;
    descP->event        = event;

    res = enif_make_resource(env, descP);
    enif_release_resource(descP); // We should really store a reference ...


    /* Keep track of the creator
     * This should not be a problem but just in case
     * the *open* function is used with the wrong kind
     * of environment...
     */
    if (enif_self(env, &descP->ctrlPid) == NULL)
        return make_error(env, atom_exself);

    if (enif_monitor_process(env, descP,
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
     enif_select(env,
                 event,
                 ERL_NIF_SELECT_READ,
                 descP, NULL, atom_undefined);
#endif


    return make_ok(env, res);
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
    char*         err;
    int           port;

    if ((err = decode_laddress(env, descP->domain, addr, &local)) != NULL)
        return make_error1(env, err);

    if (IS_SOCKET_ERROR(sock_bind(descP->sock,
                                  (struct sockaddr*) &local.u, local.len))) {
        return make_error2(env, sock_errno());
    }

    port = which_address_port(&local);
    if (port == 0) {
        SOCKLEN_T addrLen = sizeof(local.u);
        sys_memzero((char *) &local.u, addrLen);
        sock_name(descP->sock, &local.u.sa, &addrLen);
        port = which_address_port(&local);
    } else if (port == -1) {
        port = 0;
    }

    return make_ok(env, enif_make_int(env, port));

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
                      SocketAddress* localP)
{
    if (IS_BIN(env, localAddr)) {
        return decode_laddress_binary(env, domain, localAddr, localP);
    } else if (IS_TUPLE(env, localAddr)) {
        return decode_laddress_tuple(env, domain, localAddr, localP);
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
                             SocketAddress* localP)
{
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
         ) > sizeof(localP->u.sal.sun_path))
        return str_einval;

    sys_memzero((char*)&localP->u, sizeof(struct sockaddr_un));
    localP->u.sal.sun_family = domain;
    sys_memcpy(localP->u.sal.sun_path, bin.data, bin.size);
    localP->len = offsetof(struct sockaddr_un, sun_path) + bin.size;
#ifndef NO_SA_LEN
    localP->u.sal.sun_len = localP->len;
#endif
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
                            SocketAddress* localP)
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
                                    localP);

    } else if (IS_ATOM(env, laddrt[0]) &&
               IS_NUM(env, laddrt[1])) {

        /* There are only two atoms we handle:
         *    - any
         *    - loopback
         */

        unsigned int len;
        char         a[16]; // Just in case...

        if (!(GET_ATOM_LEN(env, laddrt[1], &len) &&
              (len > 0) &&
              (len <= (sizeof("loopback")))))
            return str_einval;

        if (!GET_ATOM(env, laddrt[0], a, sizeof(a)))
            return str_einval;

        return decode_address_atom(env,
                                   domain, a, len, port,
                                   localP);

    } else {
        return str_einval;
    }

}


/* Decode the 4- or 8-element address tuple
 * and initiate the socket address structure.
 */
static
char* decode_address_tuple(ErlNifEnv*          env,
                           int                 domain,
                           const ERL_NIF_TERM* addrt,
                           int                 port,
                           SocketAddress*      addrP)
{

    /* We now *know* that the size of the tuple is correct,
     * so we don't need to check anything here, just unpack.
     */

    switch (domain) {
    case AF_INET:
        {
            int  a, v;
            char laddr[4];

            sys_memzero((char*)&addrP->u, sizeof(struct sockaddr_in));
#ifndef NO_SA_LEN
            addrP->u.sai.sin_len    = sizeof(struct sockaddr_in);
#endif
            addrP->u.sai.sin_family = domain;
            addrP->u.sai.sin_port   = sock_htons(port);
            for (a = 0; a < 4; a++) {
                if (!GET_INT(env, addrt[a], &v))
                    return str_einval;
                laddr[a] = v;
            }
            sys_memcpy(&addrP->u.sai.sin_addr, &laddr, sizeof(laddr));
            addrP->len = sizeof(struct sockaddr_in);
            return NULL;
        }
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        {
            int  a, v;
            char laddr[16];

            sys_memzero((char*)&addrP->u, sizeof(struct sockaddr_in6));
#ifndef NO_SA_LEN
            addrP->u.sai6.sin6_len      = sizeof(struct sockaddr_in6);
#endif
            addrP->u.sai6.sin6_family   = domain;
            addrP->u.sai6.sin6_port     = sock_htons(port);
            addrP->u.sai6.sin6_flowinfo = 0;
            /* The address tuple is of size 8
             * and each element is a two byte integer
             */
            for (a = 0; a < 8; a++) {
                if (!GET_INT(env, addrt[a], &v))
                    return str_einval;
                laddr[a*2  ] = ((v >> 8) & 0xFF);
                laddr[a*2+1] = (v & 0xFF);
            }
            sys_memcpy(&addrP->u.sai6.sin6_addr, &laddr, sizeof(laddr));
            addrP->len = sizeof(struct sockaddr_in6);
            return NULL;
        }
        break;
#endif

    } /* switch (domain) */

    return str_eafnosupport;

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
                          SocketAddress* localP)
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
            sys_memzero((char*) localP, sizeof(struct sockaddr_in));
#ifndef NO_SA_LEN
            localP->u.sai.sin_len         = sizeof(struct sockaddr_in6);
#endif
            localP->u.sai.sin_family      = domain;
            localP->u.sai.sin_port        = sock_htons(port);
            localP->u.sai.sin_addr.s_addr = addr.s_addr;
            localP->len                   = sizeof(struct sockaddr_in);
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
            sys_memzero((char*)localP, sizeof(struct sockaddr_in6));
#ifndef NO_SA_LEN
            localP->u.sai6.sin6_len      = sizeof(struct sockaddr_in6);
#endif
            localP->u.sai6.sin6_family   = domain;
            localP->u.sai6.sin6_port     = sock_htons(port);
            localP->u.sai6.sin6_flowinfo = 0;
            localP->u.sai6.sin6_addr     = *paddr;
            localP->len                  = sizeof(struct sockaddr_in6);
        }
        break;
#endif

    default:
        return str_einval;
        break;
    }

    return NULL;
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
    int   code;
    char* xerr;

    /* Verify that we are where in the proper state */

    if (!IS_OPEN(descP))
        return make_error(env, atom_exbadstate);

    if (IS_CONNECTED(descP))
        return make_error(env, atom_eisconn);

    if (IS_CONNECTING(descP))
        return make_error(env, atom_einval);

    if ((xerr = decode_address_tuple(env,
                                     descP->domain, addr, port,
                                     &descP->remote)) != NULL)
        return make_error1(env, xerr);

    code = sock_connect(descP->sock,
                        (struct sockaddr*) &descP->remote.u, descP->remote.len);

    if (IS_SOCKET_ERROR(code) &&
        ((sock_errno() == ERRNO_BLOCK) ||   /* Winsock2            */
         (sock_errno() == EINPROGRESS))) {  /* Unix & OSE!!        */
        ERL_NIF_TERM ref = MKREF(env);
        descP->state = SOCKET_STATE_CONNECTING;
        enif_select(env,
                    descP->sock,
                    (ERL_NIF_SELECT_WRITE),
                    descP, NULL, ref);
        return make_ok(env, ref);
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
        return make_error(env, atom_eisnconn);

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

    sz = sizeof(descP->inet.remote);
    sys_memzero((char *) &descP->inet.remote, sz);
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
 *  U t i l i t y   F u n c t i o n s
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


/* Create an ok two (2) tuple in the form: {ok, Any}.
 * The second element (Any) is already in the form of an
 * ERL_NIF_TERM so all we have to do is create the tuple.
 */
static
ERL_NIF_TERM make_ok(ErlNifEnv* env, ERL_NIF_TERM any)
{
    return MKT2(env, atom_ok, any);
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
}


/* =========================================================================
 * socket_stop - Callback function for resource stop
 *
 */
static
void socket_stop(ErlNifEnv* env, void* obj, int fd, int is_direct_call)
{
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
  {"nif_is_loaded", 0, nif_is_loaded},
  {"nif_info",      0, nif_info},
  // {"nif_debug",      1, nif_debug_},

  // The proper "socket" interface
  {"nif_open",                4, nif_open},
  {"nif_bind",                3, nif_bind},
  {"nif_connect",             3, nif_connect},
  {"nif_listen",              2, nif_listen},
  {"nif_accept",              2, nif_accept},
  {"nif_accept4",             3, nif_accept4},
  {"nif_send",                3, nif_send},
  {"nif_sendto",              5, nif_sendto},
  {"nif_recv",                2, nif_recv},
  {"nif_recvfrom",            2, nif_recvfrom},
  {"nif_close",               1, nif_close},
  {"nif_setopt",              3, nif_setopt},
  {"nif_getopt",              2, nif_getopt},

  /* "Extra" functions to "complete" the socket interface.
   * For instance, the function nif_finalize_connection
   * is called after the connect *select* has "completed".
   */
  {"nif_finalize_connection", 1, nif_finalize_connection},
  {"nif_cancel",              2, nif_cancel},
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

/* =======================================================================
 * load_info - A map of misc info (e.g global debug)
 */

static
int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    socketData.dbg = extract_debug_on_load(env, load_info,
                                           SOCKET_NIF_DEBUG_DEFAULT);

    /* Misc atoms */
    // atom_active       = MKA(env, str_active);
    // atom_active_n     = MKA(env, str_active_n);
    // atom_active_once  = MKA(env, str_active_once);
    // atom_binary       = MKA(env, str_binary);
    // atom_buildDate    = MKA(env, str_buildDate);
    // atom_closed       = MKA(env, str_closed);
    atom_error        = MKA(env, str_error);
    atom_false        = MKA(env, str_false);
    // atom_list         = MKA(env, str_list);
    // atom_mode         = MKA(env, str_mode);
    atom_ok           = MKA(env, str_ok);
    // atom_once         = MKA(env, str_once);
    // atom_passive      = MKA(env, str_passive);
    // atom_receiver     = MKA(env, str_receiver);
    // atom_tcp_closed   = MKA(env, str_tcp_closed);
    atom_true         = MKA(env, str_true);
    atom_undefined    = MKA(env, str_undefined);
    // atom_version      = MKA(env, str_version);

    /* Error codes */
    atom_eagain       = MKA(env, str_eagain);
    atom_eafnosupport = MKA(env, str_eafnosupport);
    atom_einval       = MKA(env, str_einval);
    atom_eisconn      = MKA(env, str_eisconn);
    atom_eisnconn     = MKA(env, str_eisnconn);
    // atom_exalloc      = MKA(env, str_exalloc);
    atom_exbadstate   = MKA(env, str_exbadstate);
    // atom_exnotopen    = MKA(env, str_exnotopen);
    atom_exmon        = MKA(env, str_exmon);
    atom_exself       = MKA(env, str_exself);
    // atom_exsend       = MKA(env, str_exsend);

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
