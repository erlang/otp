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
 *  Purpose : The NIF (C) part of the net interface
 *            This is a module of miscellaneous functions.
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

/* Various defaults... */
#define SOCKET_DEBUG_DEFAULT     TRUE
#define SOCKET_IOW_DEFAULT       FALSE

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
// #define INVALID_SOCKET -1
// #define INVALID_EVENT  -1
// #define SOCKET_ERROR   -1

// #define SOCKET int
// #define HANDLE long int


/* *** Misc macros and defines *** */

#ifdef __WIN32__
#define get_errno() WSAGetLastError()
#else
#define get_errno() errno
#endif

// #if defined(TCP_CA_NAME_MAX)
// #define SOCKET_OPT_TCP_CONGESTION_NAME_MAX TCP_CA_NAME_MAX
// #else
/* This is really excessive, but just in case... */
// #define SOCKET_OPT_TCP_CONGESTION_NAME_MAX 256
// #endif

#define HOSTNAME_LEN 256
#define SERVICE_LEN  256


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
#define MKLA(E,A,L)         enif_make_list_from_array((E), (A), (L))
#define MKEL(E)             enif_make_list((E), 0)
#define MKREF(E)            enif_make_ref((E))
#define MKS(E,S)            enif_make_string((E), (S), ERL_NIF_LATIN1)
#define MKSL(E,S,L)         enif_make_string_len((E), (S), (L), ERL_NIF_LATIN1)
#define MKSBIN(E,B,ST,SZ)   enif_make_sub_binary((E), (B), (ST), (SZ))
#define MKT2(E,E1,E2)       enif_make_tuple2((E), (E1), (E2))
#define MKT3(E,E1,E2,E3)    enif_make_tuple3((E), (E1), (E2), (E3))
#define MKT4(E,E1,E2,E3,E4) enif_make_tuple4((E), (E1), (E2), (E3), (E4))
#define MKT5(E,E1,E2,E3,E4,E5) \
    enif_make_tuple5((E), (E1), (E2), (E3), (E4), (E5))
#define MKT8(E,E1,E2,E3,E4,E5,E6,E7,E8) \
    enif_make_tuple8((E), (E1), (E2), (E3), (E4), (E5), (E6), (E7), (E8))

#define MCREATE(N)          enif_mutex_create((N))
#define MDESTROY(M)         enif_mutex_destroy((M))
#define MLOCK(M)            enif_mutex_lock((M))
#define MUNLOCK(M)          enif_mutex_unlock((M))

#define MONP(E,D,P,M)       enif_monitor_process((E), (D), (P), (M))
#define DEMONP(E,D,M)       enif_demonitor_process((E), (D), (M))

#define SELECT(E,FD,M,O,P,R)                            \
    enif_select((E), (FD), (M), (O), (P), (R))
#define SELECT_READ(E, DP, P, R)                                       \
    SELECT((E), (DP)->sock, (ERL_NIF_SELECT_READ), (DP), (P), (R))
#define SELECT_WRITE(E, DP, P, R)                                      \
    SELECT((E), (DP)->sock, (ERL_NIF_SELECT_WRITE), (DP), (P), (R))
#define SELECT_STOP(E, DP)                                              \
    enif_select((E), (DP)->sock, (ERL_NIF_SELECT_STOP), (DP), NULL, atom_undefined)

#define IS_ATOM(E,  TE) enif_is_atom((E),   (TE))
#define IS_BIN(E,   TE) enif_is_binary((E), (TE))
#define IS_NUM(E,   TE) enif_is_number((E), (TE))
#define IS_TUPLE(E, TE) enif_is_tuple((E),  (TE))
#define IS_LIST(E,  TE) enif_is_list((E),   (TE))

#define COMPARE(L, R) enif_compare((L), (R))

#define GET_ATOM_LEN(E, TE, LP) \
    enif_get_atom_length((E), (TE), (LP), ERL_NIF_LATIN1)
#define GET_ATOM(E, TE, BP, MAX) \
    enif_get_atom((E), (TE), (BP), (MAX), ERL_NIF_LATIN1)
#define GET_BIN(E, TE, BP)          enif_inspect_iolist_as_binary((E), (TE), (BP))
#define GET_INT(E, TE, IP)          enif_get_int((E), (TE), (IP))
#define GET_LIST_ELEM(E, L, HP, TP) enif_get_list_cell((E), (L), (HP), (TP))
#define GET_LIST_LEN(E, L, LP)      enif_get_list_length((E), (L), (LP))
#define GET_STR(E, L, B, SZ)      \
    enif_get_string((E), (L), (B), (SZ), ERL_NIF_LATIN1)
#define GET_UINT(E, TE, IP)         enif_get_uint((E), (TE), (IP))
#define GET_TUPLE(E, TE, TSZ, TA)   enif_get_tuple((E), (TE), (TSZ), (TA))

#define ALLOC_BIN(SZ, BP)           enif_alloc_binary((SZ), (BP))
#define REALLOC_BIN(SZ, BP)         enif_realloc_binary((SZ), (BP))


#ifdef HAVE_SOCKLEN_T
#  define SOCKLEN_T socklen_t
#else
#  define SOCKLEN_T size_t
#endif

#define NDEBUG( ___COND___ , proto ) \
    if ( ___COND___ ) {              \
        dbg_printf proto;            \
        fflush(stdout);              \
    }
#define NDBG( proto ) NDEBUG( data.debug , proto )

/* The general purpose socket address */
typedef union {
    struct sockaddr     sa;

    struct sockaddr_in  in;

#ifdef HAVE_IN6
    struct sockaddr_in6 in6;
#endif

} SockAddress;

typedef struct {
    BOOLEAN_T debug;
} NetData;


static NetData data;


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
static ERL_NIF_TERM nif_command(ErlNifEnv*         env,
                                int                argc,
                                const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM nif_getnameinfo(ErlNifEnv*         env,
                                    int                argc,
                                    const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM nif_getaddrinfo(ErlNifEnv*         env,
                                    int                argc,
                                    const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM nif_if_name2index(ErlNifEnv*         env,
                                      int                argc,
                                      const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_if_index2name(ErlNifEnv*         env,
                                      int                argc,
                                      const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_if_names(ErlNifEnv*         env,
                                 int                argc,
                                 const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM ncommand(ErlNifEnv*   env,
                             ERL_NIF_TERM cmd);
static ERL_NIF_TERM ngetnameinfo(ErlNifEnv*         env,
                                 const SockAddress* saP,
                                 SOCKLEN_T          saLen,
                                 int                flags);
static ERL_NIF_TERM ngetaddrinfo(ErlNifEnv* env,
                                 char*      host,
                                 char*      serv);
static ERL_NIF_TERM nif_name2index(ErlNifEnv* env,
                                   char*      ifn);
static ERL_NIF_TERM nif_index2name(ErlNifEnv*   env,
                                   unsigned int id);
static ERL_NIF_TERM nif_names(ErlNifEnv* env);
static unsigned int nif_names_length(struct if_nameindex* p);

/*
static void net_dtor(ErlNifEnv* env, void* obj);
static void net_stop(ErlNifEnv* env,
                     void*      obj,
                     int        fd,
                     int        is_direct_call);
static void net_down(ErlNifEnv*           env,
                     void*                obj,
                     const ErlNifPid*     pid,
                     const ErlNifMonitor* mon);
*/
static BOOLEAN_T decode_in_sockaddr(ErlNifEnv*         env,
                                    const ERL_NIF_TERM eAddr,
                                    SockAddress*       saP,
                                    SOCKLEN_T*         saLen);
static BOOLEAN_T decode_in4_sockaddr(ErlNifEnv*          env,
                                     const ERL_NIF_TERM* addrt,
                                     SockAddress*        saP,
                                     SOCKLEN_T*          saLen);
#if defined(HAVE_IN6) && defined(AF_INET6)
static BOOLEAN_T decode_in6_sockaddr(ErlNifEnv*          env,
                                     const ERL_NIF_TERM* addrt,
                                     SockAddress*        saP,
                                     SOCKLEN_T*          saLen);
#endif
static BOOLEAN_T decode_nameinfo_flags(ErlNifEnv*         env,
                                       const ERL_NIF_TERM eflags,
                                       int*               flags);
static BOOLEAN_T decode_nameinfo_flags_list(ErlNifEnv*         env,
                                            const ERL_NIF_TERM eflags,
                                            int*               flags);
static
BOOLEAN_T decode_addrinfo_string(ErlNifEnv*         env,
                                 const ERL_NIF_TERM eString,
                                 char**             stringP);
static ERL_NIF_TERM decode_bool(ErlNifEnv*   env,
                                ERL_NIF_TERM eBool,
                                BOOLEAN_T*   bool);
static ERL_NIF_TERM encode_address_info(ErlNifEnv*       env,
                                        struct addrinfo* addrInfo);
static unsigned int address_info_length(struct addrinfo* addrInfoP);

static ERL_NIF_TERM make_address_info(ErlNifEnv*       env,
                                      struct addrinfo* addrInfoP);
static ERL_NIF_TERM make_addrinfo_family(ErlNifEnv* env,
                                         int        family);
static ERL_NIF_TERM make_addrinfo_type(ErlNifEnv* env,
                                       int        socktype);
static ERL_NIF_TERM make_addrinfo_proto(ErlNifEnv* env,
                                        int        proto);
static ERL_NIF_TERM make_addrinfo_addr(ErlNifEnv*       env,
                                       struct sockaddr* addrP,
                                       SOCKLEN_T        addrLen);

static ERL_NIF_TERM make_ok2(ErlNifEnv* env, ERL_NIF_TERM val);
// static ERL_NIF_TERM make_ok3(ErlNifEnv* env, ERL_NIF_TERM val1, ERL_NIF_TERM val2);
static ERL_NIF_TERM make_error(ErlNifEnv* env, ERL_NIF_TERM reason);
static ERL_NIF_TERM make_error1(ErlNifEnv* env, char* reason);
static ERL_NIF_TERM make_error2(ErlNifEnv* env, int err);

static void dbg_printf( const char* format, ... );
static int  dbg_realtime(struct timespec* tsP);
static int  dbg_timespec2str(char *buf, unsigned int len, struct timespec *ts);

/*
static void xabort(const char* expr,
                   const char* func,
                   const char* file,
                   int         line);
*/

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
static char str_address_info[]              = "address_info";
static char str_debug[]                     = "debug";
static char str_dgram[]                     = "dgram";
static char str_error[]                     = "error";
static char str_false[]                     = "false";
static char str_idn[]                       = "idn";
static char str_idna_allow_unassigned[]     = "idna_allow_unassigned";
static char str_idna_use_std3_ascii_rules[] = "idna_use_std3_ascii_rules";
static char str_inet[]                      = "inet";
static char str_inet6[]                     = "inet6";
static char str_ip[]                        = "ip";
static char str_namereqd[]                  = "namereqd";
static char str_name_info[]                 = "name_info";
static char str_nofqdn[]                    = "nofqdn";
static char str_numerichost[]               = "numerichost";
static char str_numericserv[]               = "numericserv";
static char str_ok[]                        = "ok";
static char str_stream[]                    = "stream";
static char str_tcp[]                       = "tcp";
static char str_true[]                      = "true";
static char str_udp[]                       = "udp";
static char str_undefined[]                 = "undefined";

// static char str_lowdelay[]    = "lowdelay";
// static char str_throughput[]  = "throughput";
// static char str_reliability[] = "reliability";
// static char str_mincost[]     = "mincost";

/* (special) error string constants */
// static char str_eafnosupport[]   = "eafnosupport";
static char str_eaddrfamily[]       = "eaddrfamily";
static char str_eagain[]            = "eagain";
static char str_ebadflags[]         = "ebadflags";
static char str_efail[]             = "efail";
static char str_efamily[]           = "efamily";
static char str_einval[]            = "einval";
// static char str_eisconn[]        = "eisconn";
static char str_emem[]              = "emem";
static char str_enodata[]           = "enodata";
static char str_enoname[]           = "enoname";
// static char str_enotclosing[]    = "enotclosing";
// static char str_enotconn[]       = "enotconn";
static char str_eoverflow[]         = "eoverflow";
static char str_eservice[]          = "eservice";
static char str_esocktype[]         = "esocktype";
static char str_esystem[]           = "esystem";
// static char str_exalloc[]        = "exalloc";
// static char str_exbadstate[]     = "exbadstate";
// static char str_exbusy[]         = "exbusy";
// static char str_exmon[]          = "exmonitor";  // failed monitor
// static char str_exself[]         = "exself";     // failed self
// static char str_exsend[]         = "exsend";     // failed send


/* *** Atoms *** */

static ERL_NIF_TERM atom_address_info;
static ERL_NIF_TERM atom_debug;
static ERL_NIF_TERM atom_dgram;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_idn;
static ERL_NIF_TERM atom_idna_allow_unassigned;
static ERL_NIF_TERM atom_idna_use_std3_ascii_rules;
static ERL_NIF_TERM atom_inet;
static ERL_NIF_TERM atom_inet6;
static ERL_NIF_TERM atom_ip;
static ERL_NIF_TERM atom_namereqd;
static ERL_NIF_TERM atom_name_info;
static ERL_NIF_TERM atom_nofqdn;
static ERL_NIF_TERM atom_numerichost;
static ERL_NIF_TERM atom_numericserv;
static ERL_NIF_TERM atom_ok;
// static ERL_NIF_TERM atom_select;
static ERL_NIF_TERM atom_stream;
// static ERL_NIF_TERM atom_timeout;
static ERL_NIF_TERM atom_tcp;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_udp;
static ERL_NIF_TERM atom_undefined;

// static ERL_NIF_TERM atom_lowdelay;
// static ERL_NIF_TERM atom_throughput;
// static ERL_NIF_TERM atom_reliability;
// static ERL_NIF_TERM atom_mincost;

// static ERL_NIF_TERM atom_eafnosupport;
static ERL_NIF_TERM atom_eaddrfamily;
static ERL_NIF_TERM atom_eagain;
static ERL_NIF_TERM atom_ebadflags;
static ERL_NIF_TERM atom_efail;
static ERL_NIF_TERM atom_efamily;
static ERL_NIF_TERM atom_einval;
// static ERL_NIF_TERM atom_eisconn;
static ERL_NIF_TERM atom_emem;
static ERL_NIF_TERM atom_enodata;
static ERL_NIF_TERM atom_enoname;
// static ERL_NIF_TERM atom_enotclosing;
// static ERL_NIF_TERM atom_enotconn;
static ERL_NIF_TERM atom_eoverflow;
static ERL_NIF_TERM atom_eservice;
static ERL_NIF_TERM atom_esocktype;
static ERL_NIF_TERM atom_esystem;
// static ERL_NIF_TERM atom_exalloc;
// static ERL_NIF_TERM atom_exbadstate;
// static ERL_NIF_TERM atom_exbusy;
// static ERL_NIF_TERM atom_exmon;
// static ERL_NIF_TERM atom_exself;
// static ERL_NIF_TERM atom_exsend;

/* *** net *** */
static ErlNifResourceType*    net;
/* Maybe all of these whould be NULL? */
static ErlNifResourceTypeInit netInit = {
    NULL, // net_dtor,
    NULL, // net_stop,
    NULL  // (ErlNifResourceDown*) net_down
};



/* ----------------------------------------------------------------------
 *  N I F   F u n c t i o n s
 * ----------------------------------------------------------------------
 *
 * Utility and admin functions:
 * ----------------------------
 * nif_is_loaded/0
 * nif_info/0
 *
 * The "proper" net functions:
 * ------------------------------
 * nif_getnameinfo/2
 * nif_getaddrinfo/3
 * nif_if_name2index/1
 * nif_if_index2name/1
 * nif_if_names/0
 *
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
    ERL_NIF_TERM info;

    NDBG( ("info -> entry\r\n") );

    info = enif_make_new_map(env);

    NDBG( ("info -> done\r\n") );

    return info;
}



/* ----------------------------------------------------------------------
 * nif_command
 *
 * Description:
 * This is a general purpose utility function.
 *
 * Arguments:
 * Command - This is a general purpose command, of any type.
 *           Currently, the only supported command is:
 *
 *                  {debug, boolean()}
 */
static
ERL_NIF_TERM nif_command(ErlNifEnv*         env,
                         int                argc,
                         const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ecmd, result;

    NDBG( ("command -> entry (%d)\r\n", argc) );

    if (argc != 1)
        return enif_make_badarg(env);

    ecmd = argv[0];

    NDBG( ("command -> ecmd: %T\r\n", ecmd) );

    result = ncommand(env, ecmd);

    NDBG( ("command -> result: %T\r\n", result) );

    return result;
}



/*
 * The command can, in principle, be anything, though currently we only
 * support a debug command.
 */
static
ERL_NIF_TERM ncommand(ErlNifEnv*   env,
                      ERL_NIF_TERM cmd)
{
    const ERL_NIF_TERM* t;
    int                 tsz;

    if (IS_TUPLE(env, cmd)) {
        /* Could be the debug tuple */
        if (!GET_TUPLE(env, cmd, &tsz, &t))
            return make_error(env, atom_einval);

        if (tsz != 2)
            return make_error(env, atom_einval);

        /* First element should be the atom 'debug' */
        if (COMPARE(t[0], atom_debug) != 0)
            return make_error(env, atom_einval);

        return decode_bool(env, t[1], &data.debug);

    } else {
        return make_error(env, atom_einval);
    }

}




/* ----------------------------------------------------------------------
 * nif_getnameinfo
 *
 * Description:
 * Address-to-name translation in protocol-independent manner.
 *
 * Arguments:
 * SockAddr - Socket Address (address and port)
 * Flags    - The flags argument modifies the behavior of getnameinfo().
 *            Not used!
 */

static
ERL_NIF_TERM nif_getnameinfo(ErlNifEnv*         env,
                             int                argc,
                             const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM result, eSockAddr;
    unsigned int eFlags;
    int          flags = 0; // Just in case...
    SockAddress  sa;
    SOCKLEN_T    saLen = 0; // Just in case...

    NDBG( ("nif_getnameinfo -> entry (%d)\r\n", argc) );

    if ((argc != 2) ||
        !GET_UINT(env, argv[1], &eFlags)) {
        return enif_make_badarg(env);
    }
    eSockAddr = argv[0];

    NDBG( ("nif_getnameinfo -> "
           "\r\n   SockAddr: %T"
           "\r\n   Flags:    %T"
           "\r\n", argv[0], argv[1]) );

    if (!decode_nameinfo_flags(env, eFlags, &flags))
        return enif_make_badarg(env);

    if (decode_in_sockaddr(env, eSockAddr, &sa, &saLen))
        return enif_make_badarg(env);

    result = ngetnameinfo(env, &sa, saLen, flags);

    NDBG( ("nif_getnameinfo -> done when result: %T\r\n", result) );

    return result;
}



/* Given the provided sock(et) address (and honts), retreive the host and
 * service info.
 */
static
ERL_NIF_TERM ngetnameinfo(ErlNifEnv*         env,
                          const SockAddress* saP,
                          SOCKLEN_T          saLen,
                          int                flags)
{
    ERL_NIF_TERM result;
    char         host[HOSTNAME_LEN];
    SOCKLEN_T    hostLen = sizeof(host);
    char         serv[SERVICE_LEN];
    SOCKLEN_T    servLen = sizeof(serv);

    int res = getnameinfo((struct sockaddr*) saP, saLen,
                          host, hostLen,
                          serv, servLen,
                          flags);

    switch (res) {
    case 0:
        {
            ERL_NIF_TERM info = MKT3(env,
                                     atom_name_info,
                                     MKS(env, host),
                                     MKS(env, serv));
            result = make_ok2(env, info);
        }
        break;

    case EAI_AGAIN:
        result = make_error(env, atom_eagain);
        break;

    case EAI_BADFLAGS:
        result = make_error(env, atom_ebadflags);
        break;

    case EAI_FAIL:
        result = make_error(env, atom_efail);
        break;

    case EAI_FAMILY:
        result = make_error(env, atom_efamily);
        break;

    case EAI_MEMORY:
        result = make_error(env, atom_emem);
        break;

    case EAI_NONAME:
        result = make_error(env, atom_enoname);
        break;

    case EAI_OVERFLOW:
        result = make_error(env, atom_eoverflow);
        break;

    case EAI_SYSTEM:
        result = make_error2(env, get_errno());
        break;

    default:
        result = make_error(env, atom_einval);
        break;
    }

    return result;
}



/* ----------------------------------------------------------------------
 * nif_getaddrinfo
 *
 * Description:
 * Network address and service translation.
 *
 * Arguments:
 * Host    - Host name (either a string or the atom undefined)
 * Service - Service name (either a string or the atom undefined)
 * Hints   - Hints for the lookup (address info record) (currently *ignored*)
 */

static
ERL_NIF_TERM nif_getaddrinfo(ErlNifEnv*         env,
                             int                argc,
                             const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM     result, eHostName, eServName; //, eHints;
    char*            hostName;
    char*            servName;
    // struct addrinfo* hints;

    NDBG( ("nif_getaddrinfo -> entry (%d)\r\n", argc) );

    if (argc != 3) {
        return enif_make_badarg(env);
    }
    eHostName = argv[0];
    eServName = argv[1];
    // eHints    = argv[2];

    NDBG( ("nif_getaddrinfo -> "
           "\r\n   Host:    %T"
           "\r\n   Service: %T"
           "\r\n   Hints:   %T"
           "\r\n", argv[0], argv[1], argv[2]) );

    if (!decode_addrinfo_string(env, eHostName, &hostName))
        return enif_make_badarg(env);

    if (!decode_addrinfo_string(env, eServName, &servName))
        return enif_make_badarg(env);

    /*
    if (decode_addrinfo_hints(env, eHints, &hints))
        return enif_make_badarg(env);
    */

    if ((hostName == NULL) && (servName == NULL))
        return enif_make_badarg(env);

    result = ngetaddrinfo(env, hostName, servName);

    if (hostName != NULL)
        FREE(hostName);

    if (servName != NULL)
        FREE(servName);

    /*
    if (hints != NULL)
        FREE(hints);
    */

    NDBG( ("nif_getaddrinfo -> done when result: %T\r\n", result) );

    return result;
}


static
ERL_NIF_TERM ngetaddrinfo(ErlNifEnv* env,
                          char*      host,
                          char*      serv)
{
    ERL_NIF_TERM     result;
    struct addrinfo* addrInfoP;
    int              res;

    res = getaddrinfo(host, serv, NULL, &addrInfoP);

    switch (res) {
    case 0:
        {
            ERL_NIF_TERM addrInfo = encode_address_info(env, addrInfoP);
            freeaddrinfo(addrInfoP);
            result = make_ok2(env, addrInfo);
        }
        break;

    case EAI_ADDRFAMILY:
        result = make_error(env, atom_eaddrfamily);
        break;

    case EAI_AGAIN:
        result = make_error(env, atom_eagain);
        break;

    case EAI_BADFLAGS:
        result = make_error(env, atom_ebadflags);
        break;

    case EAI_FAIL:
        result = make_error(env, atom_efail);
        break;

    case EAI_FAMILY:
        result = make_error(env, atom_efamily);
        break;

    case EAI_MEMORY:
        result = make_error(env, atom_emem);
        break;

    case EAI_NODATA:
        result = make_error(env, atom_enodata);
        break;

    case EAI_NONAME:
        result = make_error(env, atom_enoname);
        break;

    case EAI_SERVICE:
        result = make_error(env, atom_eservice);
        break;

    case EAI_SOCKTYPE:
        result = make_error(env, atom_esocktype);
        break;

    case EAI_SYSTEM:
        result = make_error(env, atom_esystem);
        break;

    default:
        result = make_error(env, atom_einval);
        break;
    }

    return result;
}


/* ----------------------------------------------------------------------
 * nif_if_name2index
 *
 * Description:
 * Perform a Interface Name to Interface Index translation.
 *
 * Arguments:
 * Ifn - Interface name to be translated.
 */

static
ERL_NIF_TERM nif_if_name2index(ErlNifEnv*         env,
                               int                argc,
                               const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM eifn, result;
    char         ifn[IF_NAMESIZE+1];

    NDBG( ("nif_if_name2index -> entry (%d)\r\n", argc) );

    if (argc != 1) {
        return enif_make_badarg(env);
    }
    eifn = argv[0];

    NDBG( ("nif_name2index -> "
           "\r\n   Ifn: %T"
           "\r\n", argv[0]) );

    if (0 >= GET_STR(env, eifn, ifn, sizeof(ifn)))
        return make_error2(env, atom_einval);

    result = nif_name2index(env, ifn);

    NDBG( ("nif_if_name2index -> done when result: %T\r\n", result) );

    return result;
}



static
ERL_NIF_TERM nif_name2index(ErlNifEnv* env,
                            char*      ifn)
{
     unsigned int idx = if_nametoindex(ifn);

     if (idx == 0)
         return make_error2(env, get_errno());
     else
         return make_ok2(env, idx);

}



/* ----------------------------------------------------------------------
 * nif_if_index2name
 *
 * Description:
 * Perform a Interface Index to Interface Name translation.
 *
 * Arguments:
 * Idx - Interface index to be translated.
 */

static
ERL_NIF_TERM nif_if_index2name(ErlNifEnv*         env,
                               int                argc,
                               const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM result;
    unsigned int idx;

    NDBG( ("nif_if_index2name -> entry (%d)\r\n", argc) );

    if ((argc != 1) ||
        !GET_UINT(env, argv[0], &idx)) {
        return enif_make_badarg(env);
    }

    NDBG( ("nif_index2name -> "
           "\r\n   Idx: %T"
           "\r\n", argv[0]) );

    result = nif_index2name(env, idx);

    NDBG( ("nif_if_index2name -> done when result: %T\r\n", result) );

    return result;
}



static
ERL_NIF_TERM nif_index2name(ErlNifEnv*   env,
                            unsigned int idx)
{
    ERL_NIF_TERM result;
    char*        ifn = MALLOC(IF_NAMESIZE+1);

    if (ifn == NULL)
        return enif_make_badarg(env); // PLACEHOLDER

    if (NULL == if_indextoname(idx, ifn)) {
        result = make_ok2(env, MKS(env, ifn));
    } else {
        result = make_error2(env, get_errno());
    }

    FREE(ifn);

    return result;
}



/* ----------------------------------------------------------------------
 * nif_if_names
 *
 * Description:
 * Get network interface names and indexes.
 *
 */

static
ERL_NIF_TERM nif_if_names(ErlNifEnv*         env,
                          int                argc,
                          const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM result;

    NDBG( ("nif_if_names -> entry (%d)\r\n", argc) );

    if (argc != 0) {
        return enif_make_badarg(env);
    }

    result = nif_names(env);

    NDBG( ("nif_if_names -> done when result: %T\r\n", result) );

    return result;
}



static
ERL_NIF_TERM nif_names(ErlNifEnv* env)
{
    ERL_NIF_TERM         result;
    struct if_nameindex* ifs = if_nameindex();

    NDBG( ("nif_names -> ifs: 0x%lX\r\n", ifs) );

    if (ifs == NULL) {
        result = make_error2(env, get_errno());
    } else {
        /*
         * We got some interfaces:
         * 1) Calculate how many - the only way is to iterate through the list
         *    until its end (which is indicated by an entry with index = zero
         *    and if_name = NULL).
         * 2) Allocate an ERL_NIF_TERM array of the calculated length.
         * 3) Iterate through the array of interfaces and for each create
         *    a two tuple: {Idx, If}
         *
         * Or shall we instead build a list in reverse order and then when
         * its done, reverse that? Check
         */
        unsigned int len = nif_names_length(ifs);

        NDBG( ("nif_names -> len: %d\r\n", len) );

        if (len > 0) {
            ERL_NIF_TERM* array = MALLOC(len * sizeof(ERL_NIF_TERM));
            unsigned int  i;

            for (i = 0; i < len; i++) {
                array[i] = MKT2(env,
                                MKI(env, ifs[i].if_index),
                                MKS(env, ifs[i].if_name));
            }

            result = make_ok2(env, MKLA(env, array, len));
            FREE(array);
        } else {
            result = make_ok2(env, enif_make_list(env, 0));
        }
    }

    if (ifs != NULL)
        if_freenameindex(ifs);

    return result;
}


static
unsigned int nif_names_length(struct if_nameindex* p)
{
    unsigned int len = 0;

    while ((p[len].if_index == 0) && (p[len].if_name == NULL)) {
        len++;
    }

    return len;
}



/* ----------------------------------------------------------------------
 *  U t i l i t y   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

/* Decode an in_sockaddr (the socket address).
 * This is the (erlang) type: in_sockaddr(), which is either
 * a in4_sockaddr() (tuple of size 3) or a in6_sockaddr()
 * (tuple of size 5). See the net erlang module for details.
 *
 * We first detect which which of the tuples it is:
 *   - Size 3: maybe in4_sockaddr
 *   - Size 5: maybe in6_sockaddr
 */
static
BOOLEAN_T decode_in_sockaddr(ErlNifEnv*         env,
                             const ERL_NIF_TERM eAddr,
                             SockAddress*       saP,
                             SOCKLEN_T*         saLen)
{
    const ERL_NIF_TERM* addrt;
    int                 addrtSz;

    if (!GET_TUPLE(env, eAddr, &addrtSz, &addrt))
        return FALSE;

    switch (addrtSz) {
    case 3:
        return decode_in4_sockaddr(env, addrt, saP, saLen);
        break;

#ifdef HAVE_IN6
    case 5:
        return decode_in6_sockaddr(env, addrt, saP, saLen);
        break;
#endif

    default:
        return FALSE;
        break;
    }

}


/* Decode an in4_sockaddr record. This is a tuple of size 3.
 * The size has already been verified, but not its content.
 * So, the first element should be the atom 'in4_sockaddr'.
 * The second the port number, an integer. And the third,
 * the actual ip address, a 4-tuple.
 */
static
BOOLEAN_T decode_in4_sockaddr(ErlNifEnv*          env,
                              const ERL_NIF_TERM* addrt,
                              SockAddress*        saP,
                              SOCKLEN_T*          saLen)
{
    unsigned int        len;
    char                tag[16]; // Just in case...
    int                 port;
    int                 ipAddrSz;
    const ERL_NIF_TERM* ipAddrT;
    int                 a, v;
    char                addr[4];

    /* The first element: Verify record tag (atom()): in4_sockaddr */

    if (!(GET_ATOM_LEN(env, addrt[0], &len) &&
          (len > 0) &&
          (len <= (sizeof(tag)))))
        return FALSE;

    if (!GET_ATOM(env, addrt[0], tag, sizeof(tag)))
        return FALSE;

    if (strncmp(tag, "in4_sockaddr", len) != 0)
        return FALSE;


    /* Get second element: port number (integer) */

    if (!GET_INT(env, addrt[1], &port))
        return FALSE;


    /* And finally, get the third element, the ip address (a 4 tuple) */

    if (!GET_TUPLE(env, addrt[2], &ipAddrSz, &ipAddrT))
        return FALSE;

    if (ipAddrSz != 4)
        return FALSE;


    /* And finally initialize the sockaddr structure (and size) */
    sys_memzero((char*)saP, sizeof(struct sockaddr_in));
    saP->in.sin_family = AF_INET;
    saP->in.sin_port   = htons(port);
    for (a = 0; a < 4; a++) {
        if (!GET_INT(env, ipAddrT[a], &v))
            return FALSE;
        addr[a] = v;
    }
    sys_memcpy(&saP->in.sin_addr, &addr, sizeof(addr));
    *saLen = sizeof(struct sockaddr_in);
    return TRUE;
}



#if defined(HAVE_IN6) && defined(AF_INET6)
/* Decode an in6_sockaddr record. This is a tuple of size 5.
 * The size has already been verified, but not its content.
 * So, the first element should be the atom 'in6_sockaddr'.
 * The second the port number, an integer. The third, the
 * actual ip address, a 8-tuple. The forth, the flowinfo,
 * an integer. And finally, the fifth element, the scope_id,
 * also an integer. *Not used here*.
 */
static
BOOLEAN_T decode_in6_sockaddr(ErlNifEnv*          env,
                              const ERL_NIF_TERM* addrt,
                              SockAddress*        saP,
                              SOCKLEN_T*          saLen)
{
    unsigned int        len;
    char                tag[16]; // Just in case...
    int                 port;
    int                 ipAddrSz;
    const ERL_NIF_TERM* ipAddrT;
    int                 flowInfo;
    int                 a, v;
    char                addr[16];


    /* The first element: Verify record tag (atom()): in6_sockaddr */

    if (!(GET_ATOM_LEN(env, addrt[0], &len) &&
          (len > 0) &&
          (len <= (sizeof(tag)))))
        return FALSE;

    if (!GET_ATOM(env, addrt[0], tag, sizeof(tag)))
        return FALSE;

    if (strncmp(tag, "in6_sockaddr", len) != 0)
        return FALSE;


    /* Get second element: port number (integer) */

    if (!GET_INT(env, addrt[1], &port))
        return FALSE;


    /* Get the third element, the ip address (a 8 tuple) */

    if (!GET_TUPLE(env, addrt[2], &ipAddrSz, &ipAddrT))
        return FALSE;

    if (ipAddrSz != 8)
        return FALSE;


    /* And finally, get the forth element, the flowinfo (integer) */

    if (!GET_INT(env, addrt[3], &flowInfo))
        return FALSE;


    /* And finally initialize the sockaddr structure (and size) */

    sys_memzero((char*)saP, sizeof(struct sockaddr_in6));
    saP->in6.sin6_family   = AF_INET6;
    saP->in6.sin6_port     = htons(port);
    saP->in6.sin6_flowinfo = flowInfo;
    /* The address tuple is of size 8
     * and each element is a two byte integer
     */
    for (a = 0; a < 8; a++) {
        if (!GET_INT(env, addrt[a], &v))
            return FALSE;
        addr[a*2  ] = ((v >> 8) & 0xFF);
        addr[a*2+1] = (v & 0xFF);
    }
    sys_memcpy(&saP->in6.sin6_addr, &addr, sizeof(addr));
    *saLen = sizeof(struct sockaddr_in6);

    return TRUE;
}
#endif



/* The erlang format for a set of flags is a list of atoms.
 * A special case is when there is no flags, which is
 * represented by the atom undefined.
 */
static
BOOLEAN_T decode_nameinfo_flags(ErlNifEnv*         env,
                                const ERL_NIF_TERM eflags,
                                int*               flags)
{
    BOOLEAN_T result;

    if (IS_ATOM(env, eflags)) {
        if (COMPARE(eflags, atom_undefined) == 0) {
            *flags = 0;
            result = TRUE;
        } else {
            result = FALSE;
        }
    } else if (IS_LIST(env, eflags)) {
        result = decode_nameinfo_flags_list(env, eflags, flags);
    } else {
        result = FALSE;
    }

    return result;
}



static
BOOLEAN_T decode_nameinfo_flags_list(ErlNifEnv*         env,
                                     const ERL_NIF_TERM eflags,
                                     int*               flags)
{
    ERL_NIF_TERM elem, tail, list = eflags;
    int          tmp = 0;
    BOOLEAN_T    done = FALSE;

    while (!done) {
        if (GET_LIST_ELEM(env, list, &elem, &tail)) {
            if (COMPARE(elem, atom_namereqd) == 0) {
                tmp |= NI_NAMEREQD;
            } else if (COMPARE(elem, atom_dgram) == 0) {
                tmp |= NI_DGRAM;
            } else if (COMPARE(elem, atom_nofqdn) == 0) {
                tmp |= NI_NOFQDN;
            } else if (COMPARE(elem, atom_numerichost) == 0) {
                tmp |= NI_NUMERICHOST;
            } else if (COMPARE(elem, atom_numericserv) == 0) {
                tmp |= NI_NUMERICSERV;

                /* Starting with glibc 2.3.4: */

#if defined(NI_IDN)
            } else if (COMPARE(elem, atom_idn) == 0) {
                tmp |= NI_IDN;
#endif

#if defined(NI_IDN_ALLOW_UNASSIGNED)
            } else if (COMPARE(elem, atom_idna_allow_unassigned) == 0) {
                tmp |= NI_IDN_ALLOW_UNASSIGNED;
#endif

#if defined(NI_IDN_USE_STD3_ASCII_RULES)
            } else if (COMPARE(elem, atom_idna_use_std3_ascii_rules) == 0) {
                tmp |= NI_IDN_USE_STD3_ASCII_RULES;
#endif

            } else {
                return FALSE;
            }

            list = tail;

        } else {
            done = TRUE;
        }
    }

    *flags = tmp;

    return TRUE;
}



/* Decode the address info string (hostname or service name)
 * The string is either the atom undefined or an actual string.
 */
static
BOOLEAN_T decode_addrinfo_string(ErlNifEnv*         env,
                                 const ERL_NIF_TERM eString,
                                 char**             stringP)
{
    BOOLEAN_T result;

    if (IS_ATOM(env, eString)) {
        if (COMPARE(eString, atom_undefined) == 0) {
            *stringP = NULL;
            result   = TRUE;
        } else {
            *stringP = NULL;
            result   = FALSE;
        }
    } else {
        unsigned int len;
        char*        bufP;

        if (!GET_LIST_LEN(env, eString, &len) && (len != 0)) {
            *stringP = NULL;
            result   = FALSE;
        }

        bufP = MALLOC(len);

        if (GET_STR(env, eString, bufP, len)) {
            *stringP = bufP;
            result   = TRUE;
        } else {
            *stringP = NULL;
            result   = FALSE;
        }
    }

    return result;

}



static
ERL_NIF_TERM decode_bool(ErlNifEnv*   env,
                         ERL_NIF_TERM eBool,
                         BOOLEAN_T*   bool)
{
    if (COMPARE(eBool, atom_true) == 0) {
        *bool = TRUE;
        return atom_ok;
    } else if (COMPARE(eBool, atom_false) == 0) {
        *bool = FALSE;
        return atom_ok;
    } else {
        return make_error(env, atom_einval);
    }
}



/* Encode the address info
 * The address info is a linked list och address info, which
 * will result in the result being a list of zero or more length.
 */
static
ERL_NIF_TERM encode_address_info(ErlNifEnv*       env,
                                 struct addrinfo* addrInfo)
{
    ERL_NIF_TERM result;
    unsigned int len = address_info_length(addrInfo);

    if (len > 0) {
        ERL_NIF_TERM* array = MALLOC(len * sizeof(ERL_NIF_TERM));
        unsigned int  i;

        for (i = 0; i < len; i++) {
            array[i] = make_address_info(env, &addrInfo[i]);
        }

        result = make_ok2(env, MKLA(env, array, len));
    } else {
        result = MKEL(env);
    }

    return result;
}



/* Calculate the length of the adress info linked list
 * The list is NULL-terminated, so the only way is to
 * iterate through the list until we find next = NULL.
 */
static
unsigned int address_info_length(struct addrinfo* addrInfoP)
{
    unsigned int     len;
    struct addrinfo* tmp = addrInfoP;

    if (tmp != NULL) {
        len = 1;
        while (tmp->ai_next != NULL) {
            tmp = tmp->ai_next;
            len++;
        }
    } else {
        len = 0;
    }

    return len;
}



/* Create one (erlang) instance of the address info record
 * Should we have address info as a record or as a map?
 *
 * {address_info, Fam, Type, Proto, Addr}
 */
static
ERL_NIF_TERM make_address_info(ErlNifEnv*       env,
                               struct addrinfo* addrInfoP)
{
    ERL_NIF_TERM Fam   = make_addrinfo_family(env, addrInfoP->ai_family);
    ERL_NIF_TERM Type  = make_addrinfo_type(env,   addrInfoP->ai_socktype);
    ERL_NIF_TERM Proto = make_addrinfo_proto(env,  addrInfoP->ai_protocol);
    ERL_NIF_TERM Addr  = make_addrinfo_addr(env,
                                            addrInfoP->ai_addr,
                                            addrInfoP->ai_addrlen);

    return MKT5(env, atom_address_info, Fam, Type, Proto, Addr);

}


/* Convert an "native" family to an erlang family
 * Note that this is not currently exhaustive, but only supports
 * inet and inet6. Other values will be returned as is, that is
 * in the form of an integer.
 */
static
ERL_NIF_TERM make_addrinfo_family(ErlNifEnv* env,
                                  int        family)
{
    ERL_NIF_TERM efam;

    switch (family) {
    case AF_INET:
        efam = atom_inet;
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        efam = atom_inet6;
        break;
#endif

    default:
        efam = MKI(env, family);
        break;
    }

    return efam;
}



/* Convert an "native" socket type to an erlang socket type
 * Note that this is not currently exhaustive, but only supports
 * stream and dgram. Other values will be returned as is, that is
 * in the form of an integer.
 */
static
ERL_NIF_TERM make_addrinfo_type(ErlNifEnv* env,
                                int        socktype)
{
    ERL_NIF_TERM etype;

    switch (socktype) {
    case SOCK_STREAM:
        etype = atom_stream;
        break;

    case SOCK_DGRAM:
        etype = atom_dgram;
        break;

    default:
        etype = MKI(env, socktype);
        break;
    }

    return etype;
}



/* Convert an "native" protocol to an erlang protocol
 * Note that this is not currently exhaustive, but only supports
 * tcp and udp. Other values will be returned as is, that is
 * in the form of an integer.
 */
static
ERL_NIF_TERM make_addrinfo_proto(ErlNifEnv* env,
                                 int        proto)
{
    ERL_NIF_TERM eproto;

    switch (proto) {
#if defined(SOL_IP)
    case SOL_IP:
#else
    case IPPROTO_IP:
#endif
        eproto = atom_ip;
        break;

    case IPPROTO_TCP:
        eproto = atom_tcp;
        break;

    case IPPROTO_UDP:
        eproto = atom_udp;
        break;

    default:
        eproto = MKI(env, proto);
        break;
    }

    return eproto;
}



/* Convert an "native" address to an erlang address
 * Note that this is not currently exhaustive, but only supports
 * IPv4 and IPv6 addresses. Values of other families will be
 * returned as an undefined.
 */
static
ERL_NIF_TERM make_addrinfo_addr(ErlNifEnv*       env,
                                struct sockaddr* addrP,
                                SOCKLEN_T        addrLen)
{
    ERL_NIF_TERM port, addr, eaddr;
    SockAddress* p = (SockAddress*) addrP;

    switch (addrP->sa_family) {
    case AF_INET:
        {
            unsigned char* a = (unsigned char*) &p->in.sin_addr;
            port = ntohs(p->in.sin_port);
            addr = MKT4(env,
                        MKI(env, a[0]),
                        MKI(env, a[1]),
                        MKI(env, a[2]),
                        MKI(env, a[3]));
            eaddr = MKT2(env, port, addr);
        }
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        {
            unsigned char* a = (unsigned char*) &p->in6.sin6_addr;
            port = ntohs(p->in6.sin6_port);
            addr = MKT8(env,
                        MKI(env, get_int16(a)),
                        MKI(env, get_int16(&a[ 2])),
                        MKI(env, get_int16(&a[ 4])),
                        MKI(env, get_int16(&a[ 6])),
                        MKI(env, get_int16(&a[ 8])),
                        MKI(env, get_int16(&a[10])),
                        MKI(env, get_int16(&a[12])),
                        MKI(env, get_int16(&a[14])));
            eaddr = MKT2(env, port, addr);
        }
        break;
#endif

    default:
        eaddr = atom_undefined;
        break;
    }

    return eaddr;
}



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
/*
static
ERL_NIF_TERM make_ok3(ErlNifEnv* env, ERL_NIF_TERM val1, ERL_NIF_TERM val2)
{
  return MKT3(env, atom_ok, val1, val2);
}
*/


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


/*
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
*/


/* ----------------------------------------------------------------------
 *  C o u n t e r   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

/* ----------------------------------------------------------------------
 *  C a l l b a c k   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

/* =========================================================================
 * net_dtor - Callback function for resource destructor
 *
 */
/*
static
void net_dtor(ErlNifEnv* env, void* obj)
{
}
*/


/* =========================================================================
 * net_stop - Callback function for resource stop
 *
 */
/*
static
void net_stop(ErlNifEnv* env, void* obj, int fd, int is_direct_call)
{
}
*/




/* =========================================================================
 * net_down - Callback function for resource down (monitored processes)
 *
 */
/*
static
void net_down(ErlNifEnv*           env,
              void*                obj,
              const ErlNifPid*     pid,
              const ErlNifMonitor* mon)
{
}
*/



/* ----------------------------------------------------------------------
 *  D e b u g   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

/*
 * Print a debug format string *with* both a timestamp and the
 * the name of the *current* thread.
 */
static
void dbg_printf( const char* format, ... )
{
  va_list         args;
  char            f[512 + sizeof(format)]; // This has to suffice...
  char            stamp[30];
  struct timespec ts;
  int             res;

  /*
   * We should really include self in the printout, so we can se which process
   * are executing the code. But then I must change the API....
   * ....something for later.
   */

  if (!dbg_realtime(&ts)) {
    if (dbg_timespec2str(stamp, sizeof(stamp), &ts) != 0) {
      // res = enif_snprintf(f, sizeof(f), "NET [%s] %s", TSNAME(), format);
      res = enif_snprintf(f, sizeof(f), "NET [%s]", format);
    } else {
      // res = enif_snprintf(f, sizeof(f), "NET[%s] [%s] %s", stamp, TSNAME(), format);
      res = enif_snprintf(f, sizeof(f), "NET [%s] %s", stamp, format);
    }

    if (res > 0) {
      va_start (args, format);
      erts_vfprintf (stdout, f, args); // TMP: use enif_vfprintf
      va_end (args);
      fflush(stdout);
    }
  }

  return;
}


static
int dbg_realtime(struct timespec* tsP)
{
  return clock_gettime(CLOCK_REALTIME, tsP);
}




/*
 * Convert a timespec struct into a readable/printable string
 */
static
int dbg_timespec2str(char *buf, unsigned int len, struct timespec *ts)
{
  int       ret, buflen;
  struct tm t;

  tzset();
  if (localtime_r(&(ts->tv_sec), &t) == NULL)
    return 1;

  ret = strftime(buf, len, "%F %T", &t);
  if (ret == 0)
    return 2;
  len -= ret - 1;
  buflen = strlen(buf);

  ret = snprintf(&buf[buflen], len, ".%06ld", ts->tv_nsec/1000);
  if (ret >= len)
    return 3;

  return 0;
}



/* ----------------------------------------------------------------------
 *  L o a d / u n l o a d / u p g r a d e   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

static
ErlNifFunc net_funcs[] =
{
    // Some utility functions
    {"nif_is_loaded", 0, nif_is_loaded, 0},
    {"nif_info",      0, nif_info,      0},
    {"nif_command",   1, nif_command,   0}, // Shall we let this be dirty?

    /* address and name translation in protocol-independent manner */
    {"nif_getnameinfo",         2, nif_getnameinfo,   0},
    {"nif_getaddrinfo",         3, nif_getaddrinfo,   0},

    /* Network interface (name and/or index) functions */
    {"nif_if_name2index",       1, nif_if_name2index, 0},
    {"nif_if_index2name",       1, nif_if_index2name, 0},
    {"nif_if_names",            0, nif_if_names,      0}
};



/* =======================================================================
 * load_info - A map of misc info (e.g global debug)
 */

static
int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    // We should make it possible to use load_info to get default values
    data.debug = FALSE;

    // NDBG( ("on_load -> entry\r\n") );

    /* +++ Misc atoms +++ */
    atom_address_info              = MKA(env, str_address_info);
    atom_debug                     = MKA(env, str_debug);
    atom_dgram                     = MKA(env, str_dgram);
    atom_error                     = MKA(env, str_error);
    atom_false                     = MKA(env, str_false);
    atom_idn                       = MKA(env, str_idn);
    atom_idna_allow_unassigned     = MKA(env, str_idna_allow_unassigned);
    atom_idna_use_std3_ascii_rules = MKA(env, str_idna_use_std3_ascii_rules);
    atom_inet                      = MKA(env, str_inet);
    atom_inet6                     = MKA(env, str_inet6);
    atom_ip                        = MKA(env, str_ip);
    atom_namereqd                  = MKA(env, str_namereqd);
    atom_name_info                 = MKA(env, str_name_info);
    atom_nofqdn                    = MKA(env, str_nofqdn);
    atom_numerichost               = MKA(env, str_numerichost);
    atom_numericserv               = MKA(env, str_numericserv);
    atom_ok                        = MKA(env, str_ok);
    atom_stream                    = MKA(env, str_stream);
    atom_tcp                       = MKA(env, str_tcp);
    atom_true                      = MKA(env, str_true);
    atom_udp                       = MKA(env, str_udp);
    atom_undefined                 = MKA(env, str_undefined);
    // atom_version      = MKA(env, str_version);

    // atom_lowdelay     = MKA(env, str_lowdelay);
    // atom_throughput   = MKA(env, str_throughput);
    // atom_reliability  = MKA(env, str_reliability);
    // atom_mincost      = MKA(env, str_mincost);

    /* Error codes */
    // atom_eafnosupport = MKA(env, str_eafnosupport);
    atom_eaddrfamily     = MKA(env, str_eaddrfamily);
    atom_eagain          = MKA(env, str_eagain);
    atom_ebadflags       = MKA(env, str_ebadflags);
    atom_efail           = MKA(env, str_efail);
    atom_efamily         = MKA(env, str_efamily);
    atom_einval          = MKA(env, str_einval);
    // atom_eisconn      = MKA(env, str_eisconn);
    atom_emem            = MKA(env, str_emem);
    atom_enodata         = MKA(env, str_enodata);
    atom_enoname         = MKA(env, str_enoname);
    // atom_enotclosing  = MKA(env, str_enotclosing);
    // atom_enotconn     = MKA(env, str_enotconn);
    atom_eoverflow       = MKA(env, str_eoverflow);
    atom_eservice        = MKA(env, str_eservice);
    atom_esocktype       = MKA(env, str_esocktype);
    atom_esystem         = MKA(env, str_esystem);
    // atom_exalloc      = MKA(env, str_exalloc);
    // atom_exbadstate   = MKA(env, str_exbadstate);
    // atom_exbusy       = MKA(env, str_exbusy);
    // atom_exnotopen    = MKA(env, str_exnotopen);
    // atom_exmon        = MKA(env, str_exmon);
    // atom_exself       = MKA(env, str_exself);
    // atom_exsend       = MKA(env, str_exsend);

    // For storing "global" things...
    // socketData.env       = enif_alloc_env(); // We should really check
    // socketData.version   = MKA(env, ERTS_VERSION);
    // socketData.buildDate = MKA(env, ERTS_BUILD_DATE);

    net = enif_open_resource_type_x(env,
                                    "net",
                                    &netInit,
                                    ERL_NIF_RT_CREATE,
                                    NULL);

    // NDBG( ("on_load -> done\r\n") );

    return !net;
}

ERL_NIF_INIT(net, net_funcs, on_load, NULL, NULL, NULL)
