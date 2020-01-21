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
 *  Purpose : The NIF (C) part of the net interface
 *            This is a module of miscellaneous functions.
 *
 *            We (try to) avoid name clashes by prefixing "all" internal
 *            function names with enet.
 * ----------------------------------------------------------------------
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
#elif defined(__PASE__)
/* PASE has this, but under a different name because AIX doesn't have it. */
#include <as400_protos.h>
/*
 * We don't redefine the function names because they're used in other
 * contexts, but the struct is safe to rename.
 */
#define ifaddrs ifaddrs_pase
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

#ifndef WANT_NONBLOCKING
#define WANT_NONBLOCKING
#endif
#include "sys.h"

#endif

#include <erl_nif.h>

#include "socket_dbg.h"
#include "socket_int.h"
#include "socket_tarray.h"
#include "socket_util.h"


/* All platforms fail on malloc errors. */
#define FATAL_MALLOC


#ifdef __WIN32__
#define net_gethostname(__buf__, __bufSz__) gethostname((__buf__), (__bufSz__))
#else
#define net_gethostname(__buf__, __bufSz__) gethostname((__buf__), (__bufSz__))
#endif // __WIN32__



/* *** Misc macros and defines *** */

#ifdef __WIN32__
#define get_errno() WSAGetLastError()
#else
#define get_errno() errno
#endif


#define HOSTNAME_LEN 256
#define SERVICE_LEN  256


/* MAXHOSTNAMELEN could be 64 or 255 depending
 * on the platform. Instead, use INET_MAXHOSTNAMELEN
 * which is always 255 across all platforms
 */
#define NET_MAXHOSTNAMELEN 255


/* =================================================================== *
 *                                                                     *
 *                        Various enif macros                          *
 *                                                                     *
 * =================================================================== */


#ifdef HAVE_SOCKLEN_T
#  define SOCKLEN_T socklen_t
#else
#  define SOCKLEN_T size_t
#endif

/* Debug stuff... */
#define NET_NIF_DEBUG_DEFAULT FALSE

#define NDBG( proto ) ESOCK_DBG_PRINTF( data.debug , proto )


typedef struct {
    BOOLEAN_T debug;
} NetData;



/* =================================================================== *
 *                                                                     *
 *                           Static data                               *
 *                                                                     *
 * =================================================================== */


static NetData data;



/* ----------------------------------------------------------------------
 *  F o r w a r d s
 * ----------------------------------------------------------------------
 */

/* THIS IS JUST TEMPORARY */
extern char* erl_errno_id(int error);

/* All the nif "callback" functions for the net API has
 * the exact same API:
 *
 * nif_<funcname>(ErlNifEnv*         env,
 *                int                argc,
 *                const ERL_NIF_TERM argv[]);
 *
 * So, to simplify, use some macro magic to define those.
 *
 * These are the functions making up the "official" API.
 */

#define ENET_NIF_FUNCS                  \
    ENET_NIF_FUNC_DEF(info);            \
    ENET_NIF_FUNC_DEF(command);         \
    ENET_NIF_FUNC_DEF(gethostname);     \
    ENET_NIF_FUNC_DEF(getnameinfo);     \
    ENET_NIF_FUNC_DEF(getaddrinfo);     \
    ENET_NIF_FUNC_DEF(getifaddrs);      \
    ENET_NIF_FUNC_DEF(if_name2index);   \
    ENET_NIF_FUNC_DEF(if_index2name);   \
    ENET_NIF_FUNC_DEF(if_names);

#define ENET_NIF_FUNC_DEF(F)                              \
    static ERL_NIF_TERM nif_##F(ErlNifEnv*         env,    \
                                int                argc,   \
                                const ERL_NIF_TERM argv[]);
ENET_NIF_FUNCS
#undef ENET_NIF_FUNC_DEF


/* And here comes the functions that does the actual work (for the most part) */
static ERL_NIF_TERM enet_command(ErlNifEnv*   env,
                                 ERL_NIF_TERM cmd);
static ERL_NIF_TERM enet_gethostname(ErlNifEnv* env);
static ERL_NIF_TERM enet_getnameinfo(ErlNifEnv*          env,
                                     const ESockAddress* saP,
                                     SOCKLEN_T           saLen,
                                     int                 flags);
static ERL_NIF_TERM enet_getaddrinfo(ErlNifEnv* env,
                                     char*      host,
                                     char*      serv);
static ERL_NIF_TERM enet_getifaddrs(ErlNifEnv* env,
                                    char*      netns);
static ERL_NIF_TERM enet_if_name2index(ErlNifEnv* env,
                                       char*      ifn);
static ERL_NIF_TERM enet_if_index2name(ErlNifEnv*   env,
                                       unsigned int id);
static ERL_NIF_TERM enet_if_names(ErlNifEnv* env);
static unsigned int enet_if_names_length(struct if_nameindex* p);

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

static ERL_NIF_TERM enet_getifaddrs(ErlNifEnv* env,
                                    char*      netns);
static ERL_NIF_TERM enet_getifaddrs_process(ErlNifEnv*      env,
                                            struct ifaddrs* ifap);
static unsigned int enet_getifaddrs_length(struct ifaddrs* ifap);
static BOOLEAN_T encode_ifaddrs(ErlNifEnv*      env,
                                struct ifaddrs* ifap,
                                ERL_NIF_TERM*   eifa);
static ERL_NIF_TERM encode_ifaddrs_name(ErlNifEnv* env,
                                        char*      name);
static ERL_NIF_TERM encode_ifaddrs_flags(ErlNifEnv*   env,
                                         unsigned int flags);
static ERL_NIF_TERM encode_ifaddrs_addr(ErlNifEnv*       env,
                                        struct sockaddr* sa);
static char* make_ifaddrs(ErlNifEnv*    env,
                          ERL_NIF_TERM  name,
                          ERL_NIF_TERM  flags,
                          ERL_NIF_TERM  addr,
                          ERL_NIF_TERM  netmask,
                          ERL_NIF_TERM  ifu_key,
                          ERL_NIF_TERM  ifu_value,
                          ERL_NIF_TERM  data,
                          ERL_NIF_TERM* ifAddrs);
#ifdef HAVE_SETNS
static BOOLEAN_T enet_getifaddrs_netns(ErlNifEnv*   env,
                                       ERL_NIF_TERM map,
                                       char**       netns);
static BOOLEAN_T change_network_namespace(char* netns, int* cns, int* err);
static BOOLEAN_T restore_network_namespace(int ns, int* err);
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
static ERL_NIF_TERM encode_address_infos(ErlNifEnv*       env,
                                         struct addrinfo* addrInfo);
static ERL_NIF_TERM encode_address_info(ErlNifEnv*       env,
                                        struct addrinfo* addrInfoP);
static unsigned int address_info_length(struct addrinfo* addrInfoP);

static ERL_NIF_TERM encode_address_info_family(ErlNifEnv* env,
                                         int        family);
static ERL_NIF_TERM encode_address_info_type(ErlNifEnv* env,
                                       int        socktype);
static ERL_NIF_TERM encode_address_info_proto(ErlNifEnv* env,
                                        int        proto);

static char* make_address_info(ErlNifEnv*    env,
                               ERL_NIF_TERM  fam,
                               ERL_NIF_TERM  sockType,
                               ERL_NIF_TERM  proto,
                               ERL_NIF_TERM  addr,
                               ERL_NIF_TERM* ai);

static BOOLEAN_T extract_debug(ErlNifEnv*   env,
                               ERL_NIF_TERM map);
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



/* *** Local atoms ***
 *
 * These have been deprecated:
 *     LOCAL_ATOM_DECL(idna_allow_unassigned);     Should really have been idn_...
 *     LOCAL_ATOM_DECL(idna_use_std3_ascii_rules); Should really have been idn_...
 */


#define LOCAL_ATOMS                             \
    LOCAL_ATOM_DECL(address_info);              \
    LOCAL_ATOM_DECL(automedia);                 \
    LOCAL_ATOM_DECL(broadaddr);                 \
    LOCAL_ATOM_DECL(broadcast);                 \
    LOCAL_ATOM_DECL(debug);                     \
    LOCAL_ATOM_DECL(dstaddr);                   \
    LOCAL_ATOM_DECL(dynamic);                   \
    LOCAL_ATOM_DECL(host);                      \
    LOCAL_ATOM_DECL(idn);                       \
    LOCAL_ATOM_DECL(master);                    \
    LOCAL_ATOM_DECL(multicast);                 \
    LOCAL_ATOM_DECL(name);                      \
    LOCAL_ATOM_DECL(namereqd);                  \
    LOCAL_ATOM_DECL(name_info);                 \
    LOCAL_ATOM_DECL(netmask);                   \
    LOCAL_ATOM_DECL(noarp);                     \
    LOCAL_ATOM_DECL(nofqdn);                    \
    LOCAL_ATOM_DECL(notrailers);                \
    LOCAL_ATOM_DECL(numerichost);               \
    LOCAL_ATOM_DECL(numericserv);               \
    LOCAL_ATOM_DECL(pointopoint);               \
    LOCAL_ATOM_DECL(portsel);                   \
    LOCAL_ATOM_DECL(promisc);                   \
    LOCAL_ATOM_DECL(running);                   \
    LOCAL_ATOM_DECL(service);                   \
    LOCAL_ATOM_DECL(slave);                     \
    LOCAL_ATOM_DECL(up);

#define LOCAL_ERROR_REASON_ATOMS               \
    LOCAL_ATOM_DECL(eaddrfamily);              \
    LOCAL_ATOM_DECL(ebadflags);                \
    LOCAL_ATOM_DECL(efail);                    \
    LOCAL_ATOM_DECL(efamily);                  \
    LOCAL_ATOM_DECL(efault);                   \
    LOCAL_ATOM_DECL(emem);                     \
    LOCAL_ATOM_DECL(enametoolong);             \
    LOCAL_ATOM_DECL(enodata);                  \
    LOCAL_ATOM_DECL(enoname);                  \
    LOCAL_ATOM_DECL(enxio);                    \
    LOCAL_ATOM_DECL(eoverflow);                \
    LOCAL_ATOM_DECL(eservice);                 \
    LOCAL_ATOM_DECL(esocktype);                \
    LOCAL_ATOM_DECL(esystem);

#define LOCAL_ATOM_DECL(A) static ERL_NIF_TERM atom_##A
LOCAL_ATOMS
LOCAL_ERROR_REASON_ATOMS
#undef LOCAL_ATOM_DECL


/* *** net *** */
static ErlNifResourceType*    net;
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
 * nif_info/0
 * nif_command/1
 *
 * The "proper" net functions:
 * ------------------------------
 * nif_gethostname/0
 * nif_getnameinfo/2
 * nif_getaddrinfo/3
 * nif_getifaddrs/1
 * nif_if_name2index/1
 * nif_if_index2name/1
 * nif_if_names/0
 *
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
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM info, tmp;

    NDBG( ("NET", "info -> entry\r\n") );

    tmp  = enif_make_new_map(env);
    if (!enif_make_map_put(env, tmp, atom_debug, BOOL2ATOM(data.debug), &info))
        info = tmp;

    NDBG( ("NET", "info -> done: %T\r\n", info) );

    return info;
#endif
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
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM ecmd, result;

    NDBG( ("NET", "command -> entry (%d)\r\n", argc) );

    if (argc != 1)
        return enif_make_badarg(env);

    ecmd = argv[0];

    NDBG( ("NET", "command -> ecmd: %T\r\n", ecmd) );

    result = enet_command(env, ecmd);

    NDBG( ("NET", "command -> result: %T\r\n", result) );

    return result;
#endif
}



/*
 * The command can, in principle, be anything, though currently we only
 * support a debug command.
 */
#if !defined(__WIN32__)
static
ERL_NIF_TERM enet_command(ErlNifEnv*   env,
                          ERL_NIF_TERM cmd)
{
    const ERL_NIF_TERM* t;
    int                 tsz;

    if (IS_TUPLE(env, cmd)) {
        /* Could be the debug tuple */
        if (!GET_TUPLE(env, cmd, &tsz, &t))
            return esock_make_error(env, esock_atom_einval);

        if (tsz != 2)
            return esock_make_error(env, esock_atom_einval);

        /* First element should be the atom 'debug' */
        if (COMPARE(t[0], atom_debug) != 0)
            return esock_make_error(env, esock_atom_einval);

        return decode_bool(env, t[1], &data.debug);

    } else {
        return esock_make_error(env, esock_atom_einval);
    }

}
#endif



/* ----------------------------------------------------------------------
 * nif_gethostname
 *
 * Description:
 * Access the hostname of the current processor.
 *
 */
static
ERL_NIF_TERM nif_gethostname(ErlNifEnv*         env,
                             int                argc,
                             const ERL_NIF_TERM argv[])
{
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM result;
    
    NDBG( ("NET", "nif_gethostname -> entry (%d)\r\n", argc) );

    if (argc != 0)
        return enif_make_badarg(env);

    result = enet_gethostname(env);

    NDBG( ("NET", "nif_gethostname -> done when result: %T\r\n", result) );

    return result;
#endif
}


#if !defined(__WIN32__)
static
ERL_NIF_TERM enet_gethostname(ErlNifEnv* env)
{
    ERL_NIF_TERM result;
    char         buf[NET_MAXHOSTNAMELEN + 1];
    int          res;

    res = net_gethostname(buf, sizeof(buf));

    NDBG( ("NET", "enet_gethostname -> gethostname res: %d\r\n", res) );

    switch (res) {
    case 0:
        result = esock_make_ok2(env, MKS(env, buf));
        break;

    case EFAULT:
        result = esock_make_error(env, atom_efault);
        break;

    case EINVAL:
        result = esock_make_error(env, esock_atom_einval);
        break;

    case ENAMETOOLONG:
        result = esock_make_error(env, atom_enametoolong);
        break;

    default:
        result = esock_make_error(env, MKI(env, res));
        break;
    }

    return result;
}
#endif




/* ----------------------------------------------------------------------
 * nif_getnameinfo
 *
 * Description:
 * Address-to-name translation in protocol-independent manner.
 *
 * Arguments:
 * SockAddr - Socket Address (address and port)
 * Flags    - The flags argument modifies the behavior of getnameinfo().
 */

static
ERL_NIF_TERM nif_getnameinfo(ErlNifEnv*         env,
                             int                argc,
                             const ERL_NIF_TERM argv[])
{
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM result;
    ERL_NIF_TERM eSockAddr, eFlags;
    int          flags = 0; // Just in case...
    ESockAddress sa;
    SOCKLEN_T    saLen = 0; // Just in case...
    char*        xres;

    NDBG( ("NET", "nif_getnameinfo -> entry (%d)\r\n", argc) );

    if (argc != 2)
        return enif_make_badarg(env);
    eSockAddr = argv[0];
    eFlags    = argv[1];

    NDBG( ("NET",
           "nif_getnameinfo -> "
           "\r\n   SockAddr: %T"
           "\r\n   Flags:    %T"
           "\r\n", eSockAddr, eFlags) );

    if ((xres = esock_decode_sockaddr(env, eSockAddr, &sa, &saLen)) != NULL) {
        NDBG( ("NET", "nif_getnameinfo -> failed decode sockaddr: %s\r\n", xres) );
        return esock_make_error_str(env, xres);
    }

    NDBG( ("NET", "nif_getnameinfo -> (try) decode flags\r\n") );

    if (!decode_nameinfo_flags(env, eFlags, &flags))
        return enif_make_badarg(env);

    result = enet_getnameinfo(env, &sa, saLen, flags);

    NDBG( ("NET",
           "nif_getnameinfo -> done when result: "
           "\r\n   %T\r\n", result) );

    return result;
#endif
}



/* Given the provided sock(et) address (and flags), retreive the host and
 * service info.
 */
#if !defined(__WIN32__)
static
ERL_NIF_TERM enet_getnameinfo(ErlNifEnv*          env,
                              const ESockAddress* saP,
                              SOCKLEN_T           saLen,
                              int                 flags)
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

    NDBG( ("NET", "enet_getnameinfo -> res: %d\r\n", res) );

    switch (res) {
    case 0:
        {
            ERL_NIF_TERM keys[] = {atom_host,      atom_service};
            ERL_NIF_TERM vals[] = {MKS(env, host), MKS(env, serv)};
            ERL_NIF_TERM info;
            unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);
            unsigned int numVals = sizeof(vals) / sizeof(ERL_NIF_TERM);

            ESOCK_ASSERT( (numKeys == numVals) );

            if (!MKMA(env, keys, vals, numKeys, &info))
                return enif_make_badarg(env);

            result = esock_make_ok2(env, info);
        }
        break;

#if defined(EAI_AGAIN)
    case EAI_AGAIN:
        result = esock_make_error(env, esock_atom_eagain);
        break;
#endif

#if defined(EAI_BADFLAGS)
    case EAI_BADFLAGS:
        result = esock_make_error(env, atom_ebadflags);
        break;
#endif

#if defined(EAI_FAIL)
    case EAI_FAIL:
        result = esock_make_error(env, atom_efail);
        break;
#endif

#if defined(EAI_FAMILY)
    case EAI_FAMILY:
        result = esock_make_error(env, atom_efamily);
        break;
#endif

#if defined(EAI_MEMORY)
    case EAI_MEMORY:
        result = esock_make_error(env, atom_emem);
        break;
#endif

#if defined(EAI_NONAME)
    case EAI_NONAME:
        result = esock_make_error(env, atom_enoname);
        break;
#endif

#if defined(EAI_OVERFLOW)
    case EAI_OVERFLOW:
        result = esock_make_error(env, atom_eoverflow);
        break;
#endif

#if defined(EAI_SYSTEM)
    case EAI_SYSTEM:
        result = esock_make_error_errno(env, get_errno());
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}
#endif



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
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM     result, eHostName, eServName; //, eHints;
    char*            hostName;
    char*            servName;
    // struct addrinfo* hints;

    NDBG( ("NET", "nif_getaddrinfo -> entry (%d)\r\n", argc) );

    if (argc != 3) {
        return enif_make_badarg(env);
    }
    eHostName = argv[0];
    eServName = argv[1];
    // eHints    = argv[2];

    NDBG( ("NET",
           "nif_getaddrinfo -> "
           "\r\n   ehost:    %T"
           "\r\n   eservice: %T"
           "\r\n   ehints:   %T"
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

    result = enet_getaddrinfo(env, hostName, servName);

    if (hostName != NULL)
        FREE(hostName);

    if (servName != NULL)
        FREE(servName);

    /*
      if (hints != NULL)
      FREE(hints);
    */

    NDBG( ("NET",
           "nif_getaddrinfo -> done when result: "
           "\r\n   %T\r\n", result) );

    return result;
#endif
}


#if !defined(__WIN32__)
static
ERL_NIF_TERM enet_getaddrinfo(ErlNifEnv* env,
                              char*      host,
                              char*      serv)
{
    ERL_NIF_TERM     result;
    struct addrinfo* addrInfoP;
    int              res;

    NDBG( ("NET", "enet_getaddrinfo -> entry with"
           "\r\n   host: %s"
           "\r\n   serv: %s"
           "\r\n",
           ((host == NULL) ? "NULL" : host),
           ((serv == NULL) ? "NULL" : serv)) );
    
    res = getaddrinfo(host, serv, NULL, &addrInfoP);

    NDBG( ("NET", "enet_getaddrinfo -> res: %d\r\n", res) );
    
    switch (res) {
    case 0:
        {
            ERL_NIF_TERM addrInfo = encode_address_infos(env, addrInfoP);
            freeaddrinfo(addrInfoP);
            result = esock_make_ok2(env, addrInfo);
        }
        break;

#if defined(EAI_ADDRFAMILY)
    case EAI_ADDRFAMILY:
        result = esock_make_error(env, atom_eaddrfamily);
        break;
#endif

#if defined(EAI_AGAIN)
    case EAI_AGAIN:
        result = esock_make_error(env, esock_atom_eagain);
        break;
#endif

#if defined(EAI_BADFLAGS)
    case EAI_BADFLAGS:
        result = esock_make_error(env, atom_ebadflags);
        break;
#endif

#if defined(EAI_FAIL)
    case EAI_FAIL:
        result = esock_make_error(env, atom_efail);
        break;
#endif

#if defined(EAI_FAMILY)
    case EAI_FAMILY:
        result = esock_make_error(env, atom_efamily);
        break;
#endif

#if defined(EAI_MEMORY)
    case EAI_MEMORY:
        result = esock_make_error(env, atom_emem);
        break;
#endif

#if defined(EAI_NODATA)
    case EAI_NODATA:
        result = esock_make_error(env, atom_enodata);
        break;
#endif

#if defined(EAI_NONAME)
    case EAI_NONAME:
        result = esock_make_error(env, atom_enoname);
        break;
#endif

#if defined(EAI_SERVICE)
    case EAI_SERVICE:
        result = esock_make_error(env, atom_eservice);
        break;
#endif

#if defined(EAI_SOCKTYPE)
    case EAI_SOCKTYPE:
        result = esock_make_error(env, atom_esocktype);
        break;
#endif

#if defined(EAI_SYSTEM)
    case EAI_SYSTEM:
        result = esock_make_error(env, atom_esystem);
        break;
#endif

    default:
        result = esock_make_error(env, esock_atom_einval);
        break;
    }

    return result;
}
#endif



/* ----------------------------------------------------------------------
 * nif_getifaddrs
 *
 * Description:
 * Get interface addresses
 *
 * Arguments:
 * Extra - A way to pass 'extra' arguments.
 *         Currently only used for netns (name space).
 */

static
ERL_NIF_TERM nif_getifaddrs(ErlNifEnv*         env,
                            int                argc,
                            const ERL_NIF_TERM argv[])
{
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#elif defined(HAVE_GETIFADDRS) || defined(__PASE__)
    ERL_NIF_TERM extra;
    char*        netns;
    ERL_NIF_TERM result;

    NDBG( ("NET", "nif_getifaddrs -> entry (%d)\r\n", argc) );

    if ((argc != 1) ||
        !IS_MAP(env,  argv[0])) {
        return enif_make_badarg(env);
    }
    extra = argv[0];

#ifdef HAVE_SETNS
    /* We *currently* only support one extra option: netns */
    if (!enet_getifaddrs_netns(env, extra, &netns)) {
        NDBG( ("NET", "nif_getifaddrs -> namespace: %s\r\n", netns) );
        return enif_make_badarg(env);
    }
#else
    netns = NULL;
#endif

    result = enet_getifaddrs(env, netns);

    NDBG( ("NET",
           "nif_getifaddrs -> done when result: "
           "\r\n   %T\r\n", result) );

    return result;
#else // HAVE_GETIFADDRS
    return esock_make_error(env, esock_atom_enotsup);
#endif
}


#if defined(HAVE_GETIFADDRS) || defined(__PASE__)
#ifdef HAVE_SETNS
/* enet_getifaddrs_netns - extract the netns field from the 'extra' map
 *
 * Note that the 'extra' map *may* contain other options, but here we
 * only care about 'netns'.
 */
static
BOOLEAN_T enet_getifaddrs_netns(ErlNifEnv* env, ERL_NIF_TERM map, char** netns)
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

    /* Regardless of the content of the 'extra' map, we only care about 'netns' */
    key = enif_make_atom(env, "netns");
    if (!GET_MAP_VAL(env, map, key, &value)) {
        *netns = NULL;
        return TRUE;
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



static
ERL_NIF_TERM enet_getifaddrs(ErlNifEnv* env, char* netns)
{
    ERL_NIF_TERM    result;
    struct ifaddrs* ifap;
    int             save_errno;
#ifdef HAVE_SETNS
    int             current_ns = 0;
#endif

    NDBG( ("NET", "enet_getifaddrs -> entry with"
           "\r\n   netns: %s"
           "\r\n", ((netns == NULL) ? "NULL" : netns)) );

#ifdef HAVE_SETNS
    if ((netns != NULL) &&
        !change_network_namespace(netns, &current_ns, &save_errno))
        return esock_make_error_errno(env, save_errno);
#endif

#ifdef __PASE__
    if (0 == Qp2getifaddrs(&ifap)) {
#else
    if (0 == getifaddrs(&ifap)) {
#endif
        result = enet_getifaddrs_process(env, ifap);
#ifdef __PASE__
        Qp2freeifaddrs(ifap);
#else
        freeifaddrs(ifap);
#endif
    } else {
        save_errno = get_errno();

        NDBG( ("NET", "enet_getifaddrs -> failed get addrs: %d", save_errno) );

        result = esock_make_error_errno(env, save_errno);
    }


#ifdef HAVE_SETNS
    if ((netns != NULL) &&
        !restore_network_namespace(current_ns, &save_errno))
        return esock_make_error_errno(env, save_errno);

    if (netns != NULL)
        FREE(netns);
#endif

    NDBG( ("NET", "enet_getifaddrs -> done when"
           "\r\n   result: %T"
           "\r\n", result) );

    return result;
}


static
ERL_NIF_TERM enet_getifaddrs_process(ErlNifEnv* env, struct ifaddrs* ifap)
{
    ERL_NIF_TERM result;
    unsigned int len = ((ifap == NULL) ? 0 : enet_getifaddrs_length(ifap));

    NDBG( ("NET", "enet_getifaddrs_process -> len: %d\r\n", len) );

    if (len > 0) {
        ERL_NIF_TERM*   array = MALLOC(len * sizeof(ERL_NIF_TERM));
        unsigned int    i     = 0;
        struct ifaddrs* p     = ifap;

        while (i < len) {
            ERL_NIF_TERM entry;

            if (!encode_ifaddrs(env, p, &entry)) {
                /* If this fail, we are f*ed, so give up */
                FREE(array);
                return esock_make_error(env, esock_atom_einval);
            }

            NDBG( ("NET", "enet_getifaddrs_process -> entry: %T\r\n", entry) );

            array[i] = entry;
            p = p->ifa_next;
            i++;
        }

        NDBG( ("NET", "enet_getifaddrs_process -> all entries processed\r\n") );

        result = esock_make_ok2(env, MKLA(env, array, len));
        FREE(array);        

    } else {
        result = esock_make_ok2(env, MKEL(env));
    }

    NDBG( ("NET", "enet_getifaddrs_process -> result: "
           "\r\n   %T\r\n", result) );

    return result;
}



/* Calculate the length of the interface adress linked list
 * The list is NULL-terminated, so the only way is to
 * iterate through the list until we find next = NULL.
 */
static
unsigned int enet_getifaddrs_length(struct ifaddrs* ifap)
{
    unsigned int    len = 1;
    struct ifaddrs* tmp;
    BOOLEAN_T       done = FALSE;

    tmp = ifap;

    while (!done) {
        if (tmp->ifa_next != NULL) {
            len++;
            tmp = tmp->ifa_next;
        } else {
            done = TRUE;
        }
    }

    return len;
}



static
BOOLEAN_T encode_ifaddrs(ErlNifEnv*      env,
                         struct ifaddrs* ifap,
                         ERL_NIF_TERM*   eifa)
{
    char*        xres;
    ERL_NIF_TERM ename, eflags, eaddr, enetmask, eifu_key, eifu_value, edata;
    ERL_NIF_TERM eifAddrs;

    ename     = encode_ifaddrs_name(env,  ifap->ifa_name);
    NDBG( ("NET", "encode_ifaddrs -> name: %T\r\n", ename) );
    eflags    = encode_ifaddrs_flags(env, ifap->ifa_flags);
    NDBG( ("NET", "encode_ifaddrs -> flags: %T\r\n", eflags) );
    eaddr     = encode_ifaddrs_addr(env,  ifap->ifa_addr);
    NDBG( ("NET", "encode_ifaddrs -> addr: %T\r\n", eaddr) );
    enetmask  = encode_ifaddrs_addr(env,  ifap->ifa_netmask);
    NDBG( ("NET", "encode_ifaddrs -> netmask: %T\r\n", enetmask) );
    if (ifap->ifa_dstaddr && (ifap->ifa_flags & IFF_POINTOPOINT)) {
        eifu_key   = atom_dstaddr;
        eifu_value = encode_ifaddrs_addr(env, ifap->ifa_dstaddr);
    } else if (ifap->ifa_broadaddr && (ifap->ifa_flags & IFF_BROADCAST)) {
        eifu_key   = atom_broadaddr;
        eifu_value = encode_ifaddrs_addr(env, ifap->ifa_broadaddr);
    } else {
        eifu_key   = esock_atom_undefined;
        eifu_value = esock_atom_undefined;
    }
    NDBG( ("NET", "encode_ifaddrs -> ifu: "
            "\r\n   key: %T"
            "\r\n   val: %T"
            "\r\n", eifu_key, eifu_value) );
    /* Don't know how to encode this yet...
     * We don't even know the size...
     */
    edata = esock_atom_undefined;

    if ((xres = make_ifaddrs(env,
                             ename, eflags, eaddr, enetmask,
                             eifu_key, eifu_value, edata,
                             &eifAddrs)) == NULL) {
        NDBG( ("NET", "encode_ifaddrs -> encoded ifAddrs: %T\r\n", eifAddrs) );
        *eifa = eifAddrs;

        return TRUE;

    } else {

        /* *** Ouch ***
         * 
         * Failed to construct the ifaddrs map => try for a minumum map 
         * 
         */
        ERL_NIF_TERM keys[]  = {atom_name};
        ERL_NIF_TERM vals[]  = {ename};
        unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);

        esock_warning_msg("Failed constructing IfAddr map for %s: %s",
                          ifap->ifa_name, xres);

        if (!MKMA(env, keys, vals, numKeys, eifa)) {
            // We are in a bad way, give up */
            *eifa = esock_atom_undefined;
            return FALSE;
        } else {
            return TRUE;
        }  

    }
}



static
ERL_NIF_TERM encode_ifaddrs_name(ErlNifEnv* env, char* name)
{
    return ((name == NULL) ? esock_atom_undefined : MKS(env, name));
}



static
ERL_NIF_TERM encode_ifaddrs_flags(ErlNifEnv* env, unsigned int flags)
{
    SocketTArray ta = TARRAY_CREATE(16);
    ERL_NIF_TERM eflags;

#if defined(IFF_UP)
    if (flags & IFF_UP)
        TARRAY_ADD(ta, atom_up);
#endif

#if defined(IFF_BROADCAST)
    if (flags & IFF_BROADCAST)
        TARRAY_ADD(ta, atom_broadcast);
#endif

#if defined(IFF_DEBUG)
    if (flags & IFF_DEBUG)
        TARRAY_ADD(ta, atom_debug);
#endif

#if defined(IFF_LOOPBACK)
    if (flags & IFF_LOOPBACK)
        TARRAY_ADD(ta, esock_atom_loopback);
#endif

#if defined(IFF_POINTOPOINT)
    if (flags & IFF_POINTOPOINT)
        TARRAY_ADD(ta, atom_pointopoint);
#endif

#if defined(IFF_NOTRAILERS)
    if (flags & IFF_NOTRAILERS)
        TARRAY_ADD(ta, atom_notrailers);
#endif

#if defined(IFF_RUNNING)
    if (flags & IFF_RUNNING)
        TARRAY_ADD(ta, atom_running);
#endif

#if defined(IFF_NOARP)
    if (flags & IFF_NOARP)
        TARRAY_ADD(ta, atom_noarp);
#endif

#if defined(IFF_PROMISC)
    if (flags & IFF_PROMISC)
        TARRAY_ADD(ta, atom_promisc);
#endif

#if defined(IFF_MASTER)
    if (flags & IFF_MASTER)
        TARRAY_ADD(ta, atom_master);
#endif

#if defined(IFF_SLAVE)
    if (flags & IFF_SLAVE)
        TARRAY_ADD(ta, atom_slave);
#endif

#if defined(IFF_MULTICAST)
    if (flags & IFF_MULTICAST)
        TARRAY_ADD(ta, atom_multicast);
#endif

#if defined(IFF_PORTSEL)
    if (flags & IFF_PORTSEL)
        TARRAY_ADD(ta, atom_portsel);
#endif

#if defined(IFF_AUTOMEDIA)
    if (flags & IFF_AUTOMEDIA)
        TARRAY_ADD(ta, atom_automedia);
#endif

#if defined(IFF_DYNAMIC)
    if (flags & IFF_DYNAMIC)
        TARRAY_ADD(ta, atom_dynamic);
#endif

    TARRAY_TOLIST(ta, env, &eflags);

    return eflags;
}


static
ERL_NIF_TERM encode_ifaddrs_addr(ErlNifEnv* env, struct sockaddr* sa)
{
    ERL_NIF_TERM esa;

    if (sa != NULL) {
        unsigned int sz = sizeof(ESockAddress);
        if (esock_encode_sockaddr(env, (ESockAddress*) sa, sz, &esa) != NULL) {
            NDBG( ("NET", "encode_ifaddrs_addr -> "
                   "encoding address field 0x%lX failed => use default\r\n", sa) );
            esa = esock_atom_undefined;
        }
    } else {
        esa = esock_atom_undefined;
    }

    return esa;
}


static
char* make_ifaddrs(ErlNifEnv*    env,
                   ERL_NIF_TERM  ename,
                   ERL_NIF_TERM  eflags,
                   ERL_NIF_TERM  eaddr,
                   ERL_NIF_TERM  enetmask,
                   ERL_NIF_TERM  eifu_key,
                   ERL_NIF_TERM  eifu_value,
                   ERL_NIF_TERM  edata,
                   ERL_NIF_TERM* eifAddrs)
{
    /* Several of these values can be (the atom) undefined, which
     * means that they should *not* be included in the result map.
     */
    ERL_NIF_TERM keys[6]; // There are only (at most) siz (6) fields...
    ERL_NIF_TERM vals[6];
    unsigned int len = sizeof(keys) / sizeof(ERL_NIF_TERM); // Just in case...
    unsigned int idx = 0;

    /* *** Name *** */
    NDBG( ("NET", "make_ifaddrs -> name: %T\r\n", ename) );
    keys[idx] = atom_name;
    vals[idx] = ename;
    idx++;

    /* *** Flags *** */
    NDBG( ("NET", "make_ifaddrs -> flags: %T\r\n", eflags) );
    keys[idx] = esock_atom_flags;
    vals[idx] = eflags;
    idx++;

    /* *** Addr (can be 'undefined' = NULL) *** */
    NDBG( ("NET", "make_ifaddrs -> addr: %T\r\n", eaddr) );
    if (COMPARE(eaddr, esock_atom_undefined) != 0) {    
        keys[idx] = esock_atom_addr;
        vals[idx] = eaddr;
        idx++;
    } else {
        len--;
    }

    /* *** Netmask (can be 'undefined' = NULL) *** */
    NDBG( ("NET", "make_ifaddrs -> netmask: %T\r\n", enetmask) );
    if (COMPARE(enetmask, esock_atom_undefined) != 0) {    
        keys[idx] = atom_netmask;
        vals[idx] = enetmask;
        idx++;
    } else {
        len--;
    }

    /* *** Netmask (can be 'undefined' = NULL) *** */
    NDBG( ("NET", "make_ifaddrs -> ifu: %T, %T\r\n", eifu_key, eifu_value) );
    if ((COMPARE(eifu_key, esock_atom_undefined) != 0) &&
        (COMPARE(eifu_value, esock_atom_undefined) != 0)) {
        keys[idx] = eifu_key;
        vals[idx] = eifu_value;
        idx++;
    } else {
        len--;
    }
    
    /* *** Data (can be 'undefined' = NULL) *** */
    NDBG( ("NET", "make_ifaddrs -> data: %T\r\n", edata) );
    if (COMPARE(edata, esock_atom_undefined) != 0) {    
        keys[idx] = esock_atom_data;
        vals[idx] = edata;
        idx++;
    } else {
        len--;
    }

    NDBG( ("NET", "make_ifaddrs -> construct ifa with:"
           "\r\n   len: %d"
           "\r\n"
           ) );
    if (!MKMA(env, keys, vals, len, eifAddrs)) {
        NDBG( ("NET", "make_ifaddrs -> "
               "failed contructing interface adress map\r\n") );
        *eifAddrs = esock_atom_undefined;
        return ESOCK_STR_EINVAL;
    } else {
        return NULL;
    }
}

#endif // HAVE_GETIFADDRS


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
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM eifn, result;
    char         ifn[IF_NAMESIZE+1];

    NDBG( ("NET", "nif_if_name2index -> entry (%d)\r\n", argc) );

    if (argc != 1) {
        return enif_make_badarg(env);
    }
    eifn = argv[0];

    NDBG( ("NET",
           "nif_if_name2index -> "
           "\r\n   Ifn: %T"
           "\r\n", argv[0]) );

    if (0 >= GET_STR(env, eifn, ifn, sizeof(ifn)))
        return esock_make_error(env, esock_atom_einval);

    result = enet_if_name2index(env, ifn);

    NDBG( ("NET", "nif_if_name2index -> done when result: %T\r\n", result) );

    return result;
#endif
}



#if !defined(__WIN32__)
static
ERL_NIF_TERM enet_if_name2index(ErlNifEnv* env,
                            char*      ifn)
{
    unsigned int idx;

    NDBG( ("NET", "enet_if_name2index -> entry with ifn: %s\r\n", ifn) );

    idx = if_nametoindex(ifn);

    NDBG( ("NET", "enet_if_name2index -> idx: %d\r\n", idx) );

    if (idx == 0) {
        int save_errno = get_errno();
        NDBG( ("NET", "nif_name2index -> failed: %d\r\n", save_errno) );
        return esock_make_error_errno(env, save_errno);
    } else {
         return esock_make_ok2(env, MKI(env, idx));
    }

}
#endif



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
#if defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM result;
    unsigned int idx;

    NDBG( ("NET", "nif_if_index2name -> entry (%d)\r\n", argc) );

    if ((argc != 1) ||
        !GET_UINT(env, argv[0], &idx)) {
        return enif_make_badarg(env);
    }

    NDBG( ("NET", "nif_index2name -> "
           "\r\n   Idx: %T"
           "\r\n", argv[0]) );

    result = enet_if_index2name(env, idx);

    NDBG( ("NET", "nif_if_index2name -> done when result: %T\r\n", result) );

    return result;
#endif
}



#if !defined(__WIN32__)
static
ERL_NIF_TERM enet_if_index2name(ErlNifEnv*   env,
                            unsigned int idx)
{
    ERL_NIF_TERM result;
    char*        ifn = MALLOC(IF_NAMESIZE+1);

    if (ifn == NULL)
        return enif_make_badarg(env); // PLACEHOLDER

    if (NULL != if_indextoname(idx, ifn)) {
        result = esock_make_ok2(env, MKS(env, ifn));
    } else {
        result = esock_make_error(env, atom_enxio);
    }

    FREE(ifn);

    return result;
}
#endif



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
#if defined(__WIN32__) || (defined(__ANDROID__) && (__ANDROID_API__ < 24))
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM result;

    NDBG( ("NET", "nif_if_names -> entry (%d)\r\n", argc) );

    if (argc != 0) {
        return enif_make_badarg(env);
    }

    result = enet_if_names(env);

    NDBG( ("NET", "nif_if_names -> done when result: %T\r\n", result) );

    return result;
#endif
}



/* if_nameindex and if_freenameindex were added in Android 7.0 Nougat. With
the Android NDK Unified Headers, check that the build is targeting at least
the corresponding API level 24. */
#if !defined(__WIN32__) && !(defined(__ANDROID__) && (__ANDROID_API__ < 24))
static
ERL_NIF_TERM enet_if_names(ErlNifEnv* env)
{
    ERL_NIF_TERM         result;
    struct if_nameindex* ifs = if_nameindex();

    NDBG( ("NET", "enet_if_names -> ifs: 0x%lX\r\n", ifs) );

    if (ifs == NULL) {
        result = esock_make_error_errno(env, get_errno());
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
        unsigned int len = enet_if_names_length(ifs);

        NDBG( ("NET", "enet_if_names -> len: %d\r\n", len) );

        if (len > 0) {
            ERL_NIF_TERM* array = MALLOC(len * sizeof(ERL_NIF_TERM));
            unsigned int  i;

            for (i = 0; i < len; i++) {
                array[i] = MKT2(env,
                                MKI(env, ifs[i].if_index),
                                MKS(env, ifs[i].if_name));
            }

            result = esock_make_ok2(env, MKLA(env, array, len));
            FREE(array);
        } else {
            result = esock_make_ok2(env, enif_make_list(env, 0));
        }
    }

    if (ifs != NULL)
        if_freenameindex(ifs);

    return result;
}


static
unsigned int enet_if_names_length(struct if_nameindex* p)
{
    unsigned int len = 0;
    BOOLEAN_T    done =  FALSE;

    while (!done) {

        NDBG( ("NET", "enet_if_names_length -> %d: "
               "\r\n   if_index: %d"
               "\r\n   if_name:  0x%lX"
               "\r\n", len, p[len].if_index, p[len].if_name) );

        if ((p[len].if_index == 0) && (p[len].if_name == NULL))
            done = TRUE;
        else
            len++;
    }

    return len;
}
#endif // if !defined(__WIN32__)



/* ----------------------------------------------------------------------
 *  U t i l i t y   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

/* The erlang format for a set of flags is a list of atoms.
 * A special case is when there is no flags, which is
 * represented by the atom undefined.
 */
#if !defined(__WIN32__)
static
BOOLEAN_T decode_nameinfo_flags(ErlNifEnv*         env,
                                const ERL_NIF_TERM eflags,
                                int*               flags)
{
    BOOLEAN_T result;

    if (IS_ATOM(env, eflags)) {
        NDBG( ("NET", "decode_nameinfo_flags -> is atom (%T)\r\n", eflags) );
        if (COMPARE(eflags, esock_atom_undefined) == 0) {
            *flags = 0;
            result = TRUE;
        } else {
            result = FALSE;
        }
    } else if (IS_LIST(env, eflags)) {
        NDBG( ("NET", "decode_nameinfo_flags -> is list\r\n") );
        result = decode_nameinfo_flags_list(env, eflags, flags);
    } else {
        result = FALSE;
    }

    NDBG( ("NET", "decode_nameinfo_flags -> result: %s\r\n", B2S(result)) );

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

        /*
        NDBG( ("NET", "decode_nameinfo_flags_list -> "
               "get next (list) element of"
               "\r\n   %T\r\n", list) );
        */

        if (GET_LIST_ELEM(env, list, &elem, &tail)) {

            /*
            NDBG( ("NET", "decode_nameinfo_flags_list -> got: "
                   "\r\n   element: %T"
                   "\r\n   tail:    %T"
                   "\r\n", elem, tail) );
            */

            if (COMPARE(elem, atom_namereqd) == 0) {
                tmp |= NI_NAMEREQD;
            } else if (COMPARE(elem, esock_atom_dgram) == 0) {
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

                /*
                 * In later versions of gcc these have been deprecated.
                 * That is, they results in compiler warnings.
                 * And since we "don't like that", the simplest way
                 * to deal with this is to remove the use of them.
                 * We leave them here commented out as an example.

#if defined(NI_IDN_ALLOW_UNASSIGNED)
            } else if (COMPARE(elem, atom_idna_allow_unassigned) == 0) {
                tmp |= NI_IDN_ALLOW_UNASSIGNED;
#endif

#if defined(NI_IDN_USE_STD3_ASCII_RULES)
            } else if (COMPARE(elem, atom_idna_use_std3_ascii_rules) == 0) {
                tmp |= NI_IDN_USE_STD3_ASCII_RULES;
#endif

                */

            } else {

                NDBG( ("NET", "decode_nameinfo_flags_list -> "
                       "invalid flag: %T\r\n", elem) );

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

        if (COMPARE(eString, esock_atom_undefined) == 0) {
            *stringP = NULL;
            result   = TRUE;
        } else {
            *stringP = NULL;
            result   = FALSE;
        }

    } else {

        result = esock_decode_string(env, eString, stringP);

    }

    return result;

}



static
ERL_NIF_TERM decode_bool(ErlNifEnv*   env,
                         ERL_NIF_TERM eBool,
                         BOOLEAN_T*   bool)
{
    if (COMPARE(eBool, esock_atom_true) == 0) {
        *bool = TRUE;
        return esock_atom_ok;
    } else if (COMPARE(eBool, esock_atom_false) == 0) {
        *bool = FALSE;
        return esock_atom_ok;
    } else {
        return esock_make_error(env, esock_atom_einval);
    }
}



/* Encode the address info
 * The address info is a linked list och address info, which
 * will result in the result being a list of zero or more length.
 */
static
ERL_NIF_TERM encode_address_infos(ErlNifEnv*       env,
                                  struct addrinfo* addrInfo)
{
    ERL_NIF_TERM result;
    unsigned int len = address_info_length(addrInfo);

    NDBG( ("NET", "encode_address_infos -> len: %d\r\n", len) );

    if (len > 0) {
        ERL_NIF_TERM*    array = MALLOC(len * sizeof(ERL_NIF_TERM));
        unsigned int     i     = 0;
        struct addrinfo* p     = addrInfo;

        while (i < len) {
            array[i] = encode_address_info(env, p);
            p = p->ai_next;
            i++;
        }

        result = MKLA(env, array, len);
        FREE(array);
    } else {
        result = MKEL(env);
    }

    NDBG( ("NET", "encode_address_infos -> result: "
           "\r\n   %T\r\n", result) );

    return result;
}



/* Calculate the length of the adress info linked list
 * The list is NULL-terminated, so the only way is to
 * iterate through the list until we find next = NULL.
 */
static
unsigned int address_info_length(struct addrinfo* addrInfoP)
{
    unsigned int     len = 1;
    struct addrinfo* tmp;
    BOOLEAN_T        done = FALSE;

    tmp = addrInfoP;

    while (!done) {
        if (tmp->ai_next != NULL) {
            len++;
            tmp = tmp->ai_next;
        } else {
            done = TRUE;
        }
    }

    return len;
}



/* Create one (erlang) instance of the address info record
 * Should we have address info as a record or as a map?
 *
 * {address_info, Fam, Type, Proto, Addr}
 */
static
ERL_NIF_TERM encode_address_info(ErlNifEnv*       env,
                                 struct addrinfo* addrInfoP)
{
    ERL_NIF_TERM fam, type, proto, addr, addrInfo;

    fam   = encode_address_info_family(env, addrInfoP->ai_family);
    type  = encode_address_info_type(env,   addrInfoP->ai_socktype);
    proto = encode_address_info_proto(env,  addrInfoP->ai_protocol);
    esock_encode_sockaddr(env,
                          (ESockAddress*) addrInfoP->ai_addr,
                          addrInfoP->ai_addrlen,
                          &addr);
    
    if (make_address_info(env, fam, type, proto, addr, &addrInfo) == NULL)
        return addrInfo;
    else
        return esock_atom_undefined; // We should do better...

}


/* Convert an "native" family to an erlang family (=domain).
 * Note that this is not currently exhaustive, but only supports
 * inet and inet6. Other values will be returned as is, that is
 * in the form of an integer.
 */
static
ERL_NIF_TERM encode_address_info_family(ErlNifEnv* env,
                                        int        family)
{
    ERL_NIF_TERM efam;

    if (NULL != esock_encode_domain(env, family, &efam))
        efam = MKI(env, family);

    return efam;
}



/* Convert an "native" socket type to an erlang socket type.
 * Note that this is not currently exhaustive, but only supports
 * stream and dgram. Other values will be returned as is, that is
 * in the form of an integer.
 */
static
ERL_NIF_TERM encode_address_info_type(ErlNifEnv* env,
                                      int        socktype)
{
    ERL_NIF_TERM etype;

    if (NULL != esock_encode_type(env, socktype, &etype))
        etype = MKI(env, socktype);

    return etype;
}



/* Convert an "native" protocol to an erlang protocol.
 * Note that this is not currently exhaustive, but only supports
 * tcp and udp. Other values will be returned as is, that is
 * in the form of an integer.
 */
static
ERL_NIF_TERM encode_address_info_proto(ErlNifEnv* env,
                                       int        proto)
{
    ERL_NIF_TERM eproto;

    if (NULL != esock_encode_protocol(env, proto, &eproto))
        eproto = MKI(env, proto);

    return eproto;
}



static
char* make_address_info(ErlNifEnv*    env,
                        ERL_NIF_TERM  fam,
                        ERL_NIF_TERM  sockType,
                        ERL_NIF_TERM  proto,
                        ERL_NIF_TERM  addr,
                        ERL_NIF_TERM* ai)
{
    ERL_NIF_TERM keys[] = {esock_atom_family,
                           esock_atom_type,
                           esock_atom_protocol,
                           esock_atom_addr};
    ERL_NIF_TERM vals[] = {fam, sockType, proto, addr};
    unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);
    unsigned int numVals = sizeof(vals) / sizeof(ERL_NIF_TERM);
    
    ESOCK_ASSERT( (numKeys == numVals) );
    
    if (!MKMA(env, keys, vals, numKeys, ai)) {
        *ai = esock_atom_undefined;
        return ESOCK_STR_EINVAL;
    } else {
        return NULL;
    }
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

    NDBG( ("NET", "change_network_namespace -> entry with"
            "\r\n   new ns: %s", netns) );

    if (netns != NULL) {
        current_ns = open("/proc/self/ns/net", O_RDONLY);
        if (current_ns == -1) {
            *cns = current_ns;
            *err = get_errno();
            return FALSE;
        }
        new_ns = open(netns, O_RDONLY);
        if (new_ns == -1) {
            save_errno = get_errno();
            while (close(current_ns) == -1 &&
                   get_errno() == EINTR);
            *cns = -1;
            *err = save_errno;
            return FALSE;
        }
        if (setns(new_ns, CLONE_NEWNET) != 0) {
            save_errno = get_errno();
            while ((close(new_ns) == -1) &&
                   (get_errno() == EINTR));
            while ((close(current_ns) == -1) &&
                   (get_errno() == EINTR));
            *cns = -1;
            *err = save_errno;
            return FALSE;
        } else {
            while ((close(new_ns) == -1) &&
                   (get_errno() == EINTR));
            *cns = current_ns;
            *err = 0;
            return TRUE;
        }
    } else {
        *cns = -1;
        *err = 0;
        return TRUE;
    }
}


/* *** restore network namespace ***
 * Restore the previous namespace (see above).
 */
static
BOOLEAN_T restore_network_namespace(int ns, int* err)
{
    int save_errno;

    NDBG( ("NET", "restore_network_namespace -> entry with"
            "\r\n   ns: %d", ns) );

    if (ns != -1) {
        if (setns(ns, CLONE_NEWNET) != 0) {
            /* XXX Failed to restore network namespace.
             * What to do? Tidy up and return an error...
             * Note that the thread now might still be in the namespace.
             * Can this even happen? Should the emulator be aborted?
             */
            save_errno = get_errno();
            while (close(ns) == -1 &&
                   get_errno() == EINTR);
            *err = save_errno;
            return FALSE;
        } else {
            while (close(ns) == -1 &&
                   get_errno() == EINTR);
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
 *  L o a d / u n l o a d / u p g r a d e   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

static
ErlNifFunc net_funcs[] =
{
    // Some utility functions
    {"nif_info",      0, nif_info,      0},
    {"nif_command",   1, nif_command,   0}, // Shall we let this be dirty?

    /* get/set hostname */
    {"nif_gethostname",         0, nif_gethostname,   0},

    /* address and name translation in protocol-independent manner */
    {"nif_getnameinfo",         2, nif_getnameinfo,   0},
    {"nif_getaddrinfo",         3, nif_getaddrinfo,   0},
    
    {"nif_getifaddrs",          1, nif_getifaddrs,    ERL_NIF_DIRTY_JOB_IO_BOUND},

    /* Network interface (name and/or index) functions */
    {"nif_if_name2index",       1, nif_if_name2index, 0},
    {"nif_if_index2name",       1, nif_if_index2name, 0},
    {"nif_if_names",            0, nif_if_names,      0}
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
    
    return esock_extract_bool_from_map(env, map, debug, NET_NIF_DEBUG_DEFAULT);
}
#endif


/* =======================================================================
 * load_info - A map of misc info (e.g global debug)
 */

static
int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
#if !defined(__WIN32__)    
    // We should make it possible to use load_info to get default values
    data.debug = extract_debug(env, load_info);

    NDBG( ("NET", "on_load -> entry\r\n") );
#endif

#define LOCAL_ATOM_DECL(A) atom_##A = MKA(env, #A)
LOCAL_ATOMS
LOCAL_ERROR_REASON_ATOMS
#undef LOCAL_ATOM_DECL

    // For storing "global" things...
    // data.env       = enif_alloc_env(); // We should really check
    // data.version   = MKA(env, ERTS_VERSION);
    // data.buildDate = MKA(env, ERTS_BUILD_DATE);

    net = enif_open_resource_type_x(env,
                                    "net",
                                    &netInit,
                                    ERL_NIF_RT_CREATE,
                                    NULL);

#if !defined(__WIN32__)    
    NDBG( ("NET", "on_load -> done\r\n") );
#endif

    return !net;
}

ERL_NIF_INIT(prim_net, net_funcs, on_load, NULL, NULL, NULL)
