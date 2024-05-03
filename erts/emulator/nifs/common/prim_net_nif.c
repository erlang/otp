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
#    include "config.h"
#endif

#ifndef ESOCK_ENABLE
#    include <erl_nif.h>

static
ErlNifFunc net_funcs[] = {};

static
int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void)env;
    (void)priv_data;
    (void)load_info;

    return 1;
}

ERL_NIF_INIT(prim_net, net_funcs, on_load, NULL, NULL, NULL)

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
// #define STRNCASECMP               strncasecmp
#define INCL_WINSOCK_API_TYPEDEFS 1

#ifndef WINDOWS_H_INCLUDES_WINSOCK2_H
#include <winsock2.h>
#endif
#include <windows.h>
#include <Ws2tcpip.h>   /* NEED VC 6.0 or higher */

/* Visual studio 2008+: NTDDI_VERSION needs to be set for iphlpapi.h
 * to define the right structures.
 * It needs to be set higher for IPV6 to work and it's set lower by default,
 * so we need to change it.
 */
#ifdef HAVE_SDKDDKVER_H
#  include <sdkddkver.h>
#  ifdef NTDDI_VERSION
#    undef NTDDI_VERSION
#  endif
#  define NTDDI_VERSION NTDDI_WIN10_RS2
#endif
#include <iphlpapi.h>
#include <mstcpip.h>

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


/* Debug stuff... */
#define NET_NIF_DEBUG_DEFAULT FALSE

#define NDBG( proto )       ESOCK_DBG_PRINTF( data.debug , proto )
#define NDBG2( dbg, proto ) ESOCK_DBG_PRINTF( (dbg || data.debug) , proto )

/* Global 'stuff' */
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
 * So, to simplify, we use some macro magic to define those.
 *
 * These are the functions making up the "official" API.
 */

#define ENET_NIF_FUNCS                       \
    ENET_NIF_FUNC_DEF(info);                 \
    ENET_NIF_FUNC_DEF(command);              \
    ENET_NIF_FUNC_DEF(gethostname);          \
    ENET_NIF_FUNC_DEF(getnameinfo);          \
    ENET_NIF_FUNC_DEF(getaddrinfo);          \
    ENET_NIF_FUNC_DEF(getifaddrs);           \
    ENET_NIF_FUNC_DEF(get_adapters_addresses);  \
    ENET_NIF_FUNC_DEF(get_if_entry);         \
    ENET_NIF_FUNC_DEF(get_interface_info);   \
    ENET_NIF_FUNC_DEF(get_ip_address_table); \
    ENET_NIF_FUNC_DEF(if_name2index);        \
    ENET_NIF_FUNC_DEF(if_index2name);        \
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

#if defined(HAVE_GETNAMEINFO)
static ERL_NIF_TERM enet_getnameinfo(ErlNifEnv*          env,
                                     const ESockAddress* saP,
                                     SOCKLEN_T           saLen,
                                     int                 flags);
#endif

#if defined(HAVE_GETADDRINFO)
static ERL_NIF_TERM enet_getaddrinfo(ErlNifEnv* env,
                                     char*      host,
                                     char*      serv);
#endif

#if defined(HAVE_GETIFADDRS) || defined(__PASE__)
static ERL_NIF_TERM enet_getifaddrs(ErlNifEnv* env,
                                    char*      netns);
#endif

#if defined(__WIN32__)

/* *** Get Adapters Addresses functions *** */
static BOOLEAN_T enet_get_adapters_addresses_args_debug(ErlNifEnv*         env,
                                                        const ERL_NIF_TERM eargs);
static BOOLEAN_T enet_get_adapters_addresses_args_family(ErlNifEnv*         env,
                                                         const ERL_NIF_TERM eargs,
                                                         ULONG*             fam);
static BOOLEAN_T enet_get_adapters_addresses_args_flags(ErlNifEnv*         env,
                                                         const ERL_NIF_TERM eargs,
                                                         ULONG*             flags);
static ERL_NIF_TERM enet_get_adapters_addresses(ErlNifEnv* env,
                                                BOOLEAN_T  dbg,
                                                ULONG      fam,
                                                ULONG      flags);
static ERL_NIF_TERM enet_adapters_addresses_encode(ErlNifEnv*            env,
                                                   BOOLEAN_T             dbg,
                                                   IP_ADAPTER_ADDRESSES* ipAdAddrsP);
static ERL_NIF_TERM enet_adapter_addresses_encode(ErlNifEnv*            env,
                                                  BOOLEAN_T             dbg,
                                                  IP_ADAPTER_ADDRESSES* ipAdAddrsP);
static ERL_NIF_TERM enet_adapter_encode_name(ErlNifEnv* env,
                                             WCHAR*     name);
static ERL_NIF_TERM enet_adapter_encode_friendly_name(ErlNifEnv* env,
                                                      WCHAR*     fname);
static ERL_NIF_TERM encode_if_oper_status(ErlNifEnv* env,
                                          DWORD      status);
static ERL_NIF_TERM encode_adapter_flags(ErlNifEnv*            env,
                                         IP_ADAPTER_ADDRESSES* ipAdAddrsP);
static ERL_NIF_TERM encode_adapter_unicast_addrs(ErlNifEnv*                  env,
                                                 IP_ADAPTER_UNICAST_ADDRESS* firstP);
static ERL_NIF_TERM encode_adapter_unicast_addr(ErlNifEnv*                  env,
                                                IP_ADAPTER_UNICAST_ADDRESS* addrP);
static ERL_NIF_TERM encode_adapter_unicast_addr_flags(ErlNifEnv* env,
                                                      DWORD      flags);
static ERL_NIF_TERM encode_adapter_unicast_addr_sockaddr(ErlNifEnv*       env,
                                                         struct sockaddr* addrP);
static ERL_NIF_TERM encode_adapter_unicast_addr_porig(ErlNifEnv*       env,
                                                      IP_PREFIX_ORIGIN porig);
static ERL_NIF_TERM encode_adapter_unicast_addr_sorig(ErlNifEnv*       env,
                                                      IP_SUFFIX_ORIGIN sorig);
static ERL_NIF_TERM encode_adapter_unicast_addr_dad_state(ErlNifEnv*   env,
                                                          IP_DAD_STATE dstate);
static ERL_NIF_TERM encode_adapter_anycast_addrs(ErlNifEnv*                  env,
                                                 IP_ADAPTER_ANYCAST_ADDRESS* firstP);
static ERL_NIF_TERM encode_adapter_anycast_addr(ErlNifEnv*                  env,
                                                IP_ADAPTER_ANYCAST_ADDRESS* addrP);
static ERL_NIF_TERM encode_adapter_anycast_addr_flags(ErlNifEnv* env,
                                                      DWORD      flags);
static ERL_NIF_TERM encode_adapter_anycast_addr_sockaddr(ErlNifEnv*       env,
                                                         struct sockaddr* addrP);
static ERL_NIF_TERM encode_adapter_multicast_addrs(ErlNifEnv*                  env,
                                                   IP_ADAPTER_MULTICAST_ADDRESS* firstP);
static ERL_NIF_TERM encode_adapter_multicast_addr(ErlNifEnv*                    env,
                                                  IP_ADAPTER_MULTICAST_ADDRESS* addrP);
static ERL_NIF_TERM encode_adapter_multicast_addr_flags(ErlNifEnv* env,
                                                        DWORD      flags);
static ERL_NIF_TERM encode_adapter_multicast_addr_sockaddr(ErlNifEnv*       env,
                                                           struct sockaddr* addrP);
static ERL_NIF_TERM encode_adapter_dns_server_addrs(ErlNifEnv*                     env,
                                                    IP_ADAPTER_DNS_SERVER_ADDRESS* firstP);
static ERL_NIF_TERM encode_adapter_dns_server_addr(ErlNifEnv*                     env,
                                                   IP_ADAPTER_DNS_SERVER_ADDRESS* addrP);
static ERL_NIF_TERM encode_adapter_dns_server_addr_sockaddr(ErlNifEnv*       env,
                                                            struct sockaddr* addrP);
static ERL_NIF_TERM encode_adapter_zone_indices(ErlNifEnv* env,
                                                DWORD*     zoneIndices,
                                                DWORD      len);
static ERL_NIF_TERM encode_adapter_prefixes(ErlNifEnv*         env,
                                            IP_ADAPTER_PREFIX* firstP);
static ERL_NIF_TERM encode_adapter_prefix(ErlNifEnv*         env,
                                          IP_ADAPTER_PREFIX* prefP);
static ERL_NIF_TERM encode_adapter_prefix_sockaddr(ErlNifEnv*       env,
                                                   struct sockaddr* addrP);


/* *** Get If Entry (MIB_IFROW) functions *** */
static ERL_NIF_TERM enet_get_if_entry(ErlNifEnv* env,
                                      BOOLEAN_T  dbg,
                                      DWORD      index);
static BOOLEAN_T enet_get_if_entry_args_index(ErlNifEnv*         env,
                                              const ERL_NIF_TERM eargs,
                                              DWORD*             index);
static BOOLEAN_T enet_get_if_entry_args_debug(ErlNifEnv*         env,
                                              const ERL_NIF_TERM eargs);
static ERL_NIF_TERM enet_if_row_encode(ErlNifEnv* env,
                                       BOOLEAN_T  dbg,
                                       MIB_IFROW* rowP);
static ERL_NIF_TERM encode_if_type(ErlNifEnv* env,
                                   DWORD      type);
static ERL_NIF_TERM encode_if_row_description(ErlNifEnv* env,
                                              DWORD      len,
                                              UCHAR*     buf);
static ERL_NIF_TERM encode_if_admin_status(ErlNifEnv* env,
                                           DWORD      status);
static ERL_NIF_TERM encode_internal_if_oper_status(ErlNifEnv* env,
                                                   DWORD      status);
static ERL_NIF_TERM encode_if_row_phys_address(ErlNifEnv* env,
                                               DWORD      len,
                                               UCHAR*     buf);

/* *** Get Interface Info functions *** */
static ERL_NIF_TERM enet_get_interface_info(ErlNifEnv* env,
                                            BOOLEAN_T  dbg);
static BOOLEAN_T enet_get_interface_info_args_debug(ErlNifEnv*         env,
                                                    const ERL_NIF_TERM eextra);
static ERL_NIF_TERM enet_interface_info_encode(ErlNifEnv*         env,
                                               BOOLEAN_T          dbg,
                                               IP_INTERFACE_INFO* infoP);
static void encode_adapter_index_map(ErlNifEnv*            env,
                                     BOOLEAN_T             dbg,
                                     IP_ADAPTER_INDEX_MAP* adapterP,
                                     ERL_NIF_TERM*         eadapter);
static ERL_NIF_TERM encode_adapter_index_map_name(ErlNifEnv* env, WCHAR* name);
static void make_adapter_index_map(ErlNifEnv*    env,
                                   ERL_NIF_TERM  eindex,
                                   ERL_NIF_TERM  ename,
                                   ERL_NIF_TERM* emap);
/* *** Get IP Address Table functions *** */
static ERL_NIF_TERM enet_get_ip_address_table(ErlNifEnv* env,
                                              BOOLEAN_T  dbg);
static ERL_NIF_TERM enet_get_ip_address_table_encode(ErlNifEnv*       env,
                                                     BOOLEAN_T        dbg,
                                                     MIB_IPADDRTABLE* tabP);
static ERL_NIF_TERM encode_ip_address_row(ErlNifEnv*     env,
                                          BOOLEAN_T      dbg,
                                          MIB_IPADDRROW* rowP);
static ERL_NIF_TERM encode_ip_address_row_addr(ErlNifEnv*  env,
                                               BOOLEAN_T   dbg,
                                               const char* descr,
                                               DWORD       addr);
static void make_ip_address_row(ErlNifEnv*    env,
                                ERL_NIF_TERM  eaddr,
                                ERL_NIF_TERM  eindex,
                                ERL_NIF_TERM  emask,
                                ERL_NIF_TERM  eBCastAddr,
                                ERL_NIF_TERM  eReasmSize,
                                ERL_NIF_TERM* iar);

#endif

#if defined(HAVE_IF_NAMETOINDEX)
static ERL_NIF_TERM enet_if_name2index(ErlNifEnv* env,
                                       char*      ifn);
#endif

#if defined(HAVE_IF_INDEXTONAME)
static ERL_NIF_TERM enet_if_index2name(ErlNifEnv*   env,
                                       unsigned int id);
#endif

#if defined(HAVE_IF_NAMEINDEX) && defined(HAVE_IF_FREENAMEINDEX)
static ERL_NIF_TERM enet_if_names(ErlNifEnv* env);
static unsigned int enet_if_names_length(struct if_nameindex* p);
#endif

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
static void encode_ifaddrs(ErlNifEnv*      env,
                           struct ifaddrs* ifap,
                           ERL_NIF_TERM*   eifa);
static ERL_NIF_TERM encode_ifaddrs_name(ErlNifEnv* env,
                                        char*      name);
static ERL_NIF_TERM encode_ifaddrs_flags(ErlNifEnv*   env,
                                         unsigned int flags);
static ERL_NIF_TERM encode_ifaddrs_addr(ErlNifEnv*       env,
                                        struct sockaddr* sa);
static void make_ifaddrs(ErlNifEnv*    env,
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
static ERL_NIF_TERM encode_sockaddr(ErlNifEnv* env, struct sockaddr* sa);
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
                                ERL_NIF_TERM ebool,
                                BOOLEAN_T*   ibool);
static ERL_NIF_TERM encode_address_infos(ErlNifEnv*       env,
                                         struct addrinfo* addrInfo);
static ERL_NIF_TERM encode_address_info(ErlNifEnv*       env,
                                        struct addrinfo* addrInfoP);
static unsigned int address_info_length(struct addrinfo* addrInfoP);

static ERL_NIF_TERM encode_address_info_family(ErlNifEnv* env,
                                               int        family);
static ERL_NIF_TERM encode_address_info_type(ErlNifEnv* env,
                                             int        socktype);

static void make_address_info(ErlNifEnv*    env,
                              ERL_NIF_TERM  fam,
                              ERL_NIF_TERM  sockType,
                              ERL_NIF_TERM  proto,
                              ERL_NIF_TERM  addr,
                              ERL_NIF_TERM* ai);

#if defined(__WIN32__)
static ERL_NIF_TERM encode_uchar(ErlNifEnv* env,
                                 DWORD      len,
                                 UCHAR*     buf);
static ERL_NIF_TERM encode_wchar(ErlNifEnv* env,
                                 WCHAR* name);
#endif
static BOOLEAN_T get_debug(ErlNifEnv*   env,
                           ERL_NIF_TERM map);
static int on_load(ErlNifEnv*   env,
                   void**       priv_data,
                   ERL_NIF_TERM load_info);


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
    LOCAL_ATOM_DECL(admin_status);              \
    LOCAL_ATOM_DECL(anycast_addrs);             \
    LOCAL_ATOM_DECL(atm);                       \
    LOCAL_ATOM_DECL(automedia);                 \
    LOCAL_ATOM_DECL(bcast_addr);                \
    LOCAL_ATOM_DECL(broadaddr);                 \
    LOCAL_ATOM_DECL(broadcast);                 \
    LOCAL_ATOM_DECL(dad_state);                 \
    LOCAL_ATOM_DECL(debug);                     \
    LOCAL_ATOM_DECL(deprecated);                \
    LOCAL_ATOM_DECL(description);               \
    LOCAL_ATOM_DECL(dhcp);                      \
    LOCAL_ATOM_DECL(dhcp_v4_enabled);           \
    LOCAL_ATOM_DECL(ddns_enabled);              \
    LOCAL_ATOM_DECL(disconnected);              \
    LOCAL_ATOM_DECL(dns_eligible);              \
    LOCAL_ATOM_DECL(dns_server_addrs);          \
    LOCAL_ATOM_DECL(dns_suffix);                \
    LOCAL_ATOM_DECL(down);                      \
    LOCAL_ATOM_DECL(dstaddr);                   \
    LOCAL_ATOM_DECL(duplicate);                 \
    LOCAL_ATOM_DECL(dynamic);                   \
    LOCAL_ATOM_DECL(ethernet_csmacd);           \
    LOCAL_ATOM_DECL(fddi);                      \
    LOCAL_ATOM_DECL(friendly_name);             \
    LOCAL_ATOM_DECL(host);                      \
    LOCAL_ATOM_DECL(idn);                       \
    LOCAL_ATOM_DECL(ieee1394);                  \
    LOCAL_ATOM_DECL(ieee80211);                 \
    LOCAL_ATOM_DECL(ieee80216_wman);            \
    LOCAL_ATOM_DECL(include_prefix);              \
    LOCAL_ATOM_DECL(include_wins_info);           \
    LOCAL_ATOM_DECL(include_gateways);            \
    LOCAL_ATOM_DECL(include_all_interfaces);      \
    LOCAL_ATOM_DECL(include_all_compartments);    \
    LOCAL_ATOM_DECL(include_tunnel_bindingorder); \
    LOCAL_ATOM_DECL(index);                     \
    LOCAL_ATOM_DECL(internal_oper_status);      \
    LOCAL_ATOM_DECL(invalid);                   \
    LOCAL_ATOM_DECL(in_octets);                 \
    LOCAL_ATOM_DECL(in_ucast_pkts);             \
    LOCAL_ATOM_DECL(in_nucast_pkts);            \
    LOCAL_ATOM_DECL(in_discards);               \
    LOCAL_ATOM_DECL(in_errors);                 \
    LOCAL_ATOM_DECL(in_unknown_protos);         \
    LOCAL_ATOM_DECL(ipv4_enabled);                \
    LOCAL_ATOM_DECL(ipv6_enabled);                \
    LOCAL_ATOM_DECL(ipv6_index);                  \
    LOCAL_ATOM_DECL(ipv6_managed_address_config_supported);  \
    LOCAL_ATOM_DECL(ipv6_other_stateful_config);  \
    LOCAL_ATOM_DECL(iso88025_tokenring);        \
    LOCAL_ATOM_DECL(last_change);               \
    LOCAL_ATOM_DECL(lease_lifetime);            \
    LOCAL_ATOM_DECL(length);                    \
    LOCAL_ATOM_DECL(link_layer_address);        \
    LOCAL_ATOM_DECL(lower_layer_down);          \
    LOCAL_ATOM_DECL(manual);                    \
    LOCAL_ATOM_DECL(mask);                      \
    LOCAL_ATOM_DECL(master);                    \
    LOCAL_ATOM_DECL(multicast);                 \
    LOCAL_ATOM_DECL(multicast_addrs);           \
    LOCAL_ATOM_DECL(namereqd);                  \
    LOCAL_ATOM_DECL(name_info);                 \
    LOCAL_ATOM_DECL(netbios_over_tcpip_enabled);  \
    LOCAL_ATOM_DECL(netmask);                   \
    LOCAL_ATOM_DECL(noarp);                     \
    LOCAL_ATOM_DECL(nofqdn);                    \
    LOCAL_ATOM_DECL(non_operational);           \
    LOCAL_ATOM_DECL(notrailers);                \
    LOCAL_ATOM_DECL(not_present);               \
    LOCAL_ATOM_DECL(no_multicast);              \
    LOCAL_ATOM_DECL(numerichost);               \
    LOCAL_ATOM_DECL(numericserv);               \
    LOCAL_ATOM_DECL(on_link_prefix_length);     \
    LOCAL_ATOM_DECL(operational);               \
    LOCAL_ATOM_DECL(oper_status);               \
    LOCAL_ATOM_DECL(other);                     \
    LOCAL_ATOM_DECL(out_octets);                \
    LOCAL_ATOM_DECL(out_ucast_pkts);            \
    LOCAL_ATOM_DECL(out_nucast_pkts);           \
    LOCAL_ATOM_DECL(out_discards);              \
    LOCAL_ATOM_DECL(out_errors);                \
    LOCAL_ATOM_DECL(out_qlen);                  \
    LOCAL_ATOM_DECL(phys_addr);                 \
    LOCAL_ATOM_DECL(pointopoint);               \
    LOCAL_ATOM_DECL(portsel);                   \
    LOCAL_ATOM_DECL(ppp);                       \
    LOCAL_ATOM_DECL(preferred);                 \
    LOCAL_ATOM_DECL(preferred_lifetime);        \
    LOCAL_ATOM_DECL(prefixes);                  \
    LOCAL_ATOM_DECL(prefix_origin);             \
    LOCAL_ATOM_DECL(promisc);                   \
    LOCAL_ATOM_DECL(random);                    \
    LOCAL_ATOM_DECL(reasm_size);                \
    LOCAL_ATOM_DECL(receive_only);              \
    LOCAL_ATOM_DECL(register_adapter_suffix);   \
    LOCAL_ATOM_DECL(router_advertisement);      \
    LOCAL_ATOM_DECL(running);                   \
    LOCAL_ATOM_DECL(service);                   \
    LOCAL_ATOM_DECL(slave);                     \
    LOCAL_ATOM_DECL(skip_unicast);              \
    LOCAL_ATOM_DECL(skip_anycast);              \
    LOCAL_ATOM_DECL(skip_multicast);            \
    LOCAL_ATOM_DECL(skip_dns_server);           \
    LOCAL_ATOM_DECL(skip_friendly_name);        \
    LOCAL_ATOM_DECL(software_loopback);         \
    LOCAL_ATOM_DECL(speed);                     \
    LOCAL_ATOM_DECL(suffix_origin);             \
    LOCAL_ATOM_DECL(tentative);                 \
    LOCAL_ATOM_DECL(testing);                   \
    LOCAL_ATOM_DECL(transient);                 \
    LOCAL_ATOM_DECL(tunnel);                    \
    LOCAL_ATOM_DECL(unchanged);                 \
    LOCAL_ATOM_DECL(unknown);                   \
    LOCAL_ATOM_DECL(unicast_addrs);             \
    LOCAL_ATOM_DECL(unreachable);               \
    LOCAL_ATOM_DECL(up);                        \
    LOCAL_ATOM_DECL(valid_lifetime);            \
    LOCAL_ATOM_DECL(well_known);                \
    LOCAL_ATOM_DECL(wwanpp);                    \
    LOCAL_ATOM_DECL(wwanpp2);                   \
    LOCAL_ATOM_DECL(zone_indices);

#define LOCAL_ERROR_REASON_ATOMS               \
    LOCAL_ATOM_DECL(address_not_associated);   \
    LOCAL_ATOM_DECL(can_not_complete);         \
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
    LOCAL_ATOM_DECL(esystem);                  \
    LOCAL_ATOM_DECL(insufficient_buffer);      \
    LOCAL_ATOM_DECL(invalid_data);             \
    LOCAL_ATOM_DECL(invalid_flags);            \
    LOCAL_ATOM_DECL(invalid_parameter);        \
    LOCAL_ATOM_DECL(not_found);                \
    LOCAL_ATOM_DECL(not_enough_memory);        \
    LOCAL_ATOM_DECL(not_supported);            \
    LOCAL_ATOM_DECL(no_data);                  \
    LOCAL_ATOM_DECL(no_function);              \
    LOCAL_ATOM_DECL(no_uniconde_traslation);

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
 * nif_get_adapters_addresses/1
 * nif_get_if_entry/1
 * nif_get_interface_info/1
 * nif_get_ip_address_table/1
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
    ERL_NIF_TERM info, tmp;

    NDBG( ("NET", "info -> entry\r\n") );

    tmp  = enif_make_new_map(env);
    if (!enif_make_map_put(env, tmp, atom_debug, BOOL2ATOM(data.debug), &info))
        info = tmp;

    NDBG( ("NET", "info -> done: %T\r\n", info) );

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

    NDBG( ("NET", "command -> entry (%d)\r\n", argc) );

    if (argc != 1)
        return enif_make_badarg(env);

    ecmd = argv[0];

    NDBG( ("NET", "command -> ecmd: %T\r\n", ecmd) );

    result = enet_command(env, ecmd);

    NDBG( ("NET", "command -> result: %T\r\n", result) );

    return result;
}



/*
 * The command can, in principle, be anything, though currently we only
 * support a debug command.
 */
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
    ERL_NIF_TERM result;
    
    NDBG( ("NET", "nif_gethostname -> entry (%d)\r\n", argc) );

    if (argc != 0)
        return enif_make_badarg(env);

    result = enet_gethostname(env);

    NDBG( ("NET", "nif_gethostname -> done when result: %T\r\n", result) );

    return result;
}


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
#if defined(HAVE_GETNAMEINFO)
    ERL_NIF_TERM result;
    ERL_NIF_TERM eSockAddr, eFlags;
    int          flags = 0; // Just in case...
    ESockAddress sa;
    SOCKLEN_T    saLen = 0; // Just in case...

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

    if (! esock_decode_sockaddr(env, eSockAddr, &sa, &saLen)) {
        NDBG( ("NET", "nif_getnameinfo -> failed decode sockaddr\r\n") );
        return esock_make_error(env, esock_atom_einval);
    }

    NDBG( ("NET", "nif_getnameinfo -> (try) decode flags\r\n") );

    if (!decode_nameinfo_flags(env, eFlags, &flags))
        return enif_make_badarg(env);

    result = enet_getnameinfo(env, &sa, saLen, flags);

    NDBG( ("NET",
           "nif_getnameinfo -> done when result: "
           "\r\n   %T\r\n", result) );

    return result;
#else
    return esock_make_error(env, esock_atom_enotsup);
#endif
}



/* Given the provided sock(et) address (and flags), retrieve the host and
 * service info.
 */
#if defined(HAVE_GETNAMEINFO)
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
            ERL_NIF_TERM keys[]  = {atom_host,      atom_service};
            ERL_NIF_TERM vals[]  = {MKS(env, host), MKS(env, serv)};
            size_t       numKeys = NUM(keys);
            ERL_NIF_TERM info;

            ESOCK_ASSERT( numKeys == NUM(vals) );
            ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &info) );

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

#if !defined(__WIN32__) && defined(EAI_NONAME)
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
#if defined(HAVE_GETADDRINFO)
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
#else
    return esock_make_error(env, esock_atom_enotsup);
#endif
}


#if defined(HAVE_GETADDRINFO)
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

        /* This value conflict with "some" other value on windows... */
#if !defined(__WIN32__) && defined(EAI_NONAME)
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
#ifdef HAVE_SETNS
    ERL_NIF_TERM extra;
#endif
    char*        netns;
    ERL_NIF_TERM result;

    NDBG( ("NET", "nif_getifaddrs -> entry (%d)\r\n", argc) );

    if ((argc != 1) ||
        !IS_MAP(env,  argv[0])) {
        return enif_make_badarg(env);
    }
#ifdef HAVE_SETNS
    extra = argv[0];
#endif


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

            encode_ifaddrs(env, p, &entry);

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



/* Calculate the length of the interface address linked list
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
void encode_ifaddrs(ErlNifEnv*      env,
                    struct ifaddrs* ifap,
                    ERL_NIF_TERM*   eifa)
{
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

    make_ifaddrs(env,
                 ename, eflags, eaddr, enetmask,
                 eifu_key, eifu_value, edata,
                 &eifAddrs);

    NDBG( ("NET", "encode_ifaddrs -> encoded ifAddrs: %T\r\n", eifAddrs) );
    *eifa = eifAddrs;
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
    return encode_sockaddr(env, sa);
}


static
void make_ifaddrs(ErlNifEnv*    env,
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
    size_t       len = NUM(keys); // Just in case...
    size_t       idx = 0;

    /* *** Name *** */
    NDBG( ("NET", "make_ifaddrs -> name: %T\r\n", ename) );
    keys[idx] = esock_atom_name;
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

    ESOCK_ASSERT( MKMA(env, keys, vals, len, eifAddrs) );
}

#endif // HAVE_GETIFADDRS


/* ----------------------------------------------------------------------
 * nif_get_adapters_addresses
 *
 * Description:
 * Get adapters addresses.
 * This is a windows only function!
 *
 * Arguments:
 * Args - A way to pass 'extra' arguments.
 *        #{family := unspec (default) | inet | inet6,
 *          flags  := flags(),
 *          debug  := boolean() (optional)}
 *
 * flags() :: #{skip_unicast                :: boolean() (default false),
 *              skip_anycast                :: boolean() (default true),
 *              skip_multicast              :: boolean() (default true),
 *              skip_dns_server             :: boolean() (default true),
 *              skip_friendly_name          :: boolean() (default true),
 *              include_prefix              :: boolean() (default true),
 *              include_wins_info           :: boolean() (default false),
 *              include_gateways            :: boolean() (default false),
 *              include_all_interfaces      :: boolean() (default false),
 *              include_all_compartments    :: boolean() (default false),
 *              include_tunnel_bindingorder :: boolean() (default false)}
 * Suggested Help atoms:
 *     no_skips_all_includes
 *     all_skips_no_includes
 *     no_skips_no_includes
 *     all_skips_all_includes
 */

static
ERL_NIF_TERM nif_get_adapters_addresses(ErlNifEnv*         env,
                                        int                argc,
                                        const ERL_NIF_TERM argv[])
{
#if !defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM result, eargs;
    ULONG        fam, flags;
    BOOLEAN_T    dbg;

    NDBG( ("NET", "nif_get_adapters_addresses -> entry (%d)\r\n", argc) );

    if ((argc != 1) ||
        !IS_MAP(env, argv[0])) {
        return enif_make_badarg(env);
    }
    eargs = argv[0];

    if (!enet_get_adapters_addresses_args_family(env, eargs, &fam))
        return enif_make_badarg(env);

    if (!enet_get_adapters_addresses_args_flags(env, eargs, &flags))
        return enif_make_badarg(env);

    dbg    = enet_get_adapters_addresses_args_debug(env, eargs);

    result = enet_get_adapters_addresses(env, dbg, fam, flags);

    NDBG2( dbg,
           ("NET",
            "nif_get_adapters_addresses -> done when result: "
            "\r\n   %T\r\n", result) );
    
    return result;
#endif
}



#if defined(__WIN32__)
static
BOOLEAN_T enet_get_adapters_addresses_args_debug(ErlNifEnv*         env,
                                                 const ERL_NIF_TERM eargs)
{
    return get_debug(env, eargs);
}
#endif



#if defined(__WIN32__)
static
BOOLEAN_T enet_get_adapters_addresses_args_family(ErlNifEnv*         env,
                                                  const ERL_NIF_TERM eargs,
                                                  ULONG*             fam)
{
    ERL_NIF_TERM key = esock_atom_family;
    ERL_NIF_TERM eval;
    DWORD        val;

    if (!GET_MAP_VAL(env, eargs, key, &eval)) {
        *fam = AF_UNSPEC; // Default
        return TRUE;
    } else {
        if (!IS_ATOM(env, eval))
            return FALSE;

        if (COMPARE(eval, esock_atom_unspec))
            val = AF_UNSPEC;
        else if (COMPARE(eval, esock_atom_inet))
            val = AF_INET;
        else if (COMPARE(eval, esock_atom_inet6))
            val = AF_INET6;
        else
            return FALSE;

        *fam = val;
        return TRUE;
    }
}
#endif // __WIN32__


#if defined(__WIN32__)
static
BOOLEAN_T enet_get_adapters_addresses_args_flags(ErlNifEnv*         env,
                                                  const ERL_NIF_TERM eargs,
                                                  ULONG*             flags)
{
    ERL_NIF_TERM eflags;
    ULONG        val = 0;

    if (!GET_MAP_VAL(env, eargs, esock_atom_flags, &eflags)) {
        // Default
        *flags =
            GAA_FLAG_INCLUDE_PREFIX |
            GAA_FLAG_SKIP_ANYCAST |
            GAA_FLAG_SKIP_DNS_SERVER |
            GAA_FLAG_SKIP_FRIENDLY_NAME |
            GAA_FLAG_SKIP_MULTICAST;
        return TRUE;
    } else {
        if (!IS_MAP(env, eflags))
            return FALSE;

        /* skip unicast */
        if (esock_get_bool_from_map(env, eflags, atom_skip_unicast, FALSE))
            val |= GAA_FLAG_SKIP_UNICAST;
        
        /* skip anycast */
        if (esock_get_bool_from_map(env, eflags, atom_skip_anycast, TRUE))
            val |= GAA_FLAG_SKIP_ANYCAST;
        
        /* skip multicast */
        if (esock_get_bool_from_map(env, eflags, atom_skip_multicast, TRUE))
            val |= GAA_FLAG_SKIP_MULTICAST;
        
        /* skip dns-server */
        if (esock_get_bool_from_map(env, eflags, atom_skip_dns_server, TRUE))
            val |= GAA_FLAG_SKIP_DNS_SERVER;
        
        /* skip fiendly-name */
        if (esock_get_bool_from_map(env, eflags, atom_skip_friendly_name, TRUE))
            val |= GAA_FLAG_SKIP_FRIENDLY_NAME;
        
        /* include prefix */
        if (esock_get_bool_from_map(env, eflags, atom_include_prefix, TRUE))
            val |= GAA_FLAG_INCLUDE_PREFIX;
        
        /* include wins-info */
        if (esock_get_bool_from_map(env, eflags, atom_include_wins_info, FALSE))
            val |= GAA_FLAG_INCLUDE_WINS_INFO;
        
        /* include gateways */
        if (esock_get_bool_from_map(env, eflags, atom_include_gateways, FALSE))
            val |= GAA_FLAG_INCLUDE_GATEWAYS;
        
        /* include all-interfaces */
        if (esock_get_bool_from_map(env, eflags,
                                    atom_include_all_interfaces, FALSE))
            val |= GAA_FLAG_INCLUDE_ALL_INTERFACES;
        
        /* include all-compartments */
        if (esock_get_bool_from_map(env, eflags,
                                    atom_include_all_compartments, FALSE))
            val |= GAA_FLAG_INCLUDE_ALL_COMPARTMENTS;
        
        /* include tunnel-bindingorder */
        if (esock_get_bool_from_map(env, eflags,
                                    atom_include_tunnel_bindingorder, FALSE))
            val |= GAA_FLAG_INCLUDE_TUNNEL_BINDINGORDER;

        *flags = val;
        return TRUE;
    }
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM enet_get_adapters_addresses(ErlNifEnv* env,
                                         BOOLEAN_T  dbg,
                                         ULONG      family,
                                         ULONG      flags)
{
    int                   i;
    DWORD                 ret;
    unsigned long         ipAdAddrsSz = 16 * 1024;
    IP_ADAPTER_ADDRESSES* ipAdAddrsP;
    ERL_NIF_TERM          eret, addrs, result;

    ipAdAddrsP = (IP_ADAPTER_ADDRESSES*) MALLOC(ipAdAddrsSz);
    for (i = 17;  i;  i--) {
        ret = GetAdaptersAddresses(family, flags, NULL,
                                   ipAdAddrsP, &ipAdAddrsSz);
        if (ret == NO_ERROR) {
            /* We are done! */
            break;
        } else if (ret == ERROR_BUFFER_OVERFLOW) {
            /* Not large enough */
            ipAdAddrsP = REALLOC(ipAdAddrsP, ipAdAddrsSz);
            continue;
        } else {
            /* Failure */
            i = 0;
        }
        if (ret == NO_ERROR) break;
        if (ret == ERROR_BUFFER_OVERFLOW) continue;
        i = 0;
    }

    if (! i) {

        NDBG2( dbg,
               ("NET", "enet_get_adapters_addresses -> "
                "try encode error (%d)\r\n", ret) );

        FREE(ipAdAddrsP);

        switch (ret) {
        case ERROR_ADDRESS_NOT_ASSOCIATED:
            eret = atom_address_not_associated;
            break;
        case ERROR_BUFFER_OVERFLOW:
            eret = atom_insufficient_buffer;
            break;
        case ERROR_INVALID_PARAMETER:
            eret = atom_invalid_parameter;
            break;
        case ERROR_NO_DATA:
            eret = atom_no_data;
            break;
        case ERROR_NOT_ENOUGH_MEMORY:
            eret = atom_not_enough_memory;
            break;
        default:
            eret = MKI(env, ret);
            break;
        }

        result = esock_make_error(env, eret);

    } else {

        NDBG2( dbg,
               ("NET", "enet_get_adapters_addresses -> "
                "try encode addresses\r\n") );

        addrs  = enet_adapters_addresses_encode(env, dbg, ipAdAddrsP);
        result = esock_make_ok2(env, addrs);

    }
    
    NDBG2( dbg,
           ("NET", "enet_get_adapters_addresses -> done with:"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}
#endif


#if defined(__WIN32__)
static
ERL_NIF_TERM enet_adapters_addresses_encode(ErlNifEnv*            env,
                                            BOOLEAN_T             dbg,
                                            IP_ADAPTER_ADDRESSES* ipAdAddrsP)
{
    /* No idea how many we actually need, so just get some */
    SocketTArray          adapterArray = TARRAY_CREATE(16);
    IP_ADAPTER_ADDRESSES* addrsP       = ipAdAddrsP;
    ERL_NIF_TERM          entry, result;

    NDBG2( dbg, ("NET", "enet_get_adapters_addresses -> entry\r\n") );

    while (addrsP != NULL) {
        /* Process current adapter */
        entry  = enet_adapter_addresses_encode(env, dbg, addrsP);

        NDBG2( dbg, ("NET", "enet_get_adapters_addresses -> entry encoded:"
                     "\r\n   Adapter Entry: %T"
                     "\r\n", entry) );

        TARRAY_ADD(adapterArray, entry);
        
        addrsP = addrsP->Next;
     }

     TARRAY_TOLIST(adapterArray, env, &result);

     NDBG2( dbg, ("NET", "enet_get_adapters_addresses -> done:"
                  "\r\n   %T"
                  "\r\n", result) );

     return result;

    }
#endif // __WIN32__



#if defined(__WIN32__)
static
ERL_NIF_TERM enet_adapter_addresses_encode(ErlNifEnv*            env,
                                           BOOLEAN_T             dbg,
                                           IP_ADAPTER_ADDRESSES* ipAdAddrsP)
{
    ERL_NIF_TERM ifIdx, name;
    ERL_NIF_TERM unicastAddrs, anycastAddrs, multicastAddrs, dnsServerAddrs;
    ERL_NIF_TERM dnsSuffix, description, flags, physAddr, fName, mtu, ifType;
    ERL_NIF_TERM operStatus, zoneIndices, ipv6IfIdx, prefixes;
    ERL_NIF_TERM map;

    ifIdx          = MKI(env, ipAdAddrsP->IfIndex);
    name           = MKS(env, ipAdAddrsP->AdapterName);
    unicastAddrs   = encode_adapter_unicast_addrs(env, ipAdAddrsP->FirstUnicastAddress);
    anycastAddrs   = encode_adapter_anycast_addrs(env, ipAdAddrsP->FirstAnycastAddress);
    multicastAddrs = encode_adapter_multicast_addrs(env, ipAdAddrsP->FirstMulticastAddress);
    dnsServerAddrs = encode_adapter_dns_server_addrs(env, ipAdAddrsP->FirstDnsServerAddress);
    dnsSuffix      = encode_wchar(env, ipAdAddrsP->DnsSuffix);
    description    = encode_wchar(env, ipAdAddrsP->Description);
    fName          = encode_wchar(env, ipAdAddrsP->FriendlyName);
    physAddr       = encode_uchar(env,
                                  ipAdAddrsP->PhysicalAddressLength,
                                  ipAdAddrsP->PhysicalAddress);
    flags          = encode_adapter_flags(env, ipAdAddrsP);
    mtu            = MKUI(env, ipAdAddrsP->Mtu);
    ifType         = encode_if_type(env, ipAdAddrsP->IfType);
    operStatus     = encode_if_oper_status(env, ipAdAddrsP->OperStatus);
    zoneIndices    = encode_adapter_zone_indices(env,
                                                 ipAdAddrsP->ZoneIndices,
                                                 NUM(ipAdAddrsP->ZoneIndices));
    ipv6IfIdx      = MKI(env, ipAdAddrsP->Ipv6IfIndex);
    prefixes       = encode_adapter_prefixes(env, ipAdAddrsP->FirstPrefix);

    /* *** _LH *** */
    // tLinkSpeed = MKUI64(env, ipAdAddrsP->TransmitLinkSpeed);
    // rLinkSpeed = MKUI64(env, ipAdAddrsP->ReceiveLinkSpeed);
    // winsServerAddr = ...
    // gatewayAddr = ...
    // ipv4Metric = ...
    // ipv6Metric = ...
    // luid = ...
    // dhcpv4Server = ...
    // compartmentId = ...
    // networkDuid = ...
    // connectionType = ...
    // tunnelType = ...
    // dhcpv6Server = ...
    // dhcpv6ClientDuid = ...
    // dhcpv6Iaid = ...
    // dnsSuffix = ...

    {
        ERL_NIF_TERM keys[] = {atom_index,
                               esock_atom_name,
                               atom_unicast_addrs,
                               atom_anycast_addrs,
                               atom_multicast_addrs,
                               atom_dns_server_addrs,
                               atom_dns_suffix,
                               atom_description,
                               atom_friendly_name,
                               atom_phys_addr,
                               esock_atom_flags,
                               esock_atom_mtu,
                               esock_atom_type,
                               atom_oper_status,
                               atom_zone_indices,
                               atom_ipv6_index,
                               atom_prefixes/* , */
                               /* atom_transmit_link_speed, */
                               /* atom_receive_link_speed */
        };
        ERL_NIF_TERM vals[] = {ifIdx,
                               name,
                               unicastAddrs,
                               anycastAddrs,
                               multicastAddrs,
                               dnsServerAddrs,
                               dnsSuffix,
                               description,
                               fName,
                               physAddr,
                               flags,
                               mtu,
                               ifType,
                               operStatus,
                               zoneIndices,
                               ipv6IfIdx,
                               prefixes
                               /* , */
                               /* tLinkSpeed, */
                               /* rLinkSpeed */
        };
        size_t       numKeys = NUM(keys);
        size_t       numVals = NUM(vals);

        ESOCK_ASSERT( numKeys == numVals );

        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &map) );
    }
    
    return map;
}
#endif // __WIN32__

#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_flags(ErlNifEnv*            env,
                                  IP_ADAPTER_ADDRESSES* ipAdAddrsP)
{
    ERL_NIF_TERM ddnsEnabled, regAdSuffix, dhcpv4Enabled, recvOnly;
    ERL_NIF_TERM noMulticast, ipv6OtherStatefulCfg, netbiosOverTcpipEnabled;
    ERL_NIF_TERM ipv4Enabled, ipv6Enabled, ipv6ManagedAddrCfgSup;
    ERL_NIF_TERM eflags;

#if defined(ESOCK_WIN_XP)
    /* This is just a dummy-ifdef ... there is no such flag (ESOCK_WIN_XP).
     * But this is a way to keep the code...
     */
    ddnsEnabled             = BOOL2ATOM(ipAdAddrsP->DdnsEnabled);
    regAdSuffix             = BOOL2ATOM(ipAdAddrsP->RegisterAdapterSuffix);
    dhcpv4Enabled           = BOOL2ATOM(ipAdAddrsP->Dhcpv4Enabled);
    recvOnly                = BOOL2ATOM(ipAdAddrsP->ReceiveOnly);
    noMulticast             = BOOL2ATOM(ipAdAddrsP->NoMulticast);
    ipv6OtherStatefulCfg    = BOOL2ATOM(ipAdAddrsP->Ipv6OtherStatefulConfig);
    netbiosOverTcpipEnabled = BOOL2ATOM(ipAdAddrsP->NetbiosOverTcpipEnabled);
    ipv4Enabled             = BOOL2ATOM(ipAdAddrsP->Ipv4Enabled);
    ipv6Enabled             = BOOL2ATOM(ipAdAddrsP->Ipv6Enabled);
    ipv6ManagedAddrCfgSup   = BOOL2ATOM(ipAdAddrsP->Ipv6ManagedAddressConfigurationSupported);
#else
    ddnsEnabled             = BOOL2ATOM(ipAdAddrsP->Flags & IP_ADAPTER_DDNS_ENABLED);
    regAdSuffix             = BOOL2ATOM(ipAdAddrsP->Flags & IP_ADAPTER_REGISTER_ADAPTER_SUFFIX);
    dhcpv4Enabled           = BOOL2ATOM(ipAdAddrsP->Flags & IP_ADAPTER_DHCP_ENABLED);
    recvOnly                = BOOL2ATOM(ipAdAddrsP->Flags & IP_ADAPTER_RECEIVE_ONLY);
    noMulticast             = BOOL2ATOM(ipAdAddrsP->Flags & IP_ADAPTER_NO_MULTICAST);
    ipv6OtherStatefulCfg    = BOOL2ATOM(ipAdAddrsP->Flags & IP_ADAPTER_IPV6_OTHER_STATEFUL_CONFIG);
    netbiosOverTcpipEnabled = BOOL2ATOM(ipAdAddrsP->Flags & IP_ADAPTER_NETBIOS_OVER_TCPIP_ENABLED);
    ipv4Enabled             = BOOL2ATOM(ipAdAddrsP->Flags & IP_ADAPTER_IPV4_ENABLED);
    ipv6Enabled             = BOOL2ATOM(ipAdAddrsP->Flags & IP_ADAPTER_IPV6_ENABLED);
    ipv6ManagedAddrCfgSup   = BOOL2ATOM(ipAdAddrsP->Flags & IP_ADAPTER_IPV6_MANAGE_ADDRESS_CONFIG);
#endif    

    {
        ERL_NIF_TERM keys[] = {atom_ddns_enabled,
                               atom_register_adapter_suffix,
                               atom_dhcp_v4_enabled,
                               atom_receive_only,
                               atom_no_multicast,
                               atom_ipv6_other_stateful_config,
                               atom_netbios_over_tcpip_enabled,
                               atom_ipv4_enabled,
                               atom_ipv6_enabled,
                               atom_ipv6_managed_address_config_supported};
        ERL_NIF_TERM vals[] = {ddnsEnabled,
                               regAdSuffix,
                               dhcpv4Enabled,
                               recvOnly,
                               noMulticast,
                               ipv6OtherStatefulCfg,
                               netbiosOverTcpipEnabled,
                               ipv4Enabled,
                               ipv6Enabled,
                               ipv6ManagedAddrCfgSup};
        size_t       numKeys = NUM(keys);
        size_t       numVals = NUM(vals);

        ESOCK_ASSERT( numKeys == numVals );

        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &eflags) );
    }
    
    return eflags;
}
#endif // __WIN32__

#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_unicast_addrs(ErlNifEnv*                  env,
                                          IP_ADAPTER_UNICAST_ADDRESS* firstP)
{
    IP_ADAPTER_UNICAST_ADDRESS* tmp = firstP;
    SocketTArray                ta  = TARRAY_CREATE(16);
    ERL_NIF_TERM eaddrs;
    

    while (tmp != NULL) {
        TARRAY_ADD(ta, encode_adapter_unicast_addr(env, tmp));
        tmp = tmp->Next;
    }

    TARRAY_TOLIST(ta, env, &eaddrs);

    return eaddrs;
}
#endif // __WIN32__


#if defined(__WIN32__)
/*
 * unicast_address() ::
 * #{flags                 := #{dns_eligible := boolean(),
 *                              transient    := boolean()},
 *   addr                  := socket:address(),
 *   prefix_origin         := ip_prefix_origin(),
 *   suffix_origin         := ip_suffix_origin(),
 *   dad_state             := ip_dad_state(),
 *   valid_lifetime        := ulong(),
 *   preferred_lifetime    := ulong(),
 *   lease_lifetime        := ulong(),
 *   on_link_prefix_length := uint8()}
 */
static
ERL_NIF_TERM encode_adapter_unicast_addr(ErlNifEnv*                  env,
                                         IP_ADAPTER_UNICAST_ADDRESS* addrP)
{
    ERL_NIF_TERM eflags, esa, eporig, esorig, edstate, evlt, eplt, ellt;
    /* ERL_NIF_TERM eplen; - Not on XP */
    ERL_NIF_TERM eua;

    eflags  = encode_adapter_unicast_addr_flags(env, addrP->Flags);
    esa   = encode_adapter_unicast_addr_sockaddr(env,
                                                 addrP->Address.lpSockaddr);
    eporig  = encode_adapter_unicast_addr_porig(env, addrP->PrefixOrigin);
    esorig  = encode_adapter_unicast_addr_sorig(env, addrP->SuffixOrigin);
    edstate = encode_adapter_unicast_addr_dad_state(env, addrP->DadState);
    evlt    = MKUL(env, addrP->ValidLifetime);
    eplt    = MKUL(env, addrP->PreferredLifetime);
    ellt    = MKUL(env, addrP->LeaseLifetime);
    /* eplen   = MKUI(env, addrP->OnLinkPrefixLength); - Not on XP */

    /*
    if (addrP->Address.lpSockaddr->sa_family == AF_INET)
        {
            struct sockaddr* sinP = addrP->Address.lpSockaddr;
            ERL_NIF_TERM keys[] = {esock_atom_flags,
                                   esock_atom_addr,
                                   MKA(env, "raw_addr"),
                                   MKA(env, "raw_addr_ntohl"),
                                   atom_prefix_origin,
                                   atom_suffix_origin,
                                   atom_dad_state,
                                   atom_valid_lifetime,
                                   atom_preferred_lifetime,
                                   atom_lease_lifetime
            };
            ERL_NIF_TERM vals[] = {eflags,
                                   esa,
                                   MKUI(env,  (DWORD) (((struct sockaddr_in *)sinP)->sin_addr.s_addr)),
                                   MKUI(env,  ntohl((DWORD) (((struct sockaddr_in *)sinP)->sin_addr.s_addr))),
                                   eporig,
                                   esorig,
                                   edstate,
                                   evlt,
                                   eplt,
                                   ellt
            };
            size_t       numKeys = NUM(keys);
            size_t       numVals = NUM(vals);

            ESOCK_ASSERT( numKeys == numVals );

            ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &eua) );

    } else
    */
    {
        ERL_NIF_TERM keys[] = {esock_atom_flags,
                               esock_atom_addr,
                               atom_prefix_origin,
                               atom_suffix_origin,
                               atom_dad_state,
                               atom_valid_lifetime,
                               atom_preferred_lifetime,
                               atom_lease_lifetime/* ,
                                                     on_link_prefix_length
                                                     Not on XP */
        };
        ERL_NIF_TERM vals[] = {eflags,
                               esa,
                               eporig,
                               esorig,
                               edstate,
                               evlt,
                               eplt,
                               ellt/*,
                                     eplen
                                     Not pn XP   */
        };
        size_t       numKeys = NUM(keys);
        size_t       numVals = NUM(vals);

        ESOCK_ASSERT( numKeys == numVals );

        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &eua) );

    }

    return eua;
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_unicast_addr_flags(ErlNifEnv* env,
                                               DWORD      flags)
{
    ERL_NIF_TERM map;
    ERL_NIF_TERM dnsEl   = BOOL2ATOM(flags & IP_ADAPTER_ADDRESS_DNS_ELIGIBLE);
    ERL_NIF_TERM trans   = BOOL2ATOM(flags & IP_ADAPTER_ADDRESS_TRANSIENT);
    ERL_NIF_TERM keys[]  = {atom_dns_eligible, atom_transient};
    ERL_NIF_TERM vals[]  = {dnsEl, trans};
    size_t       numKeys = NUM(keys);
    size_t       numVals = NUM(vals);

    ESOCK_ASSERT( numKeys == numVals );

    ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &map) );

    return map;
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_unicast_addr_sockaddr(ErlNifEnv*       env,
                                                  struct sockaddr* addrP)
{
    return encode_sockaddr(env, addrP);
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_unicast_addr_porig(ErlNifEnv*       env,
                                               IP_PREFIX_ORIGIN porig)
{
    ERL_NIF_TERM eporig;

    switch (porig) {
    case IpPrefixOriginOther:
        eporig = atom_other;
        break;

    case IpPrefixOriginManual:
        eporig = atom_manual;
        break;

    case IpPrefixOriginWellKnown:
        eporig = atom_well_known;
        break;

    case IpPrefixOriginDhcp:
        eporig = atom_dhcp;
        break;

    case IpPrefixOriginRouterAdvertisement:
        eporig = atom_router_advertisement;
        break;

    case IpPrefixOriginUnchanged:
        eporig = atom_unchanged;
        break;

    default:
        eporig = MKI(env, (int) porig);
        break;
    }

    return eporig;
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_unicast_addr_sorig(ErlNifEnv*       env,
                                               IP_SUFFIX_ORIGIN sorig)
{
    ERL_NIF_TERM esorig;

    switch (sorig) {
    case IpSuffixOriginOther:
        esorig = atom_other;
        break;

    case IpSuffixOriginManual:
        esorig = atom_manual;
        break;

    case IpSuffixOriginWellKnown:
        esorig = atom_well_known;
        break;

    case IpSuffixOriginDhcp:
        esorig = atom_dhcp;
        break;

    case IpSuffixOriginLinkLayerAddress:
        esorig = atom_link_layer_address;
        break;

    case IpSuffixOriginRandom:
        esorig = atom_random;
        break;

    case IpSuffixOriginUnchanged:
        esorig = atom_unchanged;
        break;

    default:
        esorig = MKI(env, (int) sorig);
        break;
    }

    return esorig;
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_unicast_addr_dad_state(ErlNifEnv*   env,
                                                   IP_DAD_STATE dstate)
{
    ERL_NIF_TERM edstate;

    switch (dstate) {
    case IpDadStateInvalid:
        edstate = atom_invalid;
        break;

    case IpDadStateTentative:
        edstate = atom_tentative;
        break;

    case IpDadStateDuplicate:
        edstate = atom_duplicate;
        break;

    case IpDadStateDeprecated:
        edstate = atom_deprecated;
        break;

    case IpDadStatePreferred:
        edstate = atom_preferred;
        break;

    default:
        edstate = MKI(env, (int) dstate);
        break;
    }

    return edstate;
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_anycast_addrs(ErlNifEnv*                  env,
                                          IP_ADAPTER_ANYCAST_ADDRESS* firstP)
{
    IP_ADAPTER_ANYCAST_ADDRESS* tmp = firstP;
    SocketTArray                ta  = TARRAY_CREATE(16);
    ERL_NIF_TERM eaddrs;
    

    while (tmp != NULL) {
        TARRAY_ADD(ta, encode_adapter_anycast_addr(env, tmp));
        tmp = tmp->Next;
    }

    TARRAY_TOLIST(ta, env, &eaddrs);

    return eaddrs;
}
#endif // __WIN32__


#if defined(__WIN32__)
/*
 * anycast_address() ::
 * #{flags := #{dns_eligible := boolean(),
 *              transient    := boolean()},
 *   addr  := socket:address()}
 */
static
ERL_NIF_TERM encode_adapter_anycast_addr(ErlNifEnv*                  env,
                                         IP_ADAPTER_ANYCAST_ADDRESS* addrP)
{
    ERL_NIF_TERM eflags, esa;
    ERL_NIF_TERM eaa;

    eflags = encode_adapter_anycast_addr_flags(env, addrP->Flags);
    esa    = encode_adapter_anycast_addr_sockaddr(env,
                                                  addrP->Address.lpSockaddr);
    {
        ERL_NIF_TERM keys[] = {esock_atom_flags,
                               esock_atom_addr};
        ERL_NIF_TERM vals[] = {eflags,
                               esa};
        size_t       numKeys = NUM(keys);
        size_t       numVals = NUM(vals);

        ESOCK_ASSERT( numKeys == numVals );

        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &eaa) );

    }

    return eaa;
}
#endif // __WIN32__



#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_anycast_addr_flags(ErlNifEnv* env,
                                               DWORD      flags)
{
    return encode_adapter_unicast_addr_flags(env, flags);
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_anycast_addr_sockaddr(ErlNifEnv*       env,
                                                  struct sockaddr* addrP)
{
    return encode_sockaddr(env, addrP);
}
#endif // __WIN32__



#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_multicast_addrs(ErlNifEnv*                    env,
                                            IP_ADAPTER_MULTICAST_ADDRESS* firstP)
{
    IP_ADAPTER_MULTICAST_ADDRESS* tmp = firstP;
    SocketTArray                  ta  = TARRAY_CREATE(16);
    ERL_NIF_TERM eaddrs;

    while (tmp != NULL) {
        TARRAY_ADD(ta, encode_adapter_multicast_addr(env, tmp));
        tmp = tmp->Next;
    }

    TARRAY_TOLIST(ta, env, &eaddrs);

    return eaddrs;
}
#endif // __WIN32__


#if defined(__WIN32__)
/*
 * multicast_address() ::
 * #{flags := #{dns_eligible := boolean(),
 *              transient    := boolean()},
 *   addr  := socket:address()}
 */
static
ERL_NIF_TERM encode_adapter_multicast_addr(ErlNifEnv*                    env,
                                           IP_ADAPTER_MULTICAST_ADDRESS* addrP)
{
    ERL_NIF_TERM eflags, esa;
    ERL_NIF_TERM ema;

    eflags = encode_adapter_multicast_addr_flags(env, addrP->Flags);
    esa    = encode_adapter_multicast_addr_sockaddr(env,
                                                    addrP->Address.lpSockaddr);
    {
        ERL_NIF_TERM keys[] = {esock_atom_flags,
                               esock_atom_addr};
        ERL_NIF_TERM vals[] = {eflags,
                               esa};
        size_t       numKeys = NUM(keys);
        size_t       numVals = NUM(vals);

        ESOCK_ASSERT( numKeys == numVals );

        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &ema) );

    }

    return ema;
}
#endif // __WIN32__



#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_multicast_addr_flags(ErlNifEnv* env,
                                                 DWORD      flags)
{
    return encode_adapter_unicast_addr_flags(env, flags);
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_multicast_addr_sockaddr(ErlNifEnv*       env,
                                                    struct sockaddr* addrP)
{
    return encode_sockaddr(env, addrP);
}
#endif // __WIN32__



#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_dns_server_addrs(ErlNifEnv*                     env,
                                             IP_ADAPTER_DNS_SERVER_ADDRESS* firstP)
{
    IP_ADAPTER_DNS_SERVER_ADDRESS* tmp = firstP;
    SocketTArray                   ta  = TARRAY_CREATE(16);
    ERL_NIF_TERM eaddrs;

    while (tmp != NULL) {
        TARRAY_ADD(ta, encode_adapter_dns_server_addr(env, tmp));
        tmp = tmp->Next;
    }

    TARRAY_TOLIST(ta, env, &eaddrs);

    return eaddrs;
}
#endif // __WIN32__


#if defined(__WIN32__)
/*
 * dns_server_address() ::
 * #{addr := socket:address()}
 */
static
ERL_NIF_TERM encode_adapter_dns_server_addr(ErlNifEnv*                     env,
                                            IP_ADAPTER_DNS_SERVER_ADDRESS* addrP)
{
    ERL_NIF_TERM esa;
    ERL_NIF_TERM edsa;

    esa = encode_adapter_dns_server_addr_sockaddr(env,
                                                  addrP->Address.lpSockaddr);
    {
        ERL_NIF_TERM keys[] = {esock_atom_addr};
        ERL_NIF_TERM vals[] = {esa};
        size_t       numKeys = NUM(keys);
        size_t       numVals = NUM(vals);

        ESOCK_ASSERT( numKeys == numVals );

        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &edsa) );

    }

    return edsa;
}
#endif // __WIN32__



#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_dns_server_addr_sockaddr(ErlNifEnv*       env,
                                                     struct sockaddr* addrP)
{
    return encode_sockaddr(env, addrP);
}
#endif // __WIN32__



#if defined(__WIN32__)
static
ERL_NIF_TERM encode_if_oper_status(ErlNifEnv* env,
                                   DWORD      status)
{
    ERL_NIF_TERM estatus;

    switch (status) {
    case IfOperStatusUp:
        estatus = esock_atom_up;
        break;
    case IfOperStatusDown:
        estatus = atom_down;
        break;
    case IfOperStatusTesting:
        estatus = atom_testing;
        break;
    case IfOperStatusUnknown:
        estatus = atom_unknown;
        break;
    case IfOperStatusDormant:
        estatus = esock_atom_dormant;
        break;
    case IfOperStatusNotPresent:
        estatus = atom_not_present;
        break;
    case IfOperStatusLowerLayerDown:
        estatus = atom_lower_layer_down;
        break;
    default:
        estatus = MKUI(env, status);
        break;
    }

    return estatus;
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_zone_indices(ErlNifEnv* env,
                                         DWORD*     zoneIndices,
                                         DWORD      len)
{
    SocketTArray ta = TARRAY_CREATE(len);
    DWORD        i;
    ERL_NIF_TERM ezi;

    for (i = 0; i < len; i++) {
        TARRAY_ADD(ta, MKUI(env, zoneIndices[i]));
    }

    TARRAY_TOLIST(ta, env, &ezi);

    return ezi;
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_prefixes(ErlNifEnv*         env,
                                     IP_ADAPTER_PREFIX* firstP)
{
    IP_ADAPTER_PREFIX* tmp = firstP;
    SocketTArray       ta  = TARRAY_CREATE(16);
    ERL_NIF_TERM       eprefs;

    while (tmp != NULL) {
        TARRAY_ADD(ta, encode_adapter_prefix(env, tmp));
        tmp = tmp->Next;
    }

    TARRAY_TOLIST(ta, env, &eprefs);

    return eprefs;
}
#endif // __WIN32__


#if defined(__WIN32__)
/*
 * prerix() ::
 * #{addr   := socket:address(),
 *   length := non_neg_integer()}
 */
static
ERL_NIF_TERM encode_adapter_prefix(ErlNifEnv*         env,
                                   IP_ADAPTER_PREFIX* prefP)
{
    ERL_NIF_TERM esa, eplen;
    ERL_NIF_TERM epref;

    esa   = encode_adapter_prefix_sockaddr(env, prefP->Address.lpSockaddr);
    eplen = MKUI(env, prefP->PrefixLength);
    /*
      if (prefP->Address.lpSockaddr->sa_family == AF_INET)
      {
      struct sockaddr* sinP = prefP->Address.lpSockaddr;
            ERL_NIF_TERM keys[] = {esock_atom_addr,
                                   atom_length,
                                   MKA(env, "raw_addr"),
                                   MKA(env, "raw_addr_ntohl")};
            ERL_NIF_TERM vals[] =
                {esa,             eplen,
                 MKUI(env,  (DWORD) (((struct sockaddr_in *)sinP)->sin_addr.s_addr)),
                 MKUI(env,  ntohl((DWORD) (((struct sockaddr_in *)sinP)->sin_addr.s_addr)))};
            size_t       numKeys = NUM(keys);
            size_t       numVals = NUM(vals);

            ESOCK_ASSERT( numKeys == numVals );

            ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &epref) );
        } else
    */
        {
            ERL_NIF_TERM keys[] = {esock_atom_addr, atom_length};
            ERL_NIF_TERM vals[] = {esa,             eplen};
            size_t       numKeys = NUM(keys);
            size_t       numVals = NUM(vals);

            ESOCK_ASSERT( numKeys == numVals );

            ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &epref) );

        }

    return epref;
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_prefix_sockaddr(ErlNifEnv*       env,
                                            struct sockaddr* addrP)
{
    return encode_sockaddr(env, addrP);
}
#endif // __WIN32__




#if defined(__WIN32__)
static
ERL_NIF_TERM enet_adapter_encode_name(ErlNifEnv* env, WCHAR* name)
{
    return encode_wchar(env, name);
}
#endif // __WIN32__



#if defined(__WIN32__)
static
ERL_NIF_TERM enet_adapter_encode_friendly_name(ErlNifEnv* env, WCHAR* fname)
{
    return encode_wchar(env, fname);
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM encode_adapter_index_map_name(ErlNifEnv* env, WCHAR* name)
{
    return encode_wchar(env, name);
}
#endif // __WIN32__


#if defined(__WIN32__)
static
void make_adapter_index_map(ErlNifEnv*    env,
                            ERL_NIF_TERM  eindex,
                            ERL_NIF_TERM  ename,
                            ERL_NIF_TERM* emap)
{
    ERL_NIF_TERM keys[2];
    ERL_NIF_TERM vals[2];
    size_t       len = NUM(keys); // Just in case...

    /* Index */
    NDBG( ("NET", "make_adapter_index_map -> index: %T\r\n", eindex) );
    keys[0] = atom_index;
    vals[0] = eindex;

    /* Name */
    NDBG( ("NET", "make_adapter_index_map -> name: %T\r\n", ename) );
    keys[1] = esock_atom_name;
    vals[1] = ename;
    
    ESOCK_ASSERT( MKMA(env, keys, vals, len, emap) );
}
#endif // __WIN32__



/* ----------------------------------------------------------------------
 * nif_get_if_entry
 *
 * Description:
 * Get If Entry
 * This is a windows only function!
 *
 * Arguments:
 * Args - A way to pass arguments.
 *        Currently only used for: index and debug:
 *        #{index := non_neg_integer(),
 *          debug := boolean() (optional)}
 *
 * Results:
 * {ok, mib_if_row()} | {error, Reason :: term()}
 */
static
ERL_NIF_TERM nif_get_if_entry(ErlNifEnv*         env,
                              int                argc,
                              const ERL_NIF_TERM argv[])
{
#if !defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM result, eargs;
    DWORD        index;
    BOOLEAN_T    dbg;

    NDBG( ("NET", "nif_get_if_entry -> entry (%d)\r\n", argc) );

    if ((argc != 1) ||
        !IS_MAP(env, argv[0])) {
        return enif_make_badarg(env);
    }
    eargs = argv[0];

    if (!enet_get_if_entry_args_index(env, eargs, &index))
        return enif_make_badarg(env);

    dbg    = enet_get_if_entry_args_debug(env, eargs);

    result = enet_get_if_entry(env, dbg, index);

    NDBG2( dbg,
           ("NET",
            "nif_get_if_entry -> done when result: "
            "\r\n   %T\r\n", result) );

    return result;
#endif
}


#if defined(__WIN32__)
static
BOOLEAN_T enet_get_if_entry_args_index(ErlNifEnv*         env,
                                       const ERL_NIF_TERM eargs,
                                       DWORD*             index)
{
    ERL_NIF_TERM key = atom_index;
    ERL_NIF_TERM eval;
    DWORD        val;
    
    if (!GET_MAP_VAL(env, eargs, key, &eval)) {
        return FALSE;
    } else {
        if (!IS_NUM(env, eval))
            return FALSE;

        if (!GET_UINT(env, eval, &val))
            return FALSE;

        *index = val;
        return TRUE;
    }
}
#endif // __WIN32__


#if defined(__WIN32__)
static
BOOLEAN_T enet_get_if_entry_args_debug(ErlNifEnv*         env,
                                       const ERL_NIF_TERM eargs)
{
    return get_debug(env, eargs);
}
#endif // __WIN32__



#if defined(__WIN32__)
static
ERL_NIF_TERM enet_get_if_entry(ErlNifEnv* env,
                               BOOLEAN_T  dbg,
                               DWORD      index)
{
    DWORD        ret;
    MIB_IFROW    ifRow;
    ERL_NIF_TERM eifRow, result;

    NDBG2( dbg,
           ("NET", "nif_get_if_entry -> entry with"
            "\r\n   index: %d\r\n", index) );

    sys_memzero(&ifRow, sizeof(ifRow));
    ifRow.dwIndex = index;
    ret = GetIfEntry(&ifRow);

    NDBG2( dbg,
           ("NET", "nif_get_if_entry -> get-if-entru result:"
            "\r\n   %d\r\n", ret) );

    switch (ret) {
        /* Success */
    case NO_ERROR:
        eifRow = enet_if_row_encode(env, dbg, &ifRow);
        result = esock_make_ok2(env, eifRow);
        break;

        /* Known errors */
    case ERROR_CAN_NOT_COMPLETE:
        result = esock_make_error(env, atom_can_not_complete);
        break;
    case ERROR_INVALID_DATA:
        result = esock_make_error(env, atom_invalid_data);
        break;
    case ERROR_INVALID_PARAMETER:
        result = esock_make_error(env, atom_invalid_parameter);
        break;
    case ERROR_NOT_FOUND:
        result = esock_make_error(env, esock_atom_not_found);
        break;
    case ERROR_NOT_SUPPORTED:
        result = esock_make_error(env, atom_not_supported);
        break;

        /* Other errors */
    default:
        result = esock_make_error(env, MKI(env, ret));
        break;
    }

    NDBG2( dbg,
           ("NET", "nif_get_if_entry -> done when:"
            "\r\n   result: %T\r\n", result) );

    return result;
}
#endif // __WIN32__


#if defined(__WIN32__)
// Returns: mib_if_row()
static
ERL_NIF_TERM enet_if_row_encode(ErlNifEnv* env,
                                BOOLEAN_T  dbg,
                                MIB_IFROW* rowP)
{
    ERL_NIF_TERM eName, eIndex, eType, eMtu, eSpeed, ePhuysAddr, eAdminStatus;
    ERL_NIF_TERM eOperStatus, eLastChange, eInOctets, eInUcastPkts;
    ERL_NIF_TERM eInNUcastPkts, eInDiscards, eInError, eInUnknownProtos;
    ERL_NIF_TERM eOutOcts, eOutUcastPkts, eOutNUcastPkts, eOutDiscards;
    ERL_NIF_TERM eOutErrors, eOutQLen, eDescr;
    ERL_NIF_TERM erow;

    NDBG2( dbg, ("NET", "enet_if_row_encode -> entry\r\n") );

    eName            = encode_wchar(env, rowP->wszName);
    eIndex           = MKUI(env, rowP->dwIndex);
    eType            = encode_if_type(env, rowP->dwType);
    eMtu             = MKUI(env, rowP->dwMtu);
    eSpeed           = MKUI(env, rowP->dwSpeed);
    ePhuysAddr       = encode_if_row_phys_address(env,
                                                  rowP->dwPhysAddrLen,
                                                  rowP->bPhysAddr);
    eAdminStatus     = encode_if_admin_status(env, rowP->dwAdminStatus);
    eOperStatus      = encode_internal_if_oper_status(env, rowP->dwOperStatus);
    eLastChange      = MKUI(env, rowP->dwLastChange);
    eInOctets        = MKUI(env, rowP->dwInOctets);
    eInUcastPkts     = MKUI(env, rowP->dwInUcastPkts);
    eInNUcastPkts    = MKUI(env, rowP->dwInNUcastPkts);
    eInDiscards      = MKUI(env, rowP->dwInDiscards);
    eInError         = MKUI(env, rowP->dwInErrors);
    eInUnknownProtos = MKUI(env, rowP->dwInUnknownProtos);
    eOutOcts         = MKUI(env, rowP->dwOutOctets);
    eOutUcastPkts    = MKUI(env, rowP->dwOutUcastPkts);
    eOutNUcastPkts   = MKUI(env, rowP->dwOutNUcastPkts);
    eOutDiscards     = MKUI(env, rowP->dwOutDiscards);
    eOutErrors       = MKUI(env, rowP->dwOutErrors);
    eOutQLen         = MKUI(env, rowP->dwOutQLen);
    eDescr           = encode_if_row_description(env,
                                                 rowP->dwDescrLen,
                                                 rowP->bDescr);
    {
        ERL_NIF_TERM keys[] = {esock_atom_name,
                               atom_index,
                               esock_atom_type,
                               esock_atom_mtu,
                               atom_speed,
                               atom_phys_addr,
                               atom_admin_status,
                               atom_internal_oper_status,
                               atom_last_change,
                               atom_in_octets,
                               atom_in_ucast_pkts,
                               atom_in_nucast_pkts,
                               atom_in_discards,
                               atom_in_errors,
                               atom_in_unknown_protos,
                               atom_out_octets,
                               atom_out_ucast_pkts,
                               atom_out_nucast_pkts,
                               atom_out_discards,
                               atom_out_errors,
                               atom_out_qlen,
                               atom_description};
        ERL_NIF_TERM vals[] = {eName,
                               eIndex,
                               eType,
                               eMtu,
                               eSpeed,
                               ePhuysAddr,
                               eAdminStatus,
                               eOperStatus,
                               eLastChange,
                               eInOctets,
                               eInUcastPkts,
                               eInNUcastPkts,
                               eInDiscards,
                               eInError,
                               eInUnknownProtos,
                               eOutOcts,
                               eOutUcastPkts,
                               eOutNUcastPkts,
                               eOutDiscards,
                               eOutErrors,
                               eOutQLen,
                               eDescr};
        size_t       numKeys = NUM(keys);
        size_t       numVals = NUM(vals);

        ESOCK_ASSERT( numKeys == numVals );
    
        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &erow) );
    }
    
    NDBG2( dbg,
           ("NET", "enet_if_row_encode -> done with:"
            "\r\n   result: %T"
            "\r\n", erow) );

    return erow;

}
#endif



#if defined(__WIN32__)
static
ERL_NIF_TERM encode_if_type(ErlNifEnv* env,
                            DWORD      type)
{
    ERL_NIF_TERM   etype;

    switch (type) {
    case IF_TYPE_OTHER:
        etype = atom_other;
        break;
    case IF_TYPE_ETHERNET_CSMACD:
        etype = atom_ethernet_csmacd;
        break;
    case IF_TYPE_ISO88025_TOKENRING:
        etype = atom_iso88025_tokenring;
        break;
    case IF_TYPE_FDDI:
        etype = atom_fddi;
        break;
    case IF_TYPE_PPP:
        etype = atom_ppp;
        break;
    case IF_TYPE_SOFTWARE_LOOPBACK:
        etype = atom_software_loopback;
        break;
    case IF_TYPE_ATM:
        etype = atom_atm;
        break;
    case IF_TYPE_IEEE80211:
        etype = atom_ieee80211;
        break;
    case IF_TYPE_TUNNEL:
        etype = atom_tunnel;
        break;
    case IF_TYPE_IEEE1394:
        etype = atom_ieee1394;
        break;
    case IF_TYPE_IEEE80216_WMAN:
        etype = atom_ieee80216_wman;
        break;
    case IF_TYPE_WWANPP:
        etype = atom_wwanpp;
        break;
    case IF_TYPE_WWANPP2:
        etype = atom_wwanpp2;
        break;
    default:
        etype = MKUI(env, type);
        break;
    }

    return etype;

}
#endif // __WIN32__


#if defined(__WIN32__)
/*
 * The description is defined as a UCHAR array with a *max* length
 * of MAXLEN_IFDESCR. But the actual length is defined by the 
 * dwDescrLen field.
 * The documentation does not specify that its a NULL terminated
 * string, but in practice that is what it is. But, if the string
 * *is* MAXLEN_IFDESCR there is no room for a NULL terminator...
 * So, just to be on the safe side we copy the text into a buffer
 * of length = len + 1 and add an extra NULL character at the last
 * position. Then we can handle the it as any NULL terminated string.
 */
static
ERL_NIF_TERM encode_if_row_description(ErlNifEnv* env,
                                       DWORD      len,
                                       UCHAR*     buf)
{
    ERL_NIF_TERM edesc;
    UCHAR*       tmp = MALLOC(len + 1);

    ESOCK_ASSERT( tmp != NULL );

    sys_memcpy(tmp, buf, len);
    tmp[len] = 0;

    edesc = MKS(env, tmp);

    FREE(tmp);

    return edesc;
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM encode_if_admin_status(ErlNifEnv* env,
                                    DWORD      status)
{
    ERL_NIF_TERM estatus;

    switch (status) {
    case IF_OPER_STATUS_NON_OPERATIONAL:
        estatus = atom_non_operational;
        break;
    case IF_OPER_STATUS_UNREACHABLE:
        estatus = atom_unreachable;
        break;
    case IF_OPER_STATUS_DISCONNECTED:
        estatus = atom_disconnected;
        break;
    case IF_OPER_STATUS_CONNECTING:
        estatus = esock_atom_connecting;
        break;
    case IF_OPER_STATUS_CONNECTED:
        estatus = esock_atom_connected;
        break;
    case IF_OPER_STATUS_OPERATIONAL:
        estatus = atom_operational;
        break;
    default:
        estatus = MKUI(env, status);
        break;
    }

    return estatus;
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM encode_internal_if_oper_status(ErlNifEnv* env,
                                            DWORD      status)
{
    ERL_NIF_TERM estatus;

    switch (status) {
    case IF_OPER_STATUS_NON_OPERATIONAL:
        estatus = atom_non_operational;
        break;
    case IF_OPER_STATUS_UNREACHABLE:
        estatus = atom_unreachable;
        break;
    case IF_OPER_STATUS_DISCONNECTED:
        estatus = atom_disconnected;
        break;
    case IF_OPER_STATUS_CONNECTING:
        estatus = esock_atom_connecting;
        break;
    case IF_OPER_STATUS_CONNECTED:
        estatus = esock_atom_connected;
        break;
    case IF_OPER_STATUS_OPERATIONAL:
        estatus = atom_operational;
        break;
    default:
        estatus = MKUI(env, status);
        break;
    }

    return estatus;
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM encode_if_row_phys_address(ErlNifEnv* env,
                                        DWORD      len,
                                        UCHAR*     buf)
{
    return encode_uchar(env, len, buf);
}
#endif // __WIN32__



/* ----------------------------------------------------------------------
 * nif_get_interface_info
 *
 * Description:
 * Get interface info table (only IPv4 interfaces)
 * This is a windows only function!
 *
 * Physical Interfaces?
 *
 * Arguments:
 * Args - A way to pass arguments.
 *        Currently only used for debug.
 *
 * Results:
 * {ok, [ip_adapter_index_map()]} | {error, Reason :: term()}
 */

static
ERL_NIF_TERM nif_get_interface_info(ErlNifEnv*         env,
                                    int                argc,
                                    const ERL_NIF_TERM argv[])
{
#if !defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM result, eargs;
    BOOLEAN_T    dbg;

    NDBG( ("NET", "nif_get_interface_info -> entry (%d)\r\n", argc) );

    if ((argc != 1) ||
        !IS_MAP(env, argv[0])) {
        return enif_make_badarg(env);
    }
    eargs = argv[0];

    dbg    = enet_get_interface_info_args_debug(env, eargs);

    result = enet_get_interface_info(env, dbg);

    NDBG2( dbg,
           ("NET",
            "nif_get_interface_info -> done when result: "
            "\r\n   %T\r\n", result) );

    return result;
#endif
}


#if defined(__WIN32__)
static
BOOLEAN_T enet_get_interface_info_args_debug(ErlNifEnv*         env,
                                             const ERL_NIF_TERM eargs)
{
    return get_debug(env, eargs);
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM enet_get_interface_info(ErlNifEnv* env,
                                     BOOLEAN_T  dbg)
{
    int                i;
    DWORD              ret;
    unsigned long      infoSize = 4 * 1024;
    IP_INTERFACE_INFO* infoP    = (IP_INTERFACE_INFO*) MALLOC(infoSize);
    ERL_NIF_TERM       eret, einfo, result;

    for (i = 17;  i;  i--) {

        NDBG2( dbg,
               ("NET", "enet_get_interface_info -> try get info with: "
                "\r\n   infoSize: %d"
                "\r\n", infoSize) );

        ret   = GetInterfaceInfo(infoP, &infoSize);

        NDBG2( dbg,
               ("NET", "enet_get_interface_info -> "
                "get-info result: %d (%d)\r\n", ret, infoSize) );

        if (ret == NO_ERROR) {
            /* We are done! */
            break;
        } else if (ret == ERROR_INSUFFICIENT_BUFFER) {
            /* Not large enough */
            infoP = REALLOC(infoP, infoSize);
            continue;
        } else {
            /* Failure */
            i = 0;
        }
    }

    NDBG2( dbg,
           ("NET", "enet_get_interface_info -> "
            "done when get info counter: %d\r\n", i) );

    if (! i) {

        NDBG2( dbg,
               ("NET", "enet_get_interface_info -> "
                "try encode error (%d)\r\n", ret) );

        FREE(infoP);

        switch (ret) {
        case ERROR_INSUFFICIENT_BUFFER:
            eret = atom_insufficient_buffer;
            break;
        case ERROR_INVALID_PARAMETER:
            eret = atom_invalid_parameter;
            break;
        case ERROR_NO_DATA:
            eret = atom_no_data;
            break;
        case ERROR_NOT_SUPPORTED:
            eret = atom_not_supported;
            break;
        default:
            eret = MKI(env, ret);
            break;
        }

        result = esock_make_error(env, eret);

    } else {

        NDBG2( dbg,
               ("NET", "enet_get_interface_info -> try encode info\r\n") );

        einfo  = enet_interface_info_encode(env, dbg, infoP);
        result = esock_make_ok2(env, einfo);
        
        FREE(infoP);
    }

    NDBG2( dbg,
           ("NET", "enet_get_interface_info -> done with:"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}
#endif // __WIN32__


#if defined(__WIN32__)
// Returns: [#{index := integer(), name := string()}]
static
ERL_NIF_TERM enet_interface_info_encode(ErlNifEnv*         env,
                                        BOOLEAN_T          dbg,
                                        IP_INTERFACE_INFO* infoP)
{
    ERL_NIF_TERM result;
    LONG         num = infoP->NumAdapters;

    NDBG2( dbg,
           ("NET", "enet_interface_info_encode -> entry with"
            "\r\n   num: %d"
            "\r\n", num) );

    if (num > 0) {
        ERL_NIF_TERM* array = MALLOC(num * sizeof(ERL_NIF_TERM));
        LONG          i     = 0;

        while (i < num) {
            ERL_NIF_TERM entry;

            NDBG2( dbg,
                   ("NET", "enet_interface_info_encode -> "
                    "try encode adapter %d"
                    "\r\n", i) );

            encode_adapter_index_map(env, dbg, &infoP->Adapter[i], &entry);

            array[i] = entry;
            i++;
        }

        result = MKLA(env, array, num);
        FREE(array);

    } else {
        result = MKEL(env);
    }

    NDBG2( dbg,
           ("NET", "enet_get_interface_info -> done with:"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;

}
#endif // __WIN32__


#if defined(__WIN32__)
/*
 * ip_adapter_index_map() :: #{name  :: string(),
 *                             index :: non_neg_integer()}
 */
static
void encode_adapter_index_map(ErlNifEnv*            env,
                              BOOLEAN_T             dbg,
                              IP_ADAPTER_INDEX_MAP* adapterP,
                              ERL_NIF_TERM*         eadapter)
{
    ERL_NIF_TERM eindex = MKI(env, adapterP->Index);
    ERL_NIF_TERM ename  = encode_adapter_index_map_name(env, adapterP->Name);
    ERL_NIF_TERM map;

    NDBG2( dbg,
           ("NET", "encode_adapter_index_map -> map fields: "
            "\r\n   index: %T"
            "\r\n   name:  %T"
            "\r\n", eindex, ename) );

    make_adapter_index_map(env, eindex, ename, &map);

    NDBG2( dbg,
           ("NET", "encode_adapter_index_map -> encoded map: %T\r\n", map) );

    *eadapter = map;
}
#endif // __WIN32__



/* ----------------------------------------------------------------------
 * nif_get_ip_address_table
 *
 * Description:
 * Get ip address table.
 * This is a windows only function!
 *
 * Active Interfaces?
 *
 * Arguments:
 * Args - A way to pass arguments.
 *        Currently only used for debug.
 *
 * Returns:
 * {ok, [mib_ip_address_row()]} | {error, Reason :: term()}
 */

static
ERL_NIF_TERM nif_get_ip_address_table(ErlNifEnv*         env,
                                      int                argc,
                                      const ERL_NIF_TERM argv[])
{
#if !defined(__WIN32__)
    return enif_raise_exception(env, MKA(env, "notsup"));
#else
    ERL_NIF_TERM result, eargs;
    BOOLEAN_T    dbg;

    NDBG( ("NET", "nif_get_ip_address_table -> entry (%d)\r\n", argc) );

    if ((argc != 1) ||
        !IS_MAP(env, argv[0])) {
        return enif_make_badarg(env);
    }
    eargs = argv[0];

    dbg    = enet_get_ip_address_table_args_debug(env, eargs);

    result = enet_get_ip_address_table(env, dbg);

    NDBG2( dbg,
           ("NET",
            "nif_get_ip_address_table -> done when result: "
            "\r\n   %T\r\n", result) );

    return result;
#endif
}



#if defined(__WIN32__)
static
BOOLEAN_T enet_get_ip_address_table_args_debug(ErlNifEnv*         env,
                                               const ERL_NIF_TERM eargs)
{
    return get_debug(env, eargs);
}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM enet_get_ip_address_table(ErlNifEnv* env,
                                       BOOLEAN_T  dbg)
{
    int                i;
    DWORD              ret;
    /* The table is *not* just an array of 'row',
     * but that is the significant part, so...
     */
    unsigned long      tabSize    = 16*sizeof(MIB_IPADDRROW);
    MIB_IPADDRTABLE*   ipAddrTabP = (MIB_IPADDRTABLE*) MALLOC(tabSize);
    ERL_NIF_TERM       eret, etable, result;

    for (i = 17;  i;  i--) {

        NDBG2( dbg,
               ("NET", "enet_get_ip_address_table -> try get table with: "
                "\r\n   tabSize: %d"
                "\r\n", tabSize) );

        ret = GetIpAddrTable(ipAddrTabP, &tabSize, FALSE);

        NDBG2( dbg,
               ("NET", "enet_get_ip_address_table -> "
                "get-tab result: %d (%d)\r\n", ret, tabSize) );

        ipAddrTabP = REALLOC(ipAddrTabP, tabSize);
        if (ret == NO_ERROR) break;
        if (ret == ERROR_INSUFFICIENT_BUFFER) continue;
        i = 0;
    }

    NDBG2( dbg,
           ("NET", "enet_get_ip_address_table -> "
            "done when get-tab counter: %d\r\n", i) );

    if (! i) {

        NDBG2( dbg,
               ("NET",
                "enet_get_ip_address_table -> try transform error\r\n") );

        FREE(ipAddrTabP);

        switch (ret) {
        case ERROR_INSUFFICIENT_BUFFER:
            eret = atom_insufficient_buffer;
            break;
        case ERROR_INVALID_PARAMETER:
            eret = atom_invalid_parameter;
            break;
        case ERROR_NOT_SUPPORTED:
            eret = atom_not_supported;
            break;
        default:
            eret = MKI(env, ret);
            break;
        }

        result = esock_make_error(env, eret);

    } else {

        NDBG2( dbg,
               ("NET",
                "enet_get_ip_address_table -> try transform table\r\n") );

        etable = enet_get_ip_address_table_encode(env, dbg, ipAddrTabP);
        result = esock_make_ok2(env, etable);
        
        FREE(ipAddrTabP);
    }

    NDBG2( dbg,
           ("NET", "enet_get_ip_address_table -> done with:"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;
}
#endif // __WIN32__



#if defined(__WIN32__)
// Returns: [row()]
static
ERL_NIF_TERM enet_get_ip_address_table_encode(ErlNifEnv*       env,
                                              BOOLEAN_T        dbg,
                                              MIB_IPADDRTABLE* tabP)
{
    ERL_NIF_TERM result;
    LONG         num = tabP->dwNumEntries;

    NDBG2( dbg,
           ("NET", "enet_get_ip_address_table_encode -> entry with"
            "\r\n   num: %d"
            "\r\n", num) );

    if (num > 0) {
        ERL_NIF_TERM* array = MALLOC(num * sizeof(ERL_NIF_TERM));
        LONG          i     = 0;

        while (i < num) {
            ERL_NIF_TERM entry;

            NDBG2( dbg,
                   ("NET", "enet_interface_info_encode -> "
                    "try encode ip-address-row %d"
                    "\r\n", i) );

            entry = encode_ip_address_row(env, dbg, &tabP->table[i]);

            array[i] = entry;
            i++;
        }

        result = MKLA(env, array, num);
        FREE(array);

    } else {
        result = MKEL(env);
    }

    NDBG2( dbg,
           ("NET", "enet_get_ip_address_table -> done with:"
            "\r\n   result: %T"
            "\r\n", result) );

    return result;

}
#endif // __WIN32__


#if defined(__WIN32__)
static
ERL_NIF_TERM encode_ip_address_row(ErlNifEnv*     env,
                                   BOOLEAN_T      dbg,
                                   MIB_IPADDRROW* rowP)
{
    ERL_NIF_TERM eaddr      = encode_ip_address_row_addr(env,
                                                         dbg, "Addr",
                                                         rowP->dwAddr);
    ERL_NIF_TERM eindex     = MKUL(env, rowP->dwIndex);
    ERL_NIF_TERM emask      = encode_ip_address_row_addr(env,
                                                         dbg, "Mask",
                                                         rowP->dwMask);
    ERL_NIF_TERM eBCastAddr = encode_ip_address_row_addr(env,
                                                         dbg, "BCaseAddr",
                                                         rowP->dwBCastAddr);
    ERL_NIF_TERM eReasmSize = MKUL(env, rowP->dwReasmSize);
    ERL_NIF_TERM map;

    NDBG2( dbg,
           ("NET", "encode_ipAddress_row_map -> map fields: "
            "\r\n   address:    %T"
            "\r\n   index:      %T"
            "\r\n   mask:       %T"
            "\r\n   bcas-addr:  %T"
            "\r\n   reasm-size: %T"
            "\r\n", eaddr, eindex, emask, eBCastAddr, eReasmSize) );

    make_ip_address_row(env, eaddr, eindex, emask, eBCastAddr, eReasmSize, &map);

    NDBG2( dbg,
           ("NET", "encode_ip_address_row -> encoded map: %T\r\n", map) );

    return map;
}
#endif // __WIN32__



#if defined(__WIN32__)
/* Converts an *IPv4* address to an erlang term (4-tuple) */
static
ERL_NIF_TERM encode_ip_address_row_addr(ErlNifEnv*  env,
                                        BOOLEAN_T   dbg,
                                        const char* descr,
                                        DWORD       addr)
{
    struct in_addr a;
    ERL_NIF_TERM   ea;

    NDBG2( dbg,
           ("NET",
            "encode_ip_address_row_addr -> entry with: "
            "\r\n   %s: %lu\r\n", descr, addr) );
    
    a.s_addr = addr;

    esock_encode_in_addr(env, &a, &ea);

    return ea;
}
#endif // __WIN32__


#if defined(__WIN32__)
static
void make_ip_address_row(ErlNifEnv*    env,
                         ERL_NIF_TERM  eaddr,
                         ERL_NIF_TERM  eindex,
                         ERL_NIF_TERM  emask,
                         ERL_NIF_TERM  eBCastAddr,
                         ERL_NIF_TERM  eReasmSize,
                         ERL_NIF_TERM* iar)
{
    ERL_NIF_TERM keys[]  = {esock_atom_addr,
                            atom_index,
                            atom_mask,
                            atom_bcast_addr,
                            atom_reasm_size};
    ERL_NIF_TERM vals[]  = {eaddr, eindex, emask, eBCastAddr, eReasmSize};
    size_t       numKeys = NUM(keys);

    ESOCK_ASSERT( numKeys == NUM(vals) );
    
    ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, iar) );

}
#endif // __WIN32__




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
#elif defined(HAVE_IF_NAMETOINDEX)
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
#else
    return esock_make_error(env, esock_atom_enotsup);
#endif
}



#if !defined(__WIN32__) && defined(HAVE_IF_NAMETOINDEX)
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
#elif defined(HAVE_IF_INDEXTONAME)
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
#else
    return esock_make_error(env, esock_atom_enotsup);
#endif
}



#if !defined(__WIN32__) && defined(HAVE_IF_INDEXTONAME)
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
#elif defined(HAVE_IF_NAMEINDEX) && defined(HAVE_IF_FREENAMEINDEX)
    ERL_NIF_TERM result;

    NDBG( ("NET", "nif_if_names -> entry (%d)\r\n", argc) );

    if (argc != 0) {
        return enif_make_badarg(env);
    }

    result = enet_if_names(env);

    NDBG( ("NET", "nif_if_names -> done when result: %T\r\n", result) );

    return result;
#else
    return esock_make_error(env, esock_atom_enotsup);
#endif
}



/* if_nameindex and if_freenameindex were added in Android 7.0 Nougat. With
the Android NDK Unified Headers, check that the build is targeting at least
the corresponding API level 24. */
/* Can we replace the ANDROID tests with the HAVE_... ? */
#if !defined(__WIN32__) && !(defined(__ANDROID__) && (__ANDROID_API__ < 24))
#if defined(HAVE_IF_NAMEINDEX) && defined(HAVE_IF_FREENAMEINDEX)
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
#endif // if defined(HAVE_IF_NAMEINDEX) && defined(HAVE_IF_FREENAMEINDEX)
#endif // if !defined(__WIN32__) && ...



/* ----------------------------------------------------------------------
 *  U t i l i t y   F u n c t i o n s
 * ----------------------------------------------------------------------
 */

/* The erlang format for a set of flags is a list of atoms.
 * A special case is when there is no flags, which is
 * represented by the atom undefined.
 */

static
ERL_NIF_TERM encode_sockaddr(ErlNifEnv* env, struct sockaddr* sa)
{
    ERL_NIF_TERM esa;

    if (sa != NULL) {
        esock_encode_sockaddr(env, (ESockAddress*) sa, -1, &esa);
    } else {
        esa = esock_atom_undefined;
    }

    return esa;
}



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



/* Calculate the length of the address info linked list
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
    proto = MKI(env, addrInfoP->ai_protocol);
    esock_encode_sockaddr(env,
                          (ESockAddress*) addrInfoP->ai_addr,
                          addrInfoP->ai_addrlen,
                          &addr);
    
    make_address_info(env, fam, type, proto, addr, &addrInfo);
    return addrInfo;
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

    esock_encode_domain(env, family, &efam);
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

    esock_encode_type(env, socktype, &etype);
    return etype;
}



static
void make_address_info(ErlNifEnv*    env,
                       ERL_NIF_TERM  fam,
                       ERL_NIF_TERM  sockType,
                       ERL_NIF_TERM  proto,
                       ERL_NIF_TERM  addr,
                       ERL_NIF_TERM* ai)
{
    ERL_NIF_TERM keys[]  = {esock_atom_family,
                            esock_atom_type,
                            esock_atom_protocol,
                            esock_atom_addr};
    ERL_NIF_TERM vals[]  = {fam, sockType, proto, addr};
    size_t       numKeys = NUM(keys);
    
    ESOCK_ASSERT( numKeys == NUM(vals) );
    ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, ai) );
}





#ifdef HAVE_SETNS
/* We should really have another API, so that we can return errno... */

/* *** change network namespace ***
 * Retrieve the current namespace and set the new.
 * Return result and previous namespace if successful.
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


static
ERL_NIF_TERM decode_bool(ErlNifEnv*   env,
                         ERL_NIF_TERM ebool,
                         BOOLEAN_T*   ibool)
{
    if (COMPARE(ebool, esock_atom_true) == 0) {
        *ibool = TRUE;
        return esock_atom_ok;
    } else if (COMPARE(ebool, esock_atom_false) == 0) {
        *ibool = FALSE;
        return esock_atom_ok;
    } else {
        return esock_make_error(env, esock_atom_einval);
    }
}



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
    {"nif_gethostname",      0, nif_gethostname,   0},

    /* address and name translation in protocol-independent manner */
    {"nif_getnameinfo",      2, nif_getnameinfo,   0},
    {"nif_getaddrinfo",      3, nif_getaddrinfo,   0},
    
    {"nif_getifaddrs",       1, nif_getifaddrs,       ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"nif_get_adapters_addresses", 1, nif_get_adapters_addresses, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"nif_get_if_entry",       1, nif_get_if_entry,       ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"nif_get_interface_info", 1, nif_get_interface_info, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"nif_get_ip_address_table", 1, nif_get_ip_address_table, ERL_NIF_DIRTY_JOB_IO_BOUND},

    /* Network interface (name and/or index) functions */
    {"nif_if_name2index",    1, nif_if_name2index, 0},
    {"nif_if_index2name",    1, nif_if_index2name, 0},
    {"nif_if_names",         0, nif_if_names,      0}
};


#if defined(__WIN32__)
/*
 * The assumption is that the 'name' string is NULL terminated
 */
static
ERL_NIF_TERM encode_wchar(ErlNifEnv* env, WCHAR* name)
{
    ERL_NIF_TERM result;
    int          len = WideCharToMultiByte(CP_UTF8, 0,
                                           name, -1,
                                           NULL, 0, NULL, NULL);

    if (!len) {
        result = esock_atom_undefined;
    } else {
        char* buf = (char*) MALLOC(len+1);

        if (0 == WideCharToMultiByte(CP_UTF8, 0,
                                     name, -1,
                                     buf, len, NULL, NULL)) {
            DWORD error = GetLastError();

            switch (error) {
            case ERROR_INSUFFICIENT_BUFFER:
                result = atom_insufficient_buffer;
                break;
            case ERROR_INVALID_FLAGS:
                result = atom_invalid_flags;
                break;
            case ERROR_INVALID_PARAMETER:
                result = atom_invalid_parameter;
                break;
            case ERROR_NO_UNICODE_TRANSLATION:
                result = atom_no_uniconde_traslation;
                break;
            default:
                result = MKI(env, error);
                break;
            }
        } else {
            result = MKS(env, buf);
        }

        FREE(buf);
    }

    return result;
}
#endif // __WIN32__


#if defined(__WIN32__)
/*
 * This builds a binary term from an array of uchar
 */
static
ERL_NIF_TERM encode_uchar(ErlNifEnv* env,
                          DWORD      len,
                          UCHAR*     buf)
{
    ERL_NIF_TERM   ebuf;
    unsigned char* p;

    p = enif_make_new_binary(env, len, &ebuf);
    ESOCK_ASSERT( p != NULL );
    sys_memcpy(p, buf, len);

    return ebuf;
}
#endif // __WIN32__


static
BOOLEAN_T get_debug(ErlNifEnv*   env,
                    ERL_NIF_TERM map)
{
    /*
     * We need to do this here since the "proper" atom has not been
     * created when this function is called.
     */
    ERL_NIF_TERM debug = MKA(env, "debug");
    
    return esock_get_bool_from_map(env, map, debug, NET_NIF_DEBUG_DEFAULT);
}


/* =======================================================================
 * load_info - A map of misc info (e.g global debug)
 */

static
int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
#if !defined(__WIN32__)    
    // We should make it possible to use load_info to get default values
    data.debug = get_debug(env, load_info);

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
#endif
