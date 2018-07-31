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
 *  Purpose : Utility "stuff" for socket and net.
 * ----------------------------------------------------------------------
 *
 */

#ifndef SOCKET_INT_H__
#define SOCKET_INT_H__

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef __WIN32__

/* All this just to replace sys/socket.h, netinet/in.h and sys/un.h??? */
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

#else /* !__WIN32__ */

#include <sys/socket.h>
#include <netinet/in.h>
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif

#endif

#include <erl_nif.h>

/* The general purpose sockaddr */
typedef union {
    /* General sockaddr */
    struct sockaddr     sa;

    /* IPv4 sockaddr */
    struct sockaddr_in  in4;

    /* IPv6 sockaddr */
#if defined(HAVE_IN6) && defined(AF_INET6)
    struct sockaddr_in6 in6;
#endif

    /* Unix Domain Socket sockaddr */
#if defined(HAVE_SYS_UN_H)
    struct sockaddr_un  un;
#endif

} SocketAddress;


/* *** Boolean *type* stuff... *** */
typedef unsigned int BOOLEAN_T;
#define TRUE  1
#define FALSE 0

#define BOOL2ATOM(__B__) ((__B__) ? esock_atom_true : esock_atom_false)

#define B2S(__B__) ((__B__) ? "true" : "false")

/* Misc error strings */
#define ESOCK_STR_EAFNOSUPPORT "eafnosupport"
#define ESOCK_STR_EAGAIN       "eagain"
#define ESOCK_STR_EINVAL       "einval"


/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 * "Global" atoms
 */
extern ERL_NIF_TERM esock_atom_addr;
extern ERL_NIF_TERM esock_atom_any;
extern ERL_NIF_TERM esock_atom_credentials;
extern ERL_NIF_TERM esock_atom_ctrl;
extern ERL_NIF_TERM esock_atom_ctrunc;
extern ERL_NIF_TERM esock_atom_data;
extern ERL_NIF_TERM esock_atom_debug;
extern ERL_NIF_TERM esock_atom_dgram;
extern ERL_NIF_TERM esock_atom_eor;
extern ERL_NIF_TERM esock_atom_error;
extern ERL_NIF_TERM esock_atom_errqueue;
extern ERL_NIF_TERM esock_atom_false;
extern ERL_NIF_TERM esock_atom_family;
extern ERL_NIF_TERM esock_atom_flags;
extern ERL_NIF_TERM esock_atom_flowinfo;
extern ERL_NIF_TERM esock_atom_ifindex;
extern ERL_NIF_TERM esock_atom_inet;
extern ERL_NIF_TERM esock_atom_inet6;
extern ERL_NIF_TERM esock_atom_iov;
extern ERL_NIF_TERM esock_atom_ip;
extern ERL_NIF_TERM esock_atom_ipv6;
extern ERL_NIF_TERM esock_atom_level;
extern ERL_NIF_TERM esock_atom_local;
extern ERL_NIF_TERM esock_atom_loopback;
extern ERL_NIF_TERM esock_atom_lowdelay;
extern ERL_NIF_TERM esock_atom_mincost;
extern ERL_NIF_TERM esock_atom_ok;
extern ERL_NIF_TERM esock_atom_oob;
extern ERL_NIF_TERM esock_atom_origdstaddr;
extern ERL_NIF_TERM esock_atom_path;
extern ERL_NIF_TERM esock_atom_pktinfo;
extern ERL_NIF_TERM esock_atom_port;
extern ERL_NIF_TERM esock_atom_protocol;
extern ERL_NIF_TERM esock_atom_raw;
extern ERL_NIF_TERM esock_atom_rdm;
extern ERL_NIF_TERM esock_atom_reliability;
extern ERL_NIF_TERM esock_atom_rights;
extern ERL_NIF_TERM esock_atom_scope_id;
extern ERL_NIF_TERM esock_atom_sctp;
extern ERL_NIF_TERM esock_atom_seqpacket;
extern ERL_NIF_TERM esock_atom_spec_dst;
extern ERL_NIF_TERM esock_atom_stream;
extern ERL_NIF_TERM esock_atom_tcp;
extern ERL_NIF_TERM esock_atom_throughput;
extern ERL_NIF_TERM esock_atom_tos;
extern ERL_NIF_TERM esock_atom_true;
extern ERL_NIF_TERM esock_atom_trunc;
extern ERL_NIF_TERM esock_atom_ttl;
extern ERL_NIF_TERM esock_atom_type;
extern ERL_NIF_TERM esock_atom_udp;
extern ERL_NIF_TERM esock_atom_undefined;


/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 * Error value (=reason) atoms
 */
extern ERL_NIF_TERM esock_atom_eafnosupport;
extern ERL_NIF_TERM esock_atom_eagain;
extern ERL_NIF_TERM esock_atom_einval;



/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 * Various wrapper macros for enif functions
 */
#define MALLOC(SZ)          enif_alloc((SZ))
#define REALLOC(P, SZ)      enif_realloc((P), (SZ))
#define FREE(P)             enif_free((P))

#define MKA(E,S)            enif_make_atom((E), (S))
#define MKBIN(E,B)          enif_make_binary((E), (B))
#define MKI(E,I)            enif_make_int((E), (I))
#define MKL(E,L)            enif_make_long((E), (L))
#define MKLA(E,A,L)         enif_make_list_from_array((E), (A), (L))
#define MKEL(E)             enif_make_list((E), 0)
#define MKMA(E,KA,VA,L,M)   enif_make_map_from_arrays((E), (KA), (VA), (L), (M))
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
#define MKTA(E, A, AL)      enif_make_tuple_from_array((E), (A), (AL))
#define MKUI(E,UI)          enif_make_uint((E), (UI))
#define MKUL(E,UL)          enif_make_ulong((E), (UL))

#define MCREATE(N)          enif_mutex_create((N))
#define MDESTROY(M)         enif_mutex_destroy((M))
#define MLOCK(M)            enif_mutex_lock((M))
#define MUNLOCK(M)          enif_mutex_unlock((M))

#define MONP(E,D,P,M)       enif_monitor_process((E), (D), (P), (M))
#define DEMONP(E,D,M)       enif_demonitor_process((E), (D), (M))

#define SELECT(E,FD,M,O,P,R)                                    \
    if (enif_select((E), (FD), (M), (O), (P), (R)) < 0)         \
        return enif_make_badarg((E));

#define COMPARE(A, B)       enif_compare((A), (B))

#define IS_ATOM(E,  TE) enif_is_atom((E),   (TE))
#define IS_BIN(E,   TE) enif_is_binary((E), (TE))
#define IS_LIST(E,  TE) enif_is_list((E),   (TE))
#define IS_MAP(E,   TE) enif_is_map((E), (TE))
#define IS_NUM(E,   TE) enif_is_number((E), (TE))
#define IS_TUPLE(E, TE) enif_is_tuple((E),  (TE))

#define GET_ATOM_LEN(E, TE, LP) \
    enif_get_atom_length((E), (TE), (LP), ERL_NIF_LATIN1)
#define GET_ATOM(E, TE, BP, MAX) \
    enif_get_atom((E), (TE), (BP), (MAX), ERL_NIF_LATIN1)
#define GET_BIN(E, TE, BP)          enif_inspect_iolist_as_binary((E), (TE), (BP))
#define GET_INT(E, TE, IP)          enif_get_int((E), (TE), (IP))
#define GET_LIST_ELEM(E, L, HP, TP) enif_get_list_cell((E), (L), (HP), (TP))
#define GET_LIST_LEN(E, L, LP)      enif_get_list_length((E), (L), (LP))
#define GET_LONG(E, TE, LP)         enif_get_long((E), (TE), (LP))
#define GET_LPID(E, T, P)           enif_get_local_pid((E), (T), (P))
#define GET_STR(E, L, B, SZ)      \
    enif_get_string((E), (L), (B), (SZ), ERL_NIF_LATIN1)
#define GET_UINT(E, TE, UIP)        enif_get_uint((E), (TE), (UIP))
#define GET_ULONG(E, TE, ULP)       enif_get_long((E), (TE), (ULP))
#define GET_TUPLE(E, TE, TSZ, TA)   enif_get_tuple((E), (TE), (TSZ), (TA))
#define GET_MAP_VAL(E, M, K, V)     enif_get_map_value((E), (M), (K), (V))

#define ALLOC_BIN(SZ, BP)         enif_alloc_binary((SZ), (BP))
#define REALLOC_BIN(SZ, BP)       enif_realloc_binary((SZ), (BP))


#endif // SOCKET_INT_H__
