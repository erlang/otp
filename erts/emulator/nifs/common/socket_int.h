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
extern ERL_NIF_TERM esock_atom_abort;
extern ERL_NIF_TERM esock_atom_accept;
extern ERL_NIF_TERM esock_atom_acceptconn;
extern ERL_NIF_TERM esock_atom_acceptfilter;
extern ERL_NIF_TERM esock_atom_adaption_layer;
extern ERL_NIF_TERM esock_atom_addr;
extern ERL_NIF_TERM esock_atom_addrform;
extern ERL_NIF_TERM esock_atom_add_membership;
extern ERL_NIF_TERM esock_atom_add_source_membership;
extern ERL_NIF_TERM esock_atom_any;
extern ERL_NIF_TERM esock_atom_associnfo;
extern ERL_NIF_TERM esock_atom_authhdr;
extern ERL_NIF_TERM esock_atom_auth_active_key;
extern ERL_NIF_TERM esock_atom_auth_asconf;
extern ERL_NIF_TERM esock_atom_auth_chunk;
extern ERL_NIF_TERM esock_atom_auth_delete_key;
extern ERL_NIF_TERM esock_atom_auth_key;
extern ERL_NIF_TERM esock_atom_auth_level;
extern ERL_NIF_TERM esock_atom_autoclose;
extern ERL_NIF_TERM esock_atom_bindtodevice;
extern ERL_NIF_TERM esock_atom_block_source;
extern ERL_NIF_TERM esock_atom_broadcast;
extern ERL_NIF_TERM esock_atom_busy_poll;
extern ERL_NIF_TERM esock_atom_checksum;
extern ERL_NIF_TERM esock_atom_close;
extern ERL_NIF_TERM esock_atom_connect;
extern ERL_NIF_TERM esock_atom_congestion;
extern ERL_NIF_TERM esock_atom_context;
extern ERL_NIF_TERM esock_atom_cork;
extern ERL_NIF_TERM esock_atom_credentials;
extern ERL_NIF_TERM esock_atom_ctrl;
extern ERL_NIF_TERM esock_atom_ctrunc;
extern ERL_NIF_TERM esock_atom_data;
extern ERL_NIF_TERM esock_atom_debug;
extern ERL_NIF_TERM esock_atom_default_send_params;
extern ERL_NIF_TERM esock_atom_delayed_ack_time;
extern ERL_NIF_TERM esock_atom_dgram;
extern ERL_NIF_TERM esock_atom_disable_fragments;
extern ERL_NIF_TERM esock_atom_domain;
extern ERL_NIF_TERM esock_atom_dontfrag;
extern ERL_NIF_TERM esock_atom_dontroute;
extern ERL_NIF_TERM esock_atom_drop_membership;
extern ERL_NIF_TERM esock_atom_drop_source_membership;
extern ERL_NIF_TERM esock_atom_dstopts;
extern ERL_NIF_TERM esock_atom_eor;
extern ERL_NIF_TERM esock_atom_error;
extern ERL_NIF_TERM esock_atom_errqueue;
extern ERL_NIF_TERM esock_atom_esp_network_level;
extern ERL_NIF_TERM esock_atom_esp_trans_level;
extern ERL_NIF_TERM esock_atom_events;
extern ERL_NIF_TERM esock_atom_explicit_eor;
extern ERL_NIF_TERM esock_atom_faith;
extern ERL_NIF_TERM esock_atom_false;
extern ERL_NIF_TERM esock_atom_family;
extern ERL_NIF_TERM esock_atom_flags;
extern ERL_NIF_TERM esock_atom_flowinfo;
extern ERL_NIF_TERM esock_atom_fragment_interleave;
extern ERL_NIF_TERM esock_atom_freebind;
extern ERL_NIF_TERM esock_atom_get_peer_addr_info;
extern ERL_NIF_TERM esock_atom_hdrincl;
extern ERL_NIF_TERM esock_atom_hmac_ident;
extern ERL_NIF_TERM esock_atom_hoplimit;
extern ERL_NIF_TERM esock_atom_hopopts;
extern ERL_NIF_TERM esock_atom_ifindex;
extern ERL_NIF_TERM esock_atom_inet;
extern ERL_NIF_TERM esock_atom_inet6;
extern ERL_NIF_TERM esock_atom_info;
extern ERL_NIF_TERM esock_atom_initmsg;
extern ERL_NIF_TERM esock_atom_iov;
extern ERL_NIF_TERM esock_atom_ip;
extern ERL_NIF_TERM esock_atom_ipcomp_level;
extern ERL_NIF_TERM esock_atom_ipv6;
extern ERL_NIF_TERM esock_atom_i_want_mapped_v4_addr;
extern ERL_NIF_TERM esock_atom_join_group;
extern ERL_NIF_TERM esock_atom_keepalive;
extern ERL_NIF_TERM esock_atom_keepcnt;
extern ERL_NIF_TERM esock_atom_keepidle;
extern ERL_NIF_TERM esock_atom_keepintvl;
extern ERL_NIF_TERM esock_atom_leave_group;
extern ERL_NIF_TERM esock_atom_level;
extern ERL_NIF_TERM esock_atom_linger;
extern ERL_NIF_TERM esock_atom_local;
extern ERL_NIF_TERM esock_atom_local_auth_chunks;
extern ERL_NIF_TERM esock_atom_loopback;
extern ERL_NIF_TERM esock_atom_lowdelay;
extern ERL_NIF_TERM esock_atom_mark;
extern ERL_NIF_TERM esock_atom_maxburst;
extern ERL_NIF_TERM esock_atom_maxseg;
extern ERL_NIF_TERM esock_atom_md5sig;
extern ERL_NIF_TERM esock_atom_mincost;
extern ERL_NIF_TERM esock_atom_minttl;
extern ERL_NIF_TERM esock_atom_msfilter;
extern ERL_NIF_TERM esock_atom_mtu;
extern ERL_NIF_TERM esock_atom_mtu_discover;
extern ERL_NIF_TERM esock_atom_multicast_all;
extern ERL_NIF_TERM esock_atom_multicast_hops;
extern ERL_NIF_TERM esock_atom_multicast_if;
extern ERL_NIF_TERM esock_atom_multicast_loop;
extern ERL_NIF_TERM esock_atom_multicast_ttl;
extern ERL_NIF_TERM esock_atom_nodelay;
extern ERL_NIF_TERM esock_atom_nodefrag;
extern ERL_NIF_TERM esock_atom_noopt;
extern ERL_NIF_TERM esock_atom_nopush;
extern ERL_NIF_TERM esock_atom_not_found;
extern ERL_NIF_TERM esock_atom_not_owner;
extern ERL_NIF_TERM esock_atom_ok;
extern ERL_NIF_TERM esock_atom_oob;
extern ERL_NIF_TERM esock_atom_oobinline;
extern ERL_NIF_TERM esock_atom_options;
extern ERL_NIF_TERM esock_atom_origdstaddr;
extern ERL_NIF_TERM esock_atom_partial_delivery_point;
extern ERL_NIF_TERM esock_atom_passcred;
extern ERL_NIF_TERM esock_atom_path;
extern ERL_NIF_TERM esock_atom_peekcred;
extern ERL_NIF_TERM esock_atom_peek_off;
extern ERL_NIF_TERM esock_atom_peer_addr_params;
extern ERL_NIF_TERM esock_atom_peer_auth_chunks;
extern ERL_NIF_TERM esock_atom_pktinfo;
extern ERL_NIF_TERM esock_atom_pktoptions;
extern ERL_NIF_TERM esock_atom_port;
extern ERL_NIF_TERM esock_atom_portrange;
extern ERL_NIF_TERM esock_atom_primary_addr;
extern ERL_NIF_TERM esock_atom_priority;
extern ERL_NIF_TERM esock_atom_protocol;
extern ERL_NIF_TERM esock_atom_raw;
extern ERL_NIF_TERM esock_atom_rcvbuf;
extern ERL_NIF_TERM esock_atom_rcvbufforce;
extern ERL_NIF_TERM esock_atom_rcvlowat;
extern ERL_NIF_TERM esock_atom_rcvtimeo;
extern ERL_NIF_TERM esock_atom_rdm;
extern ERL_NIF_TERM esock_atom_recv;
extern ERL_NIF_TERM esock_atom_recvdstaddr;
extern ERL_NIF_TERM esock_atom_recverr;
extern ERL_NIF_TERM esock_atom_recvfrom;
extern ERL_NIF_TERM esock_atom_recvif;
extern ERL_NIF_TERM esock_atom_recvmsg;
extern ERL_NIF_TERM esock_atom_recvopts;
extern ERL_NIF_TERM esock_atom_recvorigdstaddr;
extern ERL_NIF_TERM esock_atom_recvpktinfo;
extern ERL_NIF_TERM esock_atom_recvtclass;
extern ERL_NIF_TERM esock_atom_recvtos;
extern ERL_NIF_TERM esock_atom_recvttl;
extern ERL_NIF_TERM esock_atom_reliability;
extern ERL_NIF_TERM esock_atom_reset_streams;
extern ERL_NIF_TERM esock_atom_retopts;
extern ERL_NIF_TERM esock_atom_reuseaddr;
extern ERL_NIF_TERM esock_atom_reuseport;
extern ERL_NIF_TERM esock_atom_rights;
extern ERL_NIF_TERM esock_atom_router_alert;
extern ERL_NIF_TERM esock_atom_rthdr;
extern ERL_NIF_TERM esock_atom_rtoinfo;
extern ERL_NIF_TERM esock_atom_rxq_ovfl;
extern ERL_NIF_TERM esock_atom_scope_id;
extern ERL_NIF_TERM esock_atom_sctp;
extern ERL_NIF_TERM esock_atom_sec;
extern ERL_NIF_TERM esock_atom_select_sent;
extern ERL_NIF_TERM esock_atom_send;
extern ERL_NIF_TERM esock_atom_sendmsg;
extern ERL_NIF_TERM esock_atom_sendsrcaddr;
extern ERL_NIF_TERM esock_atom_sendto;
extern ERL_NIF_TERM esock_atom_seqpacket;
extern ERL_NIF_TERM esock_atom_setfib;
extern ERL_NIF_TERM esock_atom_set_peer_primary_addr;
extern ERL_NIF_TERM esock_atom_sndbuf;
extern ERL_NIF_TERM esock_atom_sndbufforce;
extern ERL_NIF_TERM esock_atom_sndlowat;
extern ERL_NIF_TERM esock_atom_sndtimeo;
extern ERL_NIF_TERM esock_atom_socket;
extern ERL_NIF_TERM esock_atom_socket_tag;
extern ERL_NIF_TERM esock_atom_spec_dst;
extern ERL_NIF_TERM esock_atom_status;
extern ERL_NIF_TERM esock_atom_stream;
extern ERL_NIF_TERM esock_atom_syncnt;
extern ERL_NIF_TERM esock_atom_tclass;
extern ERL_NIF_TERM esock_atom_tcp;
extern ERL_NIF_TERM esock_atom_throughput;
extern ERL_NIF_TERM esock_atom_timestamp;
extern ERL_NIF_TERM esock_atom_tos;
extern ERL_NIF_TERM esock_atom_transparent;
extern ERL_NIF_TERM esock_atom_true;
extern ERL_NIF_TERM esock_atom_trunc;
extern ERL_NIF_TERM esock_atom_ttl;
extern ERL_NIF_TERM esock_atom_type;
extern ERL_NIF_TERM esock_atom_udp;
extern ERL_NIF_TERM esock_atom_unblock_source;
extern ERL_NIF_TERM esock_atom_undefined;
extern ERL_NIF_TERM esock_atom_unicast_hops;
extern ERL_NIF_TERM esock_atom_unknown;
extern ERL_NIF_TERM esock_atom_usec;
extern ERL_NIF_TERM esock_atom_user_timeout;
extern ERL_NIF_TERM esock_atom_use_ext_recvinfo;
extern ERL_NIF_TERM esock_atom_use_min_mtu;
extern ERL_NIF_TERM esock_atom_v6only;


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
#define MKPID(E, P)         enif_make_pid((E), (P))
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

// #define MONP(S,E,D,P,M)  enif_monitor_process((E), (D), (P), (M))
// #define DEMONP(S,E,D,M)  enif_demonitor_process((E), (D), (M))
#define MONP(S,E,D,P,M)     esock_monitor((S), (E), (D), (P), (M))
#define DEMONP(S,E,D,M)     esock_demonitor((S), (E), (D), (M))
#define MON_INIT(M)         esock_monitor_init((M))
// #define MON_COMP(M1, M2)    esock_monitor_compare((M1), (M2))

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
#define FREE_BIN(BP)              enif_release_binary((BP))


#endif // SOCKET_INT_H__
