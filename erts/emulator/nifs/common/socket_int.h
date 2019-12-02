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

#ifdef HAVE_NETPACKET_PACKET_H
#include <netpacket/packet.h>
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

    /* Link Layer sockaddr (used for address family PACKET) */
#if defined(HAVE_NETPACKET_PACKET_H) && defined(AF_PACKET)
    struct sockaddr_ll ll;
#endif

} ESockAddress;


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
 * "Global" atoms (esock_atom_...)
 *
 * Note that when an (global) atom is added here, it must also be added
 * in the socket_nif.c file!
 */

#define GLOBAL_ATOM_DEFS                       \
    GLOBAL_ATOM_DEF(abort);                    \
    GLOBAL_ATOM_DEF(accept);                   \
    GLOBAL_ATOM_DEF(acceptconn);               \
    GLOBAL_ATOM_DEF(acceptfilter);             \
    GLOBAL_ATOM_DEF(adaption_layer);           \
    GLOBAL_ATOM_DEF(addr);                     \
    GLOBAL_ATOM_DEF(addrform);                 \
    GLOBAL_ATOM_DEF(add_membership);           \
    GLOBAL_ATOM_DEF(add_source_membership);    \
    GLOBAL_ATOM_DEF(any);                      \
    GLOBAL_ATOM_DEF(associnfo);                \
    GLOBAL_ATOM_DEF(authhdr);                  \
    GLOBAL_ATOM_DEF(auth_active_key);          \
    GLOBAL_ATOM_DEF(auth_asconf);              \
    GLOBAL_ATOM_DEF(auth_chunk);               \
    GLOBAL_ATOM_DEF(auth_delete_key);          \
    GLOBAL_ATOM_DEF(auth_key);                 \
    GLOBAL_ATOM_DEF(auth_level);               \
    GLOBAL_ATOM_DEF(autoclose);                \
    GLOBAL_ATOM_DEF(bindtodevice);             \
    GLOBAL_ATOM_DEF(block_source);             \
    GLOBAL_ATOM_DEF(broadcast);                \
    GLOBAL_ATOM_DEF(busy_poll);                \
    GLOBAL_ATOM_DEF(checksum);                 \
    GLOBAL_ATOM_DEF(close);                    \
    GLOBAL_ATOM_DEF(cmsg_cloexec);             \
    GLOBAL_ATOM_DEF(command);                  \
    GLOBAL_ATOM_DEF(conirm);                   \
    GLOBAL_ATOM_DEF(congestion);               \
    GLOBAL_ATOM_DEF(connect);                  \
    GLOBAL_ATOM_DEF(context);                  \
    GLOBAL_ATOM_DEF(cork);                     \
    GLOBAL_ATOM_DEF(credentials);              \
    GLOBAL_ATOM_DEF(ctrl);                     \
    GLOBAL_ATOM_DEF(ctrunc);                   \
    GLOBAL_ATOM_DEF(data);                     \
    GLOBAL_ATOM_DEF(debug);                    \
    GLOBAL_ATOM_DEF(default);                  \
    GLOBAL_ATOM_DEF(default_send_params);      \
    GLOBAL_ATOM_DEF(delayed_ack_time);         \
    GLOBAL_ATOM_DEF(dgram);                    \
    GLOBAL_ATOM_DEF(disable_fragments);        \
    GLOBAL_ATOM_DEF(domain);                   \
    GLOBAL_ATOM_DEF(dontfrag);                 \
    GLOBAL_ATOM_DEF(dontroute);                \
    GLOBAL_ATOM_DEF(drop_membership);          \
    GLOBAL_ATOM_DEF(drop_source_membership);   \
    GLOBAL_ATOM_DEF(dstopts);                  \
    GLOBAL_ATOM_DEF(egp);                      \
    GLOBAL_ATOM_DEF(eor);                      \
    GLOBAL_ATOM_DEF(error);                    \
    GLOBAL_ATOM_DEF(errqueue);                 \
    GLOBAL_ATOM_DEF(esp_network_level);        \
    GLOBAL_ATOM_DEF(esp_trans_level);          \
    GLOBAL_ATOM_DEF(events);                   \
    GLOBAL_ATOM_DEF(explicit_eor);             \
    GLOBAL_ATOM_DEF(faith);                    \
    GLOBAL_ATOM_DEF(false);                    \
    GLOBAL_ATOM_DEF(family);                   \
    GLOBAL_ATOM_DEF(fastroute);                \
    GLOBAL_ATOM_DEF(flags);                    \
    GLOBAL_ATOM_DEF(flowinfo);                 \
    GLOBAL_ATOM_DEF(fragment_interleave);      \
    GLOBAL_ATOM_DEF(freebind);                 \
    GLOBAL_ATOM_DEF(get_peer_addr_info);       \
    GLOBAL_ATOM_DEF(hatype);                   \
    GLOBAL_ATOM_DEF(hdrincl);                  \
    GLOBAL_ATOM_DEF(hmac_ident);               \
    GLOBAL_ATOM_DEF(hoplimit);                 \
    GLOBAL_ATOM_DEF(hopopts);                  \
    GLOBAL_ATOM_DEF(host);                     \
    GLOBAL_ATOM_DEF(icmp);                     \
    GLOBAL_ATOM_DEF(icmp6);                    \
    GLOBAL_ATOM_DEF(ifindex);                  \
    GLOBAL_ATOM_DEF(igmp);                     \
    GLOBAL_ATOM_DEF(inet);                     \
    GLOBAL_ATOM_DEF(inet6);                    \
    GLOBAL_ATOM_DEF(info);                     \
    GLOBAL_ATOM_DEF(initmsg);                  \
    GLOBAL_ATOM_DEF(iov);                      \
    GLOBAL_ATOM_DEF(ip);                       \
    GLOBAL_ATOM_DEF(ipcomp_level);             \
    GLOBAL_ATOM_DEF(ipip);                     \
    GLOBAL_ATOM_DEF(ipv6);                     \
    GLOBAL_ATOM_DEF(i_want_mapped_v4_addr);    \
    GLOBAL_ATOM_DEF(join_group);               \
    GLOBAL_ATOM_DEF(keepalive);                \
    GLOBAL_ATOM_DEF(keepcnt);                  \
    GLOBAL_ATOM_DEF(keepidle);                 \
    GLOBAL_ATOM_DEF(keepintvl);                \
    GLOBAL_ATOM_DEF(leave_group);              \
    GLOBAL_ATOM_DEF(level);                    \
    GLOBAL_ATOM_DEF(linger);                   \
    GLOBAL_ATOM_DEF(local);                    \
    GLOBAL_ATOM_DEF(local_auth_chunks);        \
    GLOBAL_ATOM_DEF(loopback);                 \
    GLOBAL_ATOM_DEF(lowdelay);                 \
    GLOBAL_ATOM_DEF(mark);                     \
    GLOBAL_ATOM_DEF(maxburst);                 \
    GLOBAL_ATOM_DEF(maxseg);                   \
    GLOBAL_ATOM_DEF(md5sig);                   \
    GLOBAL_ATOM_DEF(mincost);                  \
    GLOBAL_ATOM_DEF(minttl);                   \
    GLOBAL_ATOM_DEF(more);                     \
    GLOBAL_ATOM_DEF(msfilter);                 \
    GLOBAL_ATOM_DEF(mtu);                      \
    GLOBAL_ATOM_DEF(mtu_discover);             \
    GLOBAL_ATOM_DEF(multicast);                \
    GLOBAL_ATOM_DEF(multicast_all);            \
    GLOBAL_ATOM_DEF(multicast_hops);           \
    GLOBAL_ATOM_DEF(multicast_if);             \
    GLOBAL_ATOM_DEF(multicast_loop);           \
    GLOBAL_ATOM_DEF(multicast_ttl);            \
    GLOBAL_ATOM_DEF(nodelay);                  \
    GLOBAL_ATOM_DEF(nodefrag);                 \
    GLOBAL_ATOM_DEF(noopt);                    \
    GLOBAL_ATOM_DEF(nopush);                   \
    GLOBAL_ATOM_DEF(nosignal);                 \
    GLOBAL_ATOM_DEF(not_found);                \
    GLOBAL_ATOM_DEF(not_owner);                \
    GLOBAL_ATOM_DEF(ok);                       \
    GLOBAL_ATOM_DEF(oob);                      \
    GLOBAL_ATOM_DEF(oobinline);                \
    GLOBAL_ATOM_DEF(options);                  \
    GLOBAL_ATOM_DEF(origdstaddr);              \
    GLOBAL_ATOM_DEF(otherhost);                \
    GLOBAL_ATOM_DEF(outgoing);                 \
    GLOBAL_ATOM_DEF(packet);                   \
    GLOBAL_ATOM_DEF(partial_delivery_point);   \
    GLOBAL_ATOM_DEF(passcred);                 \
    GLOBAL_ATOM_DEF(path);                     \
    GLOBAL_ATOM_DEF(peek);                     \
    GLOBAL_ATOM_DEF(peekcred);                 \
    GLOBAL_ATOM_DEF(peek_off);                 \
    GLOBAL_ATOM_DEF(peer_addr_params);         \
    GLOBAL_ATOM_DEF(peer_auth_chunks);         \
    GLOBAL_ATOM_DEF(pktinfo);                  \
    GLOBAL_ATOM_DEF(pktoptions);               \
    GLOBAL_ATOM_DEF(pkttype);                  \
    GLOBAL_ATOM_DEF(port);                     \
    GLOBAL_ATOM_DEF(portrange);                \
    GLOBAL_ATOM_DEF(primary_addr);             \
    GLOBAL_ATOM_DEF(priority);                 \
    GLOBAL_ATOM_DEF(protocol);                 \
    GLOBAL_ATOM_DEF(raw);                      \
    GLOBAL_ATOM_DEF(rcvbuf);                   \
    GLOBAL_ATOM_DEF(rcvbufforce);              \
    GLOBAL_ATOM_DEF(rcvlowat);                 \
    GLOBAL_ATOM_DEF(rcvtimeo);                 \
    GLOBAL_ATOM_DEF(rdm);                      \
    GLOBAL_ATOM_DEF(recv);                     \
    GLOBAL_ATOM_DEF(recvdstaddr);              \
    GLOBAL_ATOM_DEF(recverr);                  \
    GLOBAL_ATOM_DEF(recvfrom);                 \
    GLOBAL_ATOM_DEF(recvhoplimit);             \
    GLOBAL_ATOM_DEF(recvif);                   \
    GLOBAL_ATOM_DEF(recvmsg);                  \
    GLOBAL_ATOM_DEF(recvopts);                 \
    GLOBAL_ATOM_DEF(recvorigdstaddr);          \
    GLOBAL_ATOM_DEF(recvpktinfo);              \
    GLOBAL_ATOM_DEF(recvtclass);               \
    GLOBAL_ATOM_DEF(recvtos);                  \
    GLOBAL_ATOM_DEF(recvttl);                  \
    GLOBAL_ATOM_DEF(reliability);              \
    GLOBAL_ATOM_DEF(reset_streams);            \
    GLOBAL_ATOM_DEF(retopts);                  \
    GLOBAL_ATOM_DEF(reuseaddr);                \
    GLOBAL_ATOM_DEF(reuseport);                \
    GLOBAL_ATOM_DEF(rights);                   \
    GLOBAL_ATOM_DEF(router_alert);             \
    GLOBAL_ATOM_DEF(rthdr);                    \
    GLOBAL_ATOM_DEF(rtoinfo);                  \
    GLOBAL_ATOM_DEF(rxq_ovfl);                 \
    GLOBAL_ATOM_DEF(scope_id);                 \
    GLOBAL_ATOM_DEF(sctp);                     \
    GLOBAL_ATOM_DEF(sec);                      \
    GLOBAL_ATOM_DEF(select_failed);            \
    GLOBAL_ATOM_DEF(select_sent);              \
    GLOBAL_ATOM_DEF(send);                     \
    GLOBAL_ATOM_DEF(sendmsg);                  \
    GLOBAL_ATOM_DEF(sendsrcaddr);              \
    GLOBAL_ATOM_DEF(sendto);                   \
    GLOBAL_ATOM_DEF(seqpacket);                \
    GLOBAL_ATOM_DEF(setfib);                   \
    GLOBAL_ATOM_DEF(set_peer_primary_addr);    \
    GLOBAL_ATOM_DEF(sndbuf);                   \
    GLOBAL_ATOM_DEF(sndbufforce);              \
    GLOBAL_ATOM_DEF(sndlowat);                 \
    GLOBAL_ATOM_DEF(sndtimeo);                 \
    GLOBAL_ATOM_DEF(socket);                   \
    GLOBAL_ATOM_DEF(socket_tag);               \
    GLOBAL_ATOM_DEF(spec_dst);                 \
    GLOBAL_ATOM_DEF(status);                   \
    GLOBAL_ATOM_DEF(stream);                   \
    GLOBAL_ATOM_DEF(syncnt);                   \
    GLOBAL_ATOM_DEF(tclass);                   \
    GLOBAL_ATOM_DEF(tcp);                      \
    GLOBAL_ATOM_DEF(throughput);               \
    GLOBAL_ATOM_DEF(timestamp);                \
    GLOBAL_ATOM_DEF(tos);                      \
    GLOBAL_ATOM_DEF(transparent);              \
    GLOBAL_ATOM_DEF(true);                     \
    GLOBAL_ATOM_DEF(trunc);                    \
    GLOBAL_ATOM_DEF(ttl);                      \
    GLOBAL_ATOM_DEF(type);                     \
    GLOBAL_ATOM_DEF(udp);                      \
    GLOBAL_ATOM_DEF(unblock_source);           \
    GLOBAL_ATOM_DEF(undefined);                \
    GLOBAL_ATOM_DEF(unicast_hops);             \
    GLOBAL_ATOM_DEF(unknown);                  \
    GLOBAL_ATOM_DEF(usec);                     \
    GLOBAL_ATOM_DEF(user_timeout);             \
    GLOBAL_ATOM_DEF(use_ext_recvinfo);         \
    GLOBAL_ATOM_DEF(use_min_mtu);              \
    GLOBAL_ATOM_DEF(v6only);


/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 * Error reason atoms
 */

#define GLOBAL_ERROR_REASON_ATOM_DEFS \
    GLOBAL_ATOM_DEF(eafnosupport);    \
    GLOBAL_ATOM_DEF(eagain);          \
    GLOBAL_ATOM_DEF(einval);


#define GLOBAL_ATOM_DEF(A) extern ERL_NIF_TERM esock_atom_##A
GLOBAL_ATOM_DEFS
GLOBAL_ERROR_REASON_ATOM_DEFS
#undef GLOBAL_ATOM_DEF


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

#define MONP(S,E,D,P,M)     esock_monitor((S), (E), (D), (P), (M))
#define DEMONP(S,E,D,M)     esock_demonitor((S), (E), (D), (M))
#define MON_INIT(M)         esock_monitor_init((M))
#define MON2T(E, M)         enif_make_monitor_term((E), (M))

#define COMPARE(A, B)        enif_compare((A), (B))
#define COMPARE_PIDS(P1, P2) enif_compare_pids((P1), (P2))

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
#define GET_INT64(E, TE, IP)          enif_get_int64((E), (TE), (IP))
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

/* Copy term T into environment E */
#define CP_TERM(E, T) enif_make_copy((E), (T))

#endif // SOCKET_INT_H__
