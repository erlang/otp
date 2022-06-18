/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2018-2021. All Rights Reserved.
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
 *  Purpose : Utility functions for the socket and net NIF(s).
 * ----------------------------------------------------------------------
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <stddef.h>
#include <sys/types.h>
#include <sys/socket.h>

#ifndef __IOS__
#include <net/if_arp.h>
#endif

#include "socket_int.h"
#include "sys.h"
#include "socket_util.h"
#include "socket_dbg.h"

#if defined(HAVE_SCTP_H)
#include <netinet/sctp.h>
#ifndef     HAVE_SCTP
#    define HAVE_SCTP
#endif
#endif

/* We don't have a "debug flag" to check here, so we 
 * should use the compile debug flag, whatever that is...
 */

// #define COMPILE_DEBUG_FLAG_WE_NEED_TO_CHECK 1
#if defined(COMPILE_DEBUG_FLAG_WE_NEED_TO_CHECK)
#define UTIL_DEBUG TRUE
#else
#define UTIL_DEBUG FALSE
#endif

#define UDBG( proto ) ESOCK_DBG_PRINTF( UTIL_DEBUG , proto )


extern char* erl_errno_id(int error); /* THIS IS JUST TEMPORARY??? */

#if (defined(HAVE_LOCALTIME_R) && defined(HAVE_STRFTIME))
#define ESOCK_USE_PRETTY_TIMESTAMP 1
#endif


static
BOOLEAN_T esock_decode_sockaddr_native(ErlNifEnv*     env,
                                       ERL_NIF_TERM   eSockAddr,
                                       ESockAddress*  sockAddrP,
                                       int            family,
                                       SOCKLEN_T*     addrLen);

static void esock_encode_packet_addr_tuple(ErlNifEnv*     env,
                                           unsigned char  len,
                                           unsigned char* addr,
                                           ERL_NIF_TERM*  eAddr);

static void esock_encode_sockaddr_native(ErlNifEnv*       env,
                                         struct sockaddr* sa,
                                         SOCKLEN_T        len,
                                         ERL_NIF_TERM     eFamily,
                                         ERL_NIF_TERM*    eSockAddr);

static void esock_encode_sockaddr_broken(ErlNifEnv*       env,
                                         struct sockaddr* sa,
                                         socklen_t        len,
                                         ERL_NIF_TERM*    eSockAddr);

static void make_sockaddr_in(ErlNifEnv*    env,
                             ERL_NIF_TERM  port,
                             ERL_NIF_TERM  addr,
                             ERL_NIF_TERM* sa);
static void make_sockaddr_in6(ErlNifEnv*    env,
                              ERL_NIF_TERM  port,
                              ERL_NIF_TERM  addr,
                              ERL_NIF_TERM  flowInfo,
                              ERL_NIF_TERM  scopeId,
                              ERL_NIF_TERM* sa);
static void make_sockaddr_un(ErlNifEnv*    env,
                             ERL_NIF_TERM  path,
                             ERL_NIF_TERM* sa);
#if defined(HAVE_NETPACKET_PACKET_H)
static void make_sockaddr_ll(ErlNifEnv*    env,
                             ERL_NIF_TERM  proto,
                             ERL_NIF_TERM  ifindex,
                             ERL_NIF_TERM  hatype,
                             ERL_NIF_TERM  pkttype,
                             ERL_NIF_TERM  addr,
                             ERL_NIF_TERM* sa);
#endif



/* *** esock_get_bool_from_map ***
 *
 * Simple utility function used to extract a boolean value from a map.
 * If it fails to extract the value (for whatever reason) the default
 * value is returned.
 */

extern
BOOLEAN_T esock_get_bool_from_map(ErlNifEnv*   env,
                                  ERL_NIF_TERM map,
                                  ERL_NIF_TERM key,
                                  BOOLEAN_T    def)
{
    ERL_NIF_TERM val;

    if (!GET_MAP_VAL(env, map, key, &val)) {
        return def;
    } else {
        if (COMPARE(val, esock_atom_true) == 0)
            return TRUE;
        else if (COMPARE(val, esock_atom_false) == 0)
            return FALSE;
        else
            return def;
    }
}


/* +++ esock_encode_iov +++
 *
 * Encode an IO Vector. In erlang we represented this as a list of binaries.
 *
 * We iterate through the IO vector, and as long as the remaining (rem)
 * number of bytes is greater than the size of the current buffer, we
 * continue. When we have a buffer that is greater than rem, we have found
 * the last buffer (it may be empty, and then the previous was last). 
 * We may need to split this (if 0 < rem < bufferSz).
 */

extern
void esock_encode_iov(ErlNifEnv*    env,
                      ssize_t       read,
                      struct iovec* iov,
                      size_t        len,
                      ErlNifBinary* data,
                      ERL_NIF_TERM* eIOV)
{
    ssize_t      rem = read;
    size_t       i;
    ERL_NIF_TERM a[len]; // At most this length

    UDBG( ("SUTIL", "esock_encode_iov -> entry with"
           "\r\n   read:      %ld"
           "\r\n   (IOV) len: %lu"
           "\r\n", (long) read, (unsigned long) len) );

    if (len == 0) {
        *eIOV = MKEL(env);
        return;
    }

    for (i = 0;  i < len;  i++) {
        UDBG( ("SUTIL", "esock_encode_iov -> process iov:"
               "\r\n   iov[%d].iov_len: %d"
               "\r\n   rem:            %d"
               "\r\n", i, iov[i].iov_len, rem) );
        if (iov[i].iov_len == rem) {
            /* We have the exact amount - we are done */
            UDBG( ("SUTIL", "esock_encode_iov -> exact => done\r\n") );
            a[i] = MKBIN(env, &data[i]);
            rem  = 0; // Besserwisser
            i++;
            break;
        } else if (iov[i].iov_len < rem) {
            /* Filled another buffer - continue */
            UDBG( ("SUTIL", "esock_encode_iov -> filled => continue\r\n") );
            a[i] = MKBIN(env, &data[i]);
            rem -= iov[i].iov_len;
        } else if (iov[i].iov_len > rem) {
            /* Partly filled buffer (=> split) - we are done */
            ERL_NIF_TERM tmp;
            UDBG( ("SUTIL", "esock_encode_iov -> split => done\r\n") );
            tmp  = MKBIN(env, &data[i]);
            a[i] = MKSBIN(env, tmp, 0, rem);
            rem  = 0; // Besserwisser
            i++;
            break;
        }
    }

    UDBG( ("SUTIL", "esock_encode_iov -> create the IOV list (%d)\r\n", i) );

    *eIOV = MKLA(env, a, i);

    UDBG( ("SUTIL", "esock_encode_msghdr -> done\r\n") );
}



/* +++ esock_decode_iov +++
 *
 * Decode an IO Vector. In erlang we represented this as a list of binaries.
 *
 * We assume that we have already figured out how long the iov (actually
 * eIOV) is (len), and therefor allocated an array of bins and iov to be
 * used.
 */

extern
BOOLEAN_T esock_decode_iov(ErlNifEnv*    env,
                           ERL_NIF_TERM  eIOV,
                           ErlNifBinary* bufs,
                           struct iovec* iov,
                           size_t        len,
                           ssize_t*      totSize)
{
    Uint16       i;
    ssize_t      sz;
    ERL_NIF_TERM elem, tail, list;

    UDBG( ("SUTIL", "esock_decode_iov -> entry with"
           "\r\n   (IOV) len: %d"
           "\r\n", read, len) );

    for (i = 0, list = eIOV, sz = 0; (i < len); i++) {

        UDBG( ("SUTIL", "esock_decode_iov -> "
               "\r\n   iov[%d].iov_len: %d"
               "\r\n   rem:            %d"
               "\r\n", i) );

        ESOCK_ASSERT( GET_LIST_ELEM(env, list, &elem, &tail) );
        // We have already tested that it is a proper list

        if (IS_BIN(env, elem) && GET_BIN(env, elem, &bufs[i])) {
            ssize_t z;

            iov[i].iov_base  = (caddr_t) bufs[i].data;
            iov[i].iov_len   = bufs[i].size;

            z = sz;
            sz += bufs[i].size;
            /* Check that + did not overflow */
            if (sz < z)
                return FALSE; // Too much data in iov
        } else {
            return FALSE; // Not a binary - not an iov
        }

        list = tail;
    }

    *totSize = sz;

    UDBG( ("SUTIL", "esock_decode_iov -> done (%d)\r\n", sz) );

    return TRUE;
}



/* +++ esock_decode_sockaddr +++
 *
 * Decode a socket address - sockaddr. In erlang its represented as
 * a map, which has a specific set of attributes, depending on one
 * mandatory attribute; family. So depending on the value of the family
 * attribute: 
 *
 *    local - sockaddr_un:  path
 *    inet  - sockaddr_in4: port, addr
 *    inet6 - sockaddr_in6: port, addr, flowinfo, scope_id
 *    unspec - sockaddr:    addr
 *    (int)  - sockaddr:     addr
 */

extern
BOOLEAN_T esock_decode_sockaddr(ErlNifEnv*    env,
                                ERL_NIF_TERM  eSockAddr,
                                ESockAddress* sockAddrP,
                                SOCKLEN_T*    addrLenP)
{
    ERL_NIF_TERM efam;
    int          decode;
    int          fam;

    UDBG( ("SUTIL", "esock_decode_sockaddr -> entry\r\n") );

    if (!IS_MAP(env, eSockAddr))
        return FALSE;

    if (!GET_MAP_VAL(env, eSockAddr, esock_atom_family, &efam))
        return FALSE;

    UDBG( ("SUTIL",
           "esock_decode_sockaddr -> try decode domain (%T)\r\n",
           efam) );
    decode = esock_decode_domain(env, efam, &fam);
    if (0 >= decode) {
        if (0 > decode)
            return esock_decode_sockaddr_native(env, eSockAddr, sockAddrP,
                                                fam, addrLenP);
        return FALSE;
    }

    UDBG( ("SUTIL", "esock_decode_sockaddr -> fam: %d\r\n", fam) );
    switch (fam) {
    case AF_INET:
        return esock_decode_sockaddr_in(env, eSockAddr,
                                        &sockAddrP->in4, addrLenP);

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        return esock_decode_sockaddr_in6(env, eSockAddr,
                                         &sockAddrP->in6, addrLenP);
#endif

#ifdef HAS_AF_LOCAL
    case AF_LOCAL:
        return esock_decode_sockaddr_un(env, eSockAddr,
                                        &sockAddrP->un, addrLenP);
#endif

#ifdef AF_UNSPEC
    case AF_UNSPEC:
        return esock_decode_sockaddr_native(env, eSockAddr, sockAddrP,
                                            AF_UNSPEC, addrLenP);
#endif

    default:
        return FALSE;
    }
}



/* +++ esock_encode_sockaddr +++
 *
 * Encode a socket address - sockaddr. In erlang its represented as
 * a map, which has a specific set of attributes, depending on one
 * mandatory attribute; family. So depending on the value of the family
 * attribute: 
 *
 *    local  - sockaddr_un:  path
 *    inet   - sockaddr_in4: port, addr
 *    inet6  - sockaddr_in6: port, addr, flowinfo, scope_id
 *    packet - sockaddr_ll:  protocol, ifindex, hatype, pkttype, addr
 *    unspec - sockaddr:     addr
 *    (int)  - sockaddr:     addr
 */

extern
void esock_encode_sockaddr(ErlNifEnv*    env,
                           ESockAddress* sockAddrP,
                           SOCKLEN_T     addrLen,
                           ERL_NIF_TERM* eSockAddr)
{
  int family;

  // Sanity check
  if (addrLen < (char *)&sockAddrP->sa.sa_data - (char *)sockAddrP) {
    // We got crap, cannot even know the address family
    esock_encode_sockaddr_broken(env, &sockAddrP->sa, addrLen, eSockAddr);
    return;
  }
  family = sockAddrP->ss.ss_family;

  UDBG( ("SUTIL", "esock_encode_sockaddr -> entry with"
	 "\r\n   family:  %d"
	 "\r\n   addrLen: %d"
	 "\r\n", family, addrLen) );

  switch (family) {
  case AF_INET:
    esock_encode_sockaddr_in(env, &sockAddrP->in4, addrLen, eSockAddr);
    break;

#if defined(HAVE_IN6) && defined(AF_INET6)
  case AF_INET6:
    esock_encode_sockaddr_in6(env, &sockAddrP->in6, addrLen, eSockAddr);
    break;
#endif

#ifdef HAS_AF_LOCAL
  case AF_LOCAL:
    esock_encode_sockaddr_un(env, &sockAddrP->un, addrLen, eSockAddr);
    break;
#endif

#ifdef AF_UNSPEC
  case AF_UNSPEC:
    esock_encode_sockaddr_native(env, &sockAddrP->sa,
				 addrLen, esock_atom_unspec, eSockAddr);
    break;
#endif

#if defined(HAVE_NETPACKET_PACKET_H)
  case AF_PACKET:
    esock_encode_sockaddr_ll(env, &sockAddrP->ll, addrLen, eSockAddr);
    break;
#endif

#if defined(AF_IMPLINK)
  case AF_IMPLINK:
    esock_encode_sockaddr_native(env, &sockAddrP->sa,
				 addrLen, esock_atom_implink, eSockAddr);
    break;
#endif

#if defined(AF_PUP)
  case AF_PUP:
    esock_encode_sockaddr_native(env, &sockAddrP->sa,
				 addrLen, esock_atom_pup, eSockAddr);
    break;
#endif

#if defined(AF_CHAOS)
  case AF_CHAOS:
    esock_encode_sockaddr_native(env, &sockAddrP->sa,
				 addrLen, esock_atom_chaos, eSockAddr);
    break;
#endif

#if defined(AF_LINK)
  case AF_LINK:
    esock_encode_sockaddr_native(env, &sockAddrP->sa,
				 addrLen, esock_atom_link, eSockAddr);
    break;
#endif

  default:
    esock_encode_sockaddr_native(env, &sockAddrP->sa,
				 addrLen, MKI(env, family), eSockAddr);
    break;
  }
}



extern
void esock_encode_hwsockaddr(ErlNifEnv*       env,
			     struct sockaddr* sockAddrP,
			     SOCKLEN_T        addrLen,
			     ERL_NIF_TERM*    eSockAddr)
{
  ERL_NIF_TERM efamily;
  int          family;

  // Sanity check
  if (addrLen < (char *)&sockAddrP->sa_data - (char *)sockAddrP) {
    // We got crap, cannot even know the address family
    esock_encode_sockaddr_broken(env, sockAddrP, addrLen, eSockAddr);
    return;
  }
  family = sockAddrP->sa_family;

  UDBG( ("SUTIL", "esock_encode_hwsockaddr -> entry with"
	 "\r\n   family:  %d"
	 "\r\n   addrLen: %d"
	 "\r\n", family, addrLen) );

  switch (family) {
#if defined(ARPHRD_NETROM)
  case ARPHRD_NETROM:
    efamily = esock_atom_arphrd_netrom;
    break;
#endif

#if defined(ARPHRD_ETHER)
  case ARPHRD_ETHER:
    efamily = esock_atom_arphrd_ether;
    break;
#endif

#if defined(ARPHRD_IEEE802)
  case ARPHRD_IEEE802:
    efamily = esock_atom_arphrd_ieee802;
    break;
#endif

#if defined(ARPHRD_DLCI)
  case ARPHRD_DLCI:
    efamily = esock_atom_arphrd_dlci;
    break;
#endif

#if defined(ARPHRD_FRELAY)
  case ARPHRD_FRELAY:
    efamily = esock_atom_arphrd_frelay;
    break;
#endif

#if defined(ARPHRD_IEEE1394)
  case ARPHRD_IEEE1394:
    efamily = esock_atom_arphrd_ieee1394;
    break;
#endif

#if defined(ARPHRD_LOOPBACK)
  case ARPHRD_LOOPBACK:
    efamily = esock_atom_arphrd_loopback;
    break;
#endif

#if defined(ARPHRD_NONE)
  case ARPHRD_NONE:
    efamily = esock_atom_arphrd_none;
    break;
#endif

  default:
    efamily = MKI(env, family);
    break;
  }

  esock_encode_sockaddr_native(env, sockAddrP, addrLen, efamily, eSockAddr);
}



/* +++ esock_decode_sockaddr_in +++
 *
 * Decode a IPv4 socket address - sockaddr_in4. In erlang its represented as
 * a map, which has a specific set of attributes (beside the mandatory family
 * attribute, which is "inherited" from the "sockaddr" type): 
 *
 *    port :: port_numbber()
 *    addr :: ip4_address()
 *
 * The erlang module ensures that both of these has values exist, so there 
 * is no need for any elaborate error handling.
 */

extern
BOOLEAN_T esock_decode_sockaddr_in(ErlNifEnv*          env,
                                   ERL_NIF_TERM        eSockAddr,
                                   struct sockaddr_in* sockAddrP,
                                   SOCKLEN_T*          addrLen)
{
    ERL_NIF_TERM eport, eaddr;
    int          port;

    UDBG( ("SUTIL", "esock_decode_sockaddr_in -> entry\r\n") );

    /* Basic init */
    sys_memzero((char*) sockAddrP, sizeof(struct sockaddr_in));

#ifndef NO_SA_LEN
    sockAddrP->sin_len = sizeof(struct sockaddr_in);
#endif

    sockAddrP->sin_family = AF_INET;

    /* Extract (e) port number from map */
    UDBG( ("SUTIL", "esock_decode_sockaddr_in -> try get port number\r\n") );
    if (! GET_MAP_VAL(env, eSockAddr, esock_atom_port, &eport))
        return FALSE;

    /* Decode port number */
    UDBG( ("SUTIL", "esock_decode_sockaddr_in -> try decode port number\r\n") );
    if (! GET_INT(env, eport, &port))
        return FALSE;
    
    sockAddrP->sin_port = htons(port);

    /* Extract (e) address from map */
    UDBG( ("SUTIL", "esock_decode_sockaddr_in -> try get (ip) address\r\n") );
    if (! GET_MAP_VAL(env, eSockAddr, esock_atom_addr, &eaddr))
        return FALSE;

    /* Decode address */
    UDBG( ("SUTIL", "esock_decode_sockaddr_in -> try decode (ip) address\r\n") );
    if (! esock_decode_in_addr(env,
                               eaddr,
                               &sockAddrP->sin_addr))
        return FALSE;

    *addrLen = sizeof(struct sockaddr_in);
    
    UDBG( ("SUTIL", "esock_decode_sockaddr_in -> done\r\n") );

    return TRUE;
}



/* +++ esock_encode_sockaddr_in +++
 *
 * Encode a IPv4 socket address - sockaddr_in4. In erlang its represented as
 * a map, which has a specific set of attributes (beside the mandatory family
 * attribute, which is "inherited" from the "sockaddr" type):
 *
 *    port :: port_numbber()
 *    addr :: ip4_address()
 *
 */

extern
void esock_encode_sockaddr_in(ErlNifEnv*          env,
                              struct sockaddr_in* sockAddrP,
                              SOCKLEN_T           addrLen,
                              ERL_NIF_TERM*       eSockAddr)
{
    ERL_NIF_TERM ePort, eAddr;
    int          port;

    UDBG( ("SUTIL", "esock_encode_sockaddr_in -> entry\r\n") );

    if (addrLen >= sizeof(struct sockaddr_in)) {

        /* The port */
        port  = ntohs(sockAddrP->sin_port);
        ePort = MKI(env, port);

        /* The address */
        esock_encode_in_addr(env, &sockAddrP->sin_addr, &eAddr);

        /* And finally construct the in4_sockaddr record */
        make_sockaddr_in(env, ePort, eAddr, eSockAddr);

    } else {
        UDBG( ("SUTIL", "esock_encode_sockaddr_in -> wrong size: "
               "\r\n   addrLen:   %d"
               "\r\n   addr size: %d"
               "\r\n", addrLen, sizeof(struct sockaddr_in)) );
        esock_encode_sockaddr_native(env, (struct sockaddr *)sockAddrP,
                                     addrLen, MKI(env, AF_INET), eSockAddr);
    }
}



/* +++ esock_decode_sockaddr_in6 +++
 *
 * Decode a IPv6 socket address - sockaddr_in6. In erlang its represented as
 * a map, which has a specific set of attributes (beside the mandatory family
 * attribute, which is "inherited" from the "sockaddr" type): 
 *
 *    port     :: port_numbber()  (integer)
 *    addr     :: ip6_address()   (tuple)
 *    flowinfo :: in6_flow_info() (integer)
 *    scope_id :: in6_scope_id()  (integer)
 *
 * The erlang module ensures that all of these has values exist, so there
 * is no need for any elaborate error handling here.
 */

#if defined(HAVE_IN6) && defined(AF_INET6)
extern
BOOLEAN_T esock_decode_sockaddr_in6(ErlNifEnv*           env,
                                    ERL_NIF_TERM         eSockAddr,
                                    struct sockaddr_in6* sockAddrP,
                                    SOCKLEN_T*           addrLen)
{
    ERL_NIF_TERM eport, eaddr, eflowInfo, escopeId;
    int          port;
    unsigned int flowInfo, scopeId;

    UDBG( ("SUTIL", "esock_decode_sockaddr_in6 -> entry\r\n") );

    /* Basic init */
    sys_memzero((char*) sockAddrP, sizeof(struct sockaddr_in6));
#ifndef NO_SA_LEN
    sockAddrP->sin6_len = sizeof(struct sockaddr_in);
#endif

    sockAddrP->sin6_family = AF_INET6;

    /* *** Extract (e) port number from map *** */
    if (! GET_MAP_VAL(env, eSockAddr, esock_atom_port, &eport))
        return FALSE;

    /* Decode port number */
    if (! GET_INT(env, eport, &port))
        return FALSE;

    UDBG( ("SUTIL", "esock_decode_sockaddr_in6 -> port: %d\r\n", port) );

    sockAddrP->sin6_port = htons(port);

    /* *** Extract (e) flowinfo from map *** */
    if (! GET_MAP_VAL(env, eSockAddr, esock_atom_flowinfo, &eflowInfo))
        return FALSE;

    /* 4: Get the flowinfo */
    if (! GET_UINT(env, eflowInfo, &flowInfo))
        return FALSE;

    UDBG( ("SUTIL", "esock_decode_sockaddr_in6 -> flowinfo: %d\r\n", flowInfo) );

    sockAddrP->sin6_flowinfo = flowInfo;
    
    /* *** Extract (e) scope_id from map *** */
    if (! GET_MAP_VAL(env, eSockAddr, esock_atom_scope_id, &escopeId))
        return FALSE;

    /* *** Get the scope_id *** */
    if (! GET_UINT(env, escopeId, &scopeId))
        return FALSE;

    UDBG( ("SUTIL", "esock_decode_sockaddr_in6 -> scopeId: %d\r\n", scopeId) );

    sockAddrP->sin6_scope_id = scopeId;

    /* *** Extract (e) address from map *** */
    if (! GET_MAP_VAL(env, eSockAddr, esock_atom_addr, &eaddr))
        return FALSE;

    /* Decode address */
    if (!esock_decode_in6_addr(env,
                               eaddr,
                               &sockAddrP->sin6_addr))
        return FALSE;

    *addrLen = sizeof(struct sockaddr_in6);

    UDBG( ("SUTIL", "esock_decode_sockaddr_in6 -> done\r\n") );

    return TRUE;
}
#endif



/* +++ esock_encode_sockaddr_in6 +++
 *
 * Encode a IPv6 socket address - sockaddr_in6. In erlang its represented as
 * a map, which has a specific set of attributes (beside the mandatory family
 * attribute, which is "inherited" from the "sockaddr" type): 
 *
 *    port     :: port_numbber()  (integer)
 *    addr     :: ip6_address()   (tuple)
 *    flowinfo :: in6_flow_info() (integer)
 *    scope_id :: in6_scope_id()  (integer)
 *
 */

#if defined(HAVE_IN6) && defined(AF_INET6)
extern
void esock_encode_sockaddr_in6(ErlNifEnv*            env,
                                struct sockaddr_in6* sockAddrP,
                                SOCKLEN_T            addrLen,
                                ERL_NIF_TERM*        eSockAddr)
{
    ERL_NIF_TERM ePort, eAddr, eFlowInfo, eScopeId;

    if (addrLen >= sizeof(struct sockaddr_in6)) {

        /* The port */
        ePort = MKI(env, ntohs(sockAddrP->sin6_port));

        /* The flowInfo */
        eFlowInfo = MKI(env, sockAddrP->sin6_flowinfo);
            
        /* The scopeId */
        eScopeId = MKI(env, sockAddrP->sin6_scope_id);
        
        /* The address */
        esock_encode_in6_addr(env, &sockAddrP->sin6_addr, &eAddr);

        /* And finally construct the in6_sockaddr record */
        make_sockaddr_in6(env, ePort, eAddr,
                          eFlowInfo, eScopeId, eSockAddr);

    } else {
        esock_encode_sockaddr_native(env, (struct sockaddr *)sockAddrP,
                                     addrLen, MKI(env, AF_INET6), eSockAddr);
    }
}
#endif



/* +++ esock_decode_sockaddr_un +++
 *
 * Decode a Unix Domain socket address - sockaddr_un. In erlang its 
 * represented as a map, which has a specific set of attributes
 * (beside the mandatory family attribute, which is "inherited" from
 * the "sockaddr" type):
 *
 *    path :: binary()
 *
 * The erlang module ensures that this value exist, so there 
 * is no need for any elaborate error handling here.
 */

#ifdef HAS_AF_LOCAL
extern
BOOLEAN_T esock_decode_sockaddr_un(ErlNifEnv*          env,
                                   ERL_NIF_TERM        eSockAddr,
                                   struct sockaddr_un* sockAddrP,
                                   SOCKLEN_T*          addrLen)
{
    ErlNifBinary bin;
    ERL_NIF_TERM epath;
    SOCKLEN_T    len;

    /* *** Extract (e) path (a binary) from map *** */
    if (! GET_MAP_VAL(env, eSockAddr, esock_atom_path, &epath))
        return FALSE;

    /* Get the path */
    if (! GET_BIN(env, epath, &bin))
        return FALSE;
    
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
         ) > sizeof(sockAddrP->sun_path))
        return FALSE;


    sys_memzero((char*) sockAddrP, sizeof(struct sockaddr_un));
    sockAddrP->sun_family = AF_LOCAL;

    sys_memcpy(sockAddrP->sun_path, bin.data, bin.size);
    len = (sockAddrP->sun_path - (char *)sockAddrP) + bin.size;

#ifndef NO_SA_LEN
    sockAddrP->sun_len = len;
#endif
    *addrLen = len;

    return TRUE;
}
#endif



/* +++ esock_encode_sockaddr_un +++
 *
 * Encode a Unix Domain socket address - sockaddr_un. In erlang it is
 * represented as a map, which has a specific set of attributes
 * (beside the mandatory family attribute, which is "inherited" from
 * the "sockaddr" type):
 *
 *    path :: binary()
 *
 */

#ifdef HAS_AF_LOCAL
extern
void esock_encode_sockaddr_un(ErlNifEnv*          env,
                              struct sockaddr_un* sockAddrP,
                              SOCKLEN_T           addrLen,
                              ERL_NIF_TERM*       eSockAddr)
{
    ERL_NIF_TERM ePath;
    size_t       n, m;

    n = sockAddrP->sun_path - (char *)sockAddrP; // offsetof
    if (addrLen >= n) {
        n = addrLen - n; // sun_path length
        if (255 < n) {
            /* It would be dangerous to create a binary
             * based on a presumably bad addrLen
             */
            *eSockAddr = esock_atom_bad_data;
        } else {
            unsigned char *path;

            m = esock_strnlen(sockAddrP->sun_path, n);
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

            /* And finally build the 'path' attribute */
            path = enif_make_new_binary(env, m, &ePath);
            ESOCK_ASSERT( path != NULL );
            sys_memcpy(path, sockAddrP->sun_path, m);

            /* And the socket address */
            make_sockaddr_un(env, ePath, eSockAddr);
        }
    } else {
        esock_encode_sockaddr_native(env, (struct sockaddr *)sockAddrP,
                                     addrLen, MKI(env, AF_LOCAL), eSockAddr);
    }
}
#endif



/* +++ esock_encode_sockaddr_ll +++
 *
 * Encode a PACKET address - sockaddr_ll (link layer). In erlang it's
 * represented as a map, which has a specific set of attributes
 * (beside the mandatory family attribute, which is "inherited" from
 * the "sockaddr" type):
 *
 *    protocol: integer() (should be an atom really)
 *    ifindex:  integer()
 *    hatype:   integer() (should be an atom really)
 *    pkttype:  integer() (should be an atom really)
 *    addr:     list()    (should be something usefull...)
 *
 */

#if defined(HAVE_NETPACKET_PACKET_H)
extern
void esock_encode_sockaddr_ll(ErlNifEnv*          env,
                              struct sockaddr_ll* sockAddrP,
                              SOCKLEN_T           addrLen,
                              ERL_NIF_TERM*       eSockAddr)
{
    ERL_NIF_TERM eProto, eIfIdx, eHaType, ePktType, eAddr;

    if (addrLen >= sizeof(struct sockaddr_ll)) {

        /* protocol - the standard ethernet protocol type */
        esock_encode_packet_protocol(env, ntohs(sockAddrP->sll_protocol),
                                     &eProto);

        /* ifindex  - the interface index of the interface */
        eIfIdx = MKI(env, sockAddrP->sll_ifindex);

        /* hatype   - is an ARP (hardware) type */
        esock_encode_packet_hatype(env, sockAddrP->sll_hatype, &eHaType);

        /* pkttype  - the packet type */
        esock_encode_packet_pkttype(env, sockAddrP->sll_pkttype, &ePktType);

        /* addr     - the physical-layer (e.g., IEEE 802.3) address */
        esock_encode_packet_addr(env,
                                 sockAddrP->sll_halen, sockAddrP->sll_addr,
                                 &eAddr);

        make_sockaddr_ll(env,
                         eProto, eIfIdx, eHaType, ePktType, eAddr,
                         eSockAddr);

    } else {
        esock_encode_sockaddr_native(env, (struct sockaddr *)sockAddrP,
                                     addrLen, MKI(env, AF_PACKET), eSockAddr);
    }
}
#endif



/* +++ esock_decode_in_addr +++
 *
 * Decode an IPv4 address. This can be three things:
 *
 *    + Then atom 'any'
 *    + Then atom 'loopback'
 *    + An ip4_address() (4 tuple)
 *
 * Note that this *only* decodes the "address" part of a
 * (IPv4) socket address.
 */

extern
BOOLEAN_T esock_decode_in_addr(ErlNifEnv*      env,
                               ERL_NIF_TERM    eAddr,
                               struct in_addr* inAddrP)
{
    struct in_addr addr;

    UDBG( ("SUTIL", "esock_decode_in_addr -> entry with"
           "\r\n   eAddr: %T"
           "\r\n", eAddr) );

    if (IS_ATOM(env, eAddr)) {

        /* This is either 'any' | 'broadcast' | 'loopback' */

        if (COMPARE(esock_atom_loopback, eAddr) == 0) {
            UDBG( ("SUTIL",
		   "esock_decode_in_addr -> address: loopback\r\n") );
            addr.s_addr = htonl(INADDR_LOOPBACK);
        } else if (COMPARE(esock_atom_any, eAddr) == 0) {
            UDBG( ("SUTIL",
                   "esock_decode_in_addr -> address: any\r\n") );
            addr.s_addr = htonl(INADDR_ANY);
        } else if (COMPARE(esock_atom_broadcast, eAddr) == 0) {
            UDBG( ("SUTIL",
                   "esock_decode_in_addr -> address: broadcast\r\n") );
            addr.s_addr = htonl(INADDR_BROADCAST);
        } else {
            UDBG( ("SUTIL",
		   "esock_decode_in_addr -> address: unknown\r\n") );
            return FALSE;
        }

        inAddrP->s_addr = addr.s_addr;

    } else {
        /* This is a 4-tuple */

        const ERL_NIF_TERM* addrt;
        int                 addrtSz;
        int                 a, v;
        char                addr[4];
        
        if (! GET_TUPLE(env, eAddr, &addrtSz, &addrt))
            return FALSE;
        
        if (addrtSz != 4)
            return FALSE;

        for (a = 0; a < 4; a++) {
            if (! GET_INT(env, addrt[a], &v))
                return FALSE;
            if (v < 0 || 255 < v)
                return FALSE;
            addr[a] = v;
        }

        sys_memcpy(inAddrP, &addr, sizeof(addr));
        
    }

    return TRUE;
}



/* +++ esock_encode_in_addr +++
 *
 * Encode an IPv4 address:
 *
 *    + An ip4_address() (4 tuple)
 *
 * Note that this *only* decodes the "address" part of a
 * (IPv4) socket address. There are several other things (port).
 */

extern
void esock_encode_in_addr(ErlNifEnv*      env,
                          struct in_addr* addrP,
                          ERL_NIF_TERM*   eAddr)
{
    size_t         i;
    ERL_NIF_TERM   at[4];
    size_t         atLen = NUM(at);
    unsigned char* a     = (unsigned char*) addrP;
    ERL_NIF_TERM   addr;
    
    /* The address */
    for (i = 0; i < atLen; i++) {
        at[i] = MKI(env, a[i]);
    }

    addr = MKTA(env, at, atLen);
    UDBG( ("SUTIL", "esock_encode_in_addr -> addr: %T\r\n", addr) );
    // *eAddr = MKTA(env, at, atLen);
    *eAddr = addr;
}



/* +++ esock_decode_in6_addr +++
 *
 * Decode an IPv6 address. This can be three things:
 *
 *    + Then atom 'any'
 *    + Then atom 'loopback'
 *    + An ip6_address() (8 tuple)
 *
 * Note that this *only* decodes the "address" part of a
 * (IPv6) socket address. There are several other things
 * (port, flowinfo and scope_id) that are handled elsewhere).
 */

#if defined(HAVE_IN6) && defined(AF_INET6)
extern
BOOLEAN_T esock_decode_in6_addr(ErlNifEnv*       env,
                                ERL_NIF_TERM     eAddr,
                                struct in6_addr* inAddrP)
{
    UDBG( ("SUTIL", "esock_decode_in6_addr -> entry with"
           "\r\n   eAddr: %T"
           "\r\n", eAddr) );

    if (IS_ATOM(env, eAddr)) {
        /* This is either 'any' or 'loopback' */

        if (COMPARE(esock_atom_loopback, eAddr) == 0) {
            *inAddrP = in6addr_loopback;
        } else if (COMPARE(esock_atom_any, eAddr) == 0) {
            *inAddrP = in6addr_any;
        } else {
            return FALSE;
        }
        
    } else {
        /* This is an 8-tuple */
        
        const ERL_NIF_TERM* tuple;
        int                 arity;
        size_t              n;
        struct in6_addr     sa;

        if (! GET_TUPLE(env, eAddr, &arity, &tuple))
            return FALSE;
        n = arity << 1;

        if (n != sizeof(sa.s6_addr))
            return FALSE;

        for (n = 0;  n < arity;  n++) {
            int v;

            if (! GET_INT(env, tuple[n], &v) ||
                v < 0 || 65535 < v)
                return FALSE;

            put_int16(v, sa.s6_addr + (n << 1));
        }
        *inAddrP = sa;
    }

    return TRUE;
}
#endif



/* +++ esock_encode_in6_addr +++
 *
 * Encode an IPv6 address:
 *
 *    + An ip6_address() (8 tuple)
 *
 * Note that this *only* encodes the "address" part of a
 * (IPv6) socket address. There are several other things
 * (port, flowinfo and scope_id) that are handled elsewhere).
 */

#if defined(HAVE_IN6) && defined(AF_INET6)
extern
void esock_encode_in6_addr(ErlNifEnv*       env,
                           struct in6_addr* addrP,
                           ERL_NIF_TERM*    eAddr)
{
    size_t         i;
    ERL_NIF_TERM   at[8];
    size_t         atLen = NUM(at);
    unsigned char* a     = UCHARP(addrP->s6_addr);
    
    /* The address */
    for (i = 0; i < atLen; i++) {
        at[i] = MKI(env, get_int16(a + i*2));
    }

    *eAddr = MKTA(env, at, atLen);
}
#endif



/* +++ esock_encode_timeval +++
 *
 * Encode a timeval struct into its erlang form, a map with two fields:
 *
 *    sec
 *    usec
 *
 */
extern
void esock_encode_timeval(ErlNifEnv*      env,
                           struct timeval* timeP,
                           ERL_NIF_TERM*   eTime)
{
    ERL_NIF_TERM keys[]  = {esock_atom_sec, esock_atom_usec};
    ERL_NIF_TERM vals[]  = {MKL(env, timeP->tv_sec), MKL(env, timeP->tv_usec)};
    size_t       numKeys = NUM(keys);

    ESOCK_ASSERT( numKeys == NUM(vals) );
    ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, eTime) );
}



/* +++ esock_decode_timeval +++
 *
 * Decode a timeval in its erlang form (a map) into its native form,
 * a timeval struct.
 *
 */
extern
BOOLEAN_T esock_decode_timeval(ErlNifEnv*      env,
                               ERL_NIF_TERM    eTime,
                               struct timeval* timeP)
{
    ERL_NIF_TERM eSec, eUSec;

    if (! GET_MAP_VAL(env, eTime, esock_atom_sec, &eSec))
        return FALSE;

    if (! GET_MAP_VAL(env, eTime, esock_atom_usec, &eUSec))
        return FALSE;

    /* Use the appropriate variable type and nif function
     * to decode the value from Erlang into the struct timeval fields
     */
    { /* time_t tv_sec; */
#if (SIZEOF_TIME_T == 8)
        ErlNifSInt64 sec;
        if (! GET_INT64(env, eSec, &sec))
            return FALSE;
#elif (SIZEOF_TIME_T == SIZEOF_INT)
        int sec;
        if (! GET_INT(env, eSec, &sec))
            return FALSE;
#else /* long or other e.g undefined */
        long sec;
        if (! GET_LONG(env, eSec, &sec))
            return FALSE;
#endif
        timeP->tv_sec = sec;
    }

    { /* suseconds_t tv_usec; */
#if (SIZEOF_SUSECONDS_T == 8)
        ErlNifSInt64 usec;
        if (! GET_INT64(env, eSec, &usec))
            return FALSE;
#elif (SIZEOF_SUSECONDS_T == SIZEOF_INT)
        int usec;
        if (! GET_INT(env, eSec, &usec))
            return FALSE;
#else /* long or other e.g undefined */
        long usec;
        if (! GET_LONG(env, eSec, &usec))
            return FALSE;
#endif
        timeP->tv_usec = usec;
    }

    return TRUE;
}



/* +++ esock_decode_domain +++
 *
 * Decode the Erlang form of the 'domain' type, that is: 
 * 
 * Return 1:
 *    inet   => AF_INET
 *    inet6  => AF_INET6
 *    local  => AF_LOCAL
 *    unspec => AF_UNSPEC
 *
 * Return -1:
 *    Int   => Int
 *
 * Otherwise return 0.
 *
 */
extern
int esock_decode_domain(ErlNifEnv*   env,
                    ERL_NIF_TERM eDomain,
                    int*         domain)
{
    if (COMPARE(esock_atom_inet, eDomain) == 0) {
        *domain = AF_INET;

#if defined(HAVE_IN6) && defined(AF_INET6)
    } else if (COMPARE(esock_atom_inet6, eDomain) == 0) {
        *domain = AF_INET6;
#endif

#ifdef HAS_AF_LOCAL
    } else if (COMPARE(esock_atom_local, eDomain) == 0) {
        *domain = AF_LOCAL;
#endif

#ifdef AF_UNSPEC
    } else if (COMPARE(esock_atom_unspec, eDomain) == 0) {
        *domain = AF_UNSPEC;
#endif

    } else {
        int d;

        d = 0;
        if (GET_INT(env, eDomain, &d)) {
            *domain = d;
            return -1;
        }
        return 0;
    }

    return 1;
}



/* +++ esock_encode_domain +++
 *
 * Encode the native domain to the Erlang form, that is: 
 * 
 *    AF_INET    => inet
 *    AF_INET6   => inet6
 *    AF_LOCAL   => local
 *    AF_UNSPEC  => unspec
 *
 */
extern
void esock_encode_domain(ErlNifEnv*    env,
                         int           domain,
                         ERL_NIF_TERM* eDomain)
{
    switch (domain) {
    case AF_INET:
        *eDomain = esock_atom_inet;
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        *eDomain = esock_atom_inet6;
        break;
#endif

#ifdef HAS_AF_LOCAL
    case AF_LOCAL:
        *eDomain = esock_atom_local;
        break;
#endif

#ifdef AF_UNSPEC
    case AF_UNSPEC:
        *eDomain = esock_atom_unspec;
        break;
#endif

    default:
        *eDomain = MKI(env, domain);
    }
}



/* +++ esock_decode_type +++
 *
 * Decode the Erlang form of the 'type' type, that is: 
 * 
 *    stream    => SOCK_STREAM
 *    dgram     => SOCK_DGRAM
 *    raw       => SOCK_RAW
 *    seqpacket => SOCK_SEQPACKET
 *
 */
extern
BOOLEAN_T esock_decode_type(ErlNifEnv*   env,
                            ERL_NIF_TERM eType,
                            int*         type)
{
    int cmp;

    /* A manual binary search to minimize the number of COMPARE:s */
    cmp = COMPARE(esock_atom_raw, eType);
    if (cmp < 0) {
        if (COMPARE(esock_atom_stream, eType) == 0) {
            *type = SOCK_STREAM;
#ifdef SOCK_SEQPACKET
        } else if (COMPARE(esock_atom_seqpacket, eType) == 0) {
            *type = SOCK_SEQPACKET;
#endif
        } else
            goto integer;
    } else if (0 < cmp) {
        if (COMPARE(esock_atom_dgram, eType) == 0) {
            *type = SOCK_DGRAM;
        } else
            goto integer;
    } else
        *type = SOCK_RAW;

    return TRUE;

 integer:
    {
        int t = 0;

        if (GET_INT(env, eType, &t)) {
            *type = t;
            return TRUE;
        }
    }
    return FALSE;
}



/* +++ esock_encode_type +++
 *
 * Encode the native type to the Erlang form, that is: 
 * 
 *    SOCK_STREAM    => stream
 *    SOCK_DGRAM     => dgram
 *    SOCK_RAW       => raw
 *    SOCK_SEQPACKET => seqpacket
 *
 */
extern
void esock_encode_type(ErlNifEnv*    env,
                       int           type,
                       ERL_NIF_TERM* eType)
{
    switch (type) {
    case SOCK_STREAM:
        *eType = esock_atom_stream;
        break;

    case SOCK_DGRAM:
        *eType = esock_atom_dgram;
        break;

    case SOCK_RAW:
        *eType = esock_atom_raw;
        break;

#ifdef SOCK_SEQPACKET
    case SOCK_SEQPACKET:
        *eType = esock_atom_seqpacket;
        break;
#endif

#ifdef SOCK_RDM
    case SOCK_RDM:
        *eType = esock_atom_rdm;
        break;
#endif

    default:
        *eType = MKI(env, type);
    }
}



/* +++ esock_encode_packet_protocol +++
 *
 * Encode the Link Layer sockaddr protocol.
 * 
 * Currently we just represent this as an unsigned int.
 */
extern
void esock_encode_packet_protocol(ErlNifEnv*     env,
                                  unsigned short protocol,
                                  ERL_NIF_TERM*  eProtocol)
{
    *eProtocol = MKUI(env, protocol);
}


/* +++ esock_encode_packet_hatype +++
 *
 * Encode the Link Layer sockaddr hatype.
 * 
 * Currently we just represent this as an unsigned int.
 */
extern
void esock_encode_packet_hatype(ErlNifEnv*     env,
                                unsigned short hatype,
                                ERL_NIF_TERM*  eHaType)
{
    *eHaType = MKUI(env, hatype);
}


/* +++ esock_encode_packet_pkttype +++
 *
 * Encode the Link Layer sockaddr pkttype.
 * 
 *    PACKET_HOST      => host
 *    PACKET_BROADCAST => broadcast
 *    PACKET_MULTICAST => multicast
 *    PACKET_OTHERHOST => otherhost
 *    PACKET_OUTGOING  => outgoing
 *    PACKET_LOOPBACK  => loopback
 *    PACKET_USER      => user
 *    PACKET_KERNEL    => kernel
 *
 */
extern
void esock_encode_packet_pkttype(ErlNifEnv*     env,
                                 unsigned short pkttype,
                                 ERL_NIF_TERM*  ePktType)
{
    switch (pkttype) {
#if defined(PACKET_HOST)
    case PACKET_HOST:
        *ePktType = esock_atom_host;
        break;
#endif

#if defined(PACKET_BROADCAST)
    case PACKET_BROADCAST:
        *ePktType = esock_atom_broadcast;
        break;
#endif

#if defined(PACKET_MULTICAST)
    case PACKET_MULTICAST:
        *ePktType = esock_atom_multicast;
        break;
#endif

#if defined(PACKET_OTHERHOST)
    case PACKET_OTHERHOST:
        *ePktType = esock_atom_otherhost;
        break;
#endif

#if defined(PACKET_OUTGOING)
    case PACKET_OUTGOING:
        *ePktType = esock_atom_outgoing;
        break;
#endif

        /* Unused? Not user space? */
#if defined(PACKET_LOOPBACK)
    case PACKET_LOOPBACK:
        *ePktType = esock_atom_loopback;
        break;
#endif

#if defined(PACKET_USER)
    case PACKET_USER:
        *ePktType = esock_atom_user;
        break;
#endif

#if defined(PACKET_KERNEL)
    case PACKET_KERNEL:
        *ePktType = esock_atom_kernel;
        break;
#endif

        /* Unused? Not user space?
         * Also, has the same value as PACKET_USER,
         * so may result in a compiler error (at least
         * on some platforms: ANDROID).
         *
#if defined(PACKET_FASTROUTE)
    case PACKET_FASTROUTE:
        *ePktType = esock_atom_fastroute;
        break;
#endif
        */

    default:
        *ePktType = MKUI(env, pkttype);
        break;
    }

}



/* +++ esock_encode_packet_addr +++
 *
 * Encode the Link Layer sockaddr address.
 * 
 */
extern
void esock_encode_packet_addr(ErlNifEnv*     env,
                              unsigned char  len,
                              unsigned char* addr,
                              ERL_NIF_TERM*  eAddr)
{
#if defined(ESOCK_PACKET_ADDRESS_AS_TUPLE)
    esock_encode_packet_addr_tuple(env, len, addr, eAddr);
#else
    SOCKOPTLEN_T vsz = len;
    ErlNifBinary val;

    if (ALLOC_BIN(vsz, &val)) {
        sys_memcpy(val.data, addr, len);
        *eAddr = MKBIN(env, &val);
    } else {
        esock_encode_packet_addr_tuple(env, len, addr, eAddr);
    }
#endif
}


static
void esock_encode_packet_addr_tuple(ErlNifEnv*     env,
                                    unsigned char  len,
                                    unsigned char* addr,
                                    ERL_NIF_TERM*  eAddr)
{
    ERL_NIF_TERM  array[len];
    unsigned char i;

    for (i = 0; i < len; i++) {
        array[i] = MKUI(env, addr[i]);
    }

    *eAddr = MKTA(env, array, len);
}



/* +++ esock_decode_sockaddr_native +++
 *
 * Decode a general sockaddr with unknown format, within Erlang
 * represented as a map, which has a specific set of attributes
 * (beside the mandatory family attribute, which is "inherited" from
 * the "sockaddr" type):
 *
 *    addr :: binary()
 *
 * The erlang module ensures that this value exist, so there
 * is no need for any elaborate error handling here.
 */

static
BOOLEAN_T esock_decode_sockaddr_native(ErlNifEnv*     env,
                                       ERL_NIF_TERM   eSockAddr,
                                       ESockAddress*  sockAddrP,
                                       int            family,
                                       SOCKLEN_T*     addrLen)
{
    ErlNifBinary bin;
    ERL_NIF_TERM eAddr;
    SOCKLEN_T    len;

    /* *** Extract (e) Addr (a binary) from map *** */
    if (! GET_MAP_VAL(env, eSockAddr, esock_atom_addr, &eAddr))
        return FALSE;

    /* Get the address */
    if (! GET_BIN(env, eAddr, &bin))
        return FALSE;

    len = sizeof(*sockAddrP) -
        (CHARP(sockAddrP->sa.sa_data) - CHARP(sockAddrP)); // Max addr size
    if ((size_t)len < bin.size)
        return FALSE;

    sys_memzero((char*) sockAddrP, sizeof(*sockAddrP));
    sockAddrP->sa.sa_family = (sa_family_t) family;
    sys_memcpy(sockAddrP->sa.sa_data, bin.data, bin.size);
    len = (sockAddrP->sa.sa_data - CHARP(sockAddrP)) + bin.size;
#ifndef NO_SA_LEN
    sockAddrP->sa.sa_len = len;
#endif
    *addrLen = len;

    return TRUE;
}



/* Encode as #{family := integer(), addr := binary()}
 * assuming at least the ->family field can be accessed
 * and hence at least 0 bytes of address
 */
static
void esock_encode_sockaddr_native(ErlNifEnv*       env,
                                  struct sockaddr* addr,
                                  SOCKLEN_T        len,
                                  ERL_NIF_TERM     eFamily,
                                  ERL_NIF_TERM*    eSockAddr)
{
    size_t size;
    ERL_NIF_TERM eData;

    size = ((char*)addr + len) - (char*)&addr->sa_data;
    eData = esock_make_new_binary(env, &addr->sa_data, size);

    {
        ERL_NIF_TERM keys[] = {esock_atom_family, esock_atom_addr};
        ERL_NIF_TERM vals[] = {eFamily, eData};
        size_t numKeys = NUM(keys);

        ESOCK_ASSERT( numKeys == NUM(vals) );
        ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, eSockAddr) );
    }
}

/* Encode as a raw binary() regarding the whole address
 * structure as a blob
 */
static void esock_encode_sockaddr_broken(ErlNifEnv*       env,
                                         struct sockaddr* addr,
                                         SOCKLEN_T        len,
                                         ERL_NIF_TERM*    eSockAddr) {
    *eSockAddr = esock_make_new_binary(env, addr, len);
}



/* +++ esock_decode_bufsz +++
 *
 * Decode an buffer size. The size of a buffer is: 
 * 
 *    eVal > 0           => Use provided value
 *    eVal == 'default'  => Use provided default
 *
 */
extern
BOOLEAN_T esock_decode_bufsz(ErlNifEnv*   env,
                             ERL_NIF_TERM eVal,
                             size_t       defSz,
                             size_t*      szp)
{
    unsigned long val;

    if (GET_ULONG(env, eVal, &val)) {
        /* Check value */
        defSz = (size_t) val;
        if (val != (unsigned long) defSz || val == 0)
            return FALSE;
    } else {
        if (COMPARE(eVal, esock_atom_default) != 0)
            return FALSE;
    }

    *szp = defSz;
    return TRUE;
}



/* *** esock_decode_string ***
 *
 * Decode a string value. A successful decode results in an 
 * allocation of the string, which the caller has to free
 * once the string has been used.
 */
extern
BOOLEAN_T esock_decode_string(ErlNifEnv*         env,
                              const ERL_NIF_TERM eString,
                              char**             stringP)
{
    BOOLEAN_T    result;
    unsigned int len;
    char*        bufP;
    
    if (!GET_LIST_LEN(env, eString, &len) && (len != 0)) {
        *stringP = NULL;
        result   = FALSE;
    } else {

        UDBG( ("SUTIL", "esock_decode_string -> len: %d\r\n", len) );

        bufP = MALLOC(len + 1); // We shall NULL-terminate
    
        if (GET_STR(env, eString, bufP, len+1)) {
            UDBG( ("SUTIL", "esock_decode_string -> buf: %s\r\n", bufP) );
            // bufP[len] = '\0';
            *stringP = bufP;
            result   = TRUE;
        } else {
            *stringP = NULL;
            result   = FALSE;
            FREE(bufP);
        }
    }

    return result;
}



/* *** esock_extract_pid_from_map ***
 *
 * Extract a pid item from a map.
 * Returns TRUE on success and FALSE on failure.
 *
 */
extern
BOOLEAN_T esock_extract_pid_from_map(ErlNifEnv*   env,
                                     ERL_NIF_TERM map,
                                     ERL_NIF_TERM key,
                                     ErlNifPid*   pid)
{
    ERL_NIF_TERM val;
    BOOLEAN_T    res;

    if (! GET_MAP_VAL(env, map, key, &val))
        return FALSE;

    res = enif_get_local_pid(env, val, pid);
    return res;
}



/* *** esock_extract_int_from_map ***
 *
 * Simple utility function used to extract a integer value from a map.
 */

extern
BOOLEAN_T esock_extract_int_from_map(ErlNifEnv*   env,
                                 ERL_NIF_TERM map,
                                 ERL_NIF_TERM key,
                                 int*         val)
{
    ERL_NIF_TERM eval;
    BOOLEAN_T    ret;

    if (! GET_MAP_VAL(env, map, key, &eval))
        return FALSE;

    ret = GET_INT(env, eval, val);
    return ret;
}



/* *** esock_decode_bool ***
 *
 * Decode a boolean value.
 *
 */
extern
BOOLEAN_T esock_decode_bool(ERL_NIF_TERM eVal, BOOLEAN_T* val)
{
    if (COMPARE(esock_atom_true, eVal) == 0)
        *val = TRUE;
    else if (COMPARE(esock_atom_false, eVal) == 0)
        *val = FALSE;
    else
        return FALSE;

    return TRUE;
}


/* *** esock_encode_bool ***
 *
 * Encode a boolean value.
 *
 */
extern
ERL_NIF_TERM esock_encode_bool(BOOLEAN_T val)
{
    if (val)
        return esock_atom_true;
    else
        return esock_atom_false;
}


/* *** esock_decode_level ***
 *
 * Decode option or cmsg level - 'socket' or protocol number.
 *
 */
extern
BOOLEAN_T esock_decode_level(ErlNifEnv* env, ERL_NIF_TERM eVal, int *val)
{
    if (COMPARE(esock_atom_socket, eVal) == 0)
        *val = SOL_SOCKET;
    else if (! GET_INT(env, eVal, val))
        return FALSE;

    return TRUE;
}

/* *** esock_encode_level ***
 *
 * Encode option or cmsg level - SOL_SOCKET or protocol number.
 *
 */
extern
ERL_NIF_TERM esock_encode_level(ErlNifEnv* env, int level)
{
    if (level == SOL_SOCKET)
        return esock_atom_socket;
    else
        return MKI(env, level);
}


/* Create an ok two (2) tuple in the form:
 *
 *         {ok, Any}
 *
 * The second element (Any) is already in the form of an
 * ERL_NIF_TERM so all we have to do is create the tuple.
 */
extern
ERL_NIF_TERM esock_make_ok2(ErlNifEnv* env, ERL_NIF_TERM any)
{
    return MKT2(env, esock_atom_ok, any);
}


/* Create an error two (2) tuple in the form:
 *
 *          {error, Reason}
 *
 * The second element (Reason) is already in the form of an
 * ERL_NIF_TERM so all we have to do is create the tuple.
 */
extern
ERL_NIF_TERM esock_make_error(ErlNifEnv* env, ERL_NIF_TERM reason)
{
    return MKT2(env, esock_atom_error, reason);
}



/* Create an error two (2) tuple in the form: {error, Reason}.
 *
 *          {error, Reason}
 *
 * The second element, Reason, is the reason string that has
 * converted into an atom.
 */
extern
ERL_NIF_TERM esock_make_error_str(ErlNifEnv* env, char* reason)
{
    return esock_make_error(env, MKA(env, reason));
}


/* Create an error two (2) tuple in the form:
 *
 *          {error, Reason}
 *
 * The second element, Reason, is the errno value in its
 * basic form (integer) which has been converted into an atom.
 */
extern
ERL_NIF_TERM esock_make_error_errno(ErlNifEnv* env, int err)
{
    return esock_make_error_str(env, erl_errno_id(err));
}



/* Create an error two (2) tuple in the form:
 *
 *          {error, {invalid, What}}}
 */
extern
ERL_NIF_TERM esock_make_error_invalid(ErlNifEnv* env, ERL_NIF_TERM what)
{
    return MKT2(env,
                esock_atom_error,
                MKT2(env, esock_atom_invalid, what));
}



/* Create an 'error' two (2) tuple in the form:
 *
 *          {error, {invalid, {integer_range, I}}}
 *
 * The second element (i) is already in the form of an
 * ERL_NIF_TERM so all we have to do is create the tuple.
 */
extern
ERL_NIF_TERM esock_make_error_integer_range(ErlNifEnv* env, ERL_NIF_TERM i)
{
    return
        esock_make_invalid(env, MKT2(env, esock_atom_integer_range, i));
}



/* Create an 'invalid' two (2) tuple in the form:
 *
 *          {invalid, Reason}
 *
 * The second element (Reason) is already in the form of an
 * ERL_NIF_TERM so all we have to do is create the tuple.
 */
extern
ERL_NIF_TERM esock_make_invalid(ErlNifEnv* env, ERL_NIF_TERM reason)
{
    return MKT2(env, esock_atom_invalid, reason);
}



/* Raise an exception {invalid, What}
 */
extern
ERL_NIF_TERM esock_raise_invalid(ErlNifEnv* env, ERL_NIF_TERM what)
{
    return enif_raise_exception(env, MKT2(env, esock_atom_invalid, what));
}



/* strnlen doesn't exist everywhere */
extern
size_t esock_strnlen(const char *s, size_t maxlen)
{
    size_t i = 0;
    while (i < maxlen && s[i] != '\0')
        i++;
    return i;
}



/* *** esock_abort ***
 *
 * Generate an abort with "extra" info. This should be called
 * via the ESOCK_ABORT macro.
 * Basically it prints the extra info onto stderr before aborting.
 *
 */
extern
void __noreturn
esock_abort(const char* expr,
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



/* *** esock_self ***
 *
 * This function returns the current pid (self) in term form,
 * or the atom undefined if not executed in the context of an (erlang) process.
 */
extern
ERL_NIF_TERM esock_self(ErlNifEnv* env)
{
    ErlNifPid pid;

    /* Make an idiot test first just to ensure we don't kill ourselves */
    if (env == NULL)
        return esock_atom_undefined;
    else if (enif_self(env, &pid) == NULL)
        return esock_atom_undefined;
    else
        return enif_make_pid(env, &pid);
}



/* *** esock_warning_msg ***
 *
 * Temporary function for issuing warning messages.
 *
 */
extern
void esock_warning_msg( const char* format, ... )
{
  va_list         args;
  char            f[512 + sizeof(format)]; // This has to suffice...
  char            stamp[64]; // Just in case...
  int             res;

  /*
   * We should really include self in the printout,
   * so we can se which process are executing the code.
   * But then I must change the API....something for later.
   */

  // 2018-06-29 12:13:21.232089
  // 29-Jun-2018::13:47:25.097097

  if (esock_timestamp_str(stamp, sizeof(stamp))) {
      res = enif_snprintf(f, sizeof(f),
                          "=WARNING MSG==== %s ===\r\n%s",
                          stamp, format);
  } else {
      res = enif_snprintf(f, sizeof(f), "=WARNING MSG==== %s", format);
  }

  if (res > 0) {
      va_start (args, format);
      enif_vfprintf (stdout, f, args);
      va_end (args);
      fflush(stdout);
  }

  return;
}


/* *** esock_timestamp ***
 *
 * Create a timestamp.
 * Produces a timestamp in the form of an "Epoch" (A real epoch
 * is the number of seconds since 1/1 1970, but our timestamp is
 * the number micro seconds since 1/1 1970).
 */

extern
ErlNifTime esock_timestamp()
{
    ErlNifTime monTime = enif_monotonic_time(ERL_NIF_USEC);
    ErlNifTime offTime = enif_time_offset(ERL_NIF_USEC);

    return (monTime + offTime);

}



/* *** esock_timestamp_str ***
 *
 * Create a timestamp string.
 * If awailable, we use the localtime_r and strftime function(s)
 * to produces a nice readable timestamp. But if not (awailable),
 * it produces a timestamp in the form of an "Epoch" (A real epoch
 * is the number of seconds since 1/1 1970, but our timestamp is
 * the number micro seconds since 1/1 1970).
 *
 */

extern
BOOLEAN_T esock_timestamp_str(char *buf, unsigned int len)
{
    return esock_format_timestamp(esock_timestamp(), buf, len);
}



/* *** esock_format_timestamp ***
 *
 * Format a timestamp.
 * If awailable, we use the localtime_r and strftime function(s)
 * to produces a nice readable timestamp. But if not (awailable),
 * it produces a timestamp in the form of an "Epoch" (A real epoch
 * is the number of seconds since 1/1 1970, but our timestamp is
 * the number micro seconds since 1/1 1970).
 */

extern
BOOLEAN_T esock_format_timestamp(ErlNifTime timestamp, char *buf, unsigned int len)
{
    unsigned  ret;
#if defined(ESOCK_USE_PRETTY_TIMESTAMP)

    time_t    sec     = timestamp / 1000000; // (if _MSEC) sec  = time / 1000;
    time_t    usec    = timestamp % 1000000; // (if _MSEC) msec = time % 1000;
    struct tm t;

    if (localtime_r(&sec, &t) == NULL)
        return FALSE;

    ret = strftime(buf, len, "%d-%b-%Y::%T", &t);
    if (ret == 0)
        return FALSE;
    len -= ret;
    buf += ret;

    ret = enif_snprintf(buf, len, ".%06lu", (unsigned long) usec);
    if (ret >= len)
        return FALSE;

    return TRUE;

#else

    ret = enif_snprintf(buf, len, "%lu", (unsigned long) timestamp);
    if (ret >= len)
        return FALSE;

    return TRUE;

#endif
}



/* =================================================================== *
 *                                                                     *
 *              Various (internal) utility functions                   *
 *                                                                     *
 * =================================================================== */

/* Construct the IPv4 socket address */
static
void make_sockaddr_in(ErlNifEnv*    env,
                           ERL_NIF_TERM  port,
                           ERL_NIF_TERM  addr,
                           ERL_NIF_TERM* sa)
{
    ERL_NIF_TERM keys[]  = {esock_atom_family, esock_atom_port, esock_atom_addr};
    ERL_NIF_TERM vals[]  = {esock_atom_inet, port, addr};
    size_t       numKeys = NUM(keys);
    
    ESOCK_ASSERT( numKeys == NUM(vals) );
    ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, sa) );
}


/* Construct the IPv6 socket address */
static
void make_sockaddr_in6(ErlNifEnv*    env,
                       ERL_NIF_TERM  port,
                       ERL_NIF_TERM  addr,
                       ERL_NIF_TERM  flowInfo,
                       ERL_NIF_TERM  scopeId,
                       ERL_NIF_TERM* sa)
{
    ERL_NIF_TERM keys[]  = {esock_atom_family,
                            esock_atom_port,
                            esock_atom_addr,
                            esock_atom_flowinfo,
                            esock_atom_scope_id};
    ERL_NIF_TERM vals[]  = {esock_atom_inet6,
                            port,
                            addr,
                            flowInfo,
                            scopeId};
    size_t       numKeys = NUM(keys);
    
    ESOCK_ASSERT( numKeys == NUM(vals) );
    ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, sa) );
}


/* Construct the Unix Domain socket address */
static
void make_sockaddr_un(ErlNifEnv*    env,
                      ERL_NIF_TERM  path,
                      ERL_NIF_TERM* sa)
{
    ERL_NIF_TERM keys[]  = {esock_atom_family, esock_atom_path};
    ERL_NIF_TERM vals[]  = {esock_atom_local,  path};
    size_t       numKeys = NUM(keys);
    
    ESOCK_ASSERT( numKeys == NUM(vals) );
    ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, sa) );
}


/* Construct the Link Layer socket address */
#ifdef HAVE_NETPACKET_PACKET_H
static
void make_sockaddr_ll(ErlNifEnv*    env,
                      ERL_NIF_TERM  proto,
                      ERL_NIF_TERM  ifindex,
                      ERL_NIF_TERM  hatype,
                      ERL_NIF_TERM  pkttype,
                      ERL_NIF_TERM  addr,
                      ERL_NIF_TERM* sa)
{
    ERL_NIF_TERM keys[]  = {esock_atom_family,
                            esock_atom_protocol,
                            esock_atom_ifindex,
                            esock_atom_hatype,
                            esock_atom_pkttype,
                            esock_atom_addr};
    ERL_NIF_TERM vals[]  = {esock_atom_packet,
                            proto,
                            ifindex,
                            hatype,
                            pkttype,
                            addr};
    size_t       numKeys = NUM(keys);
    
    ESOCK_ASSERT( numKeys == NUM(vals) );
    ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, sa) );
}
#endif


extern
ERL_NIF_TERM esock_make_new_binary(ErlNifEnv *env, void *buf, size_t size)
{
    ERL_NIF_TERM term;

    sys_memcpy(enif_make_new_binary(env, size, &term), buf, size);

    return term;
}


/* This is an expensive way to do erlang:is_integer/1.
 *
 * We need it when we have -spec:ed an argument to be integer(),
 * and enif_get_int() et.al fails since it may be a bignum.
 * Then we can not just throw a badarg, because Dialyzer assumes
 * that such a call shall return.
 *
 * So, after enif_get_int() has failed;
 * if esock_is_int() then
 *     error return
 * else
 *     badarg
 */
extern
BOOLEAN_T esock_is_integer(ErlNifEnv *env, ERL_NIF_TERM term)
{
    double d;

    /* Test that it is a number() but not a float(),
     * then it must be an integer()
     */
    if (enif_is_number(env, term))
        return (! enif_get_double(env, term, &d));
    else
        return FALSE;
}
