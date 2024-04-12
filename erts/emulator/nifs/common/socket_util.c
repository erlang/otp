/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2018-2024. All Rights Reserved.
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
#    include "config.h"
#endif

#ifdef ESOCK_ENABLE

#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <stddef.h>
#include <sys/types.h>
#if !defined(__WIN32__)
#include <sys/socket.h>
#endif

#if !defined(__IOS__) && !defined(__WIN32__)
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

#if defined(__WIN32__)
typedef u_short sa_family_t;
#endif


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

#if defined(HAVE_NET_IF_DL_H) && defined(AF_LINK)
static void esock_encode_sockaddr_dl(ErlNifEnv*          env,
                                     struct sockaddr_dl* sockAddrP,
                                     SOCKLEN_T           addrLen,
                                     ERL_NIF_TERM*       eSockAddr);
#endif
static void esock_encode_sockaddr_native(ErlNifEnv*       env,
                                         struct sockaddr* sa,
                                         SOCKLEN_T        len,
                                         ERL_NIF_TERM     eFamily,
                                         ERL_NIF_TERM*    eSockAddr);

static void esock_encode_sockaddr_broken(ErlNifEnv*       env,
                                         struct sockaddr* sa,
                                         SOCKLEN_T        len,
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
#if defined(HAVE_NET_IF_DL_H) && defined(AF_LINK)
static void make_sockaddr_dl(ErlNifEnv*    env,
                             ERL_NIF_TERM  index,
                             ERL_NIF_TERM  type,
                             ERL_NIF_TERM  nlen,
                             ERL_NIF_TERM  alen,
                             ERL_NIF_TERM  slen,
                             ERL_NIF_TERM  data,
                             ERL_NIF_TERM* sa);
#endif
#ifdef HAS_AF_LOCAL
static SOCKLEN_T sa_local_length(int l, struct sockaddr_un* sa);
#endif


/* *** esock_get_uint_from_map ***
 *
 * Simple utility function used to extract a unsigned int value from a map.
 * If it fails to extract the value (for whatever reason) the default
 * value is used.
 */

extern
unsigned int esock_get_uint_from_map(ErlNifEnv*   env,
                                     ERL_NIF_TERM map,
                                     ERL_NIF_TERM key,
                                     unsigned int def)
{
    ERL_NIF_TERM eval;
    unsigned int val;

    if (!GET_MAP_VAL(env, map, key, &eval)) {
        return def;
    } else {
        if (GET_UINT(env, eval, &val))
            return val;
        else
            return def;
    }
}


/* *** esock_get_bool_from_map ***
 *
 * Simple utility function used to extract a boolean value from a map.
 * If it fails to extract the value (for whatever reason) the default
 * value is used.
 */

extern
BOOLEAN_T esock_get_bool_from_map(ErlNifEnv*   env,
                                  ERL_NIF_TERM map,
                                  ERL_NIF_TERM key,
                                  BOOLEAN_T    def)
{
    ERL_NIF_TERM eval;

    if (!GET_MAP_VAL(env, map, key, &eval)) {
        return def;
    } else {
        if (COMPARE(eval, esock_atom_true) == 0)
            return TRUE;
        else if (COMPARE(eval, esock_atom_false) == 0)
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
                      SysIOVec*     iov,
                      size_t        len,
                      ErlNifBinary* data,
                      ERL_NIF_TERM* eIOV)
{
    ssize_t       rem = read;
    size_t        i;
    ERL_NIF_TERM* a;

    UDBG( ("SUTIL", "esock_encode_iov -> entry with"
           "\r\n   read:      %ld"
           "\r\n   (IOV) len: %lu"
           "\r\n", (long) read, (unsigned long) len) );

    if (len == 0) {
        UDBG( ("SUTIL", "esock_encode_iov -> done when empty\r\n") );
        *eIOV = MKEL(env);
        return;
    } else {
        a = MALLOC(len * sizeof(ERL_NIF_TERM)); // At most this length
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

    FREE(a);

    UDBG( ("SUTIL", "esock_encode_msghdr -> done\r\n") );
}



/* +++ esock_decode_iov +++
 *
 * Decode an IO Vector. In erlang we represented this as a list of binaries.
 *
 * We assume that we have already figured out how long the iov (actually
 * eIOV) is (len), and therefore allocated an array of bins and iov to be
 * used.
 */

extern
BOOLEAN_T esock_decode_iov(ErlNifEnv*    env,
                           ERL_NIF_TERM  eIOV,
                           ErlNifBinary* bufs,
                           SysIOVec*     iov,
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

            iov[i].iov_base  = (void*) bufs[i].data;
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
 *
 * An address length > 0 means the caller knows the length, and we use it.
 * An address length of '-1' means the caller don't know, which
 * in turn mean that "we" has to calculate.
 *
 * sys/socket.h:
 * __SOCKADDR_ALLTYPES
 */

#define SALEN(L, SZ) (((L) > 0) ? (L) : (SZ))

extern
void esock_encode_sockaddr(ErlNifEnv*    env,
                           ESockAddress* sockAddrP,
                           int           addrLen,
                           ERL_NIF_TERM* eSockAddr)
{
  int       family;
  SOCKLEN_T len;

  // Sanity check
  if ((addrLen > 0) &&
      (addrLen < (char *)&sockAddrP->sa.sa_data - (char *)sockAddrP)) {
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
      len = SALEN(addrLen, sizeof(struct sockaddr_in));
      esock_encode_sockaddr_in(env, &sockAddrP->in4, len, eSockAddr);
      break;

#if defined(HAVE_IN6) && defined(AF_INET6)
  case AF_INET6:
      len = SALEN(addrLen, sizeof(struct sockaddr_in6));      
      esock_encode_sockaddr_in6(env, &sockAddrP->in6, len, eSockAddr);
      break;
#endif

#ifdef HAS_AF_LOCAL
  case AF_LOCAL:
      len = sa_local_length(addrLen, &sockAddrP->un);
      esock_encode_sockaddr_un(env, &sockAddrP->un, len, eSockAddr);
      break;
#endif

#ifdef AF_UNSPEC
  case AF_UNSPEC:
      len = SALEN(addrLen, 0);
      esock_encode_sockaddr_native(env,
                                   &sockAddrP->sa, len,
                                   esock_atom_unspec,
                                   eSockAddr);
      break;
#endif

#if defined(HAVE_NETPACKET_PACKET_H)
  case AF_PACKET:
      len = SALEN(addrLen, sizeof(struct sockaddr_ll));      
      esock_encode_sockaddr_ll(env, &sockAddrP->ll, len, eSockAddr);
      break;
#endif

#if defined(AF_IMPLINK)
  case AF_IMPLINK:
      len = SALEN(addrLen, 0);
      esock_encode_sockaddr_native(env,
                                   &sockAddrP->sa, len,
                                   esock_atom_implink,
                                   eSockAddr);
    break;
#endif

#if defined(AF_PUP)
  case AF_PUP:
      len = SALEN(addrLen, 0);
      esock_encode_sockaddr_native(env,
                                   &sockAddrP->sa, len,
                                   esock_atom_pup,
                                   eSockAddr);
      break;
#endif

#if defined(AF_CHAOS)
  case AF_CHAOS:
      len = SALEN(addrLen, 0);
      esock_encode_sockaddr_native(env,
                                   &sockAddrP->sa, len,
                                   esock_atom_chaos,
                                   eSockAddr);
      break;
#endif

#if defined(HAVE_NET_IF_DL_H) && defined(AF_LINK)
  case AF_LINK:
      /*
       * macOS (Darwin Kernel Version 21.4.0):
       * -------------------------------------
       * struct sockaddr_dl {
       *    u_char  sdl_len;       // Total length of sockaddr
       *    u_char  sdl_family;    // AF_LINK
       *    u_short sdl_index;     // if != 0, system given index for interface
       *    u_char  sdl_type;      // interface type
       *    u_char  sdl_nlen;      // interface name length, no trailing 0 reqd.
       *    u_char  sdl_alen;      // link level address length
       *    u_char  sdl_slen;      // link layer selector length
       *    char    sdl_data[12];  // minimum work area, can be larger;
       *                           // contains both if name and ll address
       * #ifndef __APPLE__
       *    // For TokenRing
       *    u_short sdl_rcf;       // source routing control
       *    u_short sdl_route[16]; // source routing information
       * #endif
       * };
       *
       * FreeBSD (12.2-RELEASE-p14):
       * ---------------------------
       * struct sockaddr_dl {
       *    u_char  sdl_len;        // Total length of sockaddr
       *    u_char  sdl_family;     // AF_LINK
       *    u_short sdl_index;      // if != 0,
       *                            // system given index for interface
       *    u_char  sdl_type;       // interface type
       *    u_char  sdl_nlen;       // interface name length, no trailing 0 reqd
       *    u_char  sdl_alen;       // link level address length
       *    u_char  sdl_slen;       // link layer selector length
       *    char    sdl_data[46];   // minimum work area, can be larger;
       *                            // contains both if name and ll address
       * };
       *
       * OpenIndiana 2021.10
       * struct sockaddr_dl {
       *    ushort_t sdl_family;   // AF_LINK
       *    ushort_t sdl_index;    // if != 0,
       *                           // system given index for interface
       *    uchar_t sdl_type;      // interface type
       *    uchar_t sdl_nlen;      // interface name length, no trailing 0 reqd
       *    uchar_t sdl_alen;      // link level address length
       *    uchar_t sdl_slen;      // link layer selector length
       *    char    sdl_data[244]; // contains both if name and ll address
       * };
       *
       */
#if defined(ESOCK_SDL_LEN)
      len = SALEN(addrLen, sockAddrP->dl.sdl_len);
#else
      // The data area is dlen = nlen + alen
      len = SALEN(addrLen,
                  (CHARP(sockAddrP->dl.sdl_data) - CHARP(sockAddrP)) +
                  sockAddrP->dl.sdl_nlen + sockAddrP->dl.sdl_alen);
#endif
      esock_encode_sockaddr_dl(env, &sockAddrP->dl, len, eSockAddr);
    break;
#endif

  default:
      len = SALEN(addrLen, 0);
      esock_encode_sockaddr_native(env,
                                   &sockAddrP->sa, len,
                                   MKI(env, family),
                                   eSockAddr);
      break;
  }
}


#ifdef HAS_AF_LOCAL
static
SOCKLEN_T sa_local_length(int l, struct sockaddr_un* sa)
{
    if (l > 0) {
        return ((SOCKLEN_T) l);
    } else {
#if defined(SUN_LEN)
    return SUN_LEN(sa);
#else
    return (offsetof(struct sockaddr_un, sun_path) + strlen(sa->sun_path) + 1);
#endif
    }
}
#endif


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
    efamily = esock_atom_netrom;
    break;
#endif

#if defined(ARPHRD_ETHER)
  case ARPHRD_ETHER:
    efamily = esock_atom_ether;
    break;
#endif

#if defined(ARPHRD_IEEE802)
  case ARPHRD_IEEE802:
    efamily = esock_atom_ieee802;
    break;
#endif

#if defined(ARPHRD_DLCI)
  case ARPHRD_DLCI:
    efamily = esock_atom_dlci;
    break;
#endif

#if defined(ARPHRD_FRELAY)
  case ARPHRD_FRELAY:
    efamily = esock_atom_frelay;
    break;
#endif

#if defined(ARPHRD_IEEE1394)
  case ARPHRD_IEEE1394:
    efamily = esock_atom_ieee1394;
    break;
#endif

#if defined(ARPHRD_LOOPBACK)
  case ARPHRD_LOOPBACK:
    efamily = esock_atom_loopback;
    break;
#endif

#if defined(ARPHRD_NONE)
  case ARPHRD_NONE:
    efamily = esock_atom_none;
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
                                     addrLen, esock_atom_inet, eSockAddr);
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
                                     addrLen, esock_atom_inet6, eSockAddr);
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

    UDBG( ("SUTIL", "esock_encode_sockaddr_un -> entry with"
           "\r\n   addrLen: %d"
           "\r\n", addrLen) );

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

            UDBG( ("SUTIL", "esock_encode_sockaddr_un -> m: %d\r\n", m) );

            /* And finally build the 'path' attribute */
            path = enif_make_new_binary(env, m, &ePath);
            ESOCK_ASSERT( path != NULL );
            sys_memcpy(path, sockAddrP->sun_path, m);

            /* And the socket address */
            make_sockaddr_un(env, ePath, eSockAddr);
        }
    } else {
        esock_encode_sockaddr_native(env, (struct sockaddr *)sockAddrP,
                                     addrLen, esock_atom_local, eSockAddr);
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
 *    addr:     list()    (should be something useful...)
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

    UDBG( ("SUTIL", "esock_encode_sockaddr_ll -> entry with"
           "\r\n.  addrLen: %d"
           "\r\n", addrLen) );

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
                                     addrLen, esock_atom_packet, eSockAddr);
    }
}
#endif



/* +++ esock_encode_sockaddr_dl +++
 *
 * Encode a LINK address - sockaddr_dl (link-level layer). In erlang it's
 * represented as a map, which has a specific set of attributes
 * (beside the mandatory family attribute, which is "inherited" from
 * the "sockaddr" type):
 *
 * The length field (sdl_len) has already been used, so we don't use it
 * in *this* function.
 *
 *    index: non_neg_integer()
 *    type:  non_neg_integer()
 *    nlen:  non_neg_integer() (name length)
 *    alen:  non_neg_integer() (address length)
 *    slen:  non_neg_integer() (sector length)
 *    data:  binary()
 */

#if defined(HAVE_NET_IF_DL_H) && defined(AF_LINK)
extern
void esock_encode_sockaddr_dl(ErlNifEnv*          env,
                              struct sockaddr_dl* sockAddrP,
                              SOCKLEN_T           addrLen,
                              ERL_NIF_TERM*       eSockAddr)
{
    ERL_NIF_TERM eindex, etype, enlen, ealen, eslen, edata;
    SOCKLEN_T    dlen;

    UDBG( ("SUTIL", "esock_encode_sockaddr_dl -> entry with"
           "\r\n.  addrLen: %d"
           "\r\n", addrLen) );

    /* There is a minumum length (defined by the size of the data field) */
    if (addrLen >= sizeof(struct sockaddr_dl)) {

        /* index - if != 0, system given index for interface */
        eindex = MKUI(env, sockAddrP->sdl_index);

        /* type -  interface type */
        etype = MKUI(env, sockAddrP->sdl_type);

        /* nlen - interface name length, no trailing 0 reqd. */
        enlen = MKUI(env, sockAddrP->sdl_nlen);

        /* alen - link level address length */
        ealen = MKUI(env, sockAddrP->sdl_alen);

        /* slen - ink layer selector length */
        eslen = MKUI(env, sockAddrP->sdl_slen);

        /* data - minimum work area, can be larger;    *
         *        contains both if name and ll address */
        dlen  = addrLen - (CHARP(sockAddrP->sdl_data) - CHARP(sockAddrP));
        edata = esock_make_new_binary(env, &sockAddrP->sdl_data, dlen);

        make_sockaddr_dl(env,
                         eindex, etype, enlen, ealen, eslen, edata,
                         eSockAddr);

    } else {
        esock_encode_sockaddr_native(env, (struct sockaddr *)sockAddrP,
                                     addrLen, esock_atom_link, eSockAddr);
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
    ERL_NIF_TERM tmp;

    switch (hatype) {

        /*
         * ARP protocol HARDWARE identifiers.
         */

#if defined(ARPHRD_NETROM)
    case ARPHRD_NETROM:
        tmp = esock_atom_netrom;
        break;
#endif

#if defined(ARPHRD_ETHER)
    case ARPHRD_ETHER:
        tmp = esock_atom_ether;
        break;
#endif

#if defined(ARPHRD_EETHER)
    case ARPHRD_EETHER:
        tmp = esock_atom_eether;
        break;
#endif

#if defined(ARPHRD_AX25)
    case ARPHRD_AX25:
        tmp = esock_atom_ax25;
        break;
#endif

#if defined(ARPHRD_PRONET)
    case ARPHRD_PRONET:
        tmp = esock_atom_pronet;
        break;
#endif

#if defined(ARPHRD_CHAOS)
    case ARPHRD_CHAOS:
        tmp = esock_atom_chaos;
        break;
#endif

#if defined(ARPHRD_IEEE802)
    case ARPHRD_IEEE802:
        tmp = esock_atom_ieee802;
        break;
#endif

#if defined(ARPHRD_ARCNET)
    case ARPHRD_ARCNET:
        tmp = esock_atom_arcnet;
        break;
#endif

#if defined(ARPHRD_APPLETLK)
    case ARPHRD_APPLETLK:
        tmp = esock_atom_appletlk;
        break;
#endif

#if defined(ARPHRD_DLCI)
    case ARPHRD_DLCI:
        tmp = esock_atom_dlci;
        break;
#endif

#if defined(ARPHRD_ATM)
    case ARPHRD_ATM:
        tmp = esock_atom_atm;
        break;
#endif

#if defined(ARPHRD_METRICOM)
    case ARPHRD_METRICOM:
        tmp = esock_atom_metricom;
        break;
#endif

#if defined(ARPHRD_IEEE1394)
    case ARPHRD_IEEE1394:
        tmp = esock_atom_ieee1394;
        break;
#endif

#if defined(ARPHRD_EUI64)
    case ARPHRD_EUI64:
        tmp = esock_atom_eui64;
        break;
#endif

#if defined(ARPHRD_INFINIBAND)
    case ARPHRD_INFINIBAND:
        tmp = esock_atom_infiniband;
        break;
#endif


        /*
         * Dummy types for non ARP hardware
         */

#if defined(ARPHRD_TUNNEL)
    case ARPHRD_TUNNEL:
        tmp = esock_atom_tunnel;
        break;
#endif

#if defined(ARPHRD_TUNNEL6)
    case ARPHRD_TUNNEL6:
        tmp = esock_atom_tunnel6;
        break;
#endif

#if defined(ARPHRD_LOOPBACK)
    case ARPHRD_LOOPBACK:
        tmp = esock_atom_loopback;
        break;
#endif

#if defined(ARPHRD_LOCALTLK)
    case ARPHRD_LOCALTLK:
        tmp = esock_atom_localtlk;
        break;
#endif


#if defined(ARPHRD_NONE)
    case ARPHRD_NONE:
        tmp = esock_atom_none;
        break;
#endif

#if defined(ARPHRD_VOID)
    case ARPHRD_VOID:
        tmp = esock_atom_void;
        break;
#endif


        /*
         * And the rest will be just integer
         */

    default:
        tmp = MKUI(env, hatype);
        break;
    }

    *eHaType = tmp;
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
    ERL_NIF_TERM* array = MALLOC(len * sizeof(ERL_NIF_TERM));
    unsigned char i;

    for (i = 0; i < len; i++) {
        array[i] = MKUI(env, addr[i]);
    }

    *eAddr = MKTA(env, array, len);

    FREE(array);

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



/* Encode as #{family := atom() | integer(), addr := binary()}
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

    UDBG( ("SUTIL", "esock_encode_sockaddr_native -> entry with"
           "\r\n.  len:     %d"
           "\r\n.  eFamily: %T"
           "\r\n", len, eFamily) );

    if (len > 0) {
        size = ((char*)addr + len) - (char*)&addr->sa_data;
        eData = esock_make_new_binary(env, &addr->sa_data, size);
    } else {
        eData = esock_make_new_binary(env, &addr->sa_data, 0);
    }

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
    UDBG( ("SUTIL", "esock_encode_sockaddr_broken -> entry with"
           "\r\n.  len: %d"
           "\r\n", len) );

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
 * Decode option or cmsg level - 'socket' or level number.
 *
 */
extern
BOOLEAN_T esock_decode_level(ErlNifEnv* env, ERL_NIF_TERM elevel, int *level)
{
    if (COMPARE(esock_atom_socket, elevel) == 0)
        *level = SOL_SOCKET;
    else if (! GET_INT(env, elevel, level))
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


/* Takes an 'errno' value and converts it to a term.
 *
 * If the errno can be translated using erl_errno_id,
 * then we use that value otherwise we use the errno
 * integer value converted to a term.
 * Unless there is a specific error code that can be
 * handled specially.
 */
extern
ERL_NIF_TERM esock_errno_to_term(ErlNifEnv* env, int err)
{
    switch (err) {
#if defined(NO_ERROR)
    case NO_ERROR:
        return MKA(env, "no_error");
        break;
#endif

#if defined(WSA_IO_PENDING)
    case WSA_IO_PENDING:
        return MKA(env, "io_pending");
        break;
#endif

#if defined(WSA_IO_INCOMPLETE)
    case WSA_IO_INCOMPLETE:
        return MKA(env, "io_incomplete");
        break;
#endif

#if defined(WSA_OPERATION_ABORTED)
    case WSA_OPERATION_ABORTED:
        return MKA(env, "operation_aborted");
        break;
#endif

#if defined(WSA_INVALID_PARAMETER)
    case WSA_INVALID_PARAMETER:
        return MKA(env, "invalid_parameter");
        break;
#endif

#if defined(ERROR_INVALID_NETNAME)
    case ERROR_INVALID_NETNAME:
        return MKA(env, "invalid_netname");
        break;
#endif

#if defined(ERROR_NETNAME_DELETED)
    case ERROR_NETNAME_DELETED:
        return MKA(env, "netname_deleted");
        break;
#endif

#if defined(ERROR_TOO_MANY_CMDS)
        /* The network command limit has been reached */
    case ERROR_TOO_MANY_CMDS:
        return MKA(env, "too_many_cmds");
        break;
#endif        

#if defined(ERROR_DUP_NAME)
        /*  Not connected because a duplicate name exists on the network */
    case ERROR_DUP_NAME:
        return MKA(env, "duplicate_name");
        break;
#endif        

#if defined(ERROR_MORE_DATA)
        /*
         * https://stackoverflow.com/questions/31883438/sockets-using-getqueuedcompletionstatus-and-error-more-data
         */
    case ERROR_MORE_DATA:
        return MKA(env, "more_data");
        break;
#endif

#if defined(ERROR_NOT_FOUND)
    case ERROR_NOT_FOUND:
        return MKA(env, "not_found");
        break;
#endif

#if defined(ERROR_NETWORK_UNREACHABLE)
    case ERROR_NETWORK_UNREACHABLE:
        return MKA(env, "network_unreachable");
        break;
#endif

#if defined(ERROR_PORT_UNREACHABLE)
    case ERROR_PORT_UNREACHABLE:
        return MKA(env, "port_unreachable");
        break;
#endif        

    default:
        {
            char* str = erl_errno_id(err);
            if ( strcmp(str, "unknown") == 0 )
                return MKI(env, err);
            else
                return MKA(env, str);
        }
        break;
    }

    /* This is just in case of programming error.
     * We should not get this far!
     */
    return MKI(env, err);
}



/* *** esock_make_extra_error_info_term ***
 * This is used primarily for debugging.
 * Is supposed to be called via the 'MKEEI' macro.
 */
extern
ERL_NIF_TERM esock_make_extra_error_info_term(ErlNifEnv*   env,
                                              const char*  file,
                                              const char*  function,
                                              const int    line,
                                              ERL_NIF_TERM rawinfo,
                                              ERL_NIF_TERM info)
{
    ERL_NIF_TERM keys[] = {MKA(env, "file"),
                           MKA(env, "function"),
                           MKA(env, "line"),
                           MKA(env, "raw_info"),
                           MKA(env, "info")};
    ERL_NIF_TERM vals[] = {MKS(env, file),
                           MKS(env, function),
                           MKI(env, line),
                           rawinfo,
                           info};
    unsigned int numKeys = NUM(keys);
    unsigned int numVals = NUM(vals);
    ERL_NIF_TERM map;

    ESOCK_ASSERT( numKeys == numVals );
    ESOCK_ASSERT( MKMA(env, keys, vals, numKeys, &map) );

    return map;
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



/* Create an error two (2) tuple in the form:
 *
 *          {error, closed}
 */
extern
ERL_NIF_TERM esock_make_error_closed(ErlNifEnv* env)
{
    return esock_make_error(env, esock_atom_closed);
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
 *          {error, {Tag, Reason}}
 *
 * Both 'Tag' and 'Reason' are already in the form of an
 * ERL_NIF_TERM so all we have to do is create "the" tuple.
 */
extern
ERL_NIF_TERM esock_make_error_t2r(ErlNifEnv*   env,
                                  ERL_NIF_TERM tag,
                                  ERL_NIF_TERM reason)
{
    return MKT2(env, esock_atom_error, MKT2(env, tag, reason));
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
#if 0
    fflush(stdout);
    fprintf(stderr, "%s:%d:%s() Assertion failed: %s\n",
            file, line, func, expr);
    fflush(stderr);
    abort();
#else
    erts_exit(ERTS_DUMP_EXIT, "%s:%d:%s() Assertion failed: %s\n",
              file, line, func, expr);
#endif
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



/*
 * We should really include self in the printout,
 * so we can se which process are executing the code.
 * But then I must change the API....something for later.
 *
 * esock_info_msg
 * esock_warning_msg
 * esock_error_msg
 */

#define MSG_FUNCS                            \
    MSG_FUNC_DECL(info,    INFO)             \
    MSG_FUNC_DECL(warning, WARNING)          \
    MSG_FUNC_DECL(error,   ERROR)

#define MSG_FUNC_DECL(FN, MC)                                  \
    extern                                                     \
    void esock_##FN##_msg( const char* format, ... )           \
    {                                                          \
       va_list         args;                                   \
       char            f[512 + sizeof(format)];                \
       char            stamp[64];                              \
       int             res;                                    \
                                                               \
       if (esock_timestamp_str(stamp, sizeof(stamp))) {        \
          res = enif_snprintf(f, sizeof(f),                    \
                              "=" #MC " MSG==== %s ===\r\n%s", \
                              stamp, format);                  \
       } else {                                                \
          res = enif_snprintf(f,                               \
                              sizeof(f),                       \
                              "=" #MC " MSG==== %s", format);  \
       }                                                       \
                                                               \
       if (res > 0) {                                          \
           va_start (args, format);                            \
           enif_vfprintf (stdout, f, args);                    \
           va_end (args);                                      \
           fflush(stdout);                                     \
       }                                                       \
                                                               \
       return;                                                 \
    }                                                          \

MSG_FUNCS
#undef MSG_FUNC_DECL
#undef MSG_FUNCS


/* *** esock_timestamp ***
 *
 * Create a timestamp.
 * Produces a timestamp in the form of an "Epoch" (A real epoch
 * is the number of seconds since 1/1 1970, but our timestamp is
 * the number micro seconds since 1/1 1970).
 */

extern
ErlNifTime esock_timestamp(void)
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


/* Construct the Link-Level socket address */
#if defined(HAVE_NET_IF_DL_H) && defined(AF_LINK)
static
void make_sockaddr_dl(ErlNifEnv*    env,
                      ERL_NIF_TERM  index,
                      ERL_NIF_TERM  type,
                      ERL_NIF_TERM  nlen,
                      ERL_NIF_TERM  alen,
                      ERL_NIF_TERM  slen,
                      ERL_NIF_TERM  data,
                      ERL_NIF_TERM* sa)
{
    ERL_NIF_TERM keys[]  = {esock_atom_family,
        esock_atom_index,
        esock_atom_type,
        esock_atom_nlen,
        esock_atom_alen,
        esock_atom_slen,
        esock_atom_data};
    ERL_NIF_TERM vals[]  = {esock_atom_link,
        index,
        type,
        nlen,
        alen,
        slen,
        data};
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

#endif
