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
 *  Purpose : Utility functions for the socket and net NIF(s).
 * ----------------------------------------------------------------------
 *
 */

#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <stddef.h>

#include "socket_int.h"
#include "sys.h"
#include "socket_util.h"
#include "socket_dbg.h"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if defined(HAVE_SCTP_H)
#include <netinet/sctp.h>
#ifndef     HAVE_SCTP
#    define HAVE_SCTP
#endif
#endif

#ifdef HAVE_SOCKLEN_T
#  define SOCKLEN_T socklen_t
#else
#  define SOCKLEN_T size_t
#endif

#ifdef __WIN32__
#define SOCKOPTLEN_T int
#else
#define SOCKOPTLEN_T SOCKLEN_T
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
    

static void esock_encode_packet_addr_tuple(ErlNifEnv*     env,
                                           unsigned char  len,
                                           unsigned char* addr,
                                           ERL_NIF_TERM*  eAddr);

static char* make_sockaddr_in4(ErlNifEnv*    env,
                               ERL_NIF_TERM  port,
                               ERL_NIF_TERM  addr,
                               ERL_NIF_TERM* sa);
static char* make_sockaddr_in6(ErlNifEnv*    env,
                               ERL_NIF_TERM  port,
                               ERL_NIF_TERM  addr,
                               ERL_NIF_TERM  flowInfo,
                               ERL_NIF_TERM  scopeId,
                               ERL_NIF_TERM* sa);
static char* make_sockaddr_un(ErlNifEnv*    env,
                              ERL_NIF_TERM  path,
                              ERL_NIF_TERM* sa);
#if defined(HAVE_NETPACKET_PACKET_H)
static char* make_sockaddr_ll(ErlNifEnv*    env,
                              ERL_NIF_TERM  proto,
                              ERL_NIF_TERM  ifindex,
                              ERL_NIF_TERM  hatype,
                              ERL_NIF_TERM  pkttype,
                              ERL_NIF_TERM  addr,
                              ERL_NIF_TERM* sa);
#endif
static BOOLEAN_T esock_extract_from_map(ErlNifEnv*    env,
                                        ERL_NIF_TERM  map,
                                        ERL_NIF_TERM  key,
                                        ERL_NIF_TERM* val);

/* +++ esock_encode_iov +++
 *
 * Encode an IO Vector. In erlang we represented this as a list of binaries.
 *
 * We iterate through the IO vector, and as long as the remaining (rem)
 * number of bytes is greater than the size of the current buffer, we
 * contunue. When we have a buffer that is greater than rem, we have found
 * the last buffer (it may be empty, and then the previous was last). 
 * We may need to split this (if 0 < rem < bufferSz).
 */

extern
char* esock_encode_iov(ErlNifEnv*    env,
                       int           read,
                       struct iovec* iov,
                       size_t        len,
                       ErlNifBinary* data,
                       ERL_NIF_TERM* eIOV)
{
    int          rem = read;
    Uint16       i;
    BOOLEAN_T    done = FALSE;
    ERL_NIF_TERM a[len]; // At most this length

    UDBG( ("SUTIL", "esock_encode_iov -> entry with"
           "\r\n   read:      %d"
           "\r\n   (IOV) len: %d"
           "\r\n", read, len) );

    if (len == 0) {
        *eIOV = MKEL(env);
        return NULL;
    }

    for (i = 0; (!done) && (i < len); i++) {
        UDBG( ("SUTIL", "esock_encode_iov -> process iov:"
               "\r\n   iov[%d].iov_len: %d"
               "\r\n   rem:            %d"
               "\r\n", i, iov[i].iov_len, rem) );
        if (iov[i].iov_len == rem) {
            /* We have the exact amount - we are done */
            UDBG( ("SUTIL", "esock_encode_iov -> exact => done\r\n") );
            a[i] = MKBIN(env, &data[i]);
            rem  = 0; // Besserwisser
            done = TRUE;
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
            done = TRUE;
        }
    }

    UDBG( ("SUTIL", "esock_encode_iov -> create the IOV list (%d)\r\n", i) );

    *eIOV = MKLA(env, a, i);

    UDBG( ("SUTIL", "esock_encode_msghdr -> done\r\n") );

    return NULL;
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
char* esock_decode_iov(ErlNifEnv*    env,
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

        if (!GET_LIST_ELEM(env, list, &elem, &tail))
            return ESOCK_STR_EINVAL;

        if (IS_BIN(env, elem) && GET_BIN(env, elem, &bufs[i])) {
            iov[i].iov_base  = (caddr_t) bufs[i].data;
            iov[i].iov_len   = bufs[i].size;
            sz              += bufs[i].size;
        } else {
            return ESOCK_STR_EINVAL;
        }

        list = tail;
    }

    *totSize = sz;

    UDBG( ("SUTIL", "esock_decode_msghdr -> done (%d)\r\n", sz) );

    return NULL;
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
 */

extern
char* esock_decode_sockaddr(ErlNifEnv*    env,
                            ERL_NIF_TERM  eSockAddr,
                            ESockAddress* sockAddrP,
                            unsigned int* addrLen)
{
    ERL_NIF_TERM efam;
    int          fam;
    char*        xres;

    UDBG( ("SUTIL", "esock_decode_sockaddr -> entry\r\n") );

    if (!IS_MAP(env, eSockAddr))
        return ESOCK_STR_EINVAL;

    if (!GET_MAP_VAL(env, eSockAddr, esock_atom_family, &efam))
        return ESOCK_STR_EINVAL;

    UDBG( ("SUTIL", "esock_decode_sockaddr -> try decode domain (%T)\r\n", efam) );
    if ((xres = esock_decode_domain(env, efam, &fam)) != NULL)
        return xres;

    UDBG( ("SUTIL", "esock_decode_sockaddr -> fam: %d\r\n", fam) );
    switch (fam) {
    case AF_INET:
        xres = esock_decode_sockaddr_in4(env, eSockAddr,
                                         &sockAddrP->in4, addrLen);
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        xres = esock_decode_sockaddr_in6(env, eSockAddr,
                                         &sockAddrP->in6, addrLen);
        break;
#endif

#if defined(HAVE_SYS_UN_H)
    case AF_UNIX:
        xres = esock_decode_sockaddr_un(env, eSockAddr,
                                        &sockAddrP->un, addrLen);
        break;
#endif

    default:
        xres = ESOCK_STR_EAFNOSUPPORT;
        break;

    }

    return xres;
}



/* +++ esock_encode_sockaddr +++
 *
 * Encode a socket address - sockaddr. In erlang its represented as
 * a map, which has a specific set of attributes, depending on one
 * mandatory attribute; family. So depending on the value of the family
 * attribute: 
 *
 *    local - sockaddr_un:  path
 *    inet  - sockaddr_in4: port, addr
 *    inet6 - sockaddr_in6: port, addr, flowinfo, scope_id
 */

extern
char* esock_encode_sockaddr(ErlNifEnv*    env,
                            ESockAddress* sockAddrP,
                            unsigned int  addrLen,
                            ERL_NIF_TERM* eSockAddr)
{
    char* xres;

    UDBG( ("SUTIL", "esock_encode_sockaddr -> entry with"
           "\r\n   family:  %d"
           "\r\n   addrLen: %d"
           "\r\n", sockAddrP->sa.sa_family, addrLen) );

    switch (sockAddrP->sa.sa_family) {
    case AF_INET:
        xres = esock_encode_sockaddr_in4(env, &sockAddrP->in4, addrLen, eSockAddr);
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        xres = esock_encode_sockaddr_in6(env, &sockAddrP->in6, addrLen, eSockAddr);
        break;
#endif

#if defined(HAVE_SYS_UN_H)
    case AF_UNIX:
        xres = esock_encode_sockaddr_un(env, &sockAddrP->un, addrLen, eSockAddr);
        break;
#endif

#if defined(HAVE_NETPACKET_PACKET_H)
    case AF_PACKET:
        xres = esock_encode_sockaddr_ll(env, &sockAddrP->ll, addrLen, eSockAddr);
        break;
#endif

    default:
        *eSockAddr = esock_atom_undefined;
        xres       = ESOCK_STR_EAFNOSUPPORT;
        break;

    }

    return xres;
}



/* +++ esock_decode_sockaddr_in4 +++
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
char* esock_decode_sockaddr_in4(ErlNifEnv*          env,
                                ERL_NIF_TERM        eSockAddr,
                                struct sockaddr_in* sockAddrP,
                                unsigned int*       addrLen)
{
    ERL_NIF_TERM eport, eaddr;
    int          port;
    char*        xres;

    UDBG( ("SUTIL", "esock_decode_sockaddr_in4 -> entry\r\n") );

    /* Basic init */
    sys_memzero((char*) sockAddrP, sizeof(struct sockaddr_in));

#ifndef NO_SA_LEN
    sockAddrP->sin_len = sizeof(struct sockaddr_in);
#endif

    sockAddrP->sin_family = AF_INET;

    /* Extract (e) port number from map */
    UDBG( ("SUTIL", "esock_decode_sockaddr_in4 -> try get port number\r\n") );
    if (!GET_MAP_VAL(env, eSockAddr, esock_atom_port, &eport))
        return ESOCK_STR_EINVAL;

    /* Decode port number */
    UDBG( ("SUTIL", "esock_decode_sockaddr_in4 -> try decode port number\r\n") );
    if (!GET_INT(env, eport, &port))
        return ESOCK_STR_EINVAL;
    
    sockAddrP->sin_port = htons(port);

    /* Extract (e) address from map */
    UDBG( ("SUTIL", "esock_decode_sockaddr_in4 -> try get (ip) address\r\n") );
    if (!GET_MAP_VAL(env, eSockAddr, esock_atom_addr, &eaddr))
        return ESOCK_STR_EINVAL;

    /* Decode address */
    UDBG( ("SUTIL", "esock_decode_sockaddr_in4 -> try decode (ip) address\r\n") );
    if ((xres = esock_decode_ip4_address(env,
                                         eaddr,
                                         &sockAddrP->sin_addr)) != NULL)
        return xres;

    *addrLen = sizeof(struct sockaddr_in);
    
    UDBG( ("SUTIL", "esock_decode_sockaddr_in4 -> done\r\n") );

    return NULL;
}



/* +++ esock_encode_sockaddr_in4 +++
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
char* esock_encode_sockaddr_in4(ErlNifEnv*          env,
                                struct sockaddr_in* sockAddrP,
                                unsigned int        addrLen,
                                ERL_NIF_TERM*       eSockAddr)
{
    ERL_NIF_TERM ePort, eAddr;
    int          port;
    char*        xres = NULL;

    UDBG( ("SUTIL", "esock_encode_sockaddr_in4 -> entry\r\n") );

    if (addrLen >= sizeof(struct sockaddr_in)) {
        /* The port */
        port  = ntohs(sockAddrP->sin_port);
        ePort = MKI(env, port);

        /* The address */
        if ((xres = esock_encode_ip4_address(env, &sockAddrP->sin_addr,
                                             &eAddr)) == NULL) {
            /* And finally construct the in4_sockaddr record */
            xres = make_sockaddr_in4(env, ePort, eAddr, eSockAddr);
        } else {
            UDBG( ("SUTIL", "esock_encode_sockaddr_in4 -> "
                   "failed encoding (ip) address: "
                   "\r\n   xres: %s"
                   "\r\n", xres) );
            *eSockAddr = esock_atom_undefined;
            xres       = ESOCK_STR_EINVAL;
        }

    } else {
        UDBG( ("SUTIL", "esock_encode_sockaddr_in4 -> wrong size: "
               "\r\n   addrLen:   %d"
               "\r\n   addr size: %d"
               "\r\n", addrLen, sizeof(struct sockaddr_in)) );
        *eSockAddr = esock_atom_undefined;
        xres       = ESOCK_STR_EINVAL;
    }

    return xres;
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
char* esock_decode_sockaddr_in6(ErlNifEnv*           env,
                                ERL_NIF_TERM         eSockAddr,
                                struct sockaddr_in6* sockAddrP,
                                unsigned int*        addrLen)
{
    ERL_NIF_TERM eport, eaddr, eflowInfo, escopeId;
    int          port;
    unsigned int flowInfo, scopeId;
    char*        xres;

    UDBG( ("SUTIL", "esock_decode_sockaddr_in6 -> entry\r\n") );

    /* Basic init */
    sys_memzero((char*) sockAddrP, sizeof(struct sockaddr_in6));
#ifndef NO_SA_LEN
    sockAddrP->sin6_len = sizeof(struct sockaddr_in);
#endif

    sockAddrP->sin6_family = AF_INET6;

    /* *** Extract (e) port number from map *** */
    if (!GET_MAP_VAL(env, eSockAddr, esock_atom_port, &eport))
        return ESOCK_STR_EINVAL;

    /* Decode port number */
    if (!GET_INT(env, eport, &port))
        return ESOCK_STR_EINVAL;

    UDBG( ("SUTIL", "esock_decode_sockaddr_in6 -> port: %d\r\n", port) );

    sockAddrP->sin6_port = htons(port);

    /* *** Extract (e) flowinfo from map *** */
    if (!GET_MAP_VAL(env, eSockAddr, esock_atom_flowinfo, &eflowInfo))
        return ESOCK_STR_EINVAL;

    /* 4: Get the flowinfo */
    if (!GET_UINT(env, eflowInfo, &flowInfo))
        return ESOCK_STR_EINVAL;

    UDBG( ("SUTIL", "esock_decode_sockaddr_in6 -> flowinfo: %d\r\n", flowInfo) );

    sockAddrP->sin6_flowinfo = flowInfo;
    
    /* *** Extract (e) scope_id from map *** */
    if (!GET_MAP_VAL(env, eSockAddr, esock_atom_scope_id, &escopeId))
        return ESOCK_STR_EINVAL;

    /* *** Get the scope_id *** */
    if (!GET_UINT(env, escopeId, &scopeId))
        return ESOCK_STR_EINVAL;

    UDBG( ("SUTIL", "esock_decode_sockaddr_in6 -> scopeId: %d\r\n", scopeId) );

    sockAddrP->sin6_scope_id = scopeId;

    /* *** Extract (e) address from map *** */
    if (!GET_MAP_VAL(env, eSockAddr, esock_atom_addr, &eaddr))
        return ESOCK_STR_EINVAL;

    /* Decode address */
    if ((xres = esock_decode_ip6_address(env,
                                         eaddr,
                                         &sockAddrP->sin6_addr)) != NULL)
        return xres;

    *addrLen = sizeof(struct sockaddr_in6);

    UDBG( ("SUTIL", "esock_decode_sockaddr_in6 -> done\r\n") );

    return NULL;
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
char* esock_encode_sockaddr_in6(ErlNifEnv*           env,
                                struct sockaddr_in6* sockAddrP,
                                unsigned int         addrLen,
                                ERL_NIF_TERM*        eSockAddr)
{
    ERL_NIF_TERM ePort, eAddr, eFlowInfo, eScopeId;
    char*        xres;

    if (addrLen >= sizeof(struct sockaddr_in6)) {
        /* The port */
        ePort = MKI(env, ntohs(sockAddrP->sin6_port));

        /* The flowInfo */
        eFlowInfo = MKI(env, sockAddrP->sin6_flowinfo);
            
        /* The scopeId */
        eScopeId = MKI(env, sockAddrP->sin6_scope_id);
        
        /* The address */
        if ((xres = esock_encode_ip6_address(env, &sockAddrP->sin6_addr,
                                             &eAddr)) == NULL) {
            /* And finally construct the in6_sockaddr record */
            xres = make_sockaddr_in6(env,
                                     ePort, eAddr, eFlowInfo, eScopeId, eSockAddr);
        } else {
            *eSockAddr = esock_atom_undefined;
            xres       = ESOCK_STR_EINVAL;
        }

    } else {
        *eSockAddr = esock_atom_undefined;
        xres       = ESOCK_STR_EINVAL;
    }

    return xres;
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

#if defined(HAVE_SYS_UN_H)
extern
char* esock_decode_sockaddr_un(ErlNifEnv*          env,
                               ERL_NIF_TERM        eSockAddr,
                               struct sockaddr_un* sockAddrP,
                               unsigned int*       addrLen)
{
    ErlNifBinary bin;
    ERL_NIF_TERM epath;
    unsigned int len;

    /* *** Extract (e) path (a binary) from map *** */
    if (!GET_MAP_VAL(env, eSockAddr, esock_atom_path, &epath))
        return ESOCK_STR_EINVAL;

    /* Get the path */
    if (!GET_BIN(env, epath, &bin))
        return ESOCK_STR_EINVAL;
    
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
        return ESOCK_STR_EINVAL;


    sys_memzero((char*) sockAddrP, sizeof(struct sockaddr_un));
    sockAddrP->sun_family = AF_UNIX;

    sys_memcpy(sockAddrP->sun_path, bin.data, bin.size);
    len = offsetof(struct sockaddr_un, sun_path) + bin.size;

#ifndef NO_SA_LEN
    sockAddrP->sun_len = len;
#endif
    *addrLen = len;

    return NULL;
}
#endif



/* +++ esock_encode_sockaddr_un +++
 *
 * Encode a Unix Domain socket address - sockaddr_un. In erlang its 
 * represented as a map, which has a specific set of attributes
 * (beside the mandatory family attribute, which is "inherited" from
 * the "sockaddr" type):
 *
 *    path :: binary()
 *
 */

#if defined(HAVE_SYS_UN_H)
extern
char* esock_encode_sockaddr_un(ErlNifEnv*          env,
                               struct sockaddr_un* sockAddrP,
                               unsigned int        addrLen,
                               ERL_NIF_TERM*       eSockAddr)
{
    ERL_NIF_TERM ePath;
    size_t       n, m;
    char*        xres;

    if (addrLen >= offsetof(struct sockaddr_un, sun_path)) {
        n = addrLen - offsetof(struct sockaddr_un, sun_path);
        if (255 < n) {
            *eSockAddr = esock_atom_undefined;
            xres       = ESOCK_STR_EINVAL;
        } else {
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
            ePath      = MKSL(env, sockAddrP->sun_path, m);

            /* And the socket address */
            xres = make_sockaddr_un(env, ePath, eSockAddr);
        }
    } else {
        *eSockAddr = esock_atom_undefined;
        xres       = ESOCK_STR_EINVAL;
    }

    return xres;
}
#endif



/* +++ esock_encode_sockaddr_ll +++
 *
 * Encode a PACKET address - sockaddr_ll (link layer). In erlang its 
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
char* esock_encode_sockaddr_ll(ErlNifEnv*          env,
                               struct sockaddr_ll* sockAddrP,
                               unsigned int        addrLen,
                               ERL_NIF_TERM*       eSockAddr)
{
    ERL_NIF_TERM eProto, eIfIdx, eHaType, ePktType, eAddr;
    char*        xres;

    if (addrLen >= sizeof(struct sockaddr_ll)) {

        /* protocol - the standard ethernet protocol type */
        esock_encode_packet_protocol(env, ntohs(sockAddrP->sll_protocol), &eProto);

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

        xres = make_sockaddr_ll(env,
                                eProto, eIfIdx, eHaType, ePktType, eAddr,
                                eSockAddr);

    } else {
        *eSockAddr = esock_atom_undefined;
        xres       = ESOCK_STR_EINVAL;
    }

    return xres;
}
#endif



/* +++ esock_decode_ip4_address +++
 *
 * Decode a IPv4 address. This can be three things:
 *
 *    + Then atom 'any'
 *    + Then atom 'loopback'
 *    + An ip4_address() (4 tuple)
 *
 * Note that this *only* decodes the "address" part of a
 * (IPv4) socket address.
 */

extern
char* esock_decode_ip4_address(ErlNifEnv*      env,
                               ERL_NIF_TERM    eAddr,
                               struct in_addr* inAddrP)
{
    struct in_addr addr;

    UDBG( ("SUTIL", "esock_decode_ip4_address -> entry with"
           "\r\n   eAddr: %T"
           "\r\n", eAddr) );

    if (IS_ATOM(env, eAddr)) {

        /* This is either 'any' | 'broadcast' | 'loopback' */

        if (COMPARE(esock_atom_loopback, eAddr) == 0) {
            UDBG( ("SUTIL",
		   "esock_decode_ip4_address -> address: lookback\r\n") );
            addr.s_addr = htonl(INADDR_LOOPBACK);
        } else if (COMPARE(esock_atom_any, eAddr) == 0) {
            UDBG( ("SUTIL",
                   "esock_decode_ip4_address -> address: any\r\n") );
            addr.s_addr = htonl(INADDR_ANY);
        } else if (COMPARE(esock_atom_broadcast, eAddr) == 0) {
            UDBG( ("SUTIL",
                   "esock_decode_ip4_address -> address: broadcast\r\n") );
            addr.s_addr = htonl(INADDR_BROADCAST);
        } else {
            UDBG( ("SUTIL",
		   "esock_decode_ip4_address -> address: unknown\r\n") );
            return ESOCK_STR_EINVAL;
        }

        inAddrP->s_addr = addr.s_addr;

    } else {
        /* This is a 4-tuple */

        const ERL_NIF_TERM* addrt;
        int                 addrtSz;
        int                 a, v;
        char                addr[4];
        
        if (!GET_TUPLE(env, eAddr, &addrtSz, &addrt))
            return ESOCK_STR_EINVAL;
        
        if (addrtSz != 4)
            return ESOCK_STR_EINVAL;

        for (a = 0; a < 4; a++) {
            if (!GET_INT(env, addrt[a], &v))
                return ESOCK_STR_EINVAL;
            addr[a] = v;
        }

        sys_memcpy(inAddrP, &addr, sizeof(addr));
        
    }

    return NULL;
}



/* +++ esock_encode_ip4_address +++
 *
 * Encode a IPv4 address:
 *
 *    + An ip4_address() (4 tuple)
 *
 * Note that this *only* decodes the "address" part of a
 * (IPv4) socket address. There are several other things (port).
 */

extern
char* esock_encode_ip4_address(ErlNifEnv*      env,
                               struct in_addr* addrP,
                               ERL_NIF_TERM*   eAddr)
{
    unsigned int   i;
    ERL_NIF_TERM   at[4];
    unsigned int   atLen = sizeof(at) / sizeof(ERL_NIF_TERM);
    unsigned char* a     = (unsigned char*) addrP;
    ERL_NIF_TERM   addr;
    
    /* The address */
    for (i = 0; i < atLen; i++) {
        at[i] = MKI(env, a[i]);
    }

    addr = MKTA(env, at, atLen);
    UDBG( ("SUTIL", "esock_encode_ip4_address -> addr: %T\r\n", addr) );
    // *eAddr = MKTA(env, at, atLen);
    *eAddr = addr;
    
    return NULL;
}



/* +++ esock_decode_ip6_address +++
 *
 * Decode a IPv6 address. This can be three things:
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
char* esock_decode_ip6_address(ErlNifEnv*       env,
                               ERL_NIF_TERM     eAddr,
                               struct in6_addr* inAddrP)
{
    UDBG( ("SUTIL", "esock_decode_ip6_address -> entry with"
           "\r\n   eAddr: %T"
           "\r\n", eAddr) );

    if (IS_ATOM(env, eAddr)) {
        /* This is either 'any' or 'loopback' */
        const struct in6_addr* addr;

        if (COMPARE(esock_atom_loopback, eAddr) == 0) {
            addr = &in6addr_loopback;
        } else if (COMPARE(esock_atom_any, eAddr) == 0) {
            addr = &in6addr_any;
        } else {
            return ESOCK_STR_EINVAL;
        }
        
        *inAddrP = *addr;
        
    } else {
        /* This is a 8-tuple */
        
        const ERL_NIF_TERM* addrt;
        int                 addrtSz;
        int                 ai, v;
        unsigned char       addr[16];
        unsigned char*      a = addr;
        unsigned int        addrLen = sizeof(addr) / sizeof(unsigned char);
        
        if (!GET_TUPLE(env, eAddr, &addrtSz, &addrt))
            return ESOCK_STR_EINVAL;
        
        if (addrtSz != 8)
            return ESOCK_STR_EINVAL;
        
        for (ai = 0; ai < 8; ai++) {
            if (!GET_INT(env, addrt[ai], &v))
                return ESOCK_STR_EINVAL;
            put_int16(v, a);
            a += 2;
        }
        
        sys_memcpy(inAddrP, &addr, addrLen);

    }

    return NULL;
}
#endif



/* +++ esock_encode_ip6_address +++
 *
 * Encode a IPv6 address:
 *
 *    + An ip6_address() (8 tuple)
 *
 * Note that this *only* encodes the "address" part of a
 * (IPv6) socket address. There are several other things
 * (port, flowinfo and scope_id) that are handled elsewhere).
 */

#if defined(HAVE_IN6) && defined(AF_INET6)
extern
char* esock_encode_ip6_address(ErlNifEnv*       env,
                               struct in6_addr* addrP,
                               ERL_NIF_TERM*    eAddr)
{
    unsigned int   i;
    ERL_NIF_TERM   at[8];
    unsigned int   atLen = sizeof(at) / sizeof(ERL_NIF_TERM);
    unsigned char* a     = (unsigned char*) addrP;
    
    /* The address */
    for (i = 0; i < atLen; i++) {
        at[i] = MKI(env, get_int16(a + i*2));
    }

    *eAddr = MKTA(env, at, atLen);
    
    return NULL;
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
char* esock_encode_timeval(ErlNifEnv*      env,
                           struct timeval* timeP,
                           ERL_NIF_TERM*   eTime)
{
    ERL_NIF_TERM keys[] = {esock_atom_sec, esock_atom_usec};
    ERL_NIF_TERM vals[] = {MKL(env, timeP->tv_sec), MKL(env, timeP->tv_usec)};
    
    unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);
    unsigned int numVals = sizeof(vals) / sizeof(ERL_NIF_TERM);
    
    ESOCK_ASSERT( (numKeys == numVals) );
    
    if (!MKMA(env, keys, vals, numKeys, eTime))
        return ESOCK_STR_EINVAL;

    return NULL;
}



/* +++ esock_decode_timeval +++
 *
 * Decode a timeval in its erlang form (a map) into its native form,
 * a timeval struct.
 *
 */
extern
char* esock_decode_timeval(ErlNifEnv*      env,
                           ERL_NIF_TERM    eTime,
                           struct timeval* timeP)
{
    ERL_NIF_TERM eSec, eUSec;
    size_t       sz;

    // It must be a map
    if (!IS_MAP(env, eTime))
        return ESOCK_STR_EINVAL;

    // It must have atleast two attributes
    if (!enif_get_map_size(env, eTime, &sz) || (sz < 2))
        return ESOCK_STR_EINVAL;

    if (!GET_MAP_VAL(env, eTime, esock_atom_sec, &eSec))
        return ESOCK_STR_EINVAL;

    if (!GET_MAP_VAL(env, eTime, esock_atom_usec, &eUSec))
        return ESOCK_STR_EINVAL;

    /* Use the appropriate variable type and nif function
     * to decode the value from Erlang into the struct timeval fields
     */
    { /* time_t tv_sec; */
#if (SIZEOF_TIME_T == 8)
        ErlNifSInt64 sec;
        if (!GET_INT64(env, eSec, &sec))
            return ESOCK_STR_EINVAL;
#elif (SIZEOF_TIME_T == SIZEOF_INT)
        int sec;
        if (!GET_INT(env, eSec, &sec))
            return ESOCK_STR_EINVAL;
#else /* long or other e.g undefined */
        long sec;
        if (!GET_LONG(env, eSec, &sec))
            return ESOCK_STR_EINVAL;
#endif
        timeP->tv_sec = sec;
    }

    { /* suseconds_t tv_usec; */
#if (SIZEOF_SUSECONDS_T == 8)
        ErlNifSInt64 usec;
        if (!GET_INT64(env, eSec, &usec))
            return ESOCK_STR_EINVAL;
#elif (SIZEOF_SUSECONDS_T == SIZEOF_INT)
        int usec;
        if (!GET_INT(env, eSec, &usec))
            return ESOCK_STR_EINVAL;
#else /* long or other e.g undefined */
        long usec;
        if (!GET_LONG(env, eSec, &usec))
            return ESOCK_STR_EINVAL;
#endif
        timeP->tv_usec = usec;
    }

    return NULL;
}



/* +++ esock_decode_domain +++
 *
 * Decode the Erlang form of the 'domain' type, that is: 
 * 
 *    inet  => AF_INET
 *    inet6 => AF_INET6
 *    local => AF_UNIX
 *
 */
extern
char* esock_decode_domain(ErlNifEnv*   env,
                          ERL_NIF_TERM eDomain,
                          int*         domain)
{
    char* xres = NULL;

    if (COMPARE(esock_atom_inet, eDomain) == 0) {
        *domain = AF_INET;

#if defined(HAVE_IN6) && defined(AF_INET6)
    } else if (COMPARE(esock_atom_inet6, eDomain) == 0) {
        *domain = AF_INET6;
#endif

#if defined(HAVE_SYS_UN_H)
    } else if (COMPARE(esock_atom_local, eDomain) == 0) {
        *domain = AF_UNIX;
#endif

    } else {
        *domain = -1;
        xres    = ESOCK_STR_EAFNOSUPPORT;
    }

    return xres;
}



/* +++ esock_encode_domain +++
 *
 * Encode the native domain to the Erlang form, that is: 
 * 
 *    AF_INET  => inet
 *    AF_INET6 => inet6
 *    AF_UNIX  => local
 *
 */
extern
char* esock_encode_domain(ErlNifEnv*    env,
                          int           domain,
                          ERL_NIF_TERM* eDomain)
{
    char* xres = NULL;

    switch (domain) {
    case AF_INET:
        *eDomain = esock_atom_inet;
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        *eDomain = esock_atom_inet6;
        break;
#endif

#if defined(HAVE_SYS_UN_H)
    case AF_UNIX:
        *eDomain = esock_atom_local;
        break;
#endif

    default:
        *eDomain = esock_atom_undefined; // Just in case
        xres     = ESOCK_STR_EAFNOSUPPORT;
    }

    return xres;
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
char* esock_decode_type(ErlNifEnv*   env,
                        ERL_NIF_TERM eType,
                        int*         type)
{
    char* xres = NULL;

    if (COMPARE(esock_atom_stream, eType) == 0) {
        *type = SOCK_STREAM;
    } else if (COMPARE(esock_atom_dgram, eType) == 0) {
        *type = SOCK_DGRAM;
    } else if (COMPARE(esock_atom_raw, eType) == 0) {
        *type = SOCK_RAW;

#if defined(HAVE_SCTP)
    } else if (COMPARE(esock_atom_seqpacket, eType) == 0) {
        *type = SOCK_SEQPACKET;
#endif

    } else {
        *type = -1;
        xres  = ESOCK_STR_EAFNOSUPPORT;
    }

    return xres;
}



/* +++ esock_decode_type +++
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
char* esock_encode_type(ErlNifEnv*    env,
                        int           type,
                        ERL_NIF_TERM* eType)
{
    char* xres = NULL;

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

#if defined(HAVE_SCTP)
    case SOCK_SEQPACKET:
        *eType = esock_atom_seqpacket;
        break;
#endif

    default:
        *eType = esock_atom_undefined; // Just in case
        xres   = ESOCK_STR_EAFNOSUPPORT;
    }

    return xres;
}



/* +++ esock_encode_protocol +++
 *
 * Encode the native protocol to the Erlang form, that is: 
 * 
 *    SOL_IP | IPPROTO_IP => ip
 *    SOL_IPV6            => ipv6
 *    SOL_TCP             => tcp
 *    SOL_UDP             => udp
 *    SOL_SCTP            => sctp
 *
 */
extern
char* esock_encode_protocol(ErlNifEnv*    env,
                            int           proto,
                            ERL_NIF_TERM* eProto)
{
    char* xres = NULL;

    switch (proto) {
#if defined(SOL_IP)
    case SOL_IP:
#else
    case IPPROTO_IP:
#endif
        *eProto = esock_atom_ip;
        break;

#if defined(SOL_IPV6)
    case SOL_IPV6:
        *eProto = esock_atom_ipv6;
        break;
#endif

    case IPPROTO_TCP:
        *eProto = esock_atom_tcp;
        break;

    case IPPROTO_UDP:
        *eProto = esock_atom_udp;
        break;

#if defined(HAVE_SCTP)
    case IPPROTO_SCTP:
        *eProto = esock_atom_sctp;
        break;
#endif

    default:
        *eProto = esock_atom_undefined;
        xres    = ESOCK_STR_EAFNOSUPPORT;
        break;
    }

    return xres;
}



/* +++ esock_decode_protocol +++
 *
 * Decode the Erlang form of the 'protocol' type, that is: 
 * 
 *    ip   => SOL_IP | IPPROTO_IP
 *    ipv6 => SOL_IPV6
 *    tcp  => SOL_TCP
 *    udp  => SOL_UDP
 *    sctp => SOL_SCTP
 *
 */
extern
char* esock_decode_protocol(ErlNifEnv*   env,
                            ERL_NIF_TERM eProto,
                            int*         proto)
{
    char* xres = NULL;

    if (COMPARE(esock_atom_ip, eProto) == 0) {
#if defined(SOL_IP)
        *proto = SOL_IP;
#else
        *proto = IPPROTO_IP;
#endif
    } else if (COMPARE(esock_atom_ipv6, eProto) == 0) {
#if defined(SOL_IPV6)
        *proto = SOL_IPV6;
#else
        *proto = IPPROTO_IPV6;
#endif
    } else if (COMPARE(esock_atom_tcp, eProto) == 0) {
        *proto = IPPROTO_TCP;
    } else if (COMPARE(esock_atom_udp, eProto) == 0) {
        *proto = IPPROTO_UDP;
#if defined(HAVE_SCTP)
    } else if (COMPARE(esock_atom_sctp, eProto) == 0) {
        *proto = IPPROTO_SCTP;
#endif
    } else {
        *proto = -1;
        xres   = ESOCK_STR_EAFNOSUPPORT;
    }

    return xres;
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

#if defined(PACKET_FASTROUTE)
    case PACKET_FASTROUTE:
        *ePktType = esock_atom_fastroute;
        break;
#endif

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



/* +++ esock_decode_bufsz +++
 *
 * Decode an buffer size. The size of a buffer is: 
 * 
 *    Sz > 0 => Use provided value
 *    Sz     => Use provided default
 *
 */
extern
char* esock_decode_bufsz(ErlNifEnv*   env,
                         ERL_NIF_TERM eVal,
                         size_t       defSz,
                         size_t*      sz)
{
    int val;

    if (!GET_INT(env, eVal, &val))
        return ESOCK_STR_EINVAL;

    if (val > 0)
        *sz = (size_t) val;
    else
        *sz = defSz;

    return NULL;
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



/* *** esock_extract_bool_from_map ***
 *
 * Extract an boolean item from a map.
 * This function returns the retreived or the provided default value.
 */
extern
BOOLEAN_T esock_extract_bool_from_map(ErlNifEnv*   env,
                                      ERL_NIF_TERM map,
                                      ERL_NIF_TERM key,
                                      BOOLEAN_T    def)
{
    ERL_NIF_TERM val;

    if (!esock_extract_from_map(env, map, key, &val))
        return def;

    if (!IS_ATOM(env, val))
        return def;

    if (COMPARE(val, esock_atom_true) == 0)
        return TRUE;
    else
        return FALSE;
}



/* *** esock_extract_pid_from_map ***
 *
 * Extract a pid item from a map.
 * Returns TRUE on success and FALSE on failure (and then 
 * the pid value will be set to 'undefined').
 *
 */
extern
BOOLEAN_T esock_extract_pid_from_map(ErlNifEnv*   env,
                                     ERL_NIF_TERM map,
                                     ERL_NIF_TERM key,
                                     ErlNifPid*   pid)
{
    BOOLEAN_T    res;
    ERL_NIF_TERM val;

    if (!esock_extract_from_map(env, map, key, &val)) {
        enif_set_pid_undefined(pid);
        res = FALSE;
    } else {
        if (enif_get_local_pid(env, val, pid)) {
            res = TRUE;
        } else {
            enif_set_pid_undefined(pid);
            res = FALSE;
        }
    }

    return res;
}



/* *** esock_extract_from_map ***
 *
 * Extract a value from a map.
 * Returns true on success and false on failure.
 *
 */
static
BOOLEAN_T esock_extract_from_map(ErlNifEnv*    env,
                                 ERL_NIF_TERM  map,
                                 ERL_NIF_TERM  key,
                                 ERL_NIF_TERM* val)
{
    if (!GET_MAP_VAL(env, map, key, val))
        return FALSE;
    else
        return TRUE;
}



/* *** esock_decode_bool ***
 *
 * Decode a boolean value.
 *
 */
extern
BOOLEAN_T esock_decode_bool(ERL_NIF_TERM val)
{
    if (COMPARE(esock_atom_true, val) == 0) 
        return TRUE;
    else
        return FALSE;
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


/* Create an ok three (3) tuple in the form:
 *
 *         {ok, Val1, Val2}
 *
 * The second (Val1) and third (Val2) elements are already in
 * the form of an ERL_NIF_TERM so all we have to do is create
 * the tuple.
 */
extern
ERL_NIF_TERM esock_make_ok3(ErlNifEnv* env, ERL_NIF_TERM val1, ERL_NIF_TERM val2)
{
    return MKT3(env, esock_atom_ok, val1, val2);
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
void esock_abort(const char* expr,
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
    int       ret;
#if defined(ESOCK_USE_PRETTY_TIMESTAMP)

    time_t    sec     = timestamp / 1000000; // (if _MSEC) sec  = time / 1000;
    time_t    usec    = timestamp % 1000000; // (if _MSEC) msec = time % 1000;
    int       buflen;
    struct tm t;

    if (localtime_r(&sec, &t) == NULL)
        return FALSE;

    ret = strftime(buf, len, "%d-%b-%Y::%T", &t);
    if (ret == 0)
        return FALSE;
    len -= ret - 1;
    buflen = strlen(buf);

    ret = enif_snprintf(&buf[buflen], len, ".%06b64d", usec);
    if (ret >= len)
        return FALSE;

    return TRUE;

#else

    ret = enif_snprintf(buf, len, "%b64d", timestamp);
    if (ret == 0)
        return FALSE;
    else
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
char* make_sockaddr_in4(ErlNifEnv*    env,
                        ERL_NIF_TERM  port,
                        ERL_NIF_TERM  addr,
                        ERL_NIF_TERM* sa)
{
    ERL_NIF_TERM keys[] = {esock_atom_family, esock_atom_port, esock_atom_addr};
    ERL_NIF_TERM vals[] = {esock_atom_inet, port, addr};
    unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);
    unsigned int numVals = sizeof(vals) / sizeof(ERL_NIF_TERM);
    
    ESOCK_ASSERT( (numKeys == numVals) );
    
    if (!MKMA(env, keys, vals, numKeys, sa)) {
        *sa = esock_atom_undefined;
        return ESOCK_STR_EINVAL;
    } else {
        return NULL;
    }
}


/* Construct the IPv6 socket address */
static
char* make_sockaddr_in6(ErlNifEnv*    env,
                        ERL_NIF_TERM  port,
                        ERL_NIF_TERM  addr,
                        ERL_NIF_TERM  flowInfo,
                        ERL_NIF_TERM  scopeId,
                        ERL_NIF_TERM* sa)
{
    ERL_NIF_TERM keys[] = {esock_atom_family,
                           esock_atom_port,
                           esock_atom_addr,
                           esock_atom_flowinfo,
                           esock_atom_scope_id};
    ERL_NIF_TERM vals[] = {esock_atom_inet6, port, addr, flowInfo, scopeId};
    unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);
    unsigned int numVals = sizeof(vals) / sizeof(ERL_NIF_TERM);
    
    ESOCK_ASSERT( (numKeys == numVals) );
    
    if (!MKMA(env, keys, vals, numKeys, sa)) {
        *sa = esock_atom_undefined;
        return ESOCK_STR_EINVAL;
    } else {
        return NULL;
    }
}


/* Construct the Unix Domain socket address */
static
char* make_sockaddr_un(ErlNifEnv*    env,
                       ERL_NIF_TERM  path,
                       ERL_NIF_TERM* sa)
{
    ERL_NIF_TERM keys[] = {esock_atom_family, esock_atom_path};
    ERL_NIF_TERM vals[] = {esock_atom_local,  path};
    unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);
    unsigned int numVals = sizeof(vals) / sizeof(ERL_NIF_TERM);
    
    ESOCK_ASSERT( (numKeys == numVals) );
    
    if (!MKMA(env, keys, vals, numKeys, sa)) {
        *sa = esock_atom_undefined;
        return ESOCK_STR_EINVAL;
    } else {
        return NULL;
    }
}


/* Construct the Link Layer socket address */
#ifdef HAVE_NETPACKET_PACKET_H
static
char* make_sockaddr_ll(ErlNifEnv*    env,
                       ERL_NIF_TERM  proto,
                       ERL_NIF_TERM  ifindex,
                       ERL_NIF_TERM  hatype,
                       ERL_NIF_TERM  pkttype,
                       ERL_NIF_TERM  addr,
                       ERL_NIF_TERM* sa)
{
    ERL_NIF_TERM keys[] = {esock_atom_family,
                           esock_atom_protocol,
                           esock_atom_ifindex,
                           esock_atom_hatype,
                           esock_atom_pkttype,
                           esock_atom_addr};
    ERL_NIF_TERM vals[] = {esock_atom_packet, proto, ifindex,
                           hatype, pkttype, addr};
    unsigned int numKeys = sizeof(keys) / sizeof(ERL_NIF_TERM);
    unsigned int numVals = sizeof(vals) / sizeof(ERL_NIF_TERM);
    
    ESOCK_ASSERT( (numKeys == numVals) );
    
    if (!MKMA(env, keys, vals, numKeys, sa)) {
        *sa = esock_atom_undefined;
        return ESOCK_STR_EINVAL;
    } else {
        return NULL;
    }
}
#endif

