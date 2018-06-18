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
 *  Purpose : Utility functions for the socket and net NIF(s).
 * ----------------------------------------------------------------------
 *
 */

#include <stddef.h>
#include "socket_int.h"
#include "socket_util.h"
#include "socket_dbg.h"
#include "sys.h"


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
char* esock_decode_sockaddr(ErlNifEnv*     env,
                            ERL_NIF_TERM   eSockAddr,
                            SocketAddress* sockAddrP,
                            unsigned int*  addrLen)
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

#ifdef HAVE_SYS_UN_H
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
char* esock_encode_sockaddr(ErlNifEnv*     env,
                            SocketAddress* sockAddrP,
                            unsigned int   addrLen,
                            ERL_NIF_TERM*  eSockAddr)
{
    char* xres;

    switch (sockAddrP->sa.sa_family) {
    case AF_INET:
        xres = esock_encode_sockaddr_in4(env, &sockAddrP->in4, addrLen, eSockAddr);
        break;

#if defined(HAVE_IN6) && defined(AF_INET6)
    case AF_INET6:
        xres = esock_encode_sockaddr_in6(env, &sockAddrP->in6, addrLen, eSockAddr);
        break;
#endif

#ifdef HAVE_SYS_UN_H
    case AF_UNIX:
        xres = esock_encode_sockaddr_un(env, &sockAddrP->un, addrLen, eSockAddr);
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
    if ((xres = esock_decode_ip4_address(env, eaddr, sockAddrP, addrLen)) != NULL)
        return xres;

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

    sockAddrP->sin6_port = htons(port);

    /* *** Extract (e) flowinfo from map *** */
    if (!GET_MAP_VAL(env, eSockAddr, esock_atom_flowinfo, &eflowInfo))
        return ESOCK_STR_EINVAL;

    /* 4: Get the flowinfo */
    if (!GET_UINT(env, eflowInfo, &flowInfo))
        return ESOCK_STR_EINVAL;

    sockAddrP->sin6_flowinfo = flowInfo;
    
    /* *** Extract (e) scope_id from map *** */
    if (!GET_MAP_VAL(env, eSockAddr, esock_atom_scope_id, &escopeId))
        return ESOCK_STR_EINVAL;

    /* *** Get the scope_id *** */
    if (!GET_UINT(env, escopeId, &scopeId))
        return ESOCK_STR_EINVAL;

    sockAddrP->sin6_scope_id = scopeId;

    /* *** Extract (e) address from map *** */
    if (!GET_MAP_VAL(env, eSockAddr, esock_atom_addr, &eaddr))
        return ESOCK_STR_EINVAL;

    /* Decode address */
    if ((xres = esock_decode_ip6_address(env, eaddr, sockAddrP, addrLen)) != NULL)
        return xres;

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

#ifdef HAVE_SYS_UN_H
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

#ifdef HAVE_SYS_UN_H
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



/* +++ esock_decode_ip4_address +++
 *
 * Decode a IPv4 address. This can be three things:
 *
 *    + Then atom 'any'
 *    + Then atom 'loopback'
 *    + An ip4_address() (4 tuple)
 *
 * Note that this *only* decodes the "address" part of a
 * (IPv4) socket address. There are several other things (port).
 */

extern
char* esock_decode_ip4_address(ErlNifEnv*          env,
                               ERL_NIF_TERM        eAddr,
                               struct sockaddr_in* sockAddrP,
                               unsigned int*       addrLen)
{
    UDBG( ("SUTIL", "esock_decode_ip4_address -> entry with"
           "\r\n   eAddr: %T"
           "\r\n", eAddr) );

    if (IS_ATOM(env, eAddr)) {
        /* This is either 'any' or 'loopback' */
        struct in_addr addr;

        if (COMPARE(esock_atom_loopback, eAddr) == 0) {
            UDBG( ("SUTIL", "esock_decode_ip4_address -> address: lookback\r\n") );
            addr.s_addr = htonl(INADDR_LOOPBACK);
        } else if (COMPARE(esock_atom_any, eAddr) == 0) {
            UDBG( ("SUTIL", "esock_decode_ip4_address -> address: any\r\n") );
            addr.s_addr = htonl(INADDR_ANY);
        } else {
            UDBG( ("SUTIL", "esock_decode_ip4_address -> address: unknown\r\n") );
            return ESOCK_STR_EINVAL;
        }

        sockAddrP->sin_addr.s_addr = addr.s_addr;
        *addrLen                   = sizeof(struct sockaddr_in);

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

        sys_memcpy(&sockAddrP->sin_addr, &addr, sizeof(addr));
        *addrLen = sizeof(struct sockaddr_in);
        
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
char* esock_decode_ip6_address(ErlNifEnv*           env,
                               ERL_NIF_TERM         eAddr,
                               struct sockaddr_in6* sockAddrP,
                               unsigned int*        addrLen)
{
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

        sockAddrP->sin6_addr = *addr;
        *addrLen             = sizeof(struct sockaddr_in6);

    } else {
        /* This is a 8-tuple */

        const ERL_NIF_TERM* addrt;
        int                 addrtSz;
        int                 a, v;
        char                addr[16];
        
        if (!GET_TUPLE(env, eAddr, &addrtSz, &addrt))
            return ESOCK_STR_EINVAL;
        
        if (addrtSz != 8)
            return ESOCK_STR_EINVAL;

        for (a = 0; a < 8; a++) {
            if (!GET_INT(env, addrt[a], &v))
                return ESOCK_STR_EINVAL;
            addr[a*2  ] = ((v >> 8) & 0xFF);
            addr[a*2+1] = (v & 0xFF);
        }

        sys_memcpy(&sockAddrP->sin6_addr, &addr, sizeof(addr));
        *addrLen = sizeof(struct sockaddr_in6);
        
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
    unsigned char* a     = (unsigned char*) &addrP;
    
    /* The address */
    for (i = 0; i < atLen; i++) {
        at[i] = MKI(env, get_int16(a + i*2));
    }

    *eAddr = MKTA(env, at, atLen);
    
    return NULL;
}
#endif



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

#ifdef HAVE_SYS_UN_H
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

#ifdef HAVE_SYS_UN_H
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



/* +++ esock_decode_domain +++
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



/* +++ esock_decode_protocol +++
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
        *proto = SOL_IPV6;
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




/* =================================================================== *
 *                                                                     *
 *                   Various utility functions                         *
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
    ERL_NIF_TERM vals[] = {esock_atom_inet, path};
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


