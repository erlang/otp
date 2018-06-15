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
#include "sys.h"


/* THIS IS JUST TEMPORARY */
extern char* erl_errno_id(int error);


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

    if (!IS_MAP(env, eSockAddr))
        return ESOCK_STR_EINVAL;

    if (!GET_MAP_VAL(env, eSockAddr, esock_atom_family, &efam))
        return ESOCK_STR_EINVAL;

    if ((xres = esock_decode_domain(env, efam, &fam)) != NULL)
        return xres;

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



/* +++ esock_decode_sockaddr_in4 +++
 *
 * Decode a IPv4 socket address - sockaddr_in4. In erlang its represented as
 * a map, which has a specific set of attributes: 
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

    /* Basic init */
    sys_memzero((char*) sockAddrP, sizeof(struct sockaddr_in));

#ifndef NO_SA_LEN
    sockAddrP->sin_len = sizeof(struct sockaddr_in);
#endif

    sockAddrP->sin_family = AF_INET;

    /* Extract (e) port number from map */
    if (!GET_MAP_VAL(env, eSockAddr, esock_atom_port, &eport))
        return ESOCK_STR_EINVAL;

    /* Decode port number */
    if (!GET_INT(env, eport, &port))
        return ESOCK_STR_EINVAL;
    
    sockAddrP->sin_port = htons(port);

    /* Extract (e) address from map */
    if (!GET_MAP_VAL(env, eSockAddr, esock_atom_addr, &eaddr))
        return ESOCK_STR_EINVAL;

    /* Decode address */
    if (!esock_decode_ip4_address(env, eaddr, sockAddrP, addrLen))
        return ESOCK_STR_EINVAL;

    return NULL;
}


/* +++ decode_sockaddr_in6 +++
 *
 * Decode a IPv6 socket address - sockaddr_in6. In erlang its represented as
 * a map, which has a specific set of attributes: 
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
    if (!esock_decode_ip6_address(env, eaddr, sockAddrP, addrLen))
        return ESOCK_STR_EINVAL;

    return NULL;
}
#endif



/* +++ esock_decode_sockaddr_un +++
 *
 * Decode a Unix Domain socket address - sockaddr_un. In erlang its 
 * represented as a map, which has a specific set of attributes: 
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



/* +++ decode_ip4_address +++
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
    if (IS_ATOM(env, eAddr)) {
        /* This is either 'any' or 'loopback' */
        struct in_addr addr;

        if (COMPARE(esock_atom_loopback, eAddr) == 0) {
            addr.s_addr = htonl(INADDR_LOOPBACK);
        } else if (COMPARE(esock_atom_any, eAddr) == 0) {
            addr.s_addr = htonl(INADDR_ANY);
        } else {
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



/* +++ decode_ip6_address +++
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


