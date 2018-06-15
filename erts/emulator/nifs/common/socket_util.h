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

#ifndef SOCKET_UTIL_H__
#define SOCKET_UTIL_H__

#include <erl_nif.h>
#include "socket_int.h"

extern
char* esock_decode_sockaddr(ErlNifEnv*     env,
                            ERL_NIF_TERM   eSockAddr,
                            SocketAddress* sockAddrP,
                            unsigned int*  addrLen);

extern
char* esock_decode_sockaddr_in4(ErlNifEnv*          env,
                                ERL_NIF_TERM        eSockAddr,
                                struct sockaddr_in* sockAddrP,
                                unsigned int*       addrLen);

#if defined(HAVE_IN6) && defined(AF_INET6)
extern
char* esock_decode_sockaddr_in6(ErlNifEnv*           env,
                                ERL_NIF_TERM         eSockAddr,
                                struct sockaddr_in6* sockAddrP,
                                unsigned int*        addrLen);
#endif

#ifdef HAVE_SYS_UN_H
extern
char* esock_decode_sockaddr_un(ErlNifEnv*          env,
                               ERL_NIF_TERM        eSockAddr,
                               struct sockaddr_un* sockAddrP,
                               unsigned int*       addrLen);
#endif

extern
char* esock_decode_ip4_address(ErlNifEnv*          env,
                               ERL_NIF_TERM        eAddr,
                               struct sockaddr_in* sockAddrP,
                               unsigned int*       addrLen);

#if defined(HAVE_IN6) && defined(AF_INET6)
extern
char* esock_decode_ip6_address(ErlNifEnv*           env,
                               ERL_NIF_TERM         eAddr,
                               struct sockaddr_in6* sockAddrP,
                               unsigned int*        addrLen);
#endif

extern
char* esock_decode_domain(ErlNifEnv*   env,
                          ERL_NIF_TERM eDomain,
                          int*         domain);

extern
char* esock_encode_domain(ErlNifEnv*    env,
                          int           domain,
                          ERL_NIF_TERM* eDomain);

extern
char* esock_decode_type(ErlNifEnv*   env,
                        ERL_NIF_TERM eType,
                        int*         type);

extern
char* esock_encode_type(ErlNifEnv*    env,
                        int           type,
                        ERL_NIF_TERM* eType);


extern
ERL_NIF_TERM esock_make_ok2(ErlNifEnv* env, ERL_NIF_TERM any);
extern
ERL_NIF_TERM esock_make_ok3(ErlNifEnv* env, ERL_NIF_TERM val1, ERL_NIF_TERM val2);

extern
ERL_NIF_TERM esock_make_error(ErlNifEnv* env, ERL_NIF_TERM reason);
extern
ERL_NIF_TERM esock_make_error_str(ErlNifEnv* env, char* reason);
extern
ERL_NIF_TERM esock_make_error_errno(ErlNifEnv* env, int err);


#endif // SOCKET_UTIL_H__
