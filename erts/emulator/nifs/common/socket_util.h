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

#ifndef SOCKET_UTIL_H__
#define SOCKET_UTIL_H__

#include <erl_nif.h>
#include "socket_int.h"

#define CHAR(C)  ((char) (C))
#define UCHAR(C) ((unsigned char) (C))
#define INT(I)   ((int)   (I))
#define UINT(U)  ((unsigned int) (U))
#define LONG(L)  ((long) (L))
#define ULONG(L) ((unsigned long) (L))
#define SZT(I)   ((size_t) (I))
#define VOIDP(P) ((void*) (P))
#define CHARP(P) ((char*) (P))

#define ESOCK_ABORT(E)  esock_abort(E, __func__, __FILE__, __LINE__)
#define ESOCK_ASSERT(e) ((void) ((e) ? 1 : (ESOCK_ABORT(#e), 0)))

extern
char* esock_encode_iov(ErlNifEnv*    env,
                       int           read,
                       struct iovec* iov,
                       size_t        len,
                       ErlNifBinary* data,
                       ERL_NIF_TERM* eIOV);
extern
char* esock_decode_iov(ErlNifEnv*    env,
                       ERL_NIF_TERM  eIOV,
                       ErlNifBinary* bufs,
                       struct iovec* iov,
                       size_t        len,
                       ssize_t*      totSize);
extern
char* esock_decode_sockaddr(ErlNifEnv*    env,
                            ERL_NIF_TERM  eSockAddr,
                            ESockAddress* sockAddrP,
                            unsigned int* addrLen);
extern
char* esock_encode_sockaddr(ErlNifEnv*    env,
                            ESockAddress* sockAddrP,
                            unsigned int  addrLen,
                            ERL_NIF_TERM* eSockAddr);

extern
char* esock_decode_sockaddr_in4(ErlNifEnv*          env,
                                ERL_NIF_TERM        eSockAddr,
                                struct sockaddr_in* sockAddrP,
                                unsigned int*       addrLen);
extern
char* esock_encode_sockaddr_in4(ErlNifEnv*          env,
                                struct sockaddr_in* sockAddrP,
                                unsigned int        addrLen,
                                ERL_NIF_TERM*       eSockAddr);

#if defined(HAVE_IN6) && defined(AF_INET6)
extern
char* esock_decode_sockaddr_in6(ErlNifEnv*           env,
                                ERL_NIF_TERM         eSockAddr,
                                struct sockaddr_in6* sockAddrP,
                                unsigned int*        addrLen);
extern
char* esock_encode_sockaddr_in6(ErlNifEnv*           env,
                                struct sockaddr_in6* sockAddrP,
                                unsigned int         addrLen,
                                ERL_NIF_TERM*        eSockAddr);
#endif

#ifdef HAVE_SYS_UN_H
extern
char* esock_decode_sockaddr_un(ErlNifEnv*          env,
                               ERL_NIF_TERM        eSockAddr,
                               struct sockaddr_un* sockAddrP,
                               unsigned int*       addrLen);
extern
char* esock_encode_sockaddr_un(ErlNifEnv*          env,
                               struct sockaddr_un* sockAddrP,
                               unsigned int        addrLen,
                               ERL_NIF_TERM*       eSockAddr);
#endif

#ifdef HAVE_NETPACKET_PACKET_H
extern
char* esock_encode_sockaddr_ll(ErlNifEnv*          env,
                               struct sockaddr_ll* sockAddrP,
                               unsigned int        addrLen,
                               ERL_NIF_TERM*       eSockAddr);
#endif

extern
char* esock_decode_ip4_address(ErlNifEnv*      env,
                               ERL_NIF_TERM    eAddr,
                               struct in_addr* inAddrP);
extern
char* esock_encode_ip4_address(ErlNifEnv*      env,
                               struct in_addr* addrP,
                               ERL_NIF_TERM*   eAddr);

#if defined(HAVE_IN6) && defined(AF_INET6)
extern
char* esock_decode_ip6_address(ErlNifEnv*       env,
                               ERL_NIF_TERM     eAddr,
                               struct in6_addr* inAddrP);
extern
char* esock_encode_ip6_address(ErlNifEnv*       env,
                               struct in6_addr* addrP,
                               ERL_NIF_TERM*    eAddr);
#endif

extern char* esock_encode_timeval(ErlNifEnv*      env,
                                  struct timeval* timeP,
                                  ERL_NIF_TERM*   eTime);
extern char* esock_decode_timeval(ErlNifEnv*      env,
                                  ERL_NIF_TERM    eTime,
                                  struct timeval* timeP);
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
char* esock_decode_protocol(ErlNifEnv*   env,
                            ERL_NIF_TERM eProtocol,
                            int*         protocol);
extern
char* esock_encode_protocol(ErlNifEnv*    env,
                            int           type,
                            ERL_NIF_TERM* eProtocol);

extern
void esock_encode_packet_protocol(ErlNifEnv*     env,
                                  unsigned short protocol,
                                  ERL_NIF_TERM*  eProtocol);
extern
void esock_encode_packet_hatype(ErlNifEnv*     env,
                                unsigned short hatype,
                                ERL_NIF_TERM*  eHaType);
extern
void esock_encode_packet_pkttype(ErlNifEnv*     env,
                                 unsigned short pkttype,
                                 ERL_NIF_TERM*  ePktType);
extern
void esock_encode_packet_addr(ErlNifEnv*     env,
                              unsigned char  len,
                              unsigned char* addr,
                              ERL_NIF_TERM*  eAddr);

extern
char* esock_decode_bufsz(ErlNifEnv*   env,
                         ERL_NIF_TERM eVal,
                         size_t       defSz,
                         size_t*      sz);

extern
BOOLEAN_T esock_decode_string(ErlNifEnv*         env,
                              const ERL_NIF_TERM eString,
                              char**             stringP);

extern
BOOLEAN_T esock_extract_bool_from_map(ErlNifEnv*   env,
                                      ERL_NIF_TERM map,
                                      ERL_NIF_TERM key,
                                      BOOLEAN_T    def);
extern
BOOLEAN_T esock_decode_bool(ERL_NIF_TERM val);
extern
ERL_NIF_TERM esock_encode_bool(BOOLEAN_T val);

extern
size_t esock_strnlen(const char *s, size_t maxlen);
extern
void esock_abort(const char* expr,
                 const char* func,
                 const char* file,
                 int         line);

extern
ERL_NIF_TERM esock_self(ErlNifEnv* env);

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

extern
ErlNifTime esock_timestamp(void);

extern
BOOLEAN_T esock_timestamp_str(char *buf, unsigned int len);

extern
BOOLEAN_T esock_format_timestamp(ErlNifTime timestamp, char *buf, unsigned int len);

extern
void esock_warning_msg(const char* format, ... );


#endif // SOCKET_UTIL_H__
