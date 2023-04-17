/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2018-2023. All Rights Reserved.
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
 *  Purpose : Utility "stuff" for socket and prim_net (nifs).
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
#ifndef VOID
#define VOID(D)  ((void) (D))
#endif
#define VOIDP(P) ((void*) (P))
#define CHARP(P) ((char*) (P))
#define UCHARP(P) ((unsigned char*) (P))

#define ESOCK_ABORT(E)  esock_abort(E, __func__, __FILE__, __LINE__)
#define ESOCK_ASSERT(e) ((void) ((e) ? 1 : (ESOCK_ABORT(#e), 0)))

#define MKEEI(E, RI, I) \
    esock_make_extra_error_info_term((E),          \
                                     __FILE__,     \
                                     __FUNCTION__, \
                                     __LINE__,     \
                                     (RI), (I))

#if defined(ESOCK_USE_EXTENDED_ERROR_INFO)
#define ENO2T(E, ENO) MKEEI((E),                                \
                            MKI((E), (ENO)),                    \
                            esock_errno_to_term((E), (ENO)))
#else
#define ENO2T(E, ENO) esock_errno_to_term((E), (ENO))
#endif


extern
ERL_NIF_TERM esock_make_extra_error_info_term(ErlNifEnv*   env,
                                              const char*  file,
                                              const char*  function,
                                              const int    line,
                                              ERL_NIF_TERM rawinfo,
                                              ERL_NIF_TERM info);

extern
unsigned int esock_get_uint_from_map(ErlNifEnv*   env,
                                     ERL_NIF_TERM map,
                                     ERL_NIF_TERM key,
                                     unsigned int def);

extern
BOOLEAN_T esock_get_bool_from_map(ErlNifEnv*   env,
                                  ERL_NIF_TERM map,
                                  ERL_NIF_TERM key,
                                  BOOLEAN_T    def);

extern
BOOLEAN_T esock_decode_iov(ErlNifEnv*    env,
                           ERL_NIF_TERM  eIOV,
                           ErlNifBinary* bufs,
                           SysIOVec*     iov,
                           size_t        len,
                           ssize_t*      totSize);
extern
void esock_encode_iov(ErlNifEnv*    env,
                      ssize_t       read,
                      SysIOVec*     iov,
                      size_t        len,
                      ErlNifBinary* data,
                      ERL_NIF_TERM* eIOV);

extern
BOOLEAN_T esock_decode_sockaddr(ErlNifEnv*    env,
                                ERL_NIF_TERM  eSockAddr,
                                ESockAddress* sockAddrP,
                                SOCKLEN_T*    addrLen);
extern
void esock_encode_sockaddr(ErlNifEnv*    env,
                           ESockAddress* sockAddrP,
                           int           addrLen,
                           ERL_NIF_TERM* eSockAddr);
extern
void esock_encode_hwsockaddr(ErlNifEnv*       env,
			     struct sockaddr* sockAddrP,
			     SOCKLEN_T        addrLen,
			     ERL_NIF_TERM*    eSockAddr);
extern
BOOLEAN_T esock_decode_sockaddr_in(ErlNifEnv*          env,
                                   ERL_NIF_TERM        eSockAddr,
                                   struct sockaddr_in* sockAddrP,
                                   SOCKLEN_T*          addrLen);
extern
void esock_encode_sockaddr_in(ErlNifEnv*          env,
                              struct sockaddr_in* sockAddrP,
                              SOCKLEN_T           addrLen,
                              ERL_NIF_TERM*       eSockAddr);

#if defined(HAVE_IN6) && defined(AF_INET6)
extern
BOOLEAN_T esock_decode_sockaddr_in6(ErlNifEnv*           env,
                                    ERL_NIF_TERM         eSockAddr,
                                    struct sockaddr_in6* sockAddrP,
                                    SOCKLEN_T*           addrLen);
extern
void esock_encode_sockaddr_in6(ErlNifEnv*           env,
                               struct sockaddr_in6* sockAddrP,
                               SOCKLEN_T            addrLen,
                               ERL_NIF_TERM*        eSockAddr);
#endif

#ifdef HAS_AF_LOCAL
extern
BOOLEAN_T esock_decode_sockaddr_un(ErlNifEnv*          env,
                                   ERL_NIF_TERM        eSockAddr,
                                   struct sockaddr_un* sockAddrP,
                                   SOCKLEN_T*          addrLen);
extern
void esock_encode_sockaddr_un(ErlNifEnv*          env,
                              struct sockaddr_un* sockAddrP,
                              SOCKLEN_T           addrLen,
                              ERL_NIF_TERM*       eSockAddr);
#endif

#ifdef HAVE_NETPACKET_PACKET_H
extern
void esock_encode_sockaddr_ll(ErlNifEnv*          env,
                              struct sockaddr_ll* sockAddrP,
                              SOCKLEN_T           addrLen,
                              ERL_NIF_TERM*       eSockAddr);
#endif

extern
BOOLEAN_T esock_decode_in_addr(ErlNifEnv*      env,
                               ERL_NIF_TERM    eAddr,
                               struct in_addr* inAddrP);
extern
void esock_encode_in_addr(ErlNifEnv*      env,
                          struct in_addr* addrP,
                          ERL_NIF_TERM*   eAddr);

#if defined(HAVE_IN6) && defined(AF_INET6)
extern
BOOLEAN_T esock_decode_in6_addr(ErlNifEnv*       env,
                                ERL_NIF_TERM     eAddr,
                                struct in6_addr* inAddrP);
extern
void esock_encode_in6_addr(ErlNifEnv*       env,
                           struct in6_addr* addrP,
                           ERL_NIF_TERM*    eAddr);
#endif

extern void esock_encode_timeval(ErlNifEnv*      env,
                                 struct timeval* timeP,
                                 ERL_NIF_TERM*   eTime);
extern BOOLEAN_T esock_decode_timeval(ErlNifEnv*      env,
                                      ERL_NIF_TERM    eTime,
                                      struct timeval* timeP);

extern
void esock_encode_domain(ErlNifEnv*    env,
                         int           domain,
                         ERL_NIF_TERM* eDomain);
extern
int esock_decode_domain(ErlNifEnv*   env,
                        ERL_NIF_TERM eDomain,
                        int*         domain);

extern
BOOLEAN_T esock_decode_type(ErlNifEnv*   env,
                            ERL_NIF_TERM eType,
                            int*         type);
extern
void esock_encode_type(ErlNifEnv*    env,
                       int           type,
                       ERL_NIF_TERM* eType);

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
BOOLEAN_T esock_decode_bufsz(ErlNifEnv*   env,
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
BOOLEAN_T esock_extract_pid_from_map(ErlNifEnv*   env,
                                     ERL_NIF_TERM map,
                                     ERL_NIF_TERM key,
                                     ErlNifPid*   pid);
extern
BOOLEAN_T esock_extract_int_from_map(ErlNifEnv*   env,
                                     ERL_NIF_TERM map,
                                     ERL_NIF_TERM key,
                                     int*         val);
extern
BOOLEAN_T esock_decode_bool(ERL_NIF_TERM eVal, BOOLEAN_T* val);
extern
ERL_NIF_TERM esock_encode_bool(BOOLEAN_T val);

extern
BOOLEAN_T esock_decode_level(ErlNifEnv* env, ERL_NIF_TERM eVal, int *val);
extern
ERL_NIF_TERM esock_encode_level(ErlNifEnv* env, int level);

extern
size_t esock_strnlen(const char *s, size_t maxlen);
extern
void __noreturn
esock_abort(const char* expr,
            const char* func,
            const char* file,
            int         line);

extern
ERL_NIF_TERM esock_self(ErlNifEnv* env);

extern
ERL_NIF_TERM esock_make_ok2(ErlNifEnv* env, ERL_NIF_TERM any);
extern
ERL_NIF_TERM esock_errno_to_term(ErlNifEnv* env, int err);
extern
ERL_NIF_TERM esock_make_error(ErlNifEnv* env, ERL_NIF_TERM reason);
extern
ERL_NIF_TERM esock_make_error_closed(ErlNifEnv* env);
extern
ERL_NIF_TERM esock_make_error_str(ErlNifEnv* env, char* reason);
extern
ERL_NIF_TERM esock_make_error_errno(ErlNifEnv* env, int err);
extern
ERL_NIF_TERM esock_make_error_t2r(ErlNifEnv* env,
                                  ERL_NIF_TERM tag, ERL_NIF_TERM reason);
extern
ERL_NIF_TERM esock_make_error_invalid(ErlNifEnv* env, ERL_NIF_TERM what);
extern
ERL_NIF_TERM esock_make_error_integer_range(ErlNifEnv* env, ERL_NIF_TERM i);
extern
ERL_NIF_TERM esock_make_invalid(ErlNifEnv* env, ERL_NIF_TERM reason);
extern
ERL_NIF_TERM esock_raise_invalid(ErlNifEnv* env, ERL_NIF_TERM what);

extern
ERL_NIF_TERM esock_make_new_binary(ErlNifEnv *env, void *buf, size_t size);

extern
ErlNifTime esock_timestamp(void);

extern
BOOLEAN_T esock_timestamp_str(char *buf, unsigned int len);

extern
BOOLEAN_T esock_format_timestamp(ErlNifTime timestamp, char *buf, unsigned int len);

#define MSG_FUNCS                               \
    MSG_FUNC_DEF(info)                          \
    MSG_FUNC_DEF(warning)                       \
    MSG_FUNC_DEF(error)

#define MSG_FUNC_DEF(FN)                                \
    extern                                              \
    void esock_##FN##_msg(const char* format, ... );

MSG_FUNCS
#undef MSG_FUNC_DEF
#undef MSG_FUNCS

extern
BOOLEAN_T esock_is_integer(ErlNifEnv *env, ERL_NIF_TERM term);

#endif // SOCKET_UTIL_H__
