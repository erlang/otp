/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2023-2024. All Rights Reserved.
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
 *  Purpose : Asynchronous I/O functions.
 * ----------------------------------------------------------------------
 *
 * esaio = ESock Asynchronous I/O
 *
 */

#ifndef SOCKET_ASYNCIO_H__
#define SOCKET_ASYNCIO_H__

#include "socket_io.h"

extern int  esaio_init(unsigned int     numThreads,
                       const ESockData* dataP);
extern void esaio_finish(void);
extern ERL_NIF_TERM esaio_info(ErlNifEnv* env);

/*
extern ERL_NIF_TERM esaio_open_with_fd(ErlNifEnv*       env,
                                       int              fd,
                                       ERL_NIF_TERM     eopts,
                                       const ESockData* dataP);
				       */
extern ERL_NIF_TERM esaio_open_plain(ErlNifEnv*       env,
                                     int              domain,
                                     int              type,
                                     int              protocol,
                                     ERL_NIF_TERM     eopts,
                                     const ESockData* dataP);
extern ERL_NIF_TERM esaio_bind(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ESockAddress*    sockAddrP,
                               SOCKLEN_T        addrLen);
extern ERL_NIF_TERM esaio_connect(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     connRef,
                                  ESockAddress*    addrP,
                                  SOCKLEN_T        addrLen);
/*
extern ERL_NIF_TERM esaio_listen(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 int              backlog);
*/
extern ERL_NIF_TERM esaio_accept(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     accRef);
extern ERL_NIF_TERM esaio_send(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     sendRef,
                               ErlNifBinary*    sndDataP,
                               int              flags);
extern ERL_NIF_TERM esaio_sendto(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     sendRef,
                                 ErlNifBinary*    dataP,
                                 int              flags,
                                 ESockAddress*    toAddrP,
                                 SOCKLEN_T        toAddrLen);
extern ERL_NIF_TERM esaio_sendmsg(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     sendRef,
                                  ERL_NIF_TERM     eMsg,
                                  int              flags,
                                  ERL_NIF_TERM     eIOV,
                                  const ESockData* dataP);
extern ERL_NIF_TERM esaio_sendv(ErlNifEnv*       env,
                                ESockDescriptor* descP,
                                ERL_NIF_TERM     sockRef,
                                ERL_NIF_TERM     sendRef,
                                ERL_NIF_TERM     eIOV,
                                const ESockData* dataP);
/*
extern
ERL_NIF_TERM esaio_sendfile_start(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     sendRef,
                                  off_t            offset,
                                  size_t           count,
                                  ERL_NIF_TERM     fRef);
extern
ERL_NIF_TERM esaio_sendfile_cont(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 ERL_NIF_TERM     sockRef,
                                 ERL_NIF_TERM     sendRef,
                                 off_t            offset,
                                 size_t           count);
extern
ERL_NIF_TERM esaio_sendfile_deferred_close(ErlNifEnv*       env,
                                           ESockDescriptor* descP);
*/

extern ERL_NIF_TERM esaio_recv(ErlNifEnv*       env,
                               ESockDescriptor* descP,
                               ERL_NIF_TERM     sockRef,
                               ERL_NIF_TERM     recvRef,
                               ssize_t          len,
                               int              flags);
extern ERL_NIF_TERM esaio_recvfrom(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   ERL_NIF_TERM     sockRef,
                                   ERL_NIF_TERM     recvRef,
                                   ssize_t          len,
                                   int              flags);
extern ERL_NIF_TERM esaio_recvmsg(ErlNifEnv*       env,
                                  ESockDescriptor* descP,
                                  ERL_NIF_TERM     sockRef,
                                  ERL_NIF_TERM     recvRef,
                                  ssize_t          bufLen,
                                  ssize_t          ctrlLen,
                                  int              flags);
extern ERL_NIF_TERM esaio_close(ErlNifEnv*       env,
                                ESockDescriptor* descP);
extern ERL_NIF_TERM esaio_fin_close(ErlNifEnv*       env,
                                    ESockDescriptor* descP);
extern ERL_NIF_TERM esaio_shutdown(ErlNifEnv*       env,
                                   ESockDescriptor* descP,
                                   int              how);
extern ERL_NIF_TERM esaio_sockname(ErlNifEnv*       env,
                                   ESockDescriptor* descP);
extern ERL_NIF_TERM esaio_peername(ErlNifEnv*       env,
                                   ESockDescriptor* descP);
extern ERL_NIF_TERM esaio_cancel_connect(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         ERL_NIF_TERM     opRef);
extern ERL_NIF_TERM esaio_cancel_accept(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     sockRef,
                                        ERL_NIF_TERM     opRef);
extern ERL_NIF_TERM esaio_cancel_send(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     opRef);
extern ERL_NIF_TERM esaio_cancel_recv(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     opRef);

extern ERL_NIF_TERM esaio_ioctl3(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 unsigned long    req,
                                 ERL_NIF_TERM     arg);
extern ERL_NIF_TERM esaio_ioctl2(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 unsigned long    req);
/*
extern ERL_NIF_TERM esaio_ioctl3(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 unsigned long    req,
                                 ERL_NIF_TERM     arg);
extern ERL_NIF_TERM esaio_ioctl4(ErlNifEnv*       env,
                                 ESockDescriptor* descP,
                                 unsigned long    req,
                                 ERL_NIF_TERM     ename,
                                 ERL_NIF_TERM     eval);
*/

extern void esaio_dtor(ErlNifEnv*       env,
                       ESockDescriptor* descP);
extern void esaio_stop(ErlNifEnv*       env,
                       ESockDescriptor* descP);
extern void esaio_down(ErlNifEnv*           env,
                       ESockDescriptor*     descP,
                       const ErlNifPid*     pidP,
                       const ErlNifMonitor* monP);

/* Temporary (I hope) workaround */
extern void esaio_down_ctrl(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            const ErlNifPid* pidP);

#endif // SOCKET_ASYNCIO_H__
