/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2022-2023. All Rights Reserved.
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
 *  Purpose : Types and stuff for the socket I/O backend
 * ----------------------------------------------------------------------
 *
 */

#ifndef SOCKET_IO_H__
#define SOCKET_IO_H__

#include <erl_nif.h>
#include "prim_socket_int.h"


#define ESOCK_IO_OK               0
#define ESOCK_IO_ERR_UNSUPPORTED -1

typedef int (*ESockIOInit)(unsigned int     numThreads,
                           const ESockData* dataP);

typedef void (*ESockIOFinish)(void);


typedef ERL_NIF_TERM (*ESockIOInfo)(ErlNifEnv* env);
typedef ERL_NIF_TERM (*ESockIOCommand)(ErlNifEnv*   env,
                                       ERL_NIF_TERM command,
                                       ERL_NIF_TERM cdata);
typedef ERL_NIF_TERM (*ESockIOSupports0)(ErlNifEnv* env);
typedef ERL_NIF_TERM (*ESockIOSupports1)(ErlNifEnv*   env,
                                         ERL_NIF_TERM key);


typedef ERL_NIF_TERM (*ESockIOOpenWithFd)(ErlNifEnv*       env,
                                          int              fd,
                                          ERL_NIF_TERM     eopts,
                                          const ESockData* dataP);

typedef ERL_NIF_TERM (*ESockIOOpenPlain)(ErlNifEnv*       env,
                                         int              domain,
                                         int              type,
                                         int              protocol,
                                         ERL_NIF_TERM     eopts,
                                         const ESockData* dataP);

typedef ERL_NIF_TERM (*ESockIOBind)(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ESockAddress*    sockAddrP,
                                    SOCKLEN_T        addrLen);

typedef ERL_NIF_TERM (*ESockIOConnect)(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef,
                                       ERL_NIF_TERM     connRef,
                                       ESockAddress*    addrP,
                                       SOCKLEN_T        addrLen);

typedef ERL_NIF_TERM (*ESockIOListen)(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      int              backlog);

typedef ERL_NIF_TERM (*ESockIOAccept)(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     accRef);

typedef ERL_NIF_TERM (*ESockIOSend)(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     sockRef,
                                    ERL_NIF_TERM     sendRef,
                                    ErlNifBinary*    sndDataP,
                                    int              flags);

typedef ERL_NIF_TERM (*ESockIOSendTo)(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      ERL_NIF_TERM     sockRef,
                                      ERL_NIF_TERM     sendRef,
                                      ErlNifBinary*    dataP,
                                      int              flags,
                                      ESockAddress*    toAddrP,
                                      SOCKLEN_T        toAddrLen);

typedef ERL_NIF_TERM (*ESockIOSendMsg)(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef,
                                       ERL_NIF_TERM     sendRef,
                                       ERL_NIF_TERM     eMsg,
                                       int              flags,
                                       ERL_NIF_TERM     eIOV,
                                       const ESockData* dataP);

typedef ERL_NIF_TERM (*ESockIOSendv)(ErlNifEnv*       env,
                                     ESockDescriptor* descP,
                                     ERL_NIF_TERM     sockRef,
                                     ERL_NIF_TERM     sendRef,
                                     ERL_NIF_TERM     eIOV,
                                     const ESockData* dataP);

typedef ERL_NIF_TERM (*ESockIOSendFileStart)(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     sockRef,
                                             ERL_NIF_TERM     sendRef,
                                             off_t            offset,
                                             size_t           count,
                                             ERL_NIF_TERM     fRef);
typedef ERL_NIF_TERM (*ESockIOSendFileContinue)(ErlNifEnv*       env,
                                                ESockDescriptor* descP,
                                                ERL_NIF_TERM     sockRef,
                                                ERL_NIF_TERM     sendRef,
                                                off_t            offset,
                                                size_t           count);
typedef ERL_NIF_TERM (*ESockIOSendFileDeferredClose)(ErlNifEnv*       env,
                                                     ESockDescriptor* descP);

typedef ERL_NIF_TERM (*ESockIORecv)(ErlNifEnv*       env,
                                    ESockDescriptor* descP,
                                    ERL_NIF_TERM     sockRef,
                                    ERL_NIF_TERM     recvRef,
                                    ssize_t          len,
                                    int              flags);

typedef ERL_NIF_TERM (*ESockIORecvFrom)(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        ERL_NIF_TERM     sockRef,
                                        ERL_NIF_TERM     recvRef,
                                        ssize_t          len,
                                        int              flags);

typedef ERL_NIF_TERM (*ESockIORecvMsg)(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       ERL_NIF_TERM     sockRef,
                                       ERL_NIF_TERM     recvRef,
                                       ssize_t          bufLen,
                                       ssize_t          ctrlLen,
                                       int              flags);

typedef ERL_NIF_TERM (*ESockIOClose)(ErlNifEnv*       env,
                                     ESockDescriptor* descP);

typedef ERL_NIF_TERM (*ESockIOFinClose)(ErlNifEnv*       env,
                                        ESockDescriptor* descP);

typedef ERL_NIF_TERM (*ESockIOShutdown)(ErlNifEnv*       env,
                                        ESockDescriptor* descP,
                                        int              how);

typedef ERL_NIF_TERM (*ESockIOSockName)(ErlNifEnv*       env,
                                        ESockDescriptor* descP);

typedef ERL_NIF_TERM (*ESockIOPeerName)(ErlNifEnv*       env,
                                        ESockDescriptor* descP);

typedef ERL_NIF_TERM (*ESockIOCancelConnect)(ErlNifEnv*       env,
                                             ESockDescriptor* descP,
                                             ERL_NIF_TERM     opRef);

typedef ERL_NIF_TERM (*ESockIOCancelAccept)(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            ERL_NIF_TERM     sockRef,
                                            ERL_NIF_TERM     opRef);

typedef ERL_NIF_TERM (*ESockIOCancelSend)(ErlNifEnv*       env,
                                           ESockDescriptor* descP,
                                           ERL_NIF_TERM     sockRef,
                                           ERL_NIF_TERM     opRef);

typedef ERL_NIF_TERM (*ESockIOCancelRecv)(ErlNifEnv*       env,
                                          ESockDescriptor* descP,
                                          ERL_NIF_TERM     sockRef,
                                          ERL_NIF_TERM     opRef);

typedef ERL_NIF_TERM (*ESockIOSetopt)(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      int              level,
                                      int              opt,
                                      ERL_NIF_TERM     eVal);
typedef ERL_NIF_TERM (*ESockIOSetoptNative)(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            int              level,
                                            int              opt,
                                            ERL_NIF_TERM     eVal);
typedef ERL_NIF_TERM (*ESockIOSetoptOtp)(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         int              eOpt,
                                         ERL_NIF_TERM     eVal);

typedef ERL_NIF_TERM (*ESockIOGetopt)(ErlNifEnv*       env,
                                      ESockDescriptor* descP,
                                      int              level,
                                      int              opt);
typedef ERL_NIF_TERM (*ESockIOGetoptNative)(ErlNifEnv*       env,
                                            ESockDescriptor* descP,
                                            int              level,
                                            int              opt,
                                            ERL_NIF_TERM     valueSpec);
typedef ERL_NIF_TERM (*ESockIOGetoptOtp)(ErlNifEnv*       env,
                                         ESockDescriptor* descP,
                                         int              eOpt);

typedef ERL_NIF_TERM (*ESockIOIoctl_2)(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       unsigned long    req);
typedef ERL_NIF_TERM (*ESockIOIoctl_3)(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       unsigned long    req,
                                       ERL_NIF_TERM     arg);
typedef ERL_NIF_TERM (*ESockIOIoctl_4)(ErlNifEnv*       env,
                                       ESockDescriptor* descP,
                                       unsigned long    req,
                                       ERL_NIF_TERM     arg1,
                                       ERL_NIF_TERM     arg2);

typedef void (*ESockIODTor)(ErlNifEnv*       env,
                            ESockDescriptor* descP);
typedef void (*ESockIOStop)(ErlNifEnv*       env,
                            ESockDescriptor* descP);
typedef void (*ESockIODown)(ErlNifEnv*           env,
                            ESockDescriptor*     descP,
                            const ErlNifPid*     pidP,
                            const ErlNifMonitor* monP);

#endif // SOCKET_IO_H__
