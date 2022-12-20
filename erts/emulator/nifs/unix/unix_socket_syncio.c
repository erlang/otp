/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2022-2022. All Rights Reserved.
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
 *  Purpose : UNIX version of the sync I/O utility.
 * ----------------------------------------------------------------------
 *
 * essio = ESock Synchronous I/O
 *
 */

#include "socket_util.h"
#include "socket_io.h"
#include "socket_syncio.h"


/*
 * For "standard" (unix) synchronous I/O, this is just a dummy function.
 */
extern
int essio_init(unsigned int numThreads)
{
    VOID(numThreads);

    return ESOCK_IO_OK;
}


/*
 * For "standard" (unix) synchronous I/O, this is just a dummy function.
 * Also, will we ever call this?
 */
extern
void essio_finish(void)
{
    return;
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_open2(ErlNifEnv*   env,
                         int          fd,
                         ERL_NIF_TERM eopts)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/*
 */
extern
ERL_NIF_TERM essio_open4(ErlNifEnv*   env,
                         int          domain,
                         int          type,
                         int          protocol,
                         ERL_NIF_TERM eopts)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_bind(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ESockAddress*    sockAddrP,
                        SOCKLEN_T        addrLen)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/*  ========================================================================
 */
extern
ERL_NIF_TERM essio_connect(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     connRef,
                           ESockAddress*    addrP,
                           SOCKLEN_T        addrLen)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_listen(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          int              backlog)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_accept(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     accRef)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_send(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ERL_NIF_TERM     sockRef,
                        ERL_NIF_TERM     recvRef,
                        ssize_t          len,
                        int              flags)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_sendto(ErlNifEnv*       env,
                          ESockDescriptor* descP,
                          ERL_NIF_TERM     sockRef,
                          ERL_NIF_TERM     sendRef,
                          ErlNifBinary*    dataP,
                          int              flags,
                          ESockAddress*    toAddrP,
                          SOCKLEN_T        toAddrLen)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_sendmsg(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     sendRef,
                           ERL_NIF_TERM     eMsg,
                           int              flags,
                           ERL_NIF_TERM     eIOV)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_sendfile(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     sockRef,
                            off_t            offset,
                            size_t*          countP,
                            int*             errP)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_recv(ErlNifEnv*       env,
                        ESockDescriptor* descP,
                        ERL_NIF_TERM     sockRef,
                        ERL_NIF_TERM     recvRef,
                        ssize_t          len,
                        int              flags)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_recvfrom(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            ERL_NIF_TERM     sockRef,
                            ERL_NIF_TERM     recvRef,
                            ssize_t          len,
                            int              flags)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_recvmsg(ErlNifEnv*       env,
                           ESockDescriptor* descP,
                           ERL_NIF_TERM     sockRef,
                           ERL_NIF_TERM     recvRef,
                           ssize_t          bufLen,
                           ssize_t          ctrlLen,
                           int              flags)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_close(ErlNifEnv*       env,
                         ESockDescriptor* descP)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_fin_close(ErlNifEnv*       env,
                             ESockDescriptor* descP)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_shutdown(ErlNifEnv*       env,
                            ESockDescriptor* descP,
                            int              how)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_sockname(ErlNifEnv*       env,
                            ESockDescriptor* descP)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}


/* ========================================================================
 */
extern
ERL_NIF_TERM essio_peername(ErlNifEnv*       env,
                            ESockDescriptor* descP)
{
    return enif_raise_exception(env, MKA(env, "notsup"));
}
