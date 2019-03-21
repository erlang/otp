/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
 */
#include <erl_nif.h>

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <sys/types.h>

#ifdef __WIN32__
#include <winsock2.h>
#define sock_close(s) closesocket(s)
#else
#include <sys/socket.h>
#include <unistd.h>
#define sock_close(s) close(s)
#endif

#define sock_open(af, type, proto)  socket((af), (type), (proto))

static ERL_NIF_TERM getsockfd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int fd;

    fd = sock_open(AF_INET, SOCK_STREAM, 0);
    return enif_make_int(env, fd);
}

static ERL_NIF_TERM closesockfd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int fd;

    enif_get_int(env, argv[0], &fd);

    sock_close(fd);

    return enif_make_int(env, fd);
}

static ErlNifFunc nif_funcs[] =
{
    {"getsockfd", 0, getsockfd},
    {"closesockfd", 1, closesockfd}
};

ERL_NIF_INIT(gen_tcp_api_SUITE,nif_funcs,NULL,NULL,NULL,NULL)
