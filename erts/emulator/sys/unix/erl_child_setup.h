/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2015-2017. All Rights Reserved.
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
 * This file defines the interface between erts and child_setup.
 */

#ifndef _ERL_UNIX_FORKER_H
#define _ERL_UNIX_FORKER_H

#include "sys.h"

#ifdef __FreeBSD__
/* The freebsd sendmsg man page explicitly states that
   you should not close fds before they are known
   to have reached the other side, so this Ack protects
   against that. */
#define FORKER_PROTO_START_ACK 1
#endif

#define FORKER_ARGV_NO_OF_ARGS  3
#define FORKER_ARGV_PROGNAME_IX	0    /* Program name                          */
#define FORKER_ARGV_MAX_FILES	1    /* max_files                             */

#define FORKER_FLAG_USE_STDIO   (1 << 0)    /* dup the pipe to stdin/stderr   */
#define FORKER_FLAG_EXIT_STATUS (1 << 1)    /* send the exit status to parent */
#define FORKER_FLAG_DO_READ     (1 << 2)    /* dup write fd */
#define FORKER_FLAG_DO_WRITE    (1 << 3)    /* dup read  fd  */

#if SIZEOF_VOID_P == SIZEOF_LONG
typedef unsigned long ErtsSysPortId;
#elif SIZEOF_VOID_P == SIZEOF_INT
typedef unsigned int ErtsSysPortId;
#elif SIZEOF_VOID_P == SIZEOF_LONG_LONG
typedef unsigned long long ErtsSysPortId;
#endif

typedef struct ErtsSysForkerProto_ {
    enum {
        ErtsSysForkerProtoAction_Start,
        ErtsSysForkerProtoAction_StartAck,
        ErtsSysForkerProtoAction_Go,
        ErtsSysForkerProtoAction_SigChld,
        ErtsSysForkerProtoAction_Ack
    } action;
    union {
        struct {
            ErtsSysPortId port_id;
            int fds[3];
        } start;
        struct {
            pid_t os_pid;
            int error_number;
        } go;
        struct {
            ErtsSysPortId port_id;
            int error_number;
        } sigchld;
    } u;
} ErtsSysForkerProto;

#endif /* #ifndef _ERL_UNIX_FORKER_H */
