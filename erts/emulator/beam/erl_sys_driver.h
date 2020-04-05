/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2020. All Rights Reserved.
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

/*
 * Include file for erlang driver writers.
 */

#ifndef __ERL_SYS_DRIVER_H__
#define __ERL_SYS_DRIVER_H__

#ifdef __ERL_DRIVER_H__
#error erl_sys_driver.h cannot be included after erl_driver.h
#endif

#define ERL_SYS_DRV

typedef long ErlDrvEvent; /* An event to be selected on. */

typedef struct _SysDriverOpts SysDriverOpts;

#include "erl_driver.h"

/*
 * This structure contains options to all built in drivers.
 * None of the drivers use all of the fields.
 */

struct _SysDriverOpts {
    Uint ifd;			/* Input file descriptor (fd driver). */
    Uint ofd;			/* Outputfile descriptor (fd driver). */
    int packet_bytes;		/* Number of bytes in packet header. */
    int read_write;		/* Read and write bits. */
    int use_stdio;		/* Use standard I/O: TRUE or FALSE. */
    int redir_stderr;           /* Redirect stderr to stdout: TRUE/FALSE. */
    int hide_window;		/* Hide this windows (Windows). */
    int exit_status;		/* Report exit status of subprocess. */
    int overlapped_io;          /* Only has effect on windows NT et al */
    erts_osenv_t envir;		/* Environment of the port process */
    char **argv;                /* Argument vector in Unix'ish format. */
    char *wd;			/* Working directory. */
    unsigned spawn_type;        /* Bitfield of ERTS_SPAWN_DRIVER | 
				   ERTS_SPAWN_EXTERNAL | both*/ 
    int parallelism;            /* Optimize for parallelism */
    ErlDrvSizeT high_watermark;
    ErlDrvSizeT low_watermark;
    ErlDrvSizeT high_msgq_watermark;
    ErlDrvSizeT low_msgq_watermark;
    char port_watermarks_set;
    char msgq_watermarks_set;
};

#endif




