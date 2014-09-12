/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */

/*
 * Common structures for both erl_driver.h and erl_nif.h
 */

#ifndef __ERL_DRV_NIF_H__
#define __ERL_DRV_NIF_H__

typedef struct {
    int driver_major_version;
    int driver_minor_version;
    char *erts_version;
    char *otp_release;
    int thread_support;
    int smp_support;
    int async_threads;
    int scheduler_threads;
    int nif_major_version;
    int nif_minor_version;
    int dirty_scheduler_support;
}  ErlDrvSysInfo;

typedef struct {
    int suggested_stack_size;
} ErlDrvThreadOpts;

#if defined(ERL_DRV_DIRTY_SCHEDULER_SUPPORT) || defined(ERL_NIF_DIRTY_SCHEDULER_SUPPORT)
typedef enum {
    ERL_DRV_DIRTY_JOB_CPU_BOUND = 1,
    ERL_DRV_DIRTY_JOB_IO_BOUND  = 2
} ErlDrvDirtyJobFlags;
#endif

#endif  /* __ERL_DRV_NIF_H__ */




