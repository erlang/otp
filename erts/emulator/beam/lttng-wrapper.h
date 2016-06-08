/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

#ifndef __LTTNG_WRAPPER_H__
#define __LTTNG_WRAPPER_H__

#ifdef USE_LTTNG

#include "erlang_lttng.h"
#define USE_LTTNG_VM_TRACEPOINTS

#define LTTNG_BUFFER_SZ      (256)
#define LTTNG_PROC_BUFFER_SZ (16)
#define LTTNG_PORT_BUFFER_SZ (20)
#define LTTNG_MFA_BUFFER_SZ  (256)

#define lttng_decl_procbuf(Name) \
    char Name[LTTNG_PROC_BUFFER_SZ]

#define lttng_decl_portbuf(Name) \
    char Name[LTTNG_PORT_BUFFER_SZ]

#define lttng_decl_mfabuf(Name) \
    char Name[LTTNG_MFA_BUFFER_SZ]

#define lttng_decl_carrier_stats(Name) \
    lttng_carrier_stats_t Name##_STATSTRUCT, *Name = &Name##_STATSTRUCT

#define lttng_pid_to_str(pid, name) \
    erts_snprintf(name, LTTNG_PROC_BUFFER_SZ, "%T", (pid))

#define lttng_portid_to_str(pid, name) \
    erts_snprintf(name, LTTNG_PORT_BUFFER_SZ, "%T", (pid))

#define lttng_proc_to_str(p, name) \
    lttng_pid_to_str(((p) ? (p)->common.id : ERTS_INVALID_PID), name)

#define lttng_port_to_str(p, name) \
    lttng_portid_to_str(((p) ? (p)->common.id : ERTS_INVALID_PORT), name)

#define lttng_mfa_to_str(m,f,a, Name) \
    erts_snprintf(Name, LTTNG_MFA_BUFFER_SZ, "%T:%T/%lu", (Eterm)(m), (Eterm)(f), (Uint)(a))

#define lttng_proc_to_mfa_str(p, Name)                              \
    do {                                                            \
        if (ERTS_PROC_IS_EXITING((p))) {                            \
            strcpy(Name, "<exiting>");                              \
        } else {                                                    \
            BeamInstr *_fptr = find_function_from_pc((p)->i);       \
            if (_fptr) {                                            \
                lttng_mfa_to_str(_fptr[0],_fptr[1],_fptr[2], Name); \
            } else {                                                \
                strcpy(Name, "<unknown>");                          \
            }                                                       \
        }                                                           \
    } while(0)

/* ErtsRunQueue->ErtsSchedulerData->Uint */
#define lttng_rq_to_id(RQ) \
    (RQ)->scheduler->no

#define LTTNG_ENABLED(Name) \
    tracepoint_enabled(org_erlang_otp, Name)

/* include a special LTTNG_DO for do_tracepoint ? */
#define LTTNG1(Name, Arg1) \
    tracepoint(org_erlang_otp, Name, (Arg1))

#define LTTNG2(Name, Arg1, Arg2) \
    tracepoint(org_erlang_otp, Name, (Arg1), (Arg2))

#define LTTNG3(Name, Arg1, Arg2, Arg3) \
    tracepoint(org_erlang_otp, Name, (Arg1), (Arg2), (Arg3))

#define LTTNG4(Name, Arg1, Arg2, Arg3, Arg4) \
    tracepoint(org_erlang_otp, Name, (Arg1), (Arg2), (Arg3), (Arg4))

#define LTTNG5(Name, Arg1, Arg2, Arg3, Arg4, Arg5) \
    tracepoint(org_erlang_otp, Name, (Arg1), (Arg2), (Arg3), (Arg4), (Arg5))

#else /* USE_LTTNG */

#define LTTNG1(Name, Arg1) do {} while(0)
#define LTTNG2(Name, Arg1, Arg2) do {} while(0)
#define LTTNG3(Name, Arg1, Arg2, Arg3) do {} while(0)
#define LTTNG4(Name, Arg1, Arg2, Arg3, Arg4) do {} while(0)
#define LTTNG5(Name, Arg1, Arg2, Arg3, Arg4, Arg5) do {} while(0)

#endif /* USE_LTTNG */
#endif /* __LTTNG_WRAPPER_H__ */
