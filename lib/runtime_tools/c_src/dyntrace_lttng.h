/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2016. All Rights Reserved.
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

#undef TRACEPOINT_PROVIDER
#define TRACEPOINT_PROVIDER com_ericsson_dyntrace

#if !defined(DYNTRACE_LTTNG_H) || defined(TRACEPOINT_HEADER_MULTI_READ)
#define DYNTRACE_LTTNG_H

#include <lttng/tracepoint.h>

#define LTTNG1(Name, Arg1) \
    tracepoint(com_ericsson_dyntrace, Name, (Arg1))

#define LTTNG2(Name, Arg1, Arg2) \
    tracepoint(com_ericsson_dyntrace, Name, (Arg1), (Arg2))

#define LTTNG3(Name, Arg1, Arg2, Arg3) \
    tracepoint(com_ericsson_dyntrace, Name, (Arg1), (Arg2), (Arg3))

#define LTTNG4(Name, Arg1, Arg2, Arg3, Arg4) \
    tracepoint(com_ericsson_dyntrace, Name, (Arg1), (Arg2), (Arg3), (Arg4))

#define LTTNG5(Name, Arg1, Arg2, Arg3, Arg4, Arg5) \
    tracepoint(com_ericsson_dyntrace, Name, (Arg1), (Arg2), (Arg3), (Arg4), (Arg5))

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

/* Process scheduling */

TRACEPOINT_EVENT(
    com_ericsson_dyntrace,
    process_spawn,
    TP_ARGS(
        char*, p,
        char*, parent,
        char*, mfa
    ),
    TP_FIELDS(
        ctf_string(pid, p)
        ctf_string(parent, parent)
        ctf_string(entry, mfa)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_dyntrace,
    process_link,
    TP_ARGS(
        char*, from,
        char*, to,
        char*, type
    ),
    TP_FIELDS(
        ctf_string(from, from)
        ctf_string(to, to)
        ctf_string(type, type)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_dyntrace,
    process_exit,
    TP_ARGS(
        char*, p,
        char*, reason
    ),
    TP_FIELDS(
        ctf_string(pid, p)
        ctf_string(reason, reason)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_dyntrace,
    process_register,
    TP_ARGS(
        char*, pid,
        char*, name,
        char*, type
    ),
    TP_FIELDS(
        ctf_string(pid, pid)
        ctf_string(name, name)
        ctf_string(type, type)
    )
)

/* Process Memory */

TRACEPOINT_EVENT(
    com_ericsson_dyntrace,
    gc_minor_start,
    TP_ARGS(
        char*, p,
        int, need
    ),
    TP_FIELDS(
        ctf_string(pid, p)
        ctf_integer(int, need, need)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_dyntrace,
    gc_minor_end,
    TP_ARGS(
        char*, p,
        int, reclaimed
    ),
    TP_FIELDS(
        ctf_string(pid, p)
        ctf_integer(int, reclaimed, reclaimed)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_dyntrace,
    gc_major_start,
    TP_ARGS(
        char*, p,
        int, need
    ),
    TP_FIELDS(
        ctf_string(pid, p)
        ctf_integer(int, need, need)
    )
)

TRACEPOINT_EVENT(
    com_ericsson_dyntrace,
    gc_major_end,
    TP_ARGS(
        char*, p,
        int, reclaimed
    ),
    TP_FIELDS(
        ctf_string(pid, p)
        ctf_integer(int, reclaimed, reclaimed)
    )
)

#endif /* DYNTRACE_LTTNG_H */

#undef TRACEPOINT_INCLUDE
#define TRACEPOINT_INCLUDE "./dyntrace_lttng.h"

/* This part must be outside protection */
#include <lttng/tracepoint-event.h>
