/*
 * %CopyrightBegin%
 *
 * Copyright Dustin Sallings, Michal Ptaszek, Scott Lystig Fritchie 2011-2017.
 * All Rights Reserved.
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

#ifndef __DTRACE_WRAPPER_H
#define __DTRACE_WRAPPER_H

#define DTRACE_TERM_BUF_SIZE 256

/*
 * Some varieties of SystemTap macros do not like statically-sized
 * char[N] buffers.  (For example, CentOS 6's macros.)
 * So, we'll play a game to humor them.
 *
 * The code necessary to play nice with CentOS 6's SystemTap looks
 * stupid to a C programmer's eyes, so we hide the ugliness with this
 * macro, which expands:
 *
 *    DTRACE_CHARBUF(proc_name, 64);
 *
 * to become:
 *
 *    char proc_name_BUFFER[64], *proc_name = proc_name_BUFFER;
 */

#define DTRACE_CHARBUF(name, size) \
    char name##_BUFFER[size], *name = name##_BUFFER

#define DTRACE_CHARBUF_NAME(name) name##_BUFFER

#if defined(USE_DYNAMIC_TRACE) && defined(USE_VM_PROBES) 

#include "erlang_dtrace.h"

#define DTRACE_ENABLED(name)                         \
    erlang_##name##_enabled()
#define DTRACE0(name)                                \
    erlang_##name()
#define DTRACE1(name, a0)                            \
    erlang_##name(a0)
#define DTRACE2(name, a0, a1)                        \
    erlang_##name((a0), (a1))
#define DTRACE3(name, a0, a1, a2)                    \
    erlang_##name((a0), (a1), (a2))
#define DTRACE4(name, a0, a1, a2, a3)                \
    erlang_##name((a0), (a1), (a2), (a3))
#define DTRACE5(name, a0, a1, a2, a3, a4)            \
    erlang_##name((a0), (a1), (a2), (a3), (a4))
#define DTRACE6(name, a0, a1, a2, a3, a4, a5)        \
    erlang_##name((a0), (a1), (a2), (a3), (a4), (a5))
#define DTRACE7(name, a0, a1, a2, a3, a4, a5, a6)    \
    erlang_##name((a0), (a1), (a2), (a3), (a4), (a5), (a6))
#define DTRACE10(name, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) \
    erlang_##name((a0), (a1), (a2), (a3), (a4), (a5), (a6), (a7), (a8), (a9))
#define DTRACE11(name, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) \
    erlang_##name((a0), (a1), (a2), (a3), (a4), (a5), (a6), (a7), (a8), (a9), (a10))

#if defined(_SDT_PROBE) && !defined(STAP_PROBE11)
/* SLF: This is Ubuntu 11-style SystemTap hackery */
/* workaround for missing STAP macro */
#define STAP_PROBE11(provider,name,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) \
  _SDT_PROBE(provider, name, 11, \
             (arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11))
#define _SDT_ASM_OPERANDS_11(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) \
  _SDT_ASM_OPERANDS_10(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,arg10), \
    _SDT_ARG(11, arg11)
#endif

#ifdef STAP_PROBE_ADDR
/* SLF: This is CentOS 5-style SystemTap hackery */
/* SystemTap compat mode cannot support 11 args. We'll ignore the 11th */
#define STAP_PROBE11(provider,probe,parm1,parm2,parm3,parm4,parm5,parm6,parm7,parm8,parm9,parm10,parm11) \
    STAP_PROBE10(provider,probe,(parm1),(parm2),(parm3),(parm4),(parm5),(parm6),(parm7),(parm8),(parm9),(parm10))
#endif /* STAP_PROBE_ADDR */

#else   /* USE_DYNAMIC_TRACE && USE_VM_PROBES */

/* Render all macros to do nothing */
#define DTRACE_ENABLED(name)                         0
#define DTRACE0(name)                                do {} while (0)
#define DTRACE1(name, a0)                            do {} while (0)
#define DTRACE2(name, a0, a1)                        do {} while (0)
#define DTRACE3(name, a0, a1, a2)                    do {} while (0)
#define DTRACE4(name, a0, a1, a2, a3)                do {} while (0)
#define DTRACE5(name, a0, a1, a2, a3, a4)            do {} while (0)
#define DTRACE6(name, a0, a1, a2, a3, a4, a5)        do {} while (0)
#define DTRACE7(name, a0, a1, a2, a3, a4, a5, a6)    do {} while (0)
#define DTRACE10(name, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) \
                                                     do {} while (0)
#define DTRACE11(name, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) \
                                                     do {} while (0)

#endif  /* USE_DYNAMIC_TRACE && USE_VM_PROBES */

#endif  /* __DTRACE_WRAPPER_H */
