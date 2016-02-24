/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2012. All Rights Reserved.
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
#ifndef _ERL_DEBUG_H_
#define _ERL_DEBUG_H_
#ifdef DEBUG

#include "erl_term.h"

#ifdef HIPE
#include "hipe_debug.h"
#endif

/* Heap areas will be filled with this value when they are deallocated
 * after a garbage collection. This value used to be 0xff, but that is
 * an immediate and might not crash the system if it is encountered.
 * The value is now 0x01, the cons of death.
 */
#define DEBUG_BAD_BYTE 0x01
#define DEBUG_BAD_WORD 0x01010101

/*
 * VERBOSE. Use the -v option to enable the different categories.
 */
#define VERBOSE(flag, format) (flag & verbose ? erts_printf format : 0)

#define DEBUG_DEFAULT      0x0000    /* No flags are set per default         */
#define DEBUG_SYSTEM       0x0001    /* Misc system info at startup and end  */
#define DEBUG_PRIVATE_GC   0x0002    /* GC of private heaps                  */
#define DEBUG_ALLOCATION   0x0004    /* HAlloc. To find holes in the heap    */
#define DEBUG_MESSAGES     0x0008    /* Message passing                      */
#define DEBUG_THREADS      0x0010    /* Thread-related stuff                 */
#define DEBUG_PROCESSES    0x0020    /* Process creation and removal         */
#define DEBUG_MEMORY       0x0040    /* Display results of memory checks     */
#define DEBUG_SHCOPY       0x0080    /* Sharing-preserving copying of terms  */

extern Uint32 verbose;

void upp(byte*, size_t);
void pat(Eterm);
void pinfo(void);
void pp(Process*);
void ppi(Eterm);
void pba(Process*, int);
void td(Eterm);
void ps(Process*, Eterm*);

#undef ERTS_OFFHEAP_DEBUG
#define ERTS_OFFHEAP_DEBUG

#else /* Non-debug mode */

#define VERBOSE(flag,format)

#endif /* DEBUG */

#ifdef ERTS_OFFHEAP_DEBUG
#define ERTS_CHK_OFFHEAP(P) erts_check_off_heap((P))
#define ERTS_CHK_OFFHEAP2(P, HT) erts_check_off_heap2((P), (HT))
void erts_check_off_heap(Process *);
void erts_check_off_heap2(Process *, Eterm *);
#else
#define ERTS_CHK_OFFHEAP(P)
#define ERTS_CHK_OFFHEAP2(P, HT)
#endif

/*
 * These functions can be handy when developing, and perhaps useful
 * even outside debugging.
 */
extern void erts_check_off_heap(Process *p);
extern void erts_check_stack(Process *p);
extern void erts_check_heap(Process *p);
extern void erts_check_memory(Process *p, Eterm *start, Eterm *end);
extern void verify_process(Process *p);
extern void print_tagged_memory(Eterm *start, Eterm *end);
extern void print_untagged_memory(Eterm *start, Eterm *end);
extern void print_memory(Process *p);
extern void print_memory_info(Process *p);
#endif /* _ERL_DEBUG_H_ */
