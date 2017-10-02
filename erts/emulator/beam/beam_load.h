/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2017. All Rights Reserved.
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

#ifndef _BEAM_LOAD_H
#  define _BEAM_LOAD_H

#include "beam_opcodes.h"
#include "erl_process.h"

Eterm beam_make_current_old(Process *c_p, ErtsProcLocks c_p_locks,
			    Eterm module);

typedef struct gen_op_entry {
   char* name;
   int arity;
   int specific;
   int num_specific;
   int transform;
} GenOpEntry;

extern const GenOpEntry gen_opc[];

struct ErtsLiteralArea_;

/*
 * The following variables keep a sorted list of address ranges for
 * each module.  It allows us to quickly find a function given an
 * instruction pointer.
 */

/* Total code size in bytes */
extern Uint erts_total_code_size;

typedef struct BeamCodeLineTab_ BeamCodeLineTab;

/*
 * Header of code chunks which contains additional information
 * about the loaded module.
 */
typedef struct beam_code_header {
    /*
     * Number of functions.
     */
    UWord num_functions;

    /*
     * The attributes retrieved by Mod:module_info(attributes).
     */
    byte* attr_ptr;
    UWord attr_size;
    UWord attr_size_on_heap;

    /*
     * The compilation information retrieved by Mod:module_info(compile).
     */
    byte* compile_ptr;
    UWord compile_size;
    UWord compile_size_on_heap;

    /*
     * Literal area (constant pool).
     */
    struct ErtsLiteralArea_ *literal_area;

    /*
     * Pointer to the on_load function (or NULL if none).
     */
    BeamInstr* on_load_function_ptr;

    /*
     * Pointer to the line table (or NULL if none).
     */
    BeamCodeLineTab* line_table;

    /*
     * Pointer to the module MD5 sum (16 bytes)
     */
    byte* md5_ptr;

    /*
     * Start of function pointer table.  This table contains pointers to
     * all functions in the module plus an additional pointer just beyond
     * the end of the last function.
     *
     * The actual loaded code (for the first function) start just beyond
     * this table.
     */
    ErtsCodeInfo* functions[1];

}BeamCodeHeader;

#  define BEAM_NIF_MIN_FUNC_SZ 4

void erts_release_literal_area(struct ErtsLiteralArea_* literal_area);
int erts_is_module_native(BeamCodeHeader* code);
int erts_is_function_native(ErtsCodeInfo*);
void erts_beam_bif_load_init(void);
struct erl_fun_entry;
void erts_purge_state_add_fun(struct erl_fun_entry *fe);
Export *erts_suspend_process_on_pending_purge_lambda(Process *c_p,
                                                     struct erl_fun_entry*);

/*
 * Layout of the line table.
 */
struct BeamCodeLineTab_ {
    Eterm* fname_ptr;
    int loc_size;
    union {
        Uint16* p2;
        Uint32* p4;
    }loc_tab;
    const BeamInstr** func_tab[1];
};

#define LINE_INVALID_LOCATION (0)

/*
 * Macros for manipulating locations.
 */

#define IS_VALID_LOCATION(File, Line) \
    ((unsigned) (File) < 255 && (unsigned) (Line) < ((1 << 24) - 1))
#define MAKE_LOCATION(File, Line) (((File) << 24) | (Line))
#define LOC_FILE(Loc) ((Loc) >> 24)
#define LOC_LINE(Loc) ((Loc) & ((1 << 24)-1))


/*
 * MFA event debug "tracing" usage:
 *
 * #define ENABLE_DBG_TRACE_MFA
 * call dbg_set_traced_mfa("mymod","myfunc",arity)
 * for the function(s) to trace, in some init function.
 *
 * Run and get stderr printouts when interesting things happen to your MFA.
 */
#ifdef  ENABLE_DBG_TRACE_MFA

void dbg_set_traced_mfa(const char* m, const char* f, Uint a);
int dbg_is_traced_mfa(Eterm m, Eterm f, Uint a);
void dbg_vtrace_mfa(unsigned ix, const char* format, ...);
#define DBG_TRACE_MFA(M,F,A,FMT, ...) do {\
    unsigned ix;\
    if ((ix=dbg_is_traced_mfa(M,F,A))) \
        dbg_vtrace_mfa(ix, FMT"\n", ##__VA_ARGS__);\
  }while(0)

#define DBG_TRACE_MFA_P(MFA, FMT, ...) \
        DBG_TRACE_MFA((MFA)->module, (MFA)->function, (MFA)->arity, FMT, ##__VA_ARGS__)

#else
#  define dbg_set_traced_mfa(M,F,A)
#  define DBG_TRACE_MFA(M,F,A,FMT, ...)
#  define DBG_TRACE_MFA_P(MFA,FMT, ...)
#endif /* ENABLE_DBG_TRACE_MFA */

#endif /* _BEAM_LOAD_H */
