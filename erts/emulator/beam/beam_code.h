/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2024. All Rights Reserved.
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

#ifndef _BEAM_CODE_H
#define _BEAM_CODE_H

#include "sys.h"
#include "erl_process.h"
#include "erl_md5.h"

/* Macros for manipulating locations. */
#define LINE_INVALID_LOCATION (0)

#define IS_VALID_LOCATION(File, Line) \
    ((unsigned) (File) < 255 && (unsigned) (Line) < ((1 << 24) - 1))
/* Builds a location entry, silently ignoring unrepresentable locations. */
#define MAKE_LOCATION(File, Line)        \
    (IS_VALID_LOCATION((File), (Line)) ? \
        (((File) << 24) | (Line)) :      \
        LINE_INVALID_LOCATION)
#define LOC_FILE(Loc) ((Loc) >> 24)
#define LOC_LINE(Loc) ((Loc) & ((1 << 24)-1))

/* Minimum size for NIFs and SNIFs, in words. */
#ifdef BEAMASM
#  define BEAM_NATIVE_MIN_FUNC_SZ 30
#else
#  define BEAM_NATIVE_MIN_FUNC_SZ 4
#endif

#define MD5_SIZE MD5_DIGEST_LENGTH

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
    const byte *attr_ptr;
    UWord attr_size;
    UWord attr_size_on_heap;

    /*
     * The compilation information retrieved by Mod:module_info(compile).
     */
    const byte *compile_ptr;
    UWord compile_size;
    UWord compile_size_on_heap;

    /*
     * Literal area (constant pool).
     */
    struct ErtsLiteralArea_ *literal_area;

    /*
     * Pointer to the on_load function (or NULL if none).
     */
    const ErtsCodeInfo *on_load;

    /*
     * Pointer to the line table (or NULL if none).
     */
    const BeamCodeLineTab *line_table;

#ifdef BEAMASM

    /*
     * Coverage support.
     */
    Uint coverage_mode;
    void *coverage;
    byte *line_coverage_valid;
    Uint32 *loc_index_to_cover_id;
    Uint line_coverage_len;

#endif

    /*
     * Pointer to the module MD5 sum (16 bytes)
     */
    const byte *md5_ptr;

    /*
     * Boolean array with functions declared as -nifs().
     * Indexed same as functions[].
     * NULL if no -nifs().
     */
    byte* are_nifs;

    /*
     * Start of function pointer table.  This table contains pointers to
     * all functions in the module plus an additional pointer just beyond
     * the end of the last function.
     *
     * The actual loaded code (for the first function) start just beyond
     * this table.
     */
    const ErtsCodeInfo *functions[1];
} BeamCodeHeader;

/*
 * Layout of the line table.
 */
struct BeamCodeLineTab_ {
    const Eterm* fname_ptr;
    int loc_size;
    union {
        Uint16* p2;
        Uint32* p4;
    } loc_tab;
    const void** func_tab[1];
};

/* Total code size in bytes */
extern Uint erts_total_code_size;

struct ErtsLiteralArea_;
void erts_release_literal_area(struct ErtsLiteralArea_* literal_area);

struct erl_fun_entry;
void erts_purge_state_add_fun(struct erl_fun_entry *fe);
Export *erts_suspend_process_on_pending_purge_lambda(Process *c_p,
                                                     struct erl_fun_entry*);

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

#endif
