/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

extern GenOpEntry gen_opc[];

#ifdef NO_JUMP_TABLE 
#define BeamOp(Op) (Op)
#else
extern void** beam_ops;
#define BeamOp(Op) beam_ops[(Op)]
#endif


extern BeamInstr beam_debug_apply[];
extern BeamInstr* em_call_error_handler;
extern BeamInstr* em_apply_bif;
extern BeamInstr* em_call_nif;


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
    Eterm* literals_start;
    Eterm* literals_end;
    struct erl_off_heap_header* literals_off_heap;

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
    BeamInstr* functions[1];

}BeamCodeHeader;

int erts_is_module_native(BeamCodeHeader* code);

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

#endif /* _BEAM_LOAD_H */
