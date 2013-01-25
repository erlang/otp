/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2013. All Rights Reserved.
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
   int min_window;
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

/*
 * The following variables keep a sorted list of address ranges for
 * each module.  It allows us to quickly find a function given an
 * instruction pointer.
 */

/* Total code size in bytes */
extern Uint erts_total_code_size;
/*
 * Index into start of code chunks which contains additional information
 * about the loaded module.
 *
 * First number of functions.
 */

#define MI_NUM_FUNCTIONS     0

/*
 * The attributes retrieved by Mod:module_info(attributes).
 */

#define MI_ATTR_PTR          1
#define MI_ATTR_SIZE	     2
#define MI_ATTR_SIZE_ON_HEAP 3

/*
 * The compilation information retrieved by Mod:module_info(compile).
 */

#define MI_COMPILE_PTR          4
#define MI_COMPILE_SIZE         5
#define MI_COMPILE_SIZE_ON_HEAP 6

/*
 * Literal area (constant pool).
 */
#define MI_LITERALS_START	7
#define MI_LITERALS_END		8
#define MI_LITERALS_OFF_HEAP	9


/*
 * Pointer to the on_load function (or NULL if none).
 */
#define MI_ON_LOAD_FUNCTION_PTR 10

/*
 * Pointer to the line table (or NULL if none).
 */
#define MI_LINE_TABLE 11

/*
 * Start of function pointer table.  This table contains pointers to
 * all functions in the module plus an additional pointer just beyond
 * the end of the last function.
 *
 * The actual loaded code (for the first function) start just beyond
 * this table.
 */

#define MI_FUNCTIONS         12

/*
 * Layout of the line table.
 */

#define MI_LINE_FNAME_PTR 0
#define MI_LINE_LOC_TAB 1
#define MI_LINE_LOC_SIZE 2
#define MI_LINE_FUNC_TAB 3

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
