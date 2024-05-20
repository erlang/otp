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

#ifndef _ASM_LOAD_H
#define _ASM_LOAD_H

/*
 * Type for a label.
 */
typedef struct {
    Uint value;           /* Value of label (0 if not known yet). */
    int looprec_targeted; /* Non-zero if this label is the target of a
                           * loop_rec instruction. */
    int lambda_index;     /* The lambda index of this label, or -1 if not
                           * a target of a lambda. */
} Label;

/*
 * This structure associates a code offset with a source code location.
 */

typedef struct {
    int pos; /* Position in code */
    int loc; /* Location in source code */
} LineInstr;

/* This structure contains all information about the module being loaded. */
struct LoaderState_ {
    /*
     * The following are used mainly for diagnostics.
     */

    Eterm group_leader; /* Group leader (for diagnostics). */
    Eterm module;       /* Tagged atom for module name. */
    Eterm function;     /* Tagged atom for current function (or 0 if none). */
    unsigned arity;     /* Arity for current function. */

    /*
     * Used for code loading (mainly).
     */
    int specific_op; /* Specific opcode (-1 if not found). */

    const BeamCodeHeader *code_hdr; /* Actual code header */
    BeamCodeHeader *load_hdr;       /* Code header during load */

    int codev_size; /* Size of code buffer in words. */
    int ci;         /* Current index into loaded code buffer. */
    Label *labels;
    unsigned loaded_size; /* Final size of code when loaded. */
    int may_load_nif;     /* true if NIFs may later be loaded for this module */
    const ErtsCodeInfo *on_load; /* Pointer to the on_load function, if any */
    unsigned max_opcode;         /* Highest opcode used in module */

    /*
     * Generic instructions.
     */
    BeamOp *genop; /* The last generic instruction seen. */

    BifEntry **bif_imports;

    /*
     * Line table.
     */
    LineInstr *line_instr;   /* Line instructions */
    unsigned int current_li; /* Current line instruction */
    unsigned int *func_line; /* Mapping from function to first line instr */

    /*
     * Coverage tables used during loading.
     */
    void *coverage;
    byte *line_coverage_valid;
    unsigned int current_index;
    unsigned int *loc_index_to_cover_id;

    /* Translates lambda indexes to the literal holding their FunRef.
     *
     * Lambdas that lack an environment are represented by an ErlFunThing that
     * is immediately followed by an FunRef. */
    SWord *fun_refs;

    void *ba; /* Assembler used to create x86 assembly */

    const void *executable_region; /* Native module after codegen */
    void *writable_region; /* Native module after codegen, writable mapping */

    int function_number;
    int last_label;

    BeamOpAllocator op_allocator;
    BeamFile beam;
};

#endif
