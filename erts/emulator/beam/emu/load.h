
/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2023. All Rights Reserved.
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

#ifndef _EMU_LOAD_H
#define _EMU_LOAD_H

/*
 * Type for a reference to a label that must be patched.
 */

typedef struct {
    Uint pos;                   /* Position of label reference to patch. */
    Uint offset;                /* Offset from patch location.  */
    int packed;                 /* 0 (not packed), 1 (lsw), 2 (msw) */
} LabelPatch;

/*
 * Type for a label.
 */
typedef struct {
    Uint value;                /* Value of label (0 if not known yet). */
    Uint looprec_targeted;     /* Non-zero if this label is the target of a
                                * loop_rec instruction.
                                */
    LabelPatch* patches;        /* Array of label patches. */
    Uint num_patches;           /* Number of patches in array. */
    Uint num_allocated;         /* Number of allocated patches. */
} Label;

/*
 * This structure keeps load-time information about a literal.
 */

typedef struct {
    Eterm term;                     /* The tagged term (in the heap). */
    ErlHeapFragment* heap_frags;
} Literal;

/*
 * This structure keeps information about an operand that needs to be
 * patched to contain the correct address of a literal when the code is
 * frozen.
 */

typedef struct literal_patch {
    Uint pos;                        /* Position in code */
    struct literal_patch *next;
} LiteralPatch;

/*
 * This structure keeps information about an operand that needs to be
 * patched to contain the correct address for an address into the string table.
 */

typedef struct string_patch {
    Uint pos;                        /* Position in code */
    struct string_patch *next;
} StringPatch;

typedef struct lambda_patch {
    Uint pos;                        /* Position in code */
    struct lambda_patch *next;
} LambdaPatch;

/*
 * This structure associates a code offset with a source code location.
 */

typedef struct {
    int pos;                    /* Position in code */
    int loc;                    /* Location in source code */
} LineInstr;

/*
 * This structure contains all information about the module being loaded.
 */
struct LoaderState_ {
    /*
     * The following are used mainly for diagnostics.
     */

    Eterm group_leader;        /* Group leader (for diagnostics). */
    Eterm module;              /* Tagged atom for module name. */
    Eterm function;            /* Tagged atom for current function
                                * (or 0 if none).
                                */
    unsigned arity;            /* Arity for current function. */

    /*
     * Used for code loading (mainly).
     */

    int specific_op;           /* Specific opcode (-1 if not found). */
    BeamCodeHeader* code_hdr;   /* Code header */

    BeamInstr* codev;          /* Loaded code buffer */
    int        codev_size;     /* Size of code buffer in words. */
    int ci;                    /* Current index into loaded code buffer. */
    Label* labels;
    unsigned loaded_size;      /* Final size of code when loaded. */
    int may_load_nif;          /* true if NIFs may be loaded for this module */
    int on_load;               /* Index in the code for the on_load function
                                * (or 0 if there is no on_load function)
                                */

    /*
     * Generic instructions.
     */
    BeamOp* genop;		/* The last generic instruction seen. */

    BifEntry **bif_imports;

    BeamInstr catches;		/* Linked list of catch_yf instructions. */
    BeamInstr *import_patches; /* Linked lists of import entries. */

    LambdaPatch* lambda_patches; /* Linked list of position into fun table to patch. */
    LiteralPatch* literal_patches; /* Operands that need to be patched. */
    StringPatch* string_patches; /* Linked list of position into string table to patch. */

    /*
     * Line table.
     */
    LineInstr* line_instr;	/* Line instructions */
    unsigned int current_li;	/* Current line instruction */
    unsigned int* func_line;	/* Mapping from function to first line instr */

    /* Translates lambda indexes to the literal holding their FunRef.
     *
     * Lambdas that lack an environment are represented by an ErlFunThing that
     * is immediately followed by an FunRef. */
    SWord *fun_refs;

    int otp_20_or_higher;

    Uint last_func_start;
    int function_number;
    int last_label;

    BeamOpAllocator op_allocator;
    BeamFile beam;
};

#endif
