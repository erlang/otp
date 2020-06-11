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
#include "erl_fun.h"

#define ERTS_BEAM_MAX_OPARGS 8
#define ERTS_BEAM_NUM_CHUNK_TYPES 11

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

/*
 * Type for an operand for a generic instruction.
 */

typedef struct {
    unsigned type;		/* Type of operand. */
    BeamInstr val;		/* Value of operand. */
} GenOpArg;

/*
 * A generic operation.
 */

typedef struct genop {
    unsigned int op;		/* Opcode. */
    int arity;			/* Number of arguments. */
    GenOpArg def_args[ERTS_BEAM_MAX_OPARGS]; /* Default buffer for arguments. */
    GenOpArg* a;		/* The arguments. */
    struct genop* next;		/* Next genop. */
} GenOp;

/*
 * The allocation unit for generic blocks.
 */

typedef struct genop_block {
    GenOp genop[32];
    struct genop_block* next;
} GenOpBlock;

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
    Uint value;			/* Value of label (0 if not known yet). */
    Uint looprec_targeted;	/* Non-zero if this label is the target of a loop_rec
				 * instruction.
				 */
    LabelPatch* patches;        /* Array of label patches. */
    Uint num_patches;           /* Number of patches in array. */
    Uint num_allocated;         /* Number of allocated patches. */
} Label;

/*
 * This structure keeps load-time information about a lambda.
 */

typedef struct {
    ErlFunEntry* fe;		/* Entry in fun table. */
    unsigned label;		/* Label of function entry. */
    Uint32 num_free;		/* Number of free variables. */
    Eterm function;		/* Name of local function. */
    int arity;			/* Arity (including free variables). */
} Lambda;

/*
 * This structure keeps load-time information about a literal.
 */

typedef struct {
    Eterm term;			/* The tagged term (in the heap). */
    ErlHeapFragment* heap_frags;
} Literal;

/*
 * This structure keeps information about an operand that needs to be
 * patched to contain the correct address of a literal when the code is
 * frozen.
 */

typedef struct literal_patch LiteralPatch;
struct literal_patch {
    Uint pos;			/* Position in code */
    LiteralPatch* next;
};

/*
 * This structure keeps information about an operand that needs to be
 * patched to contain the correct address for an address into the string table.
 */

typedef struct string_patch StringPatch;
struct string_patch {
    int pos;			/* Position in code */
    StringPatch* next;
};

/*
 * This structure associates a code offset with a source code location.
 */

typedef struct {
    int pos;			/* Position in code */
    Uint32 loc;			/* Location in source code */
} LineInstr;

/*
 * This structure contains information for an imported function or BIF.
 */
typedef struct {
    Eterm module;		/* Tagged atom for module. */
    Eterm function;		/* Tagged atom for function. */
    int arity;			/* Arity. */
    Uint patches;		/* Index to locations in code to
				 * eventually patch with a pointer into
				 * the export entry.
				 */
    Export *bif;		/* Pointer to export entry if BIF;
				 * NULL otherwise.
				 */
} ImportEntry;

/*
 * This structure contains information for a function exported from a module.
 */

typedef struct {
    Eterm function;		/* Tagged atom for function. */
    int arity;			/* Arity. */
    BeamInstr* address;		/* Address to function in code. */
} ExportEntry;

/*
 * This structure contains all information about the module being loaded.
 */
#define MD5_SIZE 16
typedef struct LoaderState {
    /*
     * The current logical file within the binary.
     */

    char* file_name;		/* Name of file we are reading (usually chunk name). */
    byte* file_p;		/* Current pointer within file. */
    unsigned file_left;		/* Number of bytes left in file. */
    ErlDrvBinary* bin;		/* Binary holding BEAM file (or NULL) */

    /*
     * The following are used mainly for diagnostics.
     */

    Eterm group_leader;		/* Group leader (for diagnostics). */
    Eterm module;		/* Tagged atom for module name. */
    Eterm function;		/* Tagged atom for current function
				 * (or 0 if none).
				 */
    unsigned arity;		/* Arity for current function. */

    /*
     * All found chunks.
     */

    struct {
	byte* start;		/* Start of chunk (in binary). */
	unsigned size;		/* Size of chunk. */
    } chunks[ERTS_BEAM_NUM_CHUNK_TYPES];

    /*
     * Used for code loading (mainly).
     */

    byte* code_start;		/* Start of code file. */
    unsigned code_size;		/* Size of code file. */
    int specific_op;		/* Specific opcode (-1 if not found). */
    unsigned int num_functions; /* Number of functions in module. */
    unsigned int num_labels;	/* Number of labels. */
    struct beam_code_header* hdr; /* Loaded code header */
    BeamInstr* codev;	        /* Loaded code buffer */
    int        codev_size;      /* Size of code buffer in words. */
    int ci;			/* Current index into loaded code buffer. */
    Label* labels;
    StringPatch* string_patches; /* Linked list of position into string table to patch. */
    BeamInstr catches;		/* Linked list of catch_yf instructions. */
    unsigned loaded_size;	/* Final size of code when loaded. */
    byte mod_md5[MD5_SIZE];	/* MD5 for module code. */
    int may_load_nif;           /* true if NIFs may later be loaded for this module */
    int on_load;		/* Index in the code for the on_load function
				 * (or 0 if there is no on_load function)
				 */
    int otp_20_or_higher;       /* Compiled with OTP 20 or higher */
    unsigned max_opcode;        /* Highest opcode used in module */

    /*
     * Atom table.
     */

    unsigned int num_atoms;	/* Number of atoms in atom table. */
    Eterm* atom;		/* Atom table. */

    unsigned int num_exps;	/* Number of exports. */
    ExportEntry* export;	/* Pointer to export table. */

    unsigned int num_imports;   /* Number of imports. */
    ImportEntry* import;	/* Import entry (translated information). */

    /*
     * Generic instructions.
     */
    GenOp* genop;		/* The last generic instruction seen. */
    GenOp* free_genop;		/* List of free genops. */
    GenOpBlock* genop_blocks;	/* List of all block of allocated genops. */

    /*
     * Lambda table.
     */

    unsigned int num_lambdas;	/* Number of lambdas in table. */
    unsigned int lambdas_allocated; /* Size of allocated lambda table. */
    Lambda* lambdas;		/* Pointer to lambdas. */
    Lambda def_lambdas[16];	/* Default storage for lambda table. */
    char* lambda_error;		/* Delayed missing 'FunT' error. */

    /*
     * Literals (constant pool).
     */

    unsigned int num_literals;	/* Number of literals in table. */
    unsigned int allocated_literals; /* Number of literal entries allocated. */
    Literal* literals;		/* Array of literals. */
    LiteralPatch* literal_patches; /* Operands that need to be patched. */
    Uint total_literal_size;	/* Total heap size for all literals. */

    /*
     * Line table.
     */
    BeamInstr* line_item;	/* Line items from the BEAM file. */
    unsigned int num_line_items;/* Number of line items. */
    LineInstr* line_instr;	/* Line instructions */
    unsigned int num_line_instrs; /* Maximum number of line instructions */
    unsigned int current_li;	/* Current line instruction */
    unsigned int* func_line;	/* Mapping from function to first line instr */
    Eterm* fname;		/* List of file names */
    unsigned int num_fnames;	/* Number of filenames in fname table */
    int loc_size;		/* Size of location info in bytes (2/4) */
} LoaderState;

#ifdef DEBUG
# define GARBAGE 0xCC
# define DEBUG_INIT_GENOP(Dst) sys_memset(Dst, GARBAGE, sizeof(GenOp))
#else
# define DEBUG_INIT_GENOP(Dst)
#endif

#define NEW_GENOP(Stp, Dst)                     \
  do {                                          \
    if ((Stp)->free_genop == NULL) {            \
       beam_load_new_genop((Stp));              \
    }                                           \
   Dst = (Stp)->free_genop;                     \
   (Stp)->free_genop = (Stp)->free_genop->next; \
   DEBUG_INIT_GENOP(Dst);                       \
   (Dst)->a = (Dst)->def_args;                  \
  } while (0)                                   \

#define FREE_GENOP(Stp, Genop)                          \
 do {                                                   \
   if ((Genop)->a != (Genop)->def_args) {               \
       erts_free(ERTS_ALC_T_LOADER_TMP, (Genop)->a);    \
   }                                                    \
   (Genop)->next = (Stp)->free_genop;                   \
   (Stp)->free_genop = (Genop);                         \
 } while (0)

#define GENOP_NAME_ARITY(Genop, Name, Arity)    \
  do {                                          \
    (Genop)->op = genop_##Name##_##Arity;       \
    (Genop)->arity = Arity;                     \
  } while (0)

#define GENOP_ARITY(Genop, Arity)                               \
  do {                                                          \
   ASSERT((Genop)->a == (Genop)->def_args);                     \
   (Genop)->arity = (Arity);                                    \
   (Genop)->a = erts_alloc(ERTS_ALC_T_LOADER_TMP,               \
                           (Genop)->arity * sizeof(GenOpArg));  \
  } while (0)

void beam_load_new_genop(LoaderState* stp);
Uint beam_load_new_literal(LoaderState* stp, Eterm** hpp, Uint heap_size);
int beam_load_new_label(LoaderState* stp);
int beam_load_find_literal(LoaderState* stp, Eterm needle, Uint *idx);

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

/*
 * The transform engine.
 */

int erts_transform_engine(LoaderState* st);

#define TE_OK 0
#define TE_FAIL (-1)
#define TE_SHORT_WINDOW (-2)

int erts_beam_eval_predicate(unsigned int op, LoaderState* st,
                             GenOpArg var[], GenOpArg* rest_args);
GenOp* erts_beam_execute_transform(unsigned int op, LoaderState* st,
                                   GenOpArg var[], GenOpArg* rest_args);



#  define BEAM_NATIVE_MIN_FUNC_SZ 4

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
