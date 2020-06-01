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
#include "beam_file.h"

#include "erl_process.h"
#include "erl_bif_table.h"
#include "erl_fun.h"

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
#define MD5_SIZE 16
typedef struct LoaderState {
    /*
     * The current logical file within the binary.
     */
    ErlDrvBinary* bin;         /* Binary holding BEAM file (or NULL) */

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
    struct beam_code_header *hdr;       /* Loaded code header */

    BeamInstr* codev;          /* Loaded code buffer */
    int        codev_size;     /* Size of code buffer in words. */
    int ci;                    /* Current index into loaded code buffer. */
    Label* labels;
    unsigned loaded_size;      /* Final size of code when loaded. */
    int may_load_nif;          /* true if NIFs may be loaded for this module */
    int on_load;               /* Index in the code for the on_load function
                                * (or 0 if there is no on_load function)
                                */
    int otp_20_or_higher;      /* Compiled with OTP 20 or higher */

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

    /* !! */
    BeamOpAllocator op_allocator;
    BeamFile beam;
} LoaderState;

#ifdef DEBUG
# define GARBAGE 0xCC
# define DEBUG_INIT_GENOP(Dst) sys_memset(Dst, GARBAGE, sizeof(BeamOp))
#else
# define DEBUG_INIT_GENOP(Dst)
#endif

#define NEW_GENOP(Stp, Dst)                                   \
  do {                                                        \
      Dst = beamopallocator_new_op(&(Stp)->op_allocator);     \
  } while (0)

#define FREE_GENOP(Stp, Genop)                                \
  do {                                                        \
      beamopallocator_free_op(&(Stp)->op_allocator, (Genop)); \
  } while (0)

#define GENOP_NAME_ARITY(Genop, Name, Arity)    \
  do {                                          \
    (Genop)->op = genop_##Name##_##Arity;       \
    (Genop)->arity = Arity;                     \
  } while (0)

#define GENOP_ARITY(Genop, Arity)                                \
  do {                                                           \
   ASSERT((Genop)->a == (Genop)->def_args);                      \
   (Genop)->arity = (Arity);                                     \
   (Genop)->a = erts_alloc(ERTS_ALC_T_LOADER_TMP,                \
                           (Genop)->arity * sizeof(BeamOpArg));  \
  } while (0)

int beam_load_new_label(LoaderState* stp);

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

} BeamCodeHeader;

/*
 * The transform engine.
 */

int erts_transform_engine(LoaderState* st);

#define TE_OK 0
#define TE_FAIL (-1)
#define TE_SHORT_WINDOW (-2)

int erts_beam_eval_predicate(unsigned int op, LoaderState* st,
                             BeamOpArg var[], BeamOpArg* rest_args);
BeamOp* erts_beam_execute_transform(unsigned int op, LoaderState* st,
                                   BeamOpArg var[], BeamOpArg* rest_args);



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
