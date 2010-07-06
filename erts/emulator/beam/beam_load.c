/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2010. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_version.h"
#include "erl_process.h"
#include "error.h"
#include "erl_driver.h"
#include "bif.h"
#include "external.h"
#include "beam_load.h"
#include "big.h"
#include "erl_bits.h"
#include "beam_catches.h"
#include "erl_binary.h"
#include "erl_zlib.h"

#ifdef HIPE
#include "hipe_bif0.h"
#include "hipe_mode_switch.h"
#include "hipe_arch.h"
#endif

ErlDrvBinary* erts_gzinflate_buffer(char*, int);

#define MAX_OPARGS 8
#define CALLED    0
#define DEFINED   1
#define EXPORTED  2

#ifdef NO_JUMP_TABLE
#  define BeamOpCode(Op) ((BeamInstr)(Op))
#else
#  define BeamOpCode(Op) ((BeamInstr)beam_ops[Op])
#endif

#if defined(WORDS_BIGENDIAN)
# define NATIVE_ENDIAN(F)			\
  if ((F).val & BSF_NATIVE) {			\
     (F).val &= ~(BSF_LITTLE|BSF_NATIVE);	\
  } else {}
#else
# define NATIVE_ENDIAN(F)			\
  if ((F).val & BSF_NATIVE) {			\
      (F).val &= ~BSF_NATIVE;			\
      (F).val |= BSF_LITTLE;			\
  } else {}
#endif

/*
 * Errors returned from tranform_engine().
 */
#define TE_OK 0
#define TE_FAIL (-1)
#define TE_SHORT_WINDOW (-2)

typedef struct {
    Uint value;			/* Value of label (NULL if not known yet). */
    Uint patches;		/* Index (into code buffer) to first location
				 * which must be patched with the value of this label.
				 */
#ifdef ERTS_SMP
    Uint looprec_targeted;	/* Non-zero if this label is the target of a loop_rec
				 * instruction.
				 */
#endif
} Label;

/*
 * Type for a operand for a generic instruction.
 */

typedef struct {
    unsigned type;		/* Type of operand. */
    BeamInstr val;			/* Value of operand. */
    Uint bigarity;		/* Arity for bignumbers (only). */
} GenOpArg;

/*
 * A generic operation.
 */

typedef struct genop {
    int op;			/* Opcode. */
    int arity;			/* Number of arguments. */
    GenOpArg def_args[MAX_OPARGS]; /* Default buffer for arguments. */
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
    BifFunction bf;		/* Pointer to BIF function if BIF;
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

#define MakeIffId(a, b, c, d) \
  (((Uint) (a) << 24) | ((Uint) (b) << 16) | ((Uint) (c) << 8) | (Uint) (d))

#define ATOM_CHUNK 0
#define CODE_CHUNK 1
#define STR_CHUNK 2
#define IMP_CHUNK 3
#define EXP_CHUNK 4
#define NUM_MANDATORY 5

#define LAMBDA_CHUNK 5
#define LITERAL_CHUNK 6
#define ATTR_CHUNK 7
#define COMPILE_CHUNK 8

#define NUM_CHUNK_TYPES (sizeof(chunk_types)/sizeof(chunk_types[0]))

/*
 * An array with all chunk types recognized by the loader.
 */

static Uint chunk_types[] = {
    /*
     * Mandatory chunk types -- these MUST be present.
     */
    MakeIffId('A', 't', 'o', 'm'), /* 0 */
    MakeIffId('C', 'o', 'd', 'e'), /* 1 */
    MakeIffId('S', 't', 'r', 'T'), /* 2 */
    MakeIffId('I', 'm', 'p', 'T'), /* 3 */
    MakeIffId('E', 'x', 'p', 'T'), /* 4 */

    /*
     * Optional chunk types -- the loader will use them if present.
     */
    MakeIffId('F', 'u', 'n', 'T'), /* 5 */
    MakeIffId('L', 'i', 't', 'T'), /* 6 */
    MakeIffId('A', 't', 't', 'r'), /* 7 */
    MakeIffId('C', 'I', 'n', 'f'), /* 8 */
};

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
    Uint heap_size;		/* (Exact) size on the heap. */
    Uint offset;		/* Offset from temporary location to final. */
    Eterm* heap;		/* Heap for term. */
} Literal;

/*
 * This structure keeps information about an operand that needs to be
 * patched to contain the correct address of a literal when the code is
 * frozen.
 */

typedef struct literal_patch LiteralPatch;
struct literal_patch {
    int pos;			/* Position in code */
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
 * This structure contains all information about the module being loaded.
 */  

typedef struct {
    /*
     * The current logical file within the binary.
     */

    char* file_name;		/* Name of file we are reading (usually chunk name). */
    byte* file_p;		/* Current pointer within file. */
    unsigned file_left;		/* Number of bytes left in file. */

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
    } chunks[NUM_CHUNK_TYPES];

    /*
     * Used for code loading (mainly).
     */

    byte* code_start;		/* Start of code file. */
    unsigned code_size;		/* Size of code file. */
    int specific_op;		/* Specific opcode (-1 if not found). */
    int num_functions;		/* Number of functions in module. */
    int num_labels;		/* Number of labels. */
    int code_buffer_size;	/* Size of code buffer in words.  */
    BeamInstr* code;		/* Loaded code. */
    int ci;			/* Current index into loaded code. */
    Label* labels;
    BeamInstr new_bs_put_strings;	/* Linked list of i_new_bs_put_string instructions. */
    StringPatch* string_patches; /* Linked list of position into string table to patch. */
    BeamInstr catches;		/* Linked list of catch_yf instructions. */
    unsigned loaded_size;	/* Final size of code when loaded. */
    byte mod_md5[16];		/* MD5 for module code. */
    int may_load_nif;           /* true if NIFs may later be loaded for this module */  
    int on_load;		/* Index in the code for the on_load function
				 * (or 0 if there is no on_load function)
				 */

    /*
     * Atom table.
     */

    int num_atoms;		/* Number of atoms in atom table. */
    Eterm* atom;		/* Atom table. */

    int num_exps;		/* Number of exports. */
    ExportEntry* export;	/* Pointer to export table. */

    int num_imports;		/* Number of imports. */
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

    int num_lambdas;		/* Number of lambdas in table. */
    int lambdas_allocated;	/* Size of allocated lambda table. */
    Lambda* lambdas;		/* Pointer to lambdas. */
    Lambda def_lambdas[16];	/* Default storage for lambda table. */
    char* lambda_error;		/* Delayed missing 'FunT' error. */

    /*
     * Literals (constant pool).
     */

    int num_literals;		/* Number of literals in table. */
    int allocated_literals;	/* Number of literal entries allocated. */
    Literal* literals;		/* Array of literals. */
    LiteralPatch* literal_patches; /* Operands that need to be patched. */
    Uint total_literal_size;	/* Total heap size for all literals. */

    /*
     * Floating point.
     */
    int new_float_instructions;	/* New allocation scheme for floating point. */
} LoaderState;

typedef struct {
    unsigned num_functions;	/* Number of functions. */
    Eterm* func_tab[1];		/* Pointers to each function. */
} LoadedCode;

#define GetTagAndValue(Stp, Tag, Val) \
   do { \
      BeamInstr __w; \
      GetByte(Stp, __w); \
      Tag = __w & 0x07; \
      if ((__w & 0x08) == 0) { \
	 Val = __w >> 4; \
      } else if ((__w & 0x10) == 0) { \
	 Val = ((__w >> 5) << 8); \
	 GetByte(Stp, __w); \
	 Val |= __w; \
      } else { \
	if (!get_int_val(Stp, __w, &(Val))) goto load_error; \
      } \
   } while (0)


#define LoadError0(Stp, Fmt) \
    do { \
	load_printf(__LINE__, Stp, Fmt); \
	goto load_error; \
    } while (0)

#define LoadError1(Stp, Fmt, Arg1) \
    do { \
	load_printf(__LINE__, stp, Fmt, Arg1); \
	goto load_error; \
    } while (0)

#define LoadError2(Stp, Fmt, Arg1, Arg2) \
    do { \
	load_printf(__LINE__, Stp, Fmt, Arg1, Arg2); \
	goto load_error; \
    } while (0)

#define LoadError3(Stp, Fmt, Arg1, Arg2, Arg3) \
    do { \
	load_printf(__LINE__, stp, Fmt, Arg1, Arg2, Arg3); \
	goto load_error; \
    } while (0)

#define EndOfFile(Stp) (stp->file_left == 0)

#define GetInt(Stp, N, Dest) \
    if (Stp->file_left < (N)) { \
       short_file(__LINE__, Stp, (N)); \
       goto load_error; \
    } else { \
       int __n = (N); \
       BeamInstr __result = 0; \
       Stp->file_left -= (unsigned) __n; \
       while (__n-- > 0) { \
          __result = __result << 8 | *Stp->file_p++; \
       } \
       Dest = __result; \
    } while (0)

#define GetByte(Stp, Dest) \
    if ((Stp)->file_left < 1) { \
       short_file(__LINE__, (Stp), 1); \
       goto load_error; \
    } else { \
       Dest = *(Stp)->file_p++; \
       (Stp)->file_left--; \
    }

#define GetString(Stp, Dest, N) \
    if (Stp->file_left < (N)) { \
       short_file(__LINE__, Stp, (N)); \
       goto load_error; \
    } else { \
       Dest = (Stp)->file_p; \
       (Stp)->file_p += (N); \
       (Stp)->file_left -= (N); \
    }

#define GetAtom(Stp, Index, Dest) \
    if ((Index) == 0) { \
       LoadError1((Stp), "bad atom index 0 ([]) in %s", stp->file_name); \
    } else if ((Index) < (Stp)->num_atoms) { \
       Dest = (Stp)->atom[(Index)]; \
    } else { \
       LoadError2((Stp), "bad atom index %d in %s", (Index), stp->file_name); \
    }

#ifdef DEBUG
# define GARBAGE 0xCC
# define DEBUG_INIT_GENOP(Dst) memset(Dst, GARBAGE, sizeof(GenOp))
#else
# define DEBUG_INIT_GENOP(Dst)
#endif

#define NEW_GENOP(Stp, Dst) \
  do { \
    if ((Stp)->free_genop == NULL) { \
       new_genop((Stp)); \
    } \
   Dst = (Stp)->free_genop; \
   (Stp)->free_genop = (Stp)->free_genop->next; \
   DEBUG_INIT_GENOP(Dst); \
   (Dst)->a = (Dst)->def_args; \
  } while (0)

#define FREE_GENOP(Stp, Genop) \
 do { \
   if ((Genop)->a != (Genop)->def_args) { \
       erts_free(ERTS_ALC_T_LOADER_TMP, (Genop)->a); \
   } \
   (Genop)->next = (Stp)->free_genop; \
   (Stp)->free_genop = (Genop); \
 } while (0)

#define GENOP_ARITY(Genop, Arity) \
  do { \
   ASSERT((Genop)->a == (Genop)->def_args); \
   (Genop)->arity = (Arity); \
   (Genop)->a = erts_alloc(ERTS_ALC_T_LOADER_TMP, \
			   (Genop)->arity * sizeof(GenOpArg)); \
  } while (0)


static int bin_load(Process *c_p, ErtsProcLocks c_p_locks,
		    Eterm group_leader, Eterm* modp, byte* bytes, int unloaded_size);
static void init_state(LoaderState* stp);
static int insert_new_code(Process *c_p, ErtsProcLocks c_p_locks,
			   Eterm group_leader, Eterm module,
			   BeamInstr* code, Uint size, BeamInstr catches);
static int scan_iff_file(LoaderState* stp, Uint* chunk_types,
			 Uint num_types, Uint num_mandatory);
static int load_atom_table(LoaderState* stp);
static int load_import_table(LoaderState* stp);
static int read_export_table(LoaderState* stp);
static int read_lambda_table(LoaderState* stp);
static int read_literal_table(LoaderState* stp);
static int read_code_header(LoaderState* stp);
static int load_code(LoaderState* stp);
static GenOp* gen_element(LoaderState* stp, GenOpArg Fail, GenOpArg Index,
			  GenOpArg Tuple, GenOpArg Dst);
static GenOp* gen_split_values(LoaderState* stp, GenOpArg S, GenOpArg Fail,
			       GenOpArg Size, GenOpArg* Rest);
static GenOp* gen_select_val(LoaderState* stp, GenOpArg S, GenOpArg Fail,
			     GenOpArg Size, GenOpArg* Rest);
static GenOp* gen_select_big(LoaderState* stp, GenOpArg S, GenOpArg Fail,
			     GenOpArg Size, GenOpArg* Rest);
static GenOp* const_select_val(LoaderState* stp, GenOpArg S, GenOpArg Fail,
			       GenOpArg Size, GenOpArg* Rest);
static GenOp* gen_func_info(LoaderState* stp, GenOpArg mod, GenOpArg Func,
			    GenOpArg arity, GenOpArg label);

static int freeze_code(LoaderState* stp);

static void final_touch(LoaderState* stp);
static void short_file(int line, LoaderState* stp, unsigned needed);
static void load_printf(int line, LoaderState* context, char *fmt, ...);
static int transform_engine(LoaderState* st);
static void id_to_string(Uint id, char* s);
static void new_genop(LoaderState* stp);
static int get_int_val(LoaderState* stp, Uint len_code, BeamInstr* result);
static int get_erlang_integer(LoaderState* stp, Uint len_code, BeamInstr* result);
static int new_label(LoaderState* stp);
static void new_literal_patch(LoaderState* stp, int pos);
static void new_string_patch(LoaderState* stp, int pos);
static Uint new_literal(LoaderState* stp, Eterm** hpp, Uint heap_size);
static int genopargcompare(GenOpArg* a, GenOpArg* b);
static Eterm exported_from_module(Process* p, Eterm mod);
static Eterm functions_in_module(Process* p, Eterm mod);
static Eterm attributes_for_module(Process* p, Eterm mod);
static Eterm compilation_info_for_module(Process* p, Eterm mod);
static Eterm native_addresses(Process* p, Eterm mod);
int patch_funentries(Eterm Patchlist);
int patch(Eterm Addresses, Uint fe);
static int safe_mul(UWord a, UWord b, UWord* resp);


static int must_swap_floats;

/*
 * The following variables keep a sorted list of address ranges for
 * each module.  It allows us to quickly find a function given an
 * instruction pointer.
 */
Range* modules = NULL;	    /* Sorted lists of module addresses. */
int num_loaded_modules;	    /* Number of loaded modules. */
int allocated_modules;	    /* Number of slots allocated. */
Range* mid_module = NULL;   /* Cached search start point */

Uint erts_total_code_size;
/**********************************************************************/


void init_load(void)
{
    FloatDef f;

    erts_total_code_size = 0;

    beam_catches_init();

    f.fd = 1.0;
    must_swap_floats = (f.fw[0] == 0);

    allocated_modules = 128;
    modules = (Range *) erts_alloc(ERTS_ALC_T_MODULE_REFS,
				   allocated_modules*sizeof(Range));
    mid_module = modules;
    num_loaded_modules = 0;
}

static void
define_file(LoaderState* stp, char* name, int idx)
{
    stp->file_name = name;
    stp->file_p = stp->chunks[idx].start;
    stp->file_left = stp->chunks[idx].size;
}

int
erts_load_module(Process *c_p,
		 ErtsProcLocks c_p_locks,
		 Eterm group_leader, /* Group leader or NIL if none. */
		 Eterm* modp,	/*
				 * Module name as an atom (NIL to not check).
				 * On return, contains the actual module name.
				 */
		 byte* code,	/* Points to the code to load */
		 int size)	/* Size of code to load. */
{
    ErlDrvBinary* bin;
    int result;

    if (size >= 4 && code[0] == 'F' && code[1] == 'O' &&
	code[2] == 'R' && code[3] == '1') {
	/*
	 * The BEAM module is not compressed.
	 */
	result = bin_load(c_p, c_p_locks, group_leader, modp, code, size);
    } else {
	/*
	 * The BEAM module is compressed (or possibly invalid/corrupted).
	 */
	if ((bin = (ErlDrvBinary *) erts_gzinflate_buffer((char*)code, size)) == NULL) {
	    return -1;
	}
	result = bin_load(c_p, c_p_locks, group_leader, modp,
			  (byte*)bin->orig_bytes, bin->orig_size);
	driver_free_binary(bin);
    }
    return result;
}
/* #define LOAD_MEMORY_HARD_DEBUG 1*/

#if defined(LOAD_MEMORY_HARD_DEBUG) && defined(DEBUG)
/* Requires allocators ERTS_ALLOC_UTIL_HARD_DEBUG also set in erl_alloc_util.h */
extern void check_allocators(void);
extern void check_allocated_block(Uint type, void *blk);
#define CHKALLOC() check_allocators()
#define CHKBLK(TYPE,BLK) if ((BLK) != NULL) check_allocated_block((TYPE),(BLK))
#else
#define CHKALLOC() /* nothing */
#define CHKBLK(TYPE,BLK) /* nothing */
#endif

static int
bin_load(Process *c_p, ErtsProcLocks c_p_locks,
	 Eterm group_leader, Eterm* modp, byte* bytes, int unloaded_size)
{
    LoaderState state;
    int rval = -1;

    init_state(&state);
    state.module = *modp;
    state.group_leader = group_leader;

    /*
     * Scan the IFF file.
     */

#if defined(LOAD_MEMORY_HARD_DEBUG) && defined(DEBUG)
    erts_fprintf(stderr,"Loading a module\n");
#endif

    CHKALLOC();
    CHKBLK(ERTS_ALC_T_CODE,state.code);
    state.file_name = "IFF header for Beam file";
    state.file_p = bytes;
    state.file_left = unloaded_size;
    if (!scan_iff_file(&state, chunk_types, NUM_CHUNK_TYPES, NUM_MANDATORY)) {
	goto load_error;
    }

    /*
     * Read the header for the code chunk.
     */

    CHKBLK(ERTS_ALC_T_CODE,state.code);
    define_file(&state, "code chunk header", CODE_CHUNK);
    if (!read_code_header(&state)) {
	goto load_error;
    }

    /*
     * Read the atom table.
     */

    CHKBLK(ERTS_ALC_T_CODE,state.code);
    define_file(&state, "atom table", ATOM_CHUNK);
    if (!load_atom_table(&state)) {
	goto load_error;
    }

    /*
     * Read the import table.
     */

    CHKBLK(ERTS_ALC_T_CODE,state.code);
    define_file(&state, "import table", IMP_CHUNK);
    if (!load_import_table(&state)) {
	goto load_error;
    }

    /*
     * Read the lambda (fun) table.
     */

    CHKBLK(ERTS_ALC_T_CODE,state.code);
    if (state.chunks[LAMBDA_CHUNK].size > 0) {
	define_file(&state, "lambda (fun) table", LAMBDA_CHUNK);
	if (!read_lambda_table(&state)) {
	    goto load_error;
	}
    }

    /*
     * Read the literal table.
     */

    CHKBLK(ERTS_ALC_T_CODE,state.code);
    if (state.chunks[LITERAL_CHUNK].size > 0) {
	define_file(&state, "literals table (constant pool)", LITERAL_CHUNK);
	if (!read_literal_table(&state)) {
	    goto load_error;
	}
    }

    /*
     * Load the code chunk.
     */

    CHKBLK(ERTS_ALC_T_CODE,state.code);
    state.file_name = "code chunk";
    state.file_p = state.code_start;
    state.file_left = state.code_size;
    if (!load_code(&state)) {
	goto load_error;
    }
    CHKBLK(ERTS_ALC_T_CODE,state.code);
    if (!freeze_code(&state)) {
	goto load_error;
    }


    /*
     * Read and validate the export table.  (This must be done after
     * loading the code, because it contains labels.)
     */
    
    CHKBLK(ERTS_ALC_T_CODE,state.code);
    define_file(&state, "export table", EXP_CHUNK);
    if (!read_export_table(&state)) {
	goto load_error;
    }

    /*
     * Ready for the final touch: fixing the export table entries for
     * exported and imported functions.  This can't fail.
     */
    
    CHKBLK(ERTS_ALC_T_CODE,state.code);
    rval = insert_new_code(c_p, c_p_locks, state.group_leader, state.module,
			   state.code, state.loaded_size, state.catches);
    if (rval < 0) {
	goto load_error;
    }
    CHKBLK(ERTS_ALC_T_CODE,state.code);
    final_touch(&state);

    /*
     * Loading succeded.
     */
    CHKBLK(ERTS_ALC_T_CODE,state.code);
#if defined(LOAD_MEMORY_HARD_DEBUG) && defined(DEBUG)
    erts_fprintf(stderr,"Loaded %T\n",*modp);
#if 0
    debug_dump_code(state.code,state.ci);
#endif
#endif
    rval = 0;
    state.code = NULL;		/* Prevent code from being freed. */
    *modp = state.module;

    /*
     * If there is an on_load function, signal an error to
     * indicate that the on_load function must be run.
     */
    if (state.on_load) {
	rval = -5;
    }

 load_error:
    if (state.code != 0) {
	erts_free(ERTS_ALC_T_CODE, state.code);
    }
    if (state.labels != NULL) {
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) state.labels);
    }
    if (state.atom != NULL) {
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) state.atom);
    }
    if (state.import != NULL) {
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) state.import);
    }
    if (state.export != NULL) {
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) state.export);
    }
    if (state.lambdas != state.def_lambdas) {
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) state.lambdas);
    }
    if (state.literals != NULL) {
	int i;
	for (i = 0; i < state.num_literals; i++) {
	    if (state.literals[i].heap != NULL) {
		erts_free(ERTS_ALC_T_LOADER_TMP, (void *) state.literals[i].heap);
	    }
	}
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) state.literals);
    }
    while (state.literal_patches != NULL) {
	LiteralPatch* next = state.literal_patches->next;
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) state.literal_patches);
	state.literal_patches = next;
    }
    while (state.string_patches != NULL) {
	StringPatch* next = state.string_patches->next;
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) state.string_patches);
	state.string_patches = next;
    }
    while (state.genop_blocks) {
	GenOpBlock* next = state.genop_blocks->next;
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) state.genop_blocks);
	state.genop_blocks = next;
    }

    return rval;
}


static void
init_state(LoaderState* stp)
{
    stp->function = THE_NON_VALUE; /* Function not known yet */
    stp->arity = 0;
    stp->specific_op = -1;
    stp->genop = NULL;
    stp->atom = NULL;
    stp->code = NULL;
    stp->labels = NULL;
    stp->import = NULL;
    stp->export = NULL;
    stp->free_genop = NULL;
    stp->genop_blocks = NULL;
    stp->num_lambdas = 0;
    stp->lambdas_allocated = sizeof(stp->def_lambdas)/sizeof(Lambda);
    stp->lambdas = stp->def_lambdas;
    stp->lambda_error = NULL;
    stp->num_literals = 0;
    stp->allocated_literals = 0;
    stp->literals = 0;
    stp->total_literal_size = 0;
    stp->literal_patches = 0;
    stp->string_patches = 0;
    stp->new_float_instructions = 0;
    stp->may_load_nif = 0;
    stp->on_load = 0;
}

static int
insert_new_code(Process *c_p, ErtsProcLocks c_p_locks,
		Eterm group_leader, Eterm module, BeamInstr* code, Uint size, BeamInstr catches)
{
    Module* modp;
    int rval;
    int i;

    if ((rval = beam_make_current_old(c_p, c_p_locks, module)) < 0) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp,
		      "Module %T must be purged before loading\n",
		      module);
	erts_send_error_to_logger(group_leader, dsbufp);
	return rval;
    }

    /*
     * Update module table.
     */

    erts_total_code_size += size;
    modp = erts_put_module(module);
    modp->code = code;
    modp->code_length = size;
    modp->catches = catches;

    /*
     * Update address table (used for finding a function from a PC value).
     */

    if (num_loaded_modules == allocated_modules) {
	allocated_modules *= 2;
	modules = (Range *) erts_realloc(ERTS_ALC_T_MODULE_REFS,
					 (void *) modules,
					 allocated_modules * sizeof(Range));
    }
    for (i = num_loaded_modules; i > 0; i--) {
	if (code > modules[i-1].start) {
	    break;
	}
	modules[i] = modules[i-1];
    }
    modules[i].start = code;
    modules[i].end = (BeamInstr *) (((byte *)code) + size);
    num_loaded_modules++;
    mid_module = &modules[num_loaded_modules/2];
    return 0;
}

static int
scan_iff_file(LoaderState* stp, Uint* chunk_types, Uint num_types, Uint num_mandatory)
{
    MD5_CTX context;
    Uint id;
    Uint count;
    int i;

    /*
     * The binary must start with an IFF 'FOR1' chunk.
     */

    GetInt(stp, 4, id);
    if (id != MakeIffId('F', 'O', 'R', '1')) {
	LoadError0(stp, "not a BEAM file: no IFF 'FOR1' chunk");
    }

    /*
     * Retrieve the chunk size and verify it.  If the size is equal to
     * or less than the size of the binary, it is ok and we will use it
     * as the limit for the logical file size.
     */

    GetInt(stp, 4, count);
    if (count > stp->file_left) {
	LoadError2(stp, "form size %ld greater than size %ld of binary",
		  count, stp->file_left);
    }
    stp->file_left = count;

    /*
     * Verify that this is a BEAM file.
     */

    GetInt(stp, 4, id);
    if (id != MakeIffId('B', 'E', 'A', 'M')) {
	LoadError0(stp, "not a BEAM file: IFF form type is not 'BEAM'");
    }

    /*
     * Initialize the chunks[] array in the state.
     */

    for (i = 0; i < num_types; i++) {
	stp->chunks[i].start = NULL;
	stp->chunks[i].size = 0;
    }

    /*
     * Now we can go ahead and read all chunks in the BEAM form.
     */

    while (!EndOfFile(stp)) {

	/*
	 * Read the chunk id and verify that it contains ASCII characters.
	 */
	GetInt(stp, 4, id);
	for (i = 0; i < 4; i++) {
	    unsigned c = (id >> i*8) & 0xff;
	    if (c < ' ' || c > 0x7E) {
		LoadError1(stp, "non-ascii garbage '%lx' instead of chunk type id",
			   id);
	    }
	}

	/*
	 * Read the count and verify it.
	 */

	GetInt(stp, 4, count);
	if (count > stp->file_left) {
	    LoadError2(stp, "chunk size %ld for '%lx' greater than size %ld of binary",
		       count, stp->file_left);
	}

	/*
	 * See if the chunk is useful for the loader.
	 */
	for (i = 0; i < num_types; i++) {
	    if (chunk_types[i] == id) {
		stp->chunks[i].start = stp->file_p;
		stp->chunks[i].size = count;
		break;
	    }
	}

	/*
	 * Go on to the next chunk.
	 */
	count = 4*((count+3)/4);
	stp->file_p += count;
	stp->file_left -= count;
    }

    /*
     * At this point, we have read the entire IFF file, and we
     * know that it is syntactically correct.
     *
     * Now check that it contains all mandatory chunks. At the
     * same time calculate the MD5 for the module.
     */

    MD5Init(&context);
    for (i = 0; i < num_mandatory; i++) {
	if (stp->chunks[i].start != NULL) {
	    MD5Update(&context, stp->chunks[i].start, stp->chunks[i].size);
	} else {
	    char sbuf[5];

	    id_to_string(chunk_types[i], sbuf);
	    LoadError1(stp, "mandatory chunk of type '%s' not found\n", sbuf);
	}
    }
    if (LITERAL_CHUNK < num_types) {
	if (stp->chunks[LAMBDA_CHUNK].start != 0) {
	    byte* start = stp->chunks[LAMBDA_CHUNK].start;
	    Uint left = stp->chunks[LAMBDA_CHUNK].size;

	    /*
	     * The idea here is to ignore the OldUniq field for the fun; it is
	     * based on the old broken hash function, which can be different
	     * on little endian and big endian machines.
	     */
	    if (left >= 4) {
		static byte zero[4];
		MD5Update(&context, start, 4);
		start += 4;
		left -= 4;
		
		while (left >= 24) {
		    /* Include: Function Arity Index NumFree */
		    MD5Update(&context, start, 20);
		    /* Set to zero: OldUniq */
		    MD5Update(&context, zero, 4);
		    start += 24;
		    left -= 24;
		}
	    }
	    /* Can't happen for a correct 'FunT' chunk */
	    if (left > 0) {
		MD5Update(&context, start, left);
	    }
	}
	if (stp->chunks[LITERAL_CHUNK].start != 0) {
	    MD5Update(&context, stp->chunks[LITERAL_CHUNK].start,
		      stp->chunks[LITERAL_CHUNK].size);
	}
    }
    MD5Final(stp->mod_md5, &context);
    return 1;

 load_error:
    return 0;
}


static int
load_atom_table(LoaderState* stp)
{
    int i;

    GetInt(stp, 4, stp->num_atoms);
    stp->num_atoms++;
    stp->atom = erts_alloc(ERTS_ALC_T_LOADER_TMP,
			   erts_next_heap_size((stp->num_atoms*sizeof(Eterm)),
					       0));

    /*
     * Read all atoms.
     */

    for (i = 1; i < stp->num_atoms; i++) {
	byte* atom;
	Uint n;

	GetByte(stp, n);
	GetString(stp, atom, n);
	stp->atom[i] = am_atom_put((char*)atom, n);
    }

    /*
     * Check the module name if a module name was given.
     */

    if (is_nil(stp->module)) {
	stp->module = stp->atom[1];
    } else if (stp->atom[1] != stp->module) {
	char sbuf[256];
	Atom* ap;

	ap = atom_tab(atom_val(stp->atom[1]));
	memcpy(sbuf, ap->name, ap->len);
	sbuf[ap->len] = '\0';
	LoadError1(stp, "module name in object code is %s", sbuf);
    }

    return 1;

 load_error:
    return 0;
}


static int
load_import_table(LoaderState* stp)
{
    int i;

    GetInt(stp, 4, stp->num_imports);
    stp->import = erts_alloc(ERTS_ALC_T_LOADER_TMP,
			     erts_next_heap_size((stp->num_imports *
						  sizeof(ImportEntry)),
						 0));
    for (i = 0; i < stp->num_imports; i++) {
	int n;
	Eterm mod;
	Eterm func;
	Uint arity;
	Export* e;

	GetInt(stp, 4, n);
	if (n >= stp->num_atoms) {
	    LoadError2(stp, "import entry %d: invalid atom number %d", i, n);
	}
	mod = stp->import[i].module = stp->atom[n];
	GetInt(stp, 4, n);
	if (n >= stp->num_atoms) {
	    LoadError2(stp, "import entry %d: invalid atom number %d", i, n);
	}
	func = stp->import[i].function = stp->atom[n];
	GetInt(stp, 4, arity);
	if (arity > MAX_REG) {
	    LoadError2(stp, "import entry %d: invalid arity %d", i, arity);
	}
	stp->import[i].arity = arity;
	stp->import[i].patches = 0;
	stp->import[i].bf = NULL;

	/*
	 * If the export entry refers to a BIF, get the pointer to
	 * the BIF function.
	 */
	if ((e = erts_find_export_entry(mod, func, arity)) != NULL) {
	    if (e->code[3] == (BeamInstr) em_apply_bif) {
		stp->import[i].bf = (BifFunction) e->code[4];
		if (func == am_load_nif && mod == am_erlang && arity == 2) {
		    stp->may_load_nif = 1;
		}
	    }
	}
    }
    return 1;

 load_error:
    return 0;
}


static int
read_export_table(LoaderState* stp)
{
    static struct {
	Eterm mod;
	Eterm func;
	int arity;
    } allow_redef[] = {
	/* The BIFs that are allowed to be redefined by Erlang code */
	{am_erlang,am_apply,2},
	{am_erlang,am_apply,3},
    };
    int i;

    GetInt(stp, 4, stp->num_exps);
    if (stp->num_exps > stp->num_functions) {
	LoadError2(stp, "%d functions exported; only %d functions defined",
		   stp->num_exps, stp->num_functions);
    }
    stp->export
	= (ExportEntry *) erts_alloc(ERTS_ALC_T_LOADER_TMP,
				     (stp->num_exps * sizeof(ExportEntry)));

    for (i = 0; i < stp->num_exps; i++) {
	Uint n;
	Uint value;
	Eterm func;
	Uint arity;
	Export* e;

	GetInt(stp, 4, n);
	GetAtom(stp, n, func);
	stp->export[i].function = func;
	GetInt(stp, 4, arity);
	if (arity > MAX_REG) {
	    LoadError2(stp, "export table entry %d: absurdly high arity %d", i, arity);
	}
	stp->export[i].arity = arity;
	GetInt(stp, 4, n);
	if (n >= stp->num_labels) {
	    LoadError3(stp, "export table entry %d: invalid label %d (highest defined label is %d)", i, n, stp->num_labels);
	}
	value = stp->labels[n].value;
	if (value == 0) {
	    LoadError2(stp, "export table entry %d: label %d not resolved", i, n);
	}
	stp->export[i].address = stp->code + value;

	/*
	 * Check that we are not redefining a BIF (except the ones allowed to
	 * redefine).
	 */
	if ((e = erts_find_export_entry(stp->module, func, arity)) != NULL) {
	    if (e->code[3] == (BeamInstr) em_apply_bif) {
		int j;

		for (j = 0; j < sizeof(allow_redef)/sizeof(allow_redef[0]); j++) {
		    if (stp->module == allow_redef[j].mod &&
			func == allow_redef[j].func &&
			arity == allow_redef[j].arity) {
			break;
		    }
		}
		if (j == sizeof(allow_redef)/sizeof(allow_redef[0])) {
		    LoadError2(stp, "exported function %T/%d redefines BIF",
			       func, arity);
		}
	    }
	}
    }
    return 1;

 load_error:
    return 0;
}

static int
read_lambda_table(LoaderState* stp)
{
    int i;

    GetInt(stp, 4, stp->num_lambdas);
    stp->lambdas_allocated = stp->num_lambdas;
    stp->lambdas = (Lambda *) erts_alloc(ERTS_ALC_T_LOADER_TMP,
					 stp->num_lambdas * sizeof(Lambda));
    for (i = 0; i < stp->num_lambdas; i++) {
	Uint n;
	Uint32 Index;
	Uint32 OldUniq;
	ErlFunEntry* fe;
	Uint arity;

	GetInt(stp, 4, n);	/* Function. */
	GetAtom(stp, n, stp->lambdas[i].function);
	GetInt(stp, 4, arity);
	if (arity > MAX_REG) {
	    LoadError2(stp, "lambda entry %d: absurdly high arity %d", i, arity);
	}
	stp->lambdas[i].arity = arity;
	GetInt(stp, 4, n);
	if (n >= stp->num_labels) {
	    LoadError3(stp, "lambda entry %d: invalid label %d (highest defined label is %d)",
		       i, n, stp->num_labels);
	}
	stp->lambdas[i].label = n;
	GetInt(stp, 4, Index);
	GetInt(stp, 4, stp->lambdas[i].num_free);
	GetInt(stp, 4, OldUniq);
	fe = erts_put_fun_entry2(stp->module, OldUniq, i, stp->mod_md5,
				 Index, arity-stp->lambdas[i].num_free);
	stp->lambdas[i].fe = fe;
    }
    return 1;

 load_error:
    return 0;
}

static int
read_literal_table(LoaderState* stp)
{
    int i;
    BeamInstr uncompressed_sz;
    byte* uncompressed = 0;

    GetInt(stp, 4, uncompressed_sz);
    uncompressed = erts_alloc(ERTS_ALC_T_TMP, uncompressed_sz);
    if (erl_zlib_uncompress(uncompressed, &uncompressed_sz,
		   stp->file_p, stp->file_left) != Z_OK) {
	LoadError0(stp, "failed to uncompress literal table (constant pool)");
    }
    stp->file_p = uncompressed;
    stp->file_left = uncompressed_sz;
    GetInt(stp, 4, stp->num_literals);
    stp->literals = (Literal *) erts_alloc(ERTS_ALC_T_LOADER_TMP,
					   stp->num_literals * sizeof(Literal));
    stp->allocated_literals = stp->num_literals;

    for (i = 0; i < stp->num_literals; i++) {
	stp->literals[i].heap = 0;
    }

    for (i = 0; i < stp->num_literals; i++) {
	int sz;
	Sint heap_size;
	byte* p;
	Eterm val;
	Eterm* hp;

	GetInt(stp, 4, sz);	/* Size of external term format. */
	GetString(stp, p, sz);
	if ((heap_size = erts_decode_ext_size(p, sz, 1)) < 0) {
	    LoadError1(stp, "literal %d: bad external format", i);
	}
	hp = stp->literals[i].heap = erts_alloc(ERTS_ALC_T_LOADER_TMP,
						heap_size*sizeof(Eterm));
	val = erts_decode_ext(&hp, NULL, &p);
	stp->literals[i].heap_size = hp - stp->literals[i].heap;
	if (stp->literals[i].heap_size > heap_size) {
	    erl_exit(1, "overrun by %d word(s) for literal heap, term %d",
		     stp->literals[i].heap_size - heap_size, i);
	}
	if (is_non_value(val)) {
	    LoadError1(stp, "literal %d: bad external format", i);
	}
	stp->literals[i].term = val;
	stp->total_literal_size += stp->literals[i].heap_size;
    }
    erts_free(ERTS_ALC_T_TMP, uncompressed);
    return 1;

 load_error:
    if (uncompressed) {
	erts_free(ERTS_ALC_T_TMP, uncompressed);
    }
    return 0;
}


static int
read_code_header(LoaderState* stp)
{
    unsigned head_size;
    unsigned version;
    unsigned opcode_max;
    int i;

    /*
     * Read size of sub-header for code information and from it calculate
     * where the code begins.  Also, use the size to limit the file size
     * for header reading, so that we automatically get an error if the
     * size is set too small.
     */

    GetInt(stp, 4, head_size);
    stp->code_start = stp->file_p + head_size;
    stp->code_size = stp->file_left - head_size;
    stp->file_left = head_size;

    /*
     * Get and verify version of instruction set.
     */

    GetInt(stp, 4, version);
    if (version != BEAM_FORMAT_NUMBER) {
	LoadError2(stp, "wrong instruction set %d; expected %d",
		   version, BEAM_FORMAT_NUMBER);
    }

    /*
     * Verify the number of the highest opcode used.
     */

    GetInt(stp, 4, opcode_max);
    if (opcode_max > MAX_GENERIC_OPCODE) {
	LoadError2(stp, "use of opcode %d; this emulator supports only up to %d",
		   opcode_max, MAX_GENERIC_OPCODE);
    }

    GetInt(stp, 4, stp->num_labels);
    GetInt(stp, 4, stp->num_functions);

    /*
     * Initialize label table.
     */

    stp->labels = (Label *) erts_alloc(ERTS_ALC_T_LOADER_TMP,
				       stp->num_labels * sizeof(Label));
    for (i = 0; i < stp->num_labels; i++) {
	stp->labels[i].value = 0;
	stp->labels[i].patches = 0;
#ifdef ERTS_SMP
	stp->labels[i].looprec_targeted = 0;
#endif
    }

    /*
     * Initialize code area.
     */
    stp->code_buffer_size = erts_next_heap_size(2048 + stp->num_functions, 0);
    stp->code = (BeamInstr *) erts_alloc(ERTS_ALC_T_CODE,
				    sizeof(BeamInstr) * stp->code_buffer_size);

    stp->code[MI_NUM_FUNCTIONS] = stp->num_functions;
    stp->ci = MI_FUNCTIONS + stp->num_functions + 1;

    stp->code[MI_ATTR_PTR] = 0;
    stp->code[MI_ATTR_SIZE] = 0;
    stp->code[MI_ATTR_SIZE_ON_HEAP] = 0;
    stp->code[MI_COMPILE_PTR] = 0;
    stp->code[MI_COMPILE_SIZE] = 0;
    stp->code[MI_COMPILE_SIZE_ON_HEAP] = 0;
    stp->code[MI_NUM_BREAKPOINTS] = 0;

    stp->new_bs_put_strings = 0;
    stp->catches = 0;
    return 1;

 load_error:
    return 0;
}


#define VerifyTag(Stp, Actual, Expected) \
    if (Actual != Expected) { \
       LoadError2(Stp, "bad tag %d; expected %d", Actual, Expected); \
    } else {}

#define CodeNeed(w) do {						\
    ASSERT(ci <= code_buffer_size);					\
    if (code_buffer_size < ci+(w)) {					\
        code_buffer_size = erts_next_heap_size(ci+(w), 0);		\
	stp->code = code						\
	    = (BeamInstr *) erts_realloc(ERTS_ALC_T_CODE,			\
				     (void *) code,			\
				     code_buffer_size * sizeof(BeamInstr));	\
    } 									\
} while (0)
    
#define TermWords(t) (((t) / (sizeof(BeamInstr)/sizeof(Eterm))) + !!((t) % (sizeof(BeamInstr)/sizeof(Eterm))))


static int
load_code(LoaderState* stp)
{
    int i;
    int tmp;
    int ci;
    int last_func_start = 0;
    char* sign;
    int arg;			/* Number of current argument. */
    int num_specific;		/* Number of specific ops for current. */
    BeamInstr* code;
    int code_buffer_size;
    int specific;
    Uint last_label = 0;	/* Number of last label. */
    Uint function_number = 0;
    GenOp* last_op = NULL;
    GenOp** last_op_next = NULL;
    int arity;

    code = stp->code;
    code_buffer_size = stp->code_buffer_size;
    ci = stp->ci;

    for (;;) {
	int new_op;
	GenOp* tmp_op;

	ASSERT(ci <= code_buffer_size);

    get_next_instr:
	GetByte(stp, new_op);
	if (new_op >= NUM_GENERIC_OPS) {
	    LoadError1(stp, "invalid opcode %d", new_op);
	}
	if (gen_opc[new_op].name[0] == '\0') {
	    LoadError1(stp, "invalid opcode %d", new_op);
	}
				       

	/*
	 * Create a new generic operation and put it last in the chain.
	 */
	if (last_op_next == NULL) {
	    last_op_next = &(stp->genop);
	    while (*last_op_next != NULL) {
		last_op_next = &(*last_op_next)->next;
	    }
	}

	NEW_GENOP(stp, last_op);
	last_op->next = NULL;
	last_op->op = new_op;
	*last_op_next = last_op;
	last_op_next = &(last_op->next);
	stp->specific_op = -1;

	/*
	 * Read all arguments for the current operation.
	 */

	arity = gen_opc[last_op->op].arity;
	last_op->arity = 0;
	ASSERT(arity <= MAX_OPARGS);

#define GetValue(Stp, First, Val) \
   do { \
      if (((First) & 0x08) == 0) { \
	 Val = (First) >> 4; \
      } else if (((First) & 0x10) == 0) { \
         BeamInstr __w; \
	 GetByte(Stp, __w); \
	 Val = (((First) >> 5) << 8) | __w; \
      } else { \
	if (!get_int_val(Stp, (First), &(Val))) goto load_error; \
      } \
   } while (0)

	for (arg = 0; arg < arity; arg++) {
	    BeamInstr first;

	    GetByte(stp, first);
	    last_op->a[arg].type = first & 0x07;
	    switch (last_op->a[arg].type) {
	    case TAG_i:
		if ((first & 0x08) == 0) {
		    last_op->a[arg].val = first >> 4;
		} else if ((first & 0x10) == 0) {
		    BeamInstr w;
		    GetByte(stp, w);
		    ASSERT(first < 0x800);
		    last_op->a[arg].val = ((first >> 5) << 8) | w;
		} else {
		    int i = get_erlang_integer(stp, first, &(last_op->a[arg].val));
		    if (i < 0) {
			goto load_error;
		    }
		    last_op->a[arg].type = i;
		}
		break;
	    case TAG_u:
		GetValue(stp, first, last_op->a[arg].val);
		break;
	    case TAG_x:
		GetValue(stp, first, last_op->a[arg].val);
		if (last_op->a[arg].val == 0) {
		    last_op->a[arg].type = TAG_r;
		} else if (last_op->a[arg].val >= MAX_REG) {
		    LoadError1(stp, "invalid x register number: %u",
			       last_op->a[arg].val);
		}
		break;
	    case TAG_y:
		GetValue(stp, first, last_op->a[arg].val);
		if (last_op->a[arg].val >= MAX_REG) {
		    LoadError1(stp, "invalid y register number: %u",
			       last_op->a[arg].val);
		}
		last_op->a[arg].val += CP_SIZE;
		break;
	    case TAG_a:
		GetValue(stp, first, last_op->a[arg].val);
		if (last_op->a[arg].val == 0) {
		    last_op->a[arg].type = TAG_n;
		} else if (last_op->a[arg].val >= stp->num_atoms) {
		    LoadError1(stp, "bad atom index: %d", last_op->a[arg].val);
		} else {
		    last_op->a[arg].val = stp->atom[last_op->a[arg].val];
		}
		break;
	    case TAG_f:
		GetValue(stp, first, last_op->a[arg].val);
		if (last_op->a[arg].val == 0) {
		    last_op->a[arg].type = TAG_p;
		} else if (last_op->a[arg].val >= stp->num_labels) {
		    LoadError1(stp, "bad label: %d", last_op->a[arg].val);
		}
		break;
	    case TAG_h:
		GetValue(stp, first, last_op->a[arg].val);
		if (last_op->a[arg].val > 65535) {
		    LoadError1(stp, "invalid range for character data type: %u",
			       last_op->a[arg].val);
		}
		break;
	    case TAG_z:
		{
		    BeamInstr ext_tag;
		    unsigned tag;

		    GetValue(stp, first, ext_tag);
		    switch (ext_tag) {
		    case 0:	/* Floating point number */
			{
			    Eterm* hp;
#if !defined(ARCH_64) || HALFWORD_HEAP /* XXX:PaN - Should use ARCH_64 variant instead */
			    Uint high, low;
# endif
			    last_op->a[arg].val = new_literal(stp, &hp,
							      FLOAT_SIZE_OBJECT);
			    hp[0] = HEADER_FLONUM;
			    last_op->a[arg].type = TAG_q;
#if defined(ARCH_64) && !HALFWORD_HEAP
			    GetInt(stp, 8, hp[1]);
# else
			    GetInt(stp, 4, high);
			    GetInt(stp, 4, low);
			    if (must_swap_floats) {
				Uint t = high;
				high = low;
				low = t;
			    }
			    hp[1] = high;
			    hp[2] = low;
# endif
			}
			break;
		    case 1:	/* List. */
			if (arg+1 != arity) {
			    LoadError0(stp, "list argument must be the last argument");
			}
			GetTagAndValue(stp, tag, last_op->a[arg].val);
			VerifyTag(stp, tag, TAG_u);
			last_op->a[arg].type = TAG_u;
			last_op->a =
			    erts_alloc(ERTS_ALC_T_LOADER_TMP,
				       (arity+last_op->a[arg].val)
				       *sizeof(GenOpArg));
			memcpy(last_op->a, last_op->def_args,
			       arity*sizeof(GenOpArg));
			arity += last_op->a[arg].val;
			break;
		    case 2:	/* Float register. */
			GetTagAndValue(stp, tag, last_op->a[arg].val);
			VerifyTag(stp, tag, TAG_u);
			last_op->a[arg].type = TAG_l;
			break;
		    case 3: 	/* Allocation list. */
			{
			    BeamInstr n;
			    BeamInstr type;
			    BeamInstr val;
			    BeamInstr words = 0;
			    
			    stp->new_float_instructions = 1;
			    GetTagAndValue(stp, tag, n);
			    VerifyTag(stp, tag, TAG_u);
			    while (n-- > 0) {
				GetTagAndValue(stp, tag, type);
				VerifyTag(stp, tag, TAG_u);
				GetTagAndValue(stp, tag, val);
				VerifyTag(stp, tag, TAG_u);
				switch (type) {
				case 0:	/* Heap words */
				    words += val;
				    break;
				case 1:
				    words += FLOAT_SIZE_OBJECT*val;
				    break;
				default:
				    LoadError1(stp, "alloc list: bad allocation "
					       "descriptor %d", type);
				    break;
				}
			    }
			    last_op->a[arg].type = TAG_u;
			    last_op->a[arg].val = words;
			    break;
			}
		    case 4:	/* Literal. */
			{
			    BeamInstr val;

			    GetTagAndValue(stp, tag, val);
			    VerifyTag(stp, tag, TAG_u);
			    if (val >= stp->num_literals) {
				LoadError1(stp, "bad literal index %d", val);
			    }
			    last_op->a[arg].type = TAG_q;
			    last_op->a[arg].val = val;
			    break;
			}
		    default:
			LoadError1(stp, "invalid extended tag %d", ext_tag);
			break;
		    }
		}
		break;
	    default:
		LoadError1(stp, "bad tag %d", last_op->a[arg].type);
		break;
	    }
	    last_op->arity++;
	}
#undef GetValue

	ASSERT(arity == last_op->arity);

    do_transform:
	if (stp->genop == NULL) {
	    last_op_next = NULL;
	    goto get_next_instr;
	}

	if (gen_opc[stp->genop->op].transform != -1) {
	    int need;
	    tmp_op = stp->genop;

	    for (need = gen_opc[stp->genop->op].min_window-1; need > 0; need--) {
		if (tmp_op == NULL) {
		    goto get_next_instr;
		}
		tmp_op = tmp_op->next;
	    }
	    switch (transform_engine(stp)) {
	    case TE_FAIL:
		last_op_next = NULL;
		last_op = NULL;
		break;
	    case TE_OK:
		last_op_next = NULL;
		last_op = NULL;
		goto do_transform;
	    case TE_SHORT_WINDOW:
		last_op_next = NULL;
		last_op = NULL;
		goto get_next_instr;
	    }
	}

	if (stp->genop == NULL) {
	    last_op_next = NULL;
	    goto get_next_instr;
	}

	/*
	 * Special error message instruction.
	 */
	if (stp->genop->op == genop_too_old_compiler_0) {
	    LoadError0(stp, "please re-compile this module with an " 
		       ERLANG_OTP_RELEASE " compiler");
	}

	/*
	 * From the collected generic instruction, find the specific
	 * instruction.
	 */

	{
	    Uint32 mask[3] = {0, 0, 0};

	    tmp_op = stp->genop;
	    arity = gen_opc[tmp_op->op].arity;
	    if (arity > 6) {
		LoadError0(stp, "no specific operation found (arity > 6)");
	    }
	    for (arg = 0; arg < arity; arg++) {
		mask[arg/2] |= ((Uint32)1 << (tmp_op->a[arg].type)) << ((arg%2)*16);
	    }
	    specific = gen_opc[tmp_op->op].specific;
	    num_specific = gen_opc[tmp_op->op].num_specific;
	    for (i = 0; i < num_specific; i++) {
		if (((opc[specific].mask[0] & mask[0]) == mask[0]) &&
		    ((opc[specific].mask[1] & mask[1]) == mask[1]) &&
		    ((opc[specific].mask[2] & mask[2]) == mask[2])) {
		    break;
		}
		specific++;
	    }

	    /*
	     * No specific operation found.
	     */
	    if (i == num_specific) {
		stp->specific_op = -1;
		for (arg = 0; arg < tmp_op->arity; arg++) {
		    /*
		     * We'll give the error message here (instead of earlier)
		     * to get a printout of the offending operation.
		     */
		    if (tmp_op->a[arg].type == TAG_h) {
			LoadError0(stp, "the character data type not supported");
		    }
		}

		/*
		 * No specific operations and no transformations means that
		 * the instruction is obsolete.
		 */
		if (num_specific == 0 && gen_opc[tmp_op->op].transform == -1) {
		    LoadError0(stp, "please re-compile this module with an "
			       ERLANG_OTP_RELEASE " compiler ");
		}

		LoadError0(stp, "no specific operation found");
	    }

	    stp->specific_op = specific;
	    CodeNeed(opc[stp->specific_op].sz+2); /* Extra margin for packing */
	    code[ci++] = BeamOpCode(stp->specific_op);
	}
	
	/*
	 * Load the found specific operation.
	 */

	sign = opc[stp->specific_op].sign;
	ASSERT(sign != NULL);
	arg = 0;
	while (*sign) {
	    Uint tag;

	    ASSERT(arg < stp->genop->arity);
	    tag = stp->genop->a[arg].type;
	    switch (*sign) {
	    case 'r':	/* x(0) */
	    case 'n':	/* Nil */
		VerifyTag(stp, tag_to_letter[tag], *sign);
		break;
	    case 'x':	/* x(N) */
	    case 'y':	/* y(N) */
		VerifyTag(stp, tag_to_letter[tag], *sign);
		code[ci++] = tmp_op->a[arg].val * sizeof(Eterm);
		break;
	    case 'a':		/* Tagged atom */
		VerifyTag(stp, tag_to_letter[tag], *sign);
		code[ci++] = tmp_op->a[arg].val;
		break;
	    case 'i':		/* Tagged integer */
		ASSERT(is_small(tmp_op->a[arg].val));
		VerifyTag(stp, tag_to_letter[tag], *sign);
		code[ci++] = tmp_op->a[arg].val;
		break;
	    case 'c':		/* Tagged constant */
		switch (tag) {
		case TAG_i:
		    code[ci++] = (BeamInstr) make_small((Uint) tmp_op->a[arg].val);
		    break;
		case TAG_a:
		    code[ci++] = tmp_op->a[arg].val;
		    break;
		case TAG_n:
		    code[ci++] = NIL;
		    break;
		case TAG_q:
		    new_literal_patch(stp, ci);
		    code[ci++] = tmp_op->a[arg].val;
		    break;
		default:
		    LoadError1(stp, "bad tag %d for tagged constant",
			       tmp_op->a[arg].type);
		    break;
		}
		break;
	    case 's':	/* Any source (tagged constant or register) */
		switch (tag) {
		case TAG_r:
		    code[ci++] = make_rreg();
		    break;
		case TAG_x:
		    code[ci++] = make_xreg(tmp_op->a[arg].val);
		    break;
		case TAG_y:
		    code[ci++] = make_yreg(tmp_op->a[arg].val);
		    break;
		case TAG_i:
		    code[ci++] = (BeamInstr) make_small((Uint)tmp_op->a[arg].val);
		    break;
		case TAG_a:
		    code[ci++] = tmp_op->a[arg].val;
		    break;
		case TAG_n:
		    code[ci++] = NIL;
		    break;
		default:
		    LoadError1(stp, "bad tag %d for general source",
			       tmp_op->a[arg].type);
		    break;
		}
		break;
	    case 'd':	/* Destination (x(0), x(N), y(N) */
		switch (tag) {
		case TAG_r:
		    code[ci++] = make_rreg();
		    break;
		case TAG_x:
		    code[ci++] = make_xreg(tmp_op->a[arg].val);
		    break;
		case TAG_y:
		    code[ci++] = make_yreg(tmp_op->a[arg].val);
		    break;
		default:
		    LoadError1(stp, "bad tag %d for destination",
			       tmp_op->a[arg].type);
		    break;
		}
		break;
	    case 'I':	/* Untagged integer (or pointer). */
		VerifyTag(stp, tag, TAG_u);
		code[ci++] = tmp_op->a[arg].val;
		break;
	    case 't':	/* Small untagged integer -- can be packed. */
		VerifyTag(stp, tag, TAG_u);
		code[ci++] = tmp_op->a[arg].val;
		break;
	    case 'A':	/* Arity value. */
		VerifyTag(stp, tag, TAG_u);
		code[ci++] = make_arityval(tmp_op->a[arg].val);
		break;
	    case 'f':		/* Destination label */
		VerifyTag(stp, tag_to_letter[tag], *sign);
		code[ci] = stp->labels[tmp_op->a[arg].val].patches;
		stp->labels[tmp_op->a[arg].val].patches = ci;
		ci++;
		break;
	    case 'j':		/* 'f' or 'p' */
		if (tag == TAG_p) {
		    code[ci] = 0;
		} else if (tag == TAG_f) {
		    code[ci] = stp->labels[tmp_op->a[arg].val].patches;
		    stp->labels[tmp_op->a[arg].val].patches = ci;
		} else {
		    LoadError3(stp, "bad tag %d; expected %d or %d",
			       tag, TAG_f, TAG_p);
		}
		ci++;
		break;
	    case 'L':		/* Define label */
		ci--;		/* Remove label from loaded code */
		ASSERT(stp->specific_op == op_label_L);
		VerifyTag(stp, tag, TAG_u);
		last_label = tmp_op->a[arg].val;
		if (!(0 < last_label && last_label < stp->num_labels)) {
		    LoadError2(stp, "invalid label num %d (0 < label < %d)",
			       tmp_op->a[arg].val, stp->num_labels);
		}
		if (stp->labels[last_label].value != 0) {
		    LoadError1(stp, "label %d defined more than once", last_label);
		}
		stp->labels[last_label].value = ci;
		ASSERT(stp->labels[last_label].patches < ci);
		break;
	    case 'e':		/* Export entry */
		VerifyTag(stp, tag, TAG_u);
		if (tmp_op->a[arg].val >= stp->num_imports) {
		    LoadError1(stp, "invalid import table index %d", tmp_op->a[arg].val);
		}
		code[ci] = stp->import[tmp_op->a[arg].val].patches;
		stp->import[tmp_op->a[arg].val].patches = ci;
		ci++;
		break;
	    case 'b':
		VerifyTag(stp, tag, TAG_u);
		i = tmp_op->a[arg].val;
		if (i >= stp->num_imports) {
		    LoadError1(stp, "invalid import table index %d", i);
		}
		if (stp->import[i].bf == NULL) {
		    LoadError1(stp, "not a BIF: import table index %d", i);
		}
		code[ci++] = (BeamInstr) stp->import[i].bf;
		break;
	    case 'P':		/* Byte offset into tuple */ /* XXX:PaN - * sizeof(Eterm or Eterm *) ? */
		VerifyTag(stp, tag, TAG_u);
		tmp = tmp_op->a[arg].val;
		code[ci++] = (BeamInstr) ((tmp_op->a[arg].val+1) * sizeof(Eterm));
		break;
	    case 'l':		/* Floating point register. */
		VerifyTag(stp, tag_to_letter[tag], *sign);
		code[ci++] = tmp_op->a[arg].val * sizeof(FloatDef);
		break;
	    case 'q':		/* Literal */
		new_literal_patch(stp, ci);
		code[ci++] = tmp_op->a[arg].val;
		break;
	    default:
		LoadError1(stp, "bad argument tag: %d", *sign);
	    }
	    sign++;
	    arg++;
	}

	/*
	 * Load any list arguments using the primitive tags.
	 */

	for ( ; arg < tmp_op->arity; arg++) {
	    switch (tmp_op->a[arg].type) {
	    case TAG_i:
		CodeNeed(1);
		code[ci++] = make_small(tmp_op->a[arg].val);
		break;
	    case TAG_u:
	    case TAG_a:
	    case TAG_v:
		CodeNeed(1);
		code[ci++] = tmp_op->a[arg].val;
		break;
	    case TAG_f:
		CodeNeed(1);
		code[ci] = stp->labels[tmp_op->a[arg].val].patches;
		stp->labels[tmp_op->a[arg].val].patches = ci;
		ci++;
		break;
	    case TAG_q:
		{
		    Eterm lit;

		    lit = stp->literals[tmp_op->a[arg].val].term;
		    if (is_big(lit)) {
			Eterm* bigp;
			Eterm *tmp;
			Uint size;
			Uint term_size;

			bigp = big_val(lit);
			term_size = bignum_header_arity(*bigp);
			size = TermWords(term_size + 1);
			CodeNeed(size);
			tmp = (Eterm *) (code + ci);
			*tmp++ = *bigp++;
			while (term_size-- > 0) {
			    *tmp++ = *bigp++;
			}
			ci +=size;
		    } else if (is_float(lit)) {
#if defined(ARCH_64) && !HALFWORD_HEAP
			CodeNeed(1);
			code[ci++] = float_val(stp->literals[tmp_op->a[arg].val].term)[1];
#elif HALFWORD_HEAP
			Eterm* fptr;
			Uint size;
			Eterm *tmp;

			fptr = float_val(stp->literals[tmp_op->a[arg].val].term)+1;
			size = TermWords(2);
			CodeNeed(size);
			tmp = (Eterm *) (code + ci);
			*tmp++ = *fptr++;
			*tmp = *fptr;
			ci += size;
#else
			Eterm* fptr;

			fptr = float_val(stp->literals[tmp_op->a[arg].val].term)+1;
			CodeNeed(2);
			code[ci++] = *fptr++;
			code[ci++] = *fptr;
#endif
		    } else {
			LoadError0(stp, "literal is neither float nor big");
		    }
		}
		break;
	    default:
		LoadError1(stp, "unsupported primitive type '%c'",
			   tag_to_letter[tmp_op->a[arg].type]);
	    }
	}

	/*
	 * The packing engine.
	 */
	if (opc[stp->specific_op].pack[0]) {
	    char* prog;		/* Program for packing engine. */
	    BeamInstr stack[8];	/* Stack. */
	    BeamInstr* sp = stack;	/* Points to next free position. */
	    BeamInstr packed = 0;	/* Accumulator for packed operations. */

	    for (prog = opc[stp->specific_op].pack; *prog; prog++) {
		switch (*prog) {
		case 'g':	/* Get instruction; push on stack. */
		    *sp++ = code[--ci];
		    break;
		case 'i':	/* Initialize packing accumulator. */
		    packed = code[--ci];
		    break;
		case '0':	/* Tight shift */
		    packed = (packed << BEAM_TIGHT_SHIFT) | code[--ci];
		    break;
		case '6':	/* Shift 16 steps */
		    packed = (packed << BEAM_LOOSE_SHIFT) | code[--ci];
		    break;
		case 'p':	/* Put instruction (from stack). */
		    code[ci++] = *--sp;
		    break;
		case 'P':	/* Put packed operands. */
		    *sp++ = packed;
		    packed = 0;
		    break;
		default:
		    ASSERT(0);
		}
	    }
	    ASSERT(sp == stack); /* Incorrect program? */
	}

	/*
	 * Handle a few special cases.
	 */
	switch (stp->specific_op) {
	case op_i_func_info_IaaI:
	    {
		Uint offset;
		enum { FINFO_SZ = 5 };

		if (function_number >= stp->num_functions) {
		    LoadError1(stp, "too many functions in module (header said %d)",
			       stp->num_functions); 
		}

		if (stp->may_load_nif) {
		    const int finfo_ix = ci - FINFO_SZ;		    
		    enum { MIN_FUNC_SZ = 3 };		    
		    if (finfo_ix - last_func_start < MIN_FUNC_SZ && last_func_start) {		   
			/* Must make room for call_nif op */
			int pad = MIN_FUNC_SZ - (finfo_ix - last_func_start);
			ASSERT(pad > 0 && pad < MIN_FUNC_SZ);
			CodeNeed(pad);
			sys_memmove(&code[finfo_ix+pad], &code[finfo_ix], FINFO_SZ*sizeof(BeamInstr));
			sys_memset(&code[finfo_ix], 0, pad*sizeof(BeamInstr));
			ci += pad;
			stp->labels[last_label].value += pad;
		    }
		}
		last_func_start = ci;
		/*
		 * Save context for error messages.
		 */
		stp->function = code[ci-2];
		stp->arity = code[ci-1];

		ASSERT(stp->labels[last_label].value == ci - FINFO_SZ);
		offset = MI_FUNCTIONS + function_number;
		code[offset] = stp->labels[last_label].patches;
		stp->labels[last_label].patches = offset;
		function_number++;
		if (stp->arity > MAX_ARG) {
		    LoadError1(stp, "too many arguments: %d", stp->arity);
		}
#ifdef DEBUG
		ASSERT(stp->labels[0].patches == 0); /* Should not be referenced. */
		for (i = 1; i < stp->num_labels; i++) {
		    ASSERT(stp->labels[i].patches < ci);
		}
#endif
	    }
	    break;
	case op_on_load:
	    ci--;		/* Get rid of the instruction */

	    /* Remember offset for the on_load function. */
	    stp->on_load = ci;
	    break;
	case op_bs_put_string_II:
	    {
		/*
		 * At entry:
		 *
		 * code[ci-3]	&&lb_i_new_bs_put_string_II
		 * code[ci-2]	length of string
		 * code[ci-1]   offset into string table
		 *
		 * Since we don't know the address of the string table yet,
		 * just check the offset and length for validity, and use
		 * the instruction field as a link field to link all put_string
		 * instructions into a single linked list.  At exit:
		 *
		 * code[ci-3]	pointer to next i_new_bs_put_string instruction (or 0
		 *		if this is the last)
		 */
		Uint offset = code[ci-1];
		Uint len = code[ci-2];
		unsigned strtab_size = stp->chunks[STR_CHUNK].size;
		if (offset > strtab_size || offset + len > strtab_size) {
		    LoadError2(stp, "invalid string reference %d, size %d", offset, len);
		}
		code[ci-3] = stp->new_bs_put_strings;
		stp->new_bs_put_strings = ci - 3;
	    }
	    break;
	case op_i_bs_match_string_rfII:
	case op_i_bs_match_string_xfII:
	    new_string_patch(stp, ci-1);
	    break;

	case op_catch_yf:
	    /* code[ci-3]	&&lb_catch_yf
	     * code[ci-2]	y-register offset in E
	     * code[ci-1]	label; index tagged as CATCH at runtime
	     */
	    code[ci-3] = stp->catches;
	    stp->catches = ci-3;
	    break;

	    /*
	     * End of code found.
	     */
	case op_int_code_end:
	    stp->code_buffer_size = code_buffer_size;
	    stp->ci = ci;
	    return 1;
	}

	/*
	 * Delete the generic instruction just loaded.
	 */
	{
	    GenOp* next = stp->genop->next;
	    FREE_GENOP(stp, stp->genop);
	    stp->genop = next;
	    goto do_transform;
	}
    }
    

 load_error:
    return 0;
}


#define succ(St, X, Y) ((X).type == (Y).type && (X).val + 1 == (Y).val)
#define succ2(St, X, Y) ((X).type == (Y).type && (X).val + 2 == (Y).val)
#define succ3(St, X, Y) ((X).type == (Y).type && (X).val + 3 == (Y).val)

#ifdef NO_FPE_SIGNALS 
#define no_fpe_signals(St) 1
#else
#define no_fpe_signals(St) 0
#endif

/*
 * Predicate that tests whether a jump table can be used.
 */

static int
use_jump_tab(LoaderState* stp, GenOpArg Size, GenOpArg* Rest)
{
    Sint min, max;
    Sint i;

    if (Size.val < 2 || Size.val % 2 != 0) {
	return 0;
    }

    /* we may be called with sequences of tagged fixnums or atoms;
       return early in latter case, before we access the values */
    if (Rest[0].type != TAG_i || Rest[1].type != TAG_f)
	return 0;
    min = max = Rest[0].val;
    for (i = 2; i < Size.val; i += 2) {
	if (Rest[i].type != TAG_i || Rest[i+1].type != TAG_f) {
	    return 0;
	}
	if (Rest[i].val < min) {
	    min = Rest[i].val;
	} else if (max < Rest[i].val) {
	    max = Rest[i].val;
	}
    }

    return max - min <= Size.val;
}

/*
 * Predicate to test whether all values in a table are big numbers.
 */

static int
all_values_are_big(LoaderState* stp, GenOpArg Size, GenOpArg* Rest)
{
    int i;

    if (Size.val < 2 || Size.val % 2 != 0) {
	return 0;
    }

    for (i = 0; i < Size.val; i += 2) {
	if (Rest[i].type != TAG_q) {
	    return 0;
	}
	if (is_not_big(stp->literals[Rest[i].val].term)) {
	    return 0;
	}
	if (Rest[i+1].type != TAG_f) {
	    return 0;
	}
    }

    return 1;
}


/*
 * Predicate to test whether all values in a table have a fixed size.
 */

static int
fixed_size_values(LoaderState* stp, GenOpArg Size, GenOpArg* Rest)
{
    int i;

    if (Size.val < 2 || Size.val % 2 != 0) {
	return 0;
    }

    for (i = 0; i < Size.val; i += 2) {
	if (Rest[i+1].type != TAG_f)
	    return 0;
	switch (Rest[i].type) {
	case TAG_a:
	case TAG_i:
	case TAG_v:
	    break;
	case TAG_q:
	    return is_float(stp->literals[Rest[i].val].term);
	default:
	    return 0;
	}
    }

    return 1;
}

static int
mixed_types(LoaderState* stp, GenOpArg Size, GenOpArg* Rest)
{
    int i;
    Uint type;

    if (Size.val < 2 || Size.val % 2 != 0) {
	return 0;
    }

    type = Rest[0].type;
    for (i = 0; i < Size.val; i += 2) {
	if (Rest[i].type != type)
	    return 1;
    }

    return 0;
}

/*
 * Generate an instruction for element/2.
 */

static GenOp*
gen_element(LoaderState* stp, GenOpArg Fail, GenOpArg Index,
		      GenOpArg Tuple, GenOpArg Dst)
{
    GenOp* op;

    NEW_GENOP(stp, op);
    op->op = genop_i_element_4;
    op->arity = 4;
    op->a[0] = Fail;
    op->a[1] = Index;
    op->a[2] = Tuple;
    op->a[3] = Dst;
    op->next = NULL;

    /*
     * If safe, generate a faster instruction.
     */

    if (Index.type == TAG_i && Index.val > 0 &&
	(Tuple.type == TAG_r || Tuple.type == TAG_x || Tuple.type == TAG_y)) {
	op->op = genop_i_fast_element_4;
	op->a[1].type = TAG_u;
	op->a[1].val = Index.val;
    }

    return op;
}

static GenOp*
gen_bs_save(LoaderState* stp, GenOpArg Reg, GenOpArg Index)
{
    GenOp* op;

    NEW_GENOP(stp, op);
    op->op = genop_i_bs_save2_2;
    op->arity = 2;
    op->a[0] = Reg;
    op->a[1] = Index;
    if (Index.type == TAG_u) {
	op->a[1].val = Index.val+1;
    } else if (Index.type == TAG_a && Index.val == am_start) {
	op->a[1].type = TAG_u;
	op->a[1].val = 0;
    }
    op->next = NULL;
    return op;
}

static GenOp*
gen_bs_restore(LoaderState* stp, GenOpArg Reg, GenOpArg Index)
{
    GenOp* op;

    NEW_GENOP(stp, op);
    op->op = genop_i_bs_restore2_2;
    op->arity = 2;
    op->a[0] = Reg;
    op->a[1] = Index;
    if (Index.type == TAG_u) {
	op->a[1].val = Index.val+1;
    } else if (Index.type == TAG_a && Index.val == am_start) {
	op->a[1].type = TAG_u;
	op->a[1].val = 0;
    }
    op->next = NULL;
    return op;
}

/*
 * Generate the fastest instruction to fetch an integer from a binary.
 */

static GenOp*
gen_get_integer2(LoaderState* stp, GenOpArg Fail, GenOpArg Ms, GenOpArg Live,
		 GenOpArg Size, GenOpArg Unit,
		 GenOpArg Flags, GenOpArg Dst)
{
    GenOp* op;
    UWord bits;

    NEW_GENOP(stp, op);

    NATIVE_ENDIAN(Flags);
    if (Size.type == TAG_i) {
	if (!safe_mul(Size.val, Unit.val, &bits)) {
	    goto error;
	} else if ((Flags.val & BSF_SIGNED) != 0) {
	    goto generic;
	} else if (bits == 8) {
	    op->op = genop_i_bs_get_integer_8_3;
	    op->arity = 3;
	    op->a[0] = Ms;
	    op->a[1] = Fail;
	    op->a[2] = Dst;
	} else if (bits == 16 && (Flags.val & BSF_LITTLE) == 0) {
	    op->op = genop_i_bs_get_integer_16_3;
	    op->arity = 3;
	    op->a[0] = Ms;
	    op->a[1] = Fail;
	    op->a[2] = Dst;
	} else if (bits == 32 && (Flags.val & BSF_LITTLE) == 0) {
	    op->op = genop_i_bs_get_integer_32_4;
	    op->arity = 4;
	    op->a[0] = Ms;
	    op->a[1] = Fail;
	    op->a[2] = Live;
	    op->a[3] = Dst;
	} else {
	generic:
	    if (bits < SMALL_BITS) {
		op->op = genop_i_bs_get_integer_small_imm_5;
		op->arity = 5;
		op->a[0] = Ms;
		op->a[1].type = TAG_u;
		op->a[1].val = bits;
		op->a[2] = Fail;
		op->a[3] = Flags;
		op->a[4] = Dst;
	    } else {
		op->op = genop_i_bs_get_integer_imm_6;
		op->arity = 6;
		op->a[0] = Ms;
		op->a[1].type = TAG_u;
		op->a[1].val = bits;
		op->a[2] = Live;
		op->a[3] = Fail;
		op->a[4] = Flags;
		op->a[5] = Dst;
	    }
	}
    } else if (Size.type == TAG_q) {
	Eterm big = stp->literals[Size.val].term;
	Uint bigval;

	if (!term_to_Uint(big, &bigval)) {
	error:
	    op->op = genop_jump_1;
	    op->arity = 1;
	    op->a[0] = Fail;
	} else {
	    if (!safe_mul(bigval, Unit.val, &bits)) {
		goto error;
	    }
	    goto generic;
	}
    } else {
	GenOp* op2;
	NEW_GENOP(stp, op2);
	
	op->op = genop_i_fetch_2;
	op->arity = 2;
	op->a[0] = Ms;
	op->a[1] = Size;
	op->next = op2;

	op2->op = genop_i_bs_get_integer_4;
	op2->arity = 4;
	op2->a[0] = Fail;
	op2->a[1] = Live;
	op2->a[2].type = TAG_u;
	op2->a[2].val = (Unit.val << 3) | Flags.val;
	op2->a[3] = Dst;
	op2->next = NULL;
	return op;
    }
    op->next = NULL;
    return op;
}

/*
 * Generate the fastest instruction to fetch a binary from a binary.
 */

static GenOp*
gen_get_binary2(LoaderState* stp, GenOpArg Fail, GenOpArg Ms, GenOpArg Live,
		GenOpArg Size, GenOpArg Unit,
		GenOpArg Flags, GenOpArg Dst)
{
    GenOp* op;
    NEW_GENOP(stp, op);

    NATIVE_ENDIAN(Flags);
    if (Size.type == TAG_a && Size.val == am_all) {
	if (Ms.type == Dst.type && Ms.val == Dst.val) {
	    op->op = genop_i_bs_get_binary_all_reuse_3;
	    op->arity = 3;
	    op->a[0] = Ms;
	    op->a[1] = Fail;
	    op->a[2] = Unit;
	} else {
	    op->op = genop_i_bs_get_binary_all2_5;
	    op->arity = 5;
	    op->a[0] = Fail;
	    op->a[1] = Ms;
	    op->a[2] = Live;	
	    op->a[3] = Unit;
	    op->a[4] = Dst;
	}
    } else if (Size.type == TAG_i) {
	op->op = genop_i_bs_get_binary_imm2_6;
	op->arity = 6;
	op->a[0] = Fail;
	op->a[1] = Ms;
	op->a[2] = Live;
	op->a[3].type = TAG_u;
	if (!safe_mul(Size.val, Unit.val, &op->a[3].val)) {
	    goto error;
	}
	op->a[4] = Flags;
	op->a[5] = Dst;
    } else if (Size.type == TAG_q) {
	Eterm big = stp->literals[Size.val].term;
	Uint bigval;

	if (!term_to_Uint(big, &bigval)) {
	error:
	    op->op = genop_jump_1;
	    op->arity = 1;
	    op->a[0] = Fail;
	} else {
	    op->op = genop_i_bs_get_binary_imm2_6;
	    op->arity = 6;
	    op->a[0] = Fail;
	    op->a[1] = Ms;
	    op->a[2] = Live;
	    op->a[3].type = TAG_u;
	    if (!safe_mul(bigval, Unit.val, &op->a[3].val)) {
		goto error;
	    }
	    op->a[4] = Flags;
	    op->a[5] = Dst;
	}
    } else {
	op->op = genop_i_bs_get_binary2_6;
	op->arity = 6;
	op->a[0] = Fail;
	op->a[1] = Ms;
	op->a[2] = Live;
	op->a[3] = Size;
	op->a[4].type = TAG_u;
	op->a[4].val = (Unit.val << 3) | Flags.val;
	op->a[5] = Dst;
    }
    op->next = NULL;
    return op;
}

/*
 * Predicate to test whether a heap binary should be generated.
 */

static int
should_gen_heap_bin(LoaderState* stp, GenOpArg Src)
{
    return Src.val <= ERL_ONHEAP_BIN_LIMIT;
}

/*
 * Predicate to test whether a binary construction is too big.
 */

static int
binary_too_big(LoaderState* stp, GenOpArg Size)
{
    return Size.type == TAG_u && ((Size.val >> (8*sizeof(Uint)-3)) != 0);
}

static int
binary_too_big_bits(LoaderState* stp, GenOpArg Size)
{
    return Size.type == TAG_u && (((Size.val+7)/8) >> (8*sizeof(Uint)-3) != 0);
}

#define new_float_allocation(Stp) ((Stp)->new_float_instructions)

static GenOp*
gen_put_binary(LoaderState* stp, GenOpArg Fail,GenOpArg Size,
	       GenOpArg Unit, GenOpArg Flags, GenOpArg Src)
{
    GenOp* op;
    NEW_GENOP(stp, op);

    NATIVE_ENDIAN(Flags);
    if (Size.type == TAG_a && Size.val == am_all) {
	op->op = genop_i_new_bs_put_binary_all_3;
	op->arity = 3;
	op->a[0] = Fail;
	op->a[1] = Src;
	op->a[2] = Unit;
    } else if (Size.type == TAG_i) {
	op->op = genop_i_new_bs_put_binary_imm_3;
	op->arity = 3;
	op->a[0] = Fail;
	op->a[1].type = TAG_u;
	if (safe_mul(Size.val, Unit.val, &op->a[1].val)) {
	    op->a[2] = Src;
	} else {
	    op->op = genop_badarg_1;
	    op->arity = 1;
	    op->a[0] = Fail;
	}
    } else {
	op->op = genop_i_new_bs_put_binary_4;
	op->arity = 4;
	op->a[0] = Fail;
	op->a[1] = Size;
	op->a[2].type = TAG_u;
	op->a[2].val = (Unit.val << 3) | (Flags.val & 7);
	op->a[3] = Src;
    }

    op->next = NULL;
    return op;
}

static GenOp*
gen_put_integer(LoaderState* stp, GenOpArg Fail, GenOpArg Size,
		GenOpArg Unit, GenOpArg Flags, GenOpArg Src)
{
    GenOp* op;
    NEW_GENOP(stp, op);

    NATIVE_ENDIAN(Flags);
    if (Size.type == TAG_i && Size.val < 0) {
    error:
	/* Negative size must fail */
	op->op = genop_badarg_1;
	op->arity = 1;
	op->a[0] = Fail;
    } else if (Size.type == TAG_i) {
	op->op = genop_i_new_bs_put_integer_imm_4;
	op->arity = 4;
	op->a[0] = Fail;
	op->a[1].type = TAG_u;
	if (!safe_mul(Size.val, Unit.val, &op->a[1].val)) {
	    goto error;
	}
	op->a[1].val = Size.val * Unit.val;
	op->a[2].type = Flags.type;
	op->a[2].val = (Flags.val & 7);
	op->a[3] = Src;
    } else if (Size.type == TAG_q) {
	Eterm big = stp->literals[Size.val].term;
	Uint bigval;

	if (!term_to_Uint(big, &bigval)) {
	    goto error;
	} else {
	    op->op = genop_i_new_bs_put_integer_imm_4;
	    op->arity = 4;
	    op->a[0] = Fail;
	    op->a[1].type = TAG_u;
	    op->a[1].val = bigval * Unit.val;
	    op->a[2].type = Flags.type;
	    op->a[2].val = (Flags.val & 7);
	    op->a[3] = Src;
	}
    } else {
	op->op = genop_i_new_bs_put_integer_4;
	op->arity = 4;
	op->a[0] = Fail;
	op->a[1] = Size;
	op->a[2].type = TAG_u;
	op->a[2].val = (Unit.val << 3) | (Flags.val & 7);
	op->a[3] = Src;
    }
    op->next = NULL;
    return op;
}

static GenOp*
gen_put_float(LoaderState* stp, GenOpArg Fail, GenOpArg Size,
	      GenOpArg Unit, GenOpArg Flags, GenOpArg Src)
{
    GenOp* op;
    NEW_GENOP(stp, op);

    NATIVE_ENDIAN(Flags);
    if (Size.type == TAG_i) {
	op->op = genop_i_new_bs_put_float_imm_4;
	op->arity = 4;
	op->a[0] = Fail;
	op->a[1].type = TAG_u;
	if (!safe_mul(Size.val, Unit.val, &op->a[1].val)) {
	    op->op = genop_badarg_1;
	    op->arity = 1;
	    op->a[0] = Fail;
	} else {
	    op->a[2] = Flags;
	    op->a[3] = Src;
	}
    } else {
	op->op = genop_i_new_bs_put_float_4;
	op->arity = 4;
	op->a[0] = Fail;
	op->a[1] = Size;
	op->a[2].type = TAG_u;
	op->a[2].val = (Unit.val << 3) | (Flags.val & 7);
	op->a[3] = Src;
    }
    op->next = NULL;
    return op;
}

/*
 * Generate an instruction to fetch a float from a binary.
 */

static GenOp*
gen_get_float2(LoaderState* stp, GenOpArg Fail, GenOpArg Ms, GenOpArg Live,
		GenOpArg Size, GenOpArg Unit, GenOpArg Flags, GenOpArg Dst)
{
    GenOp* op;
    NEW_GENOP(stp, op);

    NATIVE_ENDIAN(Flags);
    op->op = genop_i_bs_get_float2_6;
    op->arity = 6;
    op->a[0] = Fail;
    op->a[1] = Ms;
    op->a[2] = Live;
    op->a[3] = Size;
    op->a[4].type = TAG_u;
    op->a[4].val = (Unit.val << 3) | Flags.val;
    op->a[5] = Dst;
    op->next = NULL;
    return op;
}

/*
 * Generate the fastest instruction for bs_skip_bits.
 */

static GenOp*
gen_skip_bits2(LoaderState* stp, GenOpArg Fail, GenOpArg Ms, 
	       GenOpArg Size, GenOpArg Unit, GenOpArg Flags)
{
    GenOp* op;

    NATIVE_ENDIAN(Flags);
    NEW_GENOP(stp, op);
    if (Size.type == TAG_a && Size.val == am_all) {
	op->op = genop_i_bs_skip_bits_all2_3;
	op->arity = 3;
	op->a[0] = Fail;
	op->a[1] = Ms; 
	op->a[2] = Unit;
    } else if (Size.type == TAG_i) {
	op->op = genop_i_bs_skip_bits_imm2_3;
	op->arity = 3;
	op->a[0] = Fail;
	op->a[1] = Ms; 
	op->a[2].type = TAG_u;
	if (!safe_mul(Size.val, Unit.val, &op->a[2].val)) {
	    goto error;
	}
    } else if (Size.type == TAG_q) {
	Eterm big = stp->literals[Size.val].term;
	Uint bigval;

	if (!term_to_Uint(big, &bigval)) {
	error:
	    op->op = genop_jump_1;
	    op->arity = 1;
	    op->a[0] = Fail;
	} else {
	    op->op = genop_i_bs_skip_bits_imm2_3;
	    op->arity = 3;
	    op->a[0] = Fail;
	    op->a[1] = Ms; 
	    op->a[2].type = TAG_u;
	    if (!safe_mul(bigval, Unit.val, &op->a[2].val)) {
		goto error;
	    }
	}
    } else {
	op->op = genop_i_bs_skip_bits2_4;
	op->arity = 4;
	op->a[0] = Fail;
	op->a[1] = Ms; 
	op->a[2] = Size;
	op->a[3] = Unit;
    }
    op->next = NULL;
    return op;
}

static int
smp(LoaderState* stp)
{
#ifdef ERTS_SMP
    return 1;
#else
    return 0;
#endif
}

/*
 * Mark this label.
 */
static int
smp_mark_target_label(LoaderState* stp, GenOpArg L)
{
#ifdef ERTS_SMP
    ASSERT(L.type == TAG_f);
    stp->labels[L.val].looprec_targeted = 1;
#endif
    return 1;
}

/*
 * Test whether this label was targeted by a loop_rec/2 instruction.
 */

static int
smp_already_locked(LoaderState* stp, GenOpArg L)
{
#ifdef ERTS_SMP
    ASSERT(L.type == TAG_u);
    return stp->labels[L.val].looprec_targeted;
#else
    return 0;
#endif
}

/*
 * Generate a timeout instruction for a literal timeout.
 */

static GenOp*
gen_literal_timeout(LoaderState* stp, GenOpArg Fail, GenOpArg Time)
{
    GenOp* op;
    Sint timeout;

    NEW_GENOP(stp, op);
    op->op = genop_i_wait_timeout_2;
    op->next = NULL;
    op->arity = 2;
    op->a[0] = Fail;
    op->a[1].type = TAG_u;
    
    if (Time.type == TAG_i && (timeout = Time.val) >= 0 &&
#if defined(ARCH_64) && !HALFWORD_HEAP
	(timeout >> 32) == 0
#else
	1
#endif
	) {
	op->a[1].val = timeout;
#if !defined(ARCH_64) || HALFWORD_HEAP
    } else if (Time.type == TAG_q) {
	Eterm big;

	big = stp->literals[Time.val].term;
	if (is_not_big(big)) {
	    goto error;
	}
	if (big_arity(big) > 1 || big_sign(big)) {
	    goto error;
	} else {
	    Uint u;
	    (void) term_to_Uint(big, &u);
	    op->a[1].val = (BeamInstr) u;
	}
#endif
    } else {
#if !defined(ARCH_64) || HALFWORD_HEAP
    error:
#endif
	op->op = genop_i_wait_error_0;
	op->arity = 0;
    }
    return op;
}

static GenOp*
gen_literal_timeout_locked(LoaderState* stp, GenOpArg Fail, GenOpArg Time)
{
    GenOp* op;
    Sint timeout;

    NEW_GENOP(stp, op);
    op->op = genop_i_wait_timeout_locked_2;
    op->next = NULL;
    op->arity = 2;
    op->a[0] = Fail;
    op->a[1].type = TAG_u;
    
    if (Time.type == TAG_i && (timeout = Time.val) >= 0 &&
#if defined(ARCH_64) && !HALFWORD_HEAP
	(timeout >> 32) == 0
#else
	1
#endif
	) {
	op->a[1].val = timeout;
#if !defined(ARCH_64) || HALFWORD_HEAP
    } else if (Time.type == TAG_q) {
	Eterm big;

	big = stp->literals[Time.val].term;
	if (is_not_big(big)) {
	    goto error;
	}
	if (big_arity(big) > 1 || big_sign(big)) {
	    goto error;
	} else {
	    Uint u;
	    (void) term_to_Uint(big, &u);
	    op->a[1].val = (BeamInstr) u;
	}
#endif
    } else {
#if !defined(ARCH_64) || HALFWORD_HEAP
    error:
#endif
	op->op = genop_i_wait_error_locked_0;
	op->arity = 0;
    }
    return op;
}

/*
 * Tag the list of values with tuple arity tags.
 */

static GenOp*
gen_select_tuple_arity(LoaderState* stp, GenOpArg S, GenOpArg Fail,
		       GenOpArg Size, GenOpArg* Rest)

{
    GenOp* op;
    int arity = Size.val + 3;
    int size = Size.val / 2;
    int i;

    /*
     * Verify the validity of the list.
     */

    if (Size.val % 2 != 0)
	return NULL;
    for (i = 0; i < Size.val; i += 2) {
	if (Rest[i].type != TAG_u || Rest[i+1].type != TAG_f) {
	    return NULL;
	}
    }

    /*
     * Generate the generic instruction.
     */

    NEW_GENOP(stp, op);
    op->next = NULL;
    op->op = genop_i_select_tuple_arity_3;
    GENOP_ARITY(op, arity);
    op->a[0] = S;
    op->a[1] = Fail;
    op->a[2].type = TAG_u;
    op->a[2].val = Size.val / 2;
    for (i = 0; i < Size.val; i += 2) {
	op->a[i+3].type = TAG_v;
	op->a[i+3].val = make_arityval(Rest[i].val);
	op->a[i+4] = Rest[i+1];
    }

    /*
     * Sort the values to make them useful for a binary search.
     */

    qsort(op->a+3, size, 2*sizeof(GenOpArg), 
	   (int (*)(const void *, const void *)) genopargcompare);
#ifdef DEBUG
    for (i = 3; i < arity-2; i += 2) {
	ASSERT(op->a[i].val < op->a[i+2].val);
    }
#endif
    return op;
}

/*
 * Split a list consisting of both small and bignumbers into two
 * select_val instructions.
 */

static GenOp*
gen_split_values(LoaderState* stp, GenOpArg S, GenOpArg Fail,
		 GenOpArg Size, GenOpArg* Rest)

{
    GenOp* op1;
    GenOp* op2;
    GenOp* label;
    Uint type;
    int i;

    ASSERT(Size.val >= 2 && Size.val % 2 == 0);

    NEW_GENOP(stp, label);
    label->op = genop_label_1;
    label->arity = 1;
    label->a[0].type = TAG_u;
    label->a[0].val = new_label(stp);

    NEW_GENOP(stp, op1);
    op1->op = genop_select_val_3;
    GENOP_ARITY(op1, 3 + Size.val);
    op1->arity = 3;
    op1->a[0] = S;
    op1->a[1].type = TAG_f;
    op1->a[1].val = label->a[0].val;
    op1->a[2].type = TAG_u;
    op1->a[2].val = 0;

    NEW_GENOP(stp, op2);
    op2->op = genop_select_val_3;
    GENOP_ARITY(op2, 3 + Size.val);
    op2->arity = 3;
    op2->a[0] = S;
    op2->a[1] = Fail;
    op2->a[2].type = TAG_u;
    op2->a[2].val = 0;

    op1->next = label;
    label->next = op2;
    op2->next = NULL;

    type = Rest[0].type;

    ASSERT(Size.type == TAG_u);
    for (i = 0; i < Size.val; i += 2) {
	GenOp* op = (Rest[i].type == type) ? op1 : op2;
	int dst = 3 + op->a[2].val;

	ASSERT(Rest[i+1].type == TAG_f);
	op->a[dst] = Rest[i];
	op->a[dst+1] = Rest[i+1];
	op->arity += 2;
	op->a[2].val += 2;
    }

    /*
     * None of the instructions should have zero elements in the list.
     */

    ASSERT(op1->a[2].val > 0);
    ASSERT(op2->a[2].val > 0);

    return op1;
}

/*
 * Generate a jump table.
 */

static GenOp*
gen_jump_tab(LoaderState* stp, GenOpArg S, GenOpArg Fail, GenOpArg Size, GenOpArg* Rest)
{
    Sint min, max;
    Sint i;
    Sint size;
    Sint arity;
    int fixed_args;
    GenOp* op;

    ASSERT(Size.val >= 2 && Size.val % 2 == 0);

    /*
     * Calculate the minimum and maximum values and size of jump table.
     */

    ASSERT(Rest[0].type == TAG_i);
    min = max = Rest[0].val;
    for (i = 2; i < Size.val; i += 2) {
	ASSERT(Rest[i].type == TAG_i && Rest[i+1].type == TAG_f);
	if (Rest[i].val < min) {
	    min = Rest[i].val;
	} else if (max < Rest[i].val) {
	    max = Rest[i].val;
	}
    }
    size = max - min + 1;


    /*
     * Allocate structure and fill in the fixed fields.
     */

    NEW_GENOP(stp, op);
    op->next = NULL;
    if (min == 0) {
	op->op = genop_i_jump_on_val_zero_3;
	fixed_args = 3;
    } else {
	op->op = genop_i_jump_on_val_4;
	fixed_args = 4;
    }
    arity = fixed_args + size;
    GENOP_ARITY(op, arity);
    op->a[0] = S;
    op->a[1] = Fail;
    op->a[2].type = TAG_u;
    op->a[2].val = size;
    op->a[3].type = TAG_u;
    op->a[3].val = min;


    /*
     * Fill in the jump table.
     */

    for (i = fixed_args; i < arity; i++) {
	op->a[i] = Fail;
    }
    for (i = 0; i < Size.val; i += 2) {
	int index;
	index = fixed_args+Rest[i].val-min;
	ASSERT(fixed_args <= index && index < arity);
	op->a[index] = Rest[i+1];
    }
    return op;
}

/* 
 *  Compare function for qsort().
 */

static int
genopargcompare(GenOpArg* a, GenOpArg* b)
{
    if (a->val < b->val)
	return -1;
    else if (a->val == b->val)
	return 0;
    else
	return 1;
}

/*
 * Generate a select_val instruction.  We know that a jump table is not suitable,
 * and that all values are of the same type (integer, atoms, floats; never bignums).
 */

static GenOp*
gen_select_val(LoaderState* stp, GenOpArg S, GenOpArg Fail,
	       GenOpArg Size, GenOpArg* Rest)
{
    GenOp* op;
    int arity = Size.val + 3;
    int size = Size.val / 2;
    int i;

    NEW_GENOP(stp, op);
    op->next = NULL;
    if (Rest[0].type != TAG_q) {
	op->op = genop_i_select_val_3;
    } else {
	ASSERT(is_float(stp->literals[Rest[0].val].term));
	op->op = genop_i_select_float_3;
    }
    GENOP_ARITY(op, arity);
    op->a[0] = S;
    op->a[1] = Fail;
    op->a[2].type = TAG_u;
    op->a[2].val = size;
    for (i = 3; i < arity; i++) {
	op->a[i] = Rest[i-3];
    }

    /*
     * Sort the values to make them useful for a binary search.
     */

    qsort(op->a+3, size, 2*sizeof(GenOpArg), 
	  (int (*)(const void *, const void *)) genopargcompare);
#ifdef DEBUG
    for (i = 3; i < arity-2; i += 2) {
	ASSERT(op->a[i].val < op->a[i+2].val);
    }
#endif

    return op;
}

/* 
 *  Compare function for qsort().
 */

static int
genbigcompare(GenOpArg* a, GenOpArg* b)
{
    int val = (int)(b->bigarity - a->bigarity);
    
    return val != 0 ? val : ((int) (a->val - b->val));
}

/*
 * Generate a select_val instruction for big numbers.
 */

static GenOp*
gen_select_big(LoaderState* stp, GenOpArg S, GenOpArg Fail,
	       GenOpArg Size, GenOpArg* Rest)
{
    GenOp* op;
    int arity = Size.val + 2 + 1;
    int size = Size.val / 2;
    int i;

    NEW_GENOP(stp, op);
    op->next = NULL;
    op->op = genop_i_select_big_2;
    GENOP_ARITY(op, arity);
    op->a[0] = S;
    op->a[1] = Fail;
    for (i = 0; i < Size.val; i += 2) {
	ASSERT(Rest[i].type == TAG_q);
	op->a[i+2] = Rest[i];
	op->a[i+2].bigarity = *big_val(stp->literals[op->a[i+2].val].term);
	op->a[i+3] = Rest[i+1];
    }
    ASSERT(i+2 == arity-1);
    op->a[arity-1].type = TAG_u;
    op->a[arity-1].val = 0;

    /*
     * Sort the values in descending arity order.
     */

    qsort(op->a+2, size, 2*sizeof(GenOpArg), 
	  (int (*)(const void *, const void *)) genbigcompare);

    return op;
}


/*
 * Replace a select_val instruction with a constant controlling expression
 * with a jump instruction.
 */

static GenOp*
const_select_val(LoaderState* stp, GenOpArg S, GenOpArg Fail,
		 GenOpArg Size, GenOpArg* Rest)
{
    GenOp* op;
    int i;

    ASSERT(Size.type == TAG_u);
    ASSERT(S.type == TAG_q);

    NEW_GENOP(stp, op);
    op->next = NULL;
    op->op = genop_jump_1;
    op->arity = 1;

    /*
     * Search for a literal matching the controlling expression.
     */

    if (S.type == TAG_q) {
	Eterm expr = stp->literals[S.val].term;
	for (i = 0; i < Size.val; i += 2) {
	    if (Rest[i].type == TAG_q) {
		Eterm term = stp->literals[Rest[i].val].term;
		if (eq(term, expr)) {
		    ASSERT(Rest[i+1].type == TAG_f);
		    op->a[0] = Rest[i+1];
		    return op;
		}
	    }
	}
    }

    /*
     * No match.  Use the failure label.
     */

    op->a[0] = Fail;
    return op;
}


static GenOp*
gen_func_info(LoaderState* stp, GenOpArg mod, GenOpArg func,
	      GenOpArg arity, GenOpArg label)
{
    GenOp* fi;
    GenOp* op;

    NEW_GENOP(stp, fi);
    fi->op = genop_i_func_info_4;
    fi->arity = 4;
    fi->a[0].type = TAG_u;	/* untagged Zero */
    fi->a[0].val = 0;
    fi->a[1] = mod;
    fi->a[2] = func;
    fi->a[3] = arity;

    NEW_GENOP(stp, op);
    op->op = genop_label_1;
    op->arity = 1;
    op->a[0] = label;
    
    fi->next = op;
    op->next = NULL;

    return fi;
}



static GenOp*
gen_make_fun2(LoaderState* stp, GenOpArg idx)
{
    ErlFunEntry* fe;
    GenOp* op;

    if (idx.val >= stp->num_lambdas) {
	stp->lambda_error = "missing or short chunk 'FunT'";
	fe = 0;
    } else {
	fe = stp->lambdas[idx.val].fe;
    }

    NEW_GENOP(stp, op);
    op->op = genop_i_make_fun_2;
    op->arity = 2;
    op->a[0].type = TAG_u;
    op->a[0].val = (BeamInstr) fe;
    op->a[1].type = TAG_u;
    op->a[1].val = stp->lambdas[idx.val].num_free;
    op->next = NULL;
    return op;
}
/*
 * Rewrite gc_bifs with one parameter (the common case). Utilized
 * in ops.tab to rewrite instructions calling bif's in guards
 * to use a garbage collecting implementation. The instructions
 * are sometimes once again rewritten to handle literals (putting the
 * parameter in the mostly unused r[0] before the instruction is executed).
 */
static GenOp*
gen_guard_bif1(LoaderState* stp, GenOpArg Fail, GenOpArg Live, GenOpArg Bif,
	      GenOpArg Src, GenOpArg Dst)
{
    GenOp* op;
    BifFunction bf;

    NEW_GENOP(stp, op);
    op->op = genop_i_gc_bif1_5;
    op->arity = 5;
    op->a[0] = Fail;
    op->a[1].type = TAG_u;
    bf = stp->import[Bif.val].bf;
    /* The translations here need to have a reverse counterpart in
       beam_emu.c:translate_gc_bif for error handling to work properly. */
    if (bf == length_1) {
	op->a[1].val = (BeamInstr) (void *) erts_gc_length_1;
    } else if (bf == size_1) {
	op->a[1].val = (BeamInstr) (void *) erts_gc_size_1;
    } else if (bf == bit_size_1) {
	op->a[1].val = (BeamInstr) (void *) erts_gc_bit_size_1;
    } else if (bf == byte_size_1) {
	op->a[1].val = (BeamInstr) (void *) erts_gc_byte_size_1;
    } else if (bf == abs_1) {
	op->a[1].val = (BeamInstr) (void *) erts_gc_abs_1;
    } else if (bf == float_1) {
	op->a[1].val = (BeamInstr) (void *) erts_gc_float_1;
    } else if (bf == round_1) {
	op->a[1].val = (BeamInstr) (void *) erts_gc_round_1;
    } else if (bf == trunc_1) {
	op->a[1].val = (BeamInstr) (void *) erts_gc_trunc_1;
    } else {
	abort();
    }
    op->a[2] = Src;
    op->a[3] = Live;
    op->a[4] = Dst;
    op->next = NULL;
    return op;
}

/*
 * This is used by the ops.tab rule that rewrites gc_bifs with two parameters
 * The instruction returned is then again rewritten to an i_load instruction
 * folowed by i_gc_bif2_jIId, to handle literals properly.
 * As opposed to the i_gc_bif1_jIsId, the instruction  i_gc_bif2_jIId is
 * always rewritten, regardless of if there actually are any literals.
 */
static GenOp*
gen_guard_bif2(LoaderState* stp, GenOpArg Fail, GenOpArg Live, GenOpArg Bif,
	      GenOpArg S1, GenOpArg S2, GenOpArg Dst)
{
    GenOp* op;
    BifFunction bf;

    NEW_GENOP(stp, op);
    op->op = genop_ii_gc_bif2_6;
    op->arity = 6;
    op->a[0] = Fail;
    op->a[1].type = TAG_u;
    bf = stp->import[Bif.val].bf;
    /* The translations here need to have a reverse counterpart in
       beam_emu.c:translate_gc_bif for error handling to work properly. */
    if (bf == binary_part_2) {
	op->a[1].val = (BeamInstr) (void *) erts_gc_binary_part_2;
    } else {
	abort();
    }
    op->a[2] = S1;
    op->a[3] = S2;
    op->a[4] = Live;
    op->a[5] = Dst;
    op->next = NULL;
    return op;
}

/*
 * This is used by the ops.tab rule that rewrites gc_bifs with three parameters
 * The instruction returned is then again rewritten to a move instruction that
 * uses r[0] for temp storage, followed by an i_load instruction,
 * folowed by i_gc_bif3_jIsId, to handle literals properly. Rewriting
 * always occur, as with the gc_bif2 counterpart.
 */
static GenOp*
gen_guard_bif3(LoaderState* stp, GenOpArg Fail, GenOpArg Live, GenOpArg Bif,
	      GenOpArg S1, GenOpArg S2, GenOpArg S3, GenOpArg Dst)
{
    GenOp* op;
    BifFunction bf;

    NEW_GENOP(stp, op);
    op->op = genop_ii_gc_bif3_7;
    op->arity = 7;
    op->a[0] = Fail;
    op->a[1].type = TAG_u;
    bf = stp->import[Bif.val].bf;
    /* The translations here need to have a reverse counterpart in
       beam_emu.c:translate_gc_bif for error handling to work properly. */
    if (bf == binary_part_3) {
	op->a[1].val = (BeamInstr) (void *) erts_gc_binary_part_3;
    } else {
	abort();
    }
    op->a[2] = S1;
    op->a[3] = S2;
    op->a[4] = S3;
    op->a[5] = Live;
    op->a[6] = Dst;
    op->next = NULL;
    return op;
}


/*
 * Freeze the code in memory, move the string table into place,
 * resolve all labels.
 */

static int
freeze_code(LoaderState* stp)
{
    BeamInstr* code = stp->code;
    Uint *literal_end = NULL;
    Uint index;
    int i;
    byte* str_table;
    unsigned strtab_size = stp->chunks[STR_CHUNK].size;
    unsigned attr_size = stp->chunks[ATTR_CHUNK].size;
    unsigned compile_size = stp->chunks[COMPILE_CHUNK].size;
    Uint size;
    unsigned catches;
    Sint decoded_size;

    /*
     * Verify that there was a correct 'FunT' chunk if there were
     * make_fun2 instructions in the file.
     */

    if (stp->lambda_error != NULL) {
	LoadError0(stp, stp->lambda_error);
    }

    
    /*
     * Calculate the final size of the code.
     */

    size = (stp->ci * sizeof(BeamInstr)) + (stp->total_literal_size * sizeof(Eterm)) +
	strtab_size + attr_size + compile_size;

    /*
     * Move the code to its final location.
     */

    code = (BeamInstr *) erts_realloc(ERTS_ALC_T_CODE, (void *) code, size);
    CHKBLK(ERTS_ALC_T_CODE,code);
    /*
     * Place a pointer to the op_int_code_end instruction in the
     * function table in the beginning of the file.
     */

    code[MI_FUNCTIONS+stp->num_functions] = (BeamInstr) (code + stp->ci - 1);
    CHKBLK(ERTS_ALC_T_CODE,code);

    /*
     * Store the pointer to the on_load function.
     */

    if (stp->on_load) {
	code[MI_ON_LOAD_FUNCTION_PTR] = (BeamInstr) (code + stp->on_load);
    } else {
	code[MI_ON_LOAD_FUNCTION_PTR] = 0;
    }
    CHKBLK(ERTS_ALC_T_CODE,code);

    literal_end = (Uint *) (code+stp->ci);
    /*
     * Place the literal heap directly after the code and fix up all
     * instructions that refer to it.
     */
    {
	Uint* ptr;
	Uint* low;
	Uint* high;
	LiteralPatch* lp;

	low = (Uint *) (code+stp->ci);
	high = low + stp->total_literal_size;
	code[MI_LITERALS_START] = (BeamInstr) low;
	code[MI_LITERALS_END] = (BeamInstr) high;
	ptr = low;
	for (i = 0; i < stp->num_literals; i++) {
	    Uint offset;

	    sys_memcpy(ptr, stp->literals[i].heap,
		       stp->literals[i].heap_size*sizeof(Eterm));
	    offset = ptr - stp->literals[i].heap;
	    stp->literals[i].offset = offset;
	    high = ptr + stp->literals[i].heap_size;
	    while (ptr < high) {
		Eterm val = *ptr;
		switch (primary_tag(val)) {
		case TAG_PRIMARY_LIST:
		case TAG_PRIMARY_BOXED:
		    *ptr++ = offset_ptr(val, offset);
		    break;
		case TAG_PRIMARY_HEADER:
		    ptr++;
		    if (header_is_thing(val)) {
			ptr += thing_arityval(val);
		    }
		    break;
		default:
		    ptr++;
		    break;
		}
	    }
	    ASSERT(ptr == high);
	}
	lp = stp->literal_patches;
	while (lp != 0) {
	    BeamInstr* op_ptr;
	    Uint literal;
	    Literal* lit;

	    op_ptr = code + lp->pos;
	    lit = &stp->literals[op_ptr[0]];
	    literal = lit->term;
	    if (is_boxed(literal) || is_list(literal)) {
		literal = offset_ptr(literal, lit->offset);
	    }
	    op_ptr[0] = literal;
	    lp = lp->next;
	}
	literal_end += stp->total_literal_size;
    }
    
    /*
     * Place the string table and, optionally, attributes, after the literal heap.
     */
    CHKBLK(ERTS_ALC_T_CODE,code);

    sys_memcpy(literal_end, stp->chunks[STR_CHUNK].start, strtab_size);
    CHKBLK(ERTS_ALC_T_CODE,code);
    str_table = (byte *) literal_end;
    if (attr_size) {
	byte* attr = str_table + strtab_size;
	sys_memcpy(attr, stp->chunks[ATTR_CHUNK].start, stp->chunks[ATTR_CHUNK].size);
	code[MI_ATTR_PTR] = (BeamInstr) attr;
	code[MI_ATTR_SIZE] = (BeamInstr) stp->chunks[ATTR_CHUNK].size;
	decoded_size = erts_decode_ext_size(attr, attr_size, 0);
	if (decoded_size < 0) {
 	    LoadError0(stp, "bad external term representation of module attributes");
 	}
	code[MI_ATTR_SIZE_ON_HEAP] = decoded_size;
    }
    CHKBLK(ERTS_ALC_T_CODE,code);
    if (compile_size) {
	byte* compile_info = str_table + strtab_size + attr_size;
    CHKBLK(ERTS_ALC_T_CODE,code);
	sys_memcpy(compile_info, stp->chunks[COMPILE_CHUNK].start,
	       stp->chunks[COMPILE_CHUNK].size);
    CHKBLK(ERTS_ALC_T_CODE,code);
	code[MI_COMPILE_PTR] = (BeamInstr) compile_info;
    CHKBLK(ERTS_ALC_T_CODE,code);
	code[MI_COMPILE_SIZE] = (BeamInstr) stp->chunks[COMPILE_CHUNK].size;
    CHKBLK(ERTS_ALC_T_CODE,code);
	decoded_size = erts_decode_ext_size(compile_info, compile_size, 0);
    CHKBLK(ERTS_ALC_T_CODE,code);
	if (decoded_size < 0) {
 	    LoadError0(stp, "bad external term representation of compilation information");
 	}
    CHKBLK(ERTS_ALC_T_CODE,code);
	code[MI_COMPILE_SIZE_ON_HEAP] = decoded_size;
    }
    CHKBLK(ERTS_ALC_T_CODE,code);

    /*
     * Go through all i_new_bs_put_strings instructions, restore the pointer to
     * the instruction and convert string offsets to pointers (to the
     * FIRST character).
     */

    index = stp->new_bs_put_strings;
    while (index != 0) {
	Uint next = code[index];
	code[index] = BeamOpCode(op_bs_put_string_II);
	code[index+2] = (BeamInstr) (str_table + code[index+2]);
	index = next;
    }
    CHKBLK(ERTS_ALC_T_CODE,code);

    {
	StringPatch* sp = stp->string_patches;

	while (sp != 0) {
	    BeamInstr* op_ptr;
	    byte* strp;

	    op_ptr = code + sp->pos;
	    strp = str_table + op_ptr[0];
	    op_ptr[0] = (BeamInstr) strp;
	    sp = sp->next;
	}
    }
    CHKBLK(ERTS_ALC_T_CODE,code);

    /*
     * Resolve all labels.
     */

    for (i = 0; i < stp->num_labels; i++) {
	Uint this_patch;
	Uint next_patch;
	Uint value = stp->labels[i].value;
	
	if (value == 0 && stp->labels[i].patches != 0) {
	    LoadError1(stp, "label %d not resolved", i);
	}
	ASSERT(value < stp->ci);
	this_patch = stp->labels[i].patches;
	while (this_patch != 0) {
	    ASSERT(this_patch < stp->ci);
	    next_patch = code[this_patch];
	    ASSERT(next_patch < stp->ci);
	    code[this_patch] = (BeamInstr) (code + value);
	    this_patch = next_patch;
	}
    }
    CHKBLK(ERTS_ALC_T_CODE,code);

    /*
     * Fix all catch_yf instructions.
     */
    index = stp->catches;
    catches = BEAM_CATCHES_NIL;
    while (index != 0) {
	BeamInstr next = code[index];
	code[index] = BeamOpCode(op_catch_yf);
	catches = beam_catches_cons((BeamInstr *)code[index+2], catches);
	code[index+2] = make_catch(catches);
	index = next;
    }
    stp->catches = catches;
    CHKBLK(ERTS_ALC_T_CODE,code);

    /*
     * Save the updated code pointer and code size.
     */

    stp->code = code;
    stp->loaded_size = size;

    CHKBLK(ERTS_ALC_T_CODE,code);
    return 1;

 load_error:
    /*
     * Make sure that the caller frees the newly reallocated block, and
     * not the old one (in case it has moved).
     */
    stp->code = code;
    return 0;
}


static void
final_touch(LoaderState* stp)
{
    int i;
    int on_load = stp->on_load;

    /*
     * Export functions.
     */

    for (i = 0; i < stp->num_exps; i++) {
	Export* ep = erts_export_put(stp->module, stp->export[i].function,
				     stp->export[i].arity);
	if (!on_load) {
	    ep->address = stp->export[i].address;
	} else {
	    /*
	     * Don't make any of the exported functions
	     * callable yet.
	     */
	    ep->address = ep->code+3;
	    ep->code[4] = (BeamInstr) stp->export[i].address;
	}
    }

    /*
     * Import functions and patch all callers.
     */

    for (i = 0; i < stp->num_imports; i++) {
	Eterm mod;
	Eterm func;
	Uint arity;
	BeamInstr import;
	Uint current;
	Uint next;

	mod = stp->import[i].module;
	func = stp->import[i].function;
	arity = stp->import[i].arity;
	import = (BeamInstr) erts_export_put(mod, func, arity);
	current = stp->import[i].patches;
	while (current != 0) {
	    ASSERT(current < stp->ci);
	    next = stp->code[current];
	    stp->code[current] = import;
	    current = next;
	}
    }

    /*
     * Fix all funs.
     */ 

    if (stp->num_lambdas > 0) {
	for (i = 0; i < stp->num_lambdas; i++) {
	    unsigned entry_label = stp->lambdas[i].label;
	    ErlFunEntry* fe = stp->lambdas[i].fe;
	    BeamInstr* code_ptr = (BeamInstr *) (stp->code + stp->labels[entry_label].value);

	    if (fe->address[0] != 0) {
		/*
		 * We are hiding a pointer into older code.
		 */
		erts_refc_dec(&fe->refc, 1);
	    }
	    fe->address = code_ptr;
#ifdef HIPE
	    hipe_set_closure_stub(fe, stp->lambdas[i].num_free);
#endif
	}
    }
}


static int
transform_engine(LoaderState* st)
{
    Uint op;
    int ap;			/* Current argument. */
    Uint* restart;		/* Where to restart if current match fails. */
    GenOpArg def_vars[TE_MAX_VARS]; /* Default buffer for variables. */
    GenOpArg* var = def_vars;
    int i;			/* General index. */
    Uint mask;
    GenOp* instr;
    Uint* pc;
    int rval;

    ASSERT(gen_opc[st->genop->op].transform != -1);
    pc = op_transform + gen_opc[st->genop->op].transform;
    restart = pc;

 restart:
    if (var != def_vars) {
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) var);
	var = def_vars;
    }
    ASSERT(restart != NULL);
    pc = restart;
    ASSERT(*pc < NUM_TOPS);	/* Valid instruction? */
    ASSERT(*pc == TOP_try_me_else || *pc == TOP_fail);
    instr = st->genop;

#define RETURN(r) rval = (r); goto do_return;

#ifdef DEBUG
    restart = NULL;
#endif
    ap = 0;
    for (;;) {
	op = *pc++;

	switch (op) {
	case TOP_is_op:
	    if (instr == NULL) {
		/*
		 * We'll need at least one more instruction to decide whether
		 * this combination matches or not.
		 */
		RETURN(TE_SHORT_WINDOW);
	    }
	    if (*pc++ != instr->op)
		goto restart;
	    break;
	case TOP_is_type:
	    mask = *pc++;

	    ASSERT(ap < instr->arity);
	    ASSERT(instr->a[ap].type < BEAM_NUM_TAGS);
	    if (((1 << instr->a[ap].type) & mask) == 0)
		goto restart;
	    break;
	case TOP_pred:
	    i = *pc++;
	    switch (i) {
#define RVAL i
#include "beam_pred_funcs.h"
#undef RVAL
	    default:
		ASSERT(0);
	    }
	    if (i == 0)
		goto restart;
	    break;
	case TOP_is_eq:
	    ASSERT(ap < instr->arity);
	    if (*pc++ != instr->a[ap].val)
		goto restart;
	    break;
	case TOP_is_same_var:
	    ASSERT(ap < instr->arity);
	    i = *pc++;
	    ASSERT(i < TE_MAX_VARS);
	    if (var[i].type != instr->a[ap].type)
		goto restart;
	    switch (var[i].type) {
	    case TAG_r: case TAG_n: break;
	    default:
		if (var[i].val != instr->a[ap].val)
		    goto restart;
	    }
	    break;
#if defined(TOP_is_bif)
	case TOP_is_bif:
	    {
		int bif_number = *pc++;
		
		/*
		 * In debug build, the type must be 'u'.
		 * In a real build, don't match.  (I.e. retain the original
		 * call instruction, this will work, but it will be a
		 * slight performance loss.)
		 */

		ASSERT(instr->a[ap].type == TAG_u);
		if (instr->a[ap].type != TAG_u)
		    goto restart;

		/*
		 * In debug build, the assertion will catch invalid indexes
		 * immediately.  In a real build, the loader will issue
		 * an diagnostic later when the instruction is loaded.
		 */

		i = instr->a[ap].val;
		ASSERT(i < st->num_imports);
		if (i >= st->num_imports || st->import[i].bf == NULL)
		    goto restart;
		if (bif_number != -1 &&
		    bif_export[bif_number]->code[4] != (BeamInstr) st->import[i].bf) {
		    goto restart;
		}
	    }
	    break;

#endif
#if defined(TOP_is_not_bif)
	case TOP_is_not_bif:
	    {
		pc++;
		
		/*
		 * In debug build, the type must be 'u'.
		 */

		ASSERT(instr->a[ap].type == TAG_u);
		if (instr->a[ap].type != TAG_u) {
		    goto restart;
		}
		i = instr->a[ap].val;

		/*
		 * erlang:apply/2,3 are strange. They exist as (dummy) BIFs
		 * so that they are included in the export table before
		 * the erlang module is loaded. They also exist in the erlang
		 * module as functions. When used in code, a special Beam
		 * instruction is used.
		 * 
		 * Below we specially recognize erlang:apply/2,3 as special.
		 * This is necessary because after setting a trace pattern on
		 * them, you cannot no longer see from the export entry that
		 * they are special.
		 */
		if (i < st->num_imports) {
		    if (st->import[i].bf != NULL ||
			(st->import[i].module == am_erlang &&
			 st->import[i].function == am_apply &&
			 (st->import[i].arity == 2 || st->import[i].arity == 3))) {
			goto restart;
		    }
		}
	    }
	    break;

#endif
#if defined(TOP_is_func)
	case TOP_is_func:
	    {
		Eterm mod = *pc++;
		Eterm func = *pc++;
		int arity = *pc++;

		ASSERT(instr->a[ap].type == TAG_u);
		if (instr->a[ap].type != TAG_u) {
		    goto restart;
		}
		i = instr->a[ap].val;
		ASSERT(i < st->num_imports);
		if (i >= st->num_imports || st->import[i].module != mod ||
		    st->import[i].function != func ||
		    (arity < MAX_ARG && st->import[i].arity != arity)) {
		    goto restart;
		}
	    }
	    break;
#endif
	case TOP_set_var_next_arg:
	    ASSERT(ap < instr->arity);
	    i = *pc++;
	    ASSERT(i < TE_MAX_VARS);
	    var[i].type = instr->a[ap].type;
	    var[i].val = instr->a[ap].val;
	    ap++;
	    break;

#if defined(TOP_rest_args)
	case TOP_rest_args:
	    {
		int n = *pc++;
		var = erts_alloc(ERTS_ALC_T_LOADER_TMP,
				 instr->arity * sizeof(GenOpArg));
		for (i = 0; i < n; i++) {
		    var[i] = def_vars[i];
		}
		while (i < instr->arity) {
		    var[i] = instr->a[i];
		    i++;
		}
	    }
	    break;
#endif

	case TOP_next_arg:
	    ap++;
	    break;
	case TOP_next_instr:
	    instr = instr->next;
	    ap = 0;
	    break;
	case TOP_commit:
	    instr = instr->next; /* The next_instr was optimized away. */

	    /*
	     * The left-hand side of this transformation matched.
	     * Delete all matched instructions.
	     */
	    while (st->genop != instr) {
		GenOp* next = st->genop->next;
		FREE_GENOP(st, st->genop);
		st->genop = next;
	    }
#ifdef DEBUG
	    instr = 0;
#endif
	    break;

#if defined(TOP_call)
	case TOP_call:
	    {
		GenOp** lastp;
		GenOp* new_instr;

		i = *pc++;
		switch (i) {
#define RVAL new_instr
#include "beam_tr_funcs.h"
#undef RVAL
		default:
		    new_instr = NULL; /* Silence compiler warning. */
		    ASSERT(0);
		}
		if (new_instr == NULL) {
		    goto restart;
		}

		lastp = &new_instr;
		while (*lastp != NULL) {
		    lastp = &((*lastp)->next);
		}
		 
		instr = instr->next; /* The next_instr was optimized away. */

		/*
		 * The left-hand side of this transformation matched.
		 * Delete all matched instructions.
		 */
		while (st->genop != instr) {
		    GenOp* next = st->genop->next;
		    FREE_GENOP(st, st->genop);
		    st->genop = next;
		}
		*lastp = st->genop;
		st->genop = new_instr;
	    }
	    break;
#endif
	case TOP_new_instr:
	    /*
	     * Note that the instructions are generated in reverse order.
	     */
	    NEW_GENOP(st, instr);
	    instr->next = st->genop;
	    st->genop = instr;
	    ap = 0;
	    break;
	case TOP_store_op:
	    instr->op = *pc++;
	    instr->arity = *pc++;
	    break;
	case TOP_store_type:
	    i = *pc++;
	    instr->a[ap].type = i;
	    instr->a[ap].val = 0;
	    break;
	case TOP_store_val:
	    i = *pc++;
	    instr->a[ap].val = i;
	    break;
	case TOP_store_var:
	    i = *pc++;
	    ASSERT(i < TE_MAX_VARS);
	    instr->a[ap].type = var[i].type;
	    instr->a[ap].val = var[i].val;
	    break;
	case TOP_try_me_else:
	    restart = pc + 1;
	    restart += *pc++;
	    ASSERT(*pc < NUM_TOPS); /* Valid instruction? */
	    break;
	case TOP_end:
	    RETURN(TE_OK);
	case TOP_fail:
	    RETURN(TE_FAIL)
	default:
	    ASSERT(0);
	}
    }
#undef RETURN

 do_return:
    if (var != def_vars) {
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) var);
    }
    return rval;
}


static void
short_file(int line, LoaderState* stp, unsigned needed)
{
    load_printf(line, stp, "unexpected end of %s when reading %d byte(s)",
		stp->file_name, needed);
}


static void
load_printf(int line, LoaderState* context, char *fmt,...)
{
    erts_dsprintf_buf_t *dsbufp;
    va_list va;

    if (is_non_value(context->module)) {
	/* Suppressed by code:get_chunk/2 */
	return;
    }

    dsbufp = erts_create_logger_dsbuf();

    erts_dsprintf(dsbufp, "%s(%d): Error loading ", __FILE__, line);

    if (is_atom(context->function))
	erts_dsprintf(dsbufp, "function %T:%T/%d", context->module,
		      context->function, context->arity);
    else
	erts_dsprintf(dsbufp, "module %T", context->module);

    if (context->genop)
	erts_dsprintf(dsbufp, ": op %s", gen_opc[context->genop->op].name);

    if (context->specific_op != -1)
	erts_dsprintf(dsbufp, ": %s", opc[context->specific_op].sign);
    else if (context->genop) {
	int i;
	for (i = 0; i < context->genop->arity; i++)
	    erts_dsprintf(dsbufp, " %c",
			  tag_to_letter[context->genop->a[i].type]);
    }

    erts_dsprintf(dsbufp, ":\n  ");

    va_start(va, fmt);
    erts_vdsprintf(dsbufp, fmt, va);
    va_end(va);

    erts_dsprintf(dsbufp, "\n");
#ifdef DEBUG
    erts_fprintf(stderr, "%s", dsbufp->str);
#endif
    erts_send_error_to_logger(context->group_leader, dsbufp);
}


static int
get_int_val(LoaderState* stp, Uint len_code, BeamInstr* result)
{
    Uint count;
    Uint val;

    len_code >>= 5;
    ASSERT(len_code < 8);
    if (len_code == 7) {
	LoadError0(stp, "can't load integers bigger than 8 bytes yet\n");
    }
    count = len_code + 2;
    if (count == 5) {
	Uint msb;
	GetByte(stp, msb);
	if (msb == 0) {
	    count--;
	}
	GetInt(stp, 4, *result);
    } else if (count <= 4) {
	GetInt(stp, count, val);
	*result = ((val << 8*(sizeof(val)-count)) >> 8*(sizeof(val)-count));
    } else {
	LoadError1(stp, "too big integer; %d bytes\n", count);
    }
    return 1;

 load_error:
    return 0;
}


static int
get_erlang_integer(LoaderState* stp, Uint len_code, BeamInstr* result)
{
    Uint count;
    Sint val;
    byte default_buf[128];
    byte* bigbuf = default_buf;
    byte* s;
    int i;
    int neg = 0;
    Uint arity;
    Eterm* hp;

    /*
     * Retrieve the size of the value in bytes.
     */

    len_code >>= 5;
    if (len_code < 7) {
	count = len_code + 2;
    } else {
	Uint tag;
	UWord len_word;

	ASSERT(len_code == 7);
	GetTagAndValue(stp, tag, len_word);
	VerifyTag(stp, TAG_u, tag);
	count = len_word + 9;
    }

    /*
     * Handle values up to the size of an int, meaning either a small or bignum.
     */

    if (count <= sizeof(val)) {
	GetInt(stp, count, val);

	val = ((val << 8*(sizeof(val)-count)) >> 8*(sizeof(val)-count));
	if (IS_SSMALL(val)) {
	    *result = val;
	    return TAG_i;
	} else {
	    *result = new_literal(stp, &hp, BIG_UINT_HEAP_SIZE);
	    (void) small_to_big(val, hp);
	    return TAG_q;
	}
    }

    /*
     * Make sure that the number will fit in our temporary buffer
     * (including margin).
     */

    if (count+8 > sizeof(default_buf)) {
	bigbuf = erts_alloc(ERTS_ALC_T_LOADER_TMP, count+8);
    }

    /*
     * Copy the number reversed to our temporary buffer.
     */

    GetString(stp, s, count);
    for (i = 0; i < count; i++) {
	bigbuf[count-i-1] = *s++;
    }

    /*
     * Check if the number is negative, and negate it if so.
     */

    if ((bigbuf[count-1] & 0x80) != 0) {
	unsigned carry = 1;

	neg = 1;
	for (i = 0; i < count; i++) {
	    bigbuf[i] = ~bigbuf[i] + carry;
	    carry = (bigbuf[i] == 0 && carry == 1);
	}
	ASSERT(carry == 0);
    }

    /*
     * Align to word boundary.
     */

    if (bigbuf[count-1] == 0) {
	count--;
    }
    if (bigbuf[count-1] == 0) {
	LoadError0(stp, "bignum not normalized");
    }
    while (count % sizeof(Eterm) != 0) {
	bigbuf[count++] = 0;
    }

    /*
     * Allocate heap space for the bignum and copy it.
     */

    arity = count/sizeof(Eterm);
    *result = new_literal(stp, &hp, arity+1);
    (void) bytes_to_big(bigbuf, count, neg, hp);

    if (bigbuf != default_buf) {
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) bigbuf);
    }
    return TAG_q;

 load_error:
    if (bigbuf != default_buf) {
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) bigbuf);
    }
    return -1;
}

/*
 * Converts an IFF id to a printable string.
 */

static void
id_to_string(Uint id, char* s)
{
    int i;

    for (i = 3; i >= 0; i--) {
	*s++ = (id >> i*8) & 0xff;
    }
    *s++ = '\0';
}

static void
new_genop(LoaderState* stp)
{
    GenOpBlock* p = (GenOpBlock *) erts_alloc(ERTS_ALC_T_LOADER_TMP,
					      sizeof(GenOpBlock));
    int i;

    p->next = stp->genop_blocks;
    stp->genop_blocks = p;
    for (i = 0; i < sizeof(p->genop)/sizeof(p->genop[0])-1; i++) {
	p->genop[i].next = p->genop + i + 1;
    }
    p->genop[i].next = NULL;
    stp->free_genop = p->genop;
}

static int
new_label(LoaderState* stp)
{
    int num = stp->num_labels;

    stp->num_labels++;
    stp->labels = (Label *) erts_realloc(ERTS_ALC_T_LOADER_TMP,
					 (void *) stp->labels,
					 stp->num_labels * sizeof(Label));
    stp->labels[num].value = 0;
    stp->labels[num].patches = 0;
    return num;
}

static void
new_literal_patch(LoaderState* stp, int pos)
{
    LiteralPatch* p = erts_alloc(ERTS_ALC_T_LOADER_TMP, sizeof(LiteralPatch));
    p->pos = pos;
    p->next = stp->literal_patches;
    stp->literal_patches = p;
}

static void
new_string_patch(LoaderState* stp, int pos)
{
    StringPatch* p = erts_alloc(ERTS_ALC_T_LOADER_TMP, sizeof(StringPatch));
    p->pos = pos;
    p->next = stp->string_patches;
    stp->string_patches = p;
}

static Uint
new_literal(LoaderState* stp, Eterm** hpp, Uint heap_size)
{
    Literal* lit;

    if (stp->allocated_literals == 0) {
	Uint need;

	ASSERT(stp->literals == 0);
	ASSERT(stp->num_literals == 0);
	stp->allocated_literals = 8;
	need = stp->allocated_literals * sizeof(Literal);
	stp->literals = (Literal *) erts_alloc(ERTS_ALC_T_LOADER_TMP,
					       need);
    } else if (stp->allocated_literals <= stp->num_literals) {
	Uint need;

	stp->allocated_literals *= 2;
	need = stp->allocated_literals * sizeof(Literal);
	stp->literals = (Literal *) erts_realloc(ERTS_ALC_T_LOADER_TMP,
						 (void *) stp->literals,
						 need);
    }

    stp->total_literal_size += heap_size;
    lit = stp->literals + stp->num_literals;
    lit->offset = 0;
    lit->heap_size = heap_size;
    lit->heap = erts_alloc(ERTS_ALC_T_LOADER_TMP, heap_size*sizeof(Eterm));
    lit->term = make_boxed(lit->heap);
    *hpp = lit->heap;
    return stp->num_literals++;
}

Eterm
erts_module_info_0(Process* p, Eterm module)
{
    Eterm *hp;
    Eterm list = NIL;
    Eterm tup;

    if (is_not_atom(module)) {
	return THE_NON_VALUE;
    }

    if (erts_get_module(module) == NULL) {
	return THE_NON_VALUE;
    }

#define BUILD_INFO(What) \
    tup = erts_module_info_1(p, module, What); \
    hp = HAlloc(p, 5); \
    tup = TUPLE2(hp, What, tup); \
    hp += 3; \
    list = CONS(hp, tup, list)

    BUILD_INFO(am_compile);
    BUILD_INFO(am_attributes);
    BUILD_INFO(am_imports);
    BUILD_INFO(am_exports);
#undef BUILD_INFO
    return list;
}

Eterm
erts_module_info_1(Process* p, Eterm module, Eterm what)
{
    if (what == am_module) {
	return module;
    } else if (what == am_imports) {
	return NIL;
    } else if (what == am_exports) {
	return exported_from_module(p, module);
    } else if (what == am_functions) {
	return functions_in_module(p, module);
    } else if (what == am_attributes) {
	return attributes_for_module(p, module);
    } else if (what == am_compile) {
	return compilation_info_for_module(p, module);
    } else if (what == am_native_addresses) {
	return native_addresses(p, module);
    }
    return THE_NON_VALUE;
}

/*
 * Builds a list of all functions in the given module:
 *     [{Name, Arity},...]
 *
 * Returns a tagged term, or 0 on error.
 */

Eterm
functions_in_module(Process* p, /* Process whose heap to use. */
		     Eterm mod) /* Tagged atom for module. */
{
    Module* modp;
    BeamInstr* code;
    int i;
    Uint num_functions;
    Eterm* hp;
    Eterm result = NIL;

    if (is_not_atom(mod)) {
	return THE_NON_VALUE;
    }

    modp = erts_get_module(mod);
    if (modp == NULL) {
	return THE_NON_VALUE;
    }
    code = modp->code;
    num_functions = code[MI_NUM_FUNCTIONS];
    hp = HAlloc(p, 5*num_functions);
    for (i = num_functions-1; i >= 0 ; i--) {
	BeamInstr* func_info = (BeamInstr *) code[MI_FUNCTIONS+i];
	Eterm name = (Eterm) func_info[3];
	int arity = (int) func_info[4];
	Eterm tuple;

	ASSERT(is_atom(name));
	tuple = TUPLE2(hp, name, make_small(arity));
	hp += 3;
	result = CONS(hp, tuple, result);
	hp += 2;
    }
    return result;
}

/*
 * Builds a list of all functions including native addresses.
 *     [{Name,Arity,NativeAddress},...]
 *
 * Returns a tagged term, or 0 on error.
 */

static Eterm
native_addresses(Process* p, Eterm mod)
{
    Module* modp;
    BeamInstr* code;
    int i;
    Eterm* hp;
    Uint num_functions;
    Uint need;
    Eterm* hp_end;
    Eterm result = NIL;

    if (is_not_atom(mod)) {
	return THE_NON_VALUE;
    }

    modp = erts_get_module(mod);
    if (modp == NULL) {
	return THE_NON_VALUE;
    }

    code = modp->code;
    num_functions = code[MI_NUM_FUNCTIONS];
    need = (6+BIG_UINT_HEAP_SIZE)*num_functions;
    hp = HAlloc(p, need);
    hp_end = hp + need;
    for (i = num_functions-1; i >= 0 ; i--) {
	BeamInstr* func_info = (BeamInstr *) code[MI_FUNCTIONS+i];
	Eterm name = (Eterm) func_info[3];
	int arity = (int) func_info[4];
	Eterm tuple;

	ASSERT(is_atom(name));
	if (func_info[1] != 0) {
	    Eterm addr = erts_bld_uint(&hp, NULL, func_info[1]);
	    tuple = erts_bld_tuple(&hp, NULL, 3, name, make_small(arity), addr);
	    result = erts_bld_cons(&hp, NULL, tuple, result);
	}
    }
    HRelease(p, hp_end, hp);
    return result;
}


/*
 * Builds a list of all exported functions in the given module:
 *     [{Name, Arity},...]
 *
 * Returns a tagged term, or 0 on error.
 */

Eterm
exported_from_module(Process* p, /* Process whose heap to use. */
		     Eterm mod) /* Tagged atom for module. */
{
    int i;
    Eterm* hp = NULL;
    Eterm* hend = NULL;
    Eterm result = NIL;

    if (is_not_atom(mod)) {
	return THE_NON_VALUE;
    }

    for (i = 0; i < export_list_size(); i++) {
	Export* ep = export_list(i);
	
	if (ep->code[0] == mod) {
	    Eterm tuple;
	    
	    if (ep->address == ep->code+3 &&
		ep->code[3] == (BeamInstr) em_call_error_handler) {
		/* There is a call to the function, but it does not exist. */ 
		continue;
	    }

	    if (hp == hend) {
		int need = 10 * 5;
		hp = HAlloc(p, need);
		hend = hp + need;
	    }
	    tuple = TUPLE2(hp, ep->code[1], make_small(ep->code[2]));
	    hp += 3;
	    result = CONS(hp, tuple, result);
	    hp += 2;
	}
    }
    HRelease(p,hend,hp);
    return result;
}


/*
 * Returns a list of all attributes for the module.
 *
 * Returns a tagged term, or 0 on error.
 */

Eterm
attributes_for_module(Process* p, /* Process whose heap to use. */
		      Eterm mod) /* Tagged atom for module. */

{
    Module* modp;
    BeamInstr* code;
    Eterm* hp;
    byte* ext;
    Eterm result = NIL;
    Eterm* end;

    if (is_not_atom(mod) || (is_not_list(result) && is_not_nil(result))) {
	return THE_NON_VALUE;
    }

    modp = erts_get_module(mod);
    if (modp == NULL) {
	return THE_NON_VALUE;
    }
    code = modp->code;
    ext = (byte *) code[MI_ATTR_PTR];
    if (ext != NULL) {
	hp = HAlloc(p, code[MI_ATTR_SIZE_ON_HEAP]);
	end = hp + code[MI_ATTR_SIZE_ON_HEAP];
	result = erts_decode_ext(&hp, &MSO(p), &ext);
	if (is_value(result)) {
	    ASSERT(hp <= end);
	}
        HRelease(p,end,hp);
    }
    return result;
}


/*
 * Returns a list containing compilation information.
 *
 * Returns a tagged term, or 0 on error.
 */

Eterm
compilation_info_for_module(Process* p, /* Process whose heap to use. */
			    Eterm mod) /* Tagged atom for module. */
{
    Module* modp;
    BeamInstr* code;
    Eterm* hp;
    byte* ext;
    Eterm result = NIL;
    Eterm* end;

    if (is_not_atom(mod) || (is_not_list(result) && is_not_nil(result))) {
	return THE_NON_VALUE;
    }

    modp = erts_get_module(mod);
    if (modp == NULL) {
	return THE_NON_VALUE;
    }
    code = modp->code;
    ext = (byte *) code[MI_COMPILE_PTR];
    if (ext != NULL) {
	hp = HAlloc(p, code[MI_COMPILE_SIZE_ON_HEAP]);
	end = hp + code[MI_COMPILE_SIZE_ON_HEAP];
	result = erts_decode_ext(&hp, &MSO(p), &ext);
	if (is_value(result)) {
	    ASSERT(hp <= end);
	}
        HRelease(p,end,hp);
    }
    return result;
}


/*
 * Returns a pointer to {module, function, arity}, or NULL if not found.
 */
BeamInstr *
find_function_from_pc(BeamInstr* pc)
{
    Range* low = modules;
    Range* high = low + num_loaded_modules;
    Range* mid = mid_module;

    while (low < high) {
	if (pc < mid->start) {
	    high = mid;
	} else if (pc > mid->end) {
	    low = mid + 1;
	} else {
	    BeamInstr** low1 = (BeamInstr **) (mid->start + MI_FUNCTIONS);
	    BeamInstr** high1 = low1 + mid->start[MI_NUM_FUNCTIONS];
	    BeamInstr** mid1;

	    while (low1 < high1) {
		mid1 = low1 + (high1-low1) / 2;
		if (pc < mid1[0]) {
		    high1 = mid1;
		} else if (pc < mid1[1]) {
		    mid_module = mid;
		    return mid1[0]+2;
		} else {
		    low1 = mid1 + 1;
		}
	    }
	    return NULL;
	}
	mid = low + (high-low) / 2;
    }
    return NULL;
}

/*
 * Read a specific chunk from a Beam binary.
 */

Eterm
code_get_chunk_2(Process* p, Eterm Bin, Eterm Chunk)
{
    LoaderState state;
    Uint chunk = 0;
    ErlSubBin* sb;
    Uint offset;
    Uint bitoffs;
    Uint bitsize;
    byte* start;
    int i;
    Eterm res;
    Eterm real_bin;
    byte* temp_alloc = NULL;

    if ((start = erts_get_aligned_binary_bytes(Bin, &temp_alloc)) == NULL) {
    error:
	erts_free_aligned_binary_bytes(temp_alloc);
	BIF_ERROR(p, BADARG);
    }
    state.module = THE_NON_VALUE; /* Suppress diagnostiscs */
    state.file_name = "IFF header for Beam file";
    state.file_p = start;
    state.file_left = binary_size(Bin);
    for (i = 0; i < 4; i++) {
	Eterm* chunkp;
	Eterm num;
	if (is_not_list(Chunk)) {
	    goto error;
	}
	chunkp = list_val(Chunk);
	num = CAR(chunkp);
	Chunk = CDR(chunkp);
	if (!is_byte(num)) {
	    goto error;
	}
	chunk = chunk << 8 | unsigned_val(num);
    }
    if (is_not_nil(Chunk)) {
	goto error;
    }
    if (!scan_iff_file(&state, &chunk, 1, 1)) {
	erts_free_aligned_binary_bytes(temp_alloc);
	return am_undefined;
    }
    ERTS_GET_REAL_BIN(Bin, real_bin, offset, bitoffs, bitsize);
    if (bitoffs) {
	res = new_binary(p, state.chunks[0].start, state.chunks[0].size);
    } else {
	sb = (ErlSubBin *) HAlloc(p, ERL_SUB_BIN_SIZE);
	sb->thing_word = HEADER_SUB_BIN;
	sb->orig = real_bin;
	sb->size = state.chunks[0].size;
	sb->bitsize = 0;
	sb->bitoffs = 0;
	sb->offs = offset + (state.chunks[0].start - start);
	sb->is_writable = 0;
	res = make_binary(sb);
    }
    erts_free_aligned_binary_bytes(temp_alloc);
    return res;
}

/*
 * Calculate the MD5 for a module.
 */
  
Eterm
code_module_md5_1(Process* p, Eterm Bin)
{
    LoaderState state;
    byte* temp_alloc = NULL;

    if ((state.file_p = erts_get_aligned_binary_bytes(Bin, &temp_alloc)) == NULL) {
	BIF_ERROR(p, BADARG);
    }
    state.module = THE_NON_VALUE; /* Suppress diagnostiscs */
    state.file_name = "IFF header for Beam file";
    state.file_left = binary_size(Bin);

    if (!scan_iff_file(&state, chunk_types, NUM_CHUNK_TYPES, NUM_MANDATORY)) {
	return am_undefined;
    }
    erts_free_aligned_binary_bytes(temp_alloc);
    return new_binary(p, state.mod_md5, sizeof(state.mod_md5));
}

#define WORDS_PER_FUNCTION 6

static BeamInstr*
make_stub(BeamInstr* fp, Eterm mod, Eterm func, Uint arity, Uint native, BeamInstr OpCode)
{
    fp[0] = (BeamInstr) BeamOp(op_i_func_info_IaaI);
    fp[1] = native;
    fp[2] = mod;
    fp[3] = func;
    fp[4] = arity;
#ifdef HIPE
    if (native) {
	fp[5] = BeamOpCode(op_move_return_nr);
	hipe_mfa_save_orig_beam_op(mod, func, arity, fp+5);
    }
#endif
    fp[5] = OpCode;
    return fp + WORDS_PER_FUNCTION;
}

static byte*
stub_copy_info(LoaderState* stp,
	       int chunk,	/* Chunk: ATTR_CHUNK or COMPILE_CHUNK */
	       byte* info,	/* Where to store info. */
	       BeamInstr* ptr_word,	/* Where to store pointer into info. */
	       BeamInstr* size_word) /* Where to store size of info. */
{
    Sint decoded_size;
    Uint size = stp->chunks[chunk].size;
    if (size != 0) {
	memcpy(info, stp->chunks[chunk].start, size);
	*ptr_word = (BeamInstr) info;
	decoded_size = erts_decode_ext_size(info, size, 0);
	if (decoded_size < 0) {
 	    return 0;
 	}
	*size_word = decoded_size;
    }
    return info + size;
}

static int
stub_read_export_table(LoaderState* stp)
{
    int i;

    GetInt(stp, 4, stp->num_exps);
    if (stp->num_exps > stp->num_functions) {
	LoadError2(stp, "%d functions exported; only %d functions defined",
		   stp->num_exps, stp->num_functions);
    }
    stp->export
	= (ExportEntry *) erts_alloc(ERTS_ALC_T_LOADER_TMP,
				     stp->num_exps * sizeof(ExportEntry));

    for (i = 0; i < stp->num_exps; i++) {
	Uint n;

	GetInt(stp, 4, n);
	GetAtom(stp, n, stp->export[i].function);
	GetInt(stp, 4, n);
	if (n > MAX_REG) {
	    LoadError2(stp, "export table entry %d: absurdly high arity %d", i, n);
	}
	stp->export[i].arity = n;
	GetInt(stp, 4, n);	/* Ignore label */
    }
    return 1;

 load_error:
    return 0;
}

static void
stub_final_touch(LoaderState* stp, BeamInstr* fp)
{
    int i;
    int n = stp->num_exps;
    Eterm function = fp[3];
    int arity = fp[4];
#ifdef HIPE
    Lambda* lp;
#endif

    /*
     * Test if the function should be exported.
     */

    for (i = 0; i < n; i++) {
	if (stp->export[i].function == function && stp->export[i].arity == arity) {
	    Export* ep = erts_export_put(fp[2], function, arity);
	    ep->address = fp+5;
	    return;
	}
    }

    /*
     * Must be a plain local function or a lambda local function.
     * Search the lambda table to find out which.
     */
    
#ifdef HIPE
    n = stp->num_lambdas;
    for (i = 0, lp = stp->lambdas; i < n; i++, lp++) {
        ErlFunEntry* fe = stp->lambdas[i].fe;
	if (lp->function == function && lp->arity == arity) {
	    fp[5] = (Eterm) BeamOpCode(op_hipe_trap_call_closure);
            fe->address = &(fp[5]);
	}
    }
#endif
    return;
}


/* Takes an erlang list of addresses:
   [{Adr, Patchtyppe} | Addresses]
   and the address of a fun_entry.
*/
int 
patch(Eterm Addresses, Uint fe) 
 {
#ifdef HIPE
  Eterm* listp;
  Eterm tuple;
  Eterm* tp;
  Eterm  patchtype;
  Uint AddressToPatch;

  while (!is_nil(Addresses)) {
    listp = list_val(Addresses);

    tuple = CAR(listp);
    if (is_not_tuple(tuple)) {
      return 0; /* Signal error */
    }

    tp = tuple_val(tuple);
    if (tp[0] != make_arityval(2)) {
      return 0; /* Signal error */
    }
    
    if(term_to_Uint(tp[1], &AddressToPatch) == 0) {
      return 0; /* Signal error */
    }

    patchtype = tp[2];    
    if (is_not_atom(patchtype)) {
      return 0; /* Signal error */
    }
    
    hipe_patch_address((Uint *)AddressToPatch, patchtype, fe);

    Addresses = CDR(listp);


  }

#endif
  return 1;
}


int
patch_funentries(Eterm Patchlist) 
 {
#ifdef HIPE   
  while (!is_nil(Patchlist)) {
    Eterm Info;
    Eterm MFA;
    Eterm Addresses;
    Eterm tuple;
    Eterm Mod;
    Eterm* listp;
    Eterm* tp;
    ErlFunEntry* fe;
    Uint index;
    Uint uniq;
    Uint native_address;
     
    listp = list_val(Patchlist);
    tuple = CAR(listp);
    Patchlist = CDR(listp);

    if (is_not_tuple(tuple)) {
      return 0; /* Signal error */
    }

    tp = tuple_val(tuple);
    if (tp[0] != make_arityval(3)) {
      return 0; /* Signal error */
    }
    
    Info = tp[1];
    if (is_not_tuple(Info)) {
      return 0; /* Signal error */
    }
    Addresses = tp[2];    
     if (is_not_list(Addresses)) {
       return 0; /* Signal error */
    }
    
    if(term_to_Uint(tp[3], &native_address) == 0) {
      return 0; /* Signal error */
    }



    tp = tuple_val(Info);
    if (tp[0] != make_arityval(3)) {
      return 0; /* Signal error */
    }
    MFA = tp[1];
    if (is_not_tuple(MFA)) {
      return 0; /* Signal error */
    }
    if(term_to_Uint(tp[2], &uniq) == 0){
      return 0; /* Signal error */
    }
    if(term_to_Uint(tp[3], &index) == 0) {
      return 0; /* Signal error */
    }




    tp = tuple_val(MFA);
    if (tp[0] != make_arityval(3)) {
      return 0; /* Signal error */
     }
    Mod = tp[1];
    if (is_not_atom(Mod)) {
      return 0; /* Signal error */
    }
      
  

    fe = erts_get_fun_entry(Mod, uniq, index);
    fe->native_address = (Uint *)native_address;
    erts_refc_dec(&fe->refc, 1);

    if (!patch(Addresses, (Uint) fe))
      return 0;

  }
#endif
  return 1; /* Signal that all went well */
}


/*
 * Do a dummy load of a module. No threaded code will be loaded.
 * Used for loading native code.
 * Will also patch all references to fun_entries to point to 
 * the new fun_entries created.
 */

Eterm
erts_make_stub_module(Process* p, Eterm Mod, Eterm Beam, Eterm Info)
{
    LoaderState state;
    BeamInstr Funcs;
    BeamInstr Patchlist;
    Eterm* tp;
    BeamInstr* code = NULL;
    BeamInstr* ptrs;
    BeamInstr* fp;
    byte* info;
    Uint ci;
    int n;
    int code_size;
    int rval;
    int i;
    ErlDrvBinary* bin = NULL;
    byte* temp_alloc = NULL;
    byte* bytes;
    Uint size;

    /*
     * Must initialize state.lambdas here because the error handling code
     * at label 'error' uses it.
     */
    init_state(&state);

    if (is_not_atom(Mod)) {
	goto error;
    }
    if (is_not_tuple(Info)) {
	goto error;
    }
    tp = tuple_val(Info);
    if (tp[0] != make_arityval(2)) {
      goto error;
    }
    Funcs = tp[1];
    Patchlist = tp[2];        
   
    if ((n = list_length(Funcs)) < 0) {
	goto error;
    }
    if ((bytes = erts_get_aligned_binary_bytes(Beam, &temp_alloc)) == NULL) {
	goto error;
    }
    size = binary_size(Beam);

    /*
     * Uncompressed if needed.
     */
    if (!(size >= 4 && bytes[0] == 'F' && bytes[1] == 'O' &&
	  bytes[2] == 'R' && bytes[3] == '1')) {
	bin = (ErlDrvBinary *) erts_gzinflate_buffer((char*)bytes, size);
	if (bin == NULL) {
	    goto error;
	}
	bytes = (byte*)bin->orig_bytes;
	size = bin->orig_size;
    }
    
    /*
     * Scan the Beam binary and read the interesting sections.
     */

    state.file_name = "IFF header for Beam file";
    state.file_p = bytes;
    state.file_left = size;
    state.module = Mod;
    state.group_leader = p->group_leader;
    state.num_functions = n;
    if (!scan_iff_file(&state, chunk_types, NUM_CHUNK_TYPES, NUM_MANDATORY)) {
	goto error;
    }
    define_file(&state, "code chunk header", CODE_CHUNK);
    if (!read_code_header(&state)) {
	goto error;
    }
    define_file(&state, "atom table", ATOM_CHUNK);
    if (!load_atom_table(&state)) {
	goto error;
    }
    define_file(&state, "export table", EXP_CHUNK);
    if (!stub_read_export_table(&state)) {
	goto error;
    }
    
    if (state.chunks[LAMBDA_CHUNK].size > 0) {
	define_file(&state, "lambda (fun) table", LAMBDA_CHUNK);
	if (!read_lambda_table(&state)) {
	    goto error;
	}
    }

    /*
     * Allocate memory for the stub module.
     */

    code_size = ((WORDS_PER_FUNCTION+1)*n + MI_FUNCTIONS + 2) * sizeof(BeamInstr);
    code_size += state.chunks[ATTR_CHUNK].size;
    code_size += state.chunks[COMPILE_CHUNK].size;
    code = erts_alloc_fnf(ERTS_ALC_T_CODE, code_size);
    if (!code) {
	goto error;
    }

    /*
     * Initialize code area.
     */

    code[MI_NUM_FUNCTIONS] = n;
    code[MI_ATTR_PTR] = 0;
    code[MI_ATTR_SIZE] = 0;
    code[MI_ATTR_SIZE_ON_HEAP] = 0;
    code[MI_COMPILE_PTR] = 0;
    code[MI_COMPILE_SIZE] = 0;
    code[MI_COMPILE_SIZE_ON_HEAP] = 0;
    code[MI_NUM_BREAKPOINTS] = 0;
    code[MI_ON_LOAD_FUNCTION_PTR] = 0;
    ci = MI_FUNCTIONS + n + 1;

    /*
     * Make stubs for all functions.
     */

    ptrs = code + MI_FUNCTIONS;
    fp = code + ci;
    for (i = 0; i < n; i++) {
	Eterm* listp;
	Eterm tuple;
	Eterm* tp;
	Eterm func;
	Eterm arity_term;
	Uint arity;
	Uint native_address;
	Eterm op;

	if (is_nil(Funcs)) {
	    break;
	}
	listp = list_val(Funcs);
	tuple = CAR(listp);
	Funcs = CDR(listp);

	/* Error checking */
	if (is_not_tuple(tuple)) {
	    goto error;
	}
	tp = tuple_val(tuple);
	if (tp[0] != make_arityval(3)) {
	    goto error;
	}
	func = tp[1];
	arity_term = tp[2];
	if (is_not_atom(func) || is_not_small(arity_term)) {
	    goto error;
	}
	arity = signed_val(arity_term);
	if (arity < 0) {
	    goto error;
	}
	if (term_to_Uint(tp[3], &native_address) == 0) {
	    goto error;
	}

	/*
	 * Set the pointer and make the stub. Put a return instruction
	 * as the body until we know what kind of trap we should put there.
	 */
	ptrs[i] = (BeamInstr) fp;
#ifdef HIPE
	op = (Eterm) BeamOpCode(op_hipe_trap_call); /* Might be changed later. */
#else
	op = (Eterm) BeamOpCode(op_move_return_nr);
#endif
	fp = make_stub(fp, Mod, func, arity, (Uint)native_address, op);
    }

    /*
     * Insert the last pointer and the int_code_end instruction.
     */

    ptrs[i] = (BeamInstr) fp;
    *fp++ = (BeamInstr) BeamOp(op_int_code_end);

    /*
     * Copy attributes and compilation information.
     */

    info = (byte *) fp;
    info = stub_copy_info(&state, ATTR_CHUNK, info,
			  code+MI_ATTR_PTR, code+MI_ATTR_SIZE_ON_HEAP);
    if (info == NULL) {
	goto error;
    }
    info = stub_copy_info(&state, COMPILE_CHUNK, info,
			  code+MI_COMPILE_PTR, code+MI_COMPILE_SIZE_ON_HEAP);
    if (info == NULL) {
	goto error;
    }

    /*
     * Insert the module in the module table.
     */

    rval = insert_new_code(p, 0, p->group_leader, Mod, code, code_size,
			   BEAM_CATCHES_NIL);
    if (rval < 0) {
	goto error;
    }

    /*
     * Export all stub functions and insert the correct type of HiPE trap.
     */

    fp = code + ci;
    for (i = 0; i < n; i++) {
	stub_final_touch(&state, fp);
	fp += WORDS_PER_FUNCTION;
    }

    if (patch_funentries(Patchlist)) {
	erts_free_aligned_binary_bytes(temp_alloc);
	if (state.lambdas != state.def_lambdas) {
	    erts_free(ERTS_ALC_T_LOADER_TMP, (void *) state.lambdas);
	}
	if (bin != NULL) {
	    driver_free_binary(bin);
	}
	return Mod;
    }

 error:
    erts_free_aligned_binary_bytes(temp_alloc);
    if (code != NULL) {
	erts_free(ERTS_ALC_T_CODE, code);
    }
    if (state.lambdas != state.def_lambdas) {
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) state.lambdas);
    }
    if (bin != NULL) {
	driver_free_binary(bin);
    }

	
    BIF_ERROR(p, BADARG);
}

#undef WORDS_PER_FUNCTION

static int safe_mul(UWord a, UWord b, UWord* resp)
{
    Uint res = a * b; /* XXX:Pan - used in bit syntax, the multiplication has to be stored in Uint */
    *resp = res;

    if (b == 0) {
	return 1;
    } else {
	return (res / b) == a;
    }
}
