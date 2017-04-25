/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
#include "beam_bp.h"
#include "big.h"
#include "erl_bits.h"
#include "beam_catches.h"
#include "erl_binary.h"
#include "erl_zlib.h"
#include "erl_map.h"
#include "erl_process_dict.h"

#ifdef HIPE
#include "hipe_bif0.h"
#include "hipe_mode_switch.h"
#include "hipe_arch.h"
#include "hipe_load.h"
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
    Sint patches;		/* Index (into code buffer) to first location
				 * which must be patched with the value of this label.
				 */
#ifdef ERTS_SMP
    Uint looprec_targeted;	/* Non-zero if this label is the target of a loop_rec
				 * instruction.
				 */
#endif
} Label;

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
#define MIN_MANDATORY 1
#define MAX_MANDATORY 5

#define LAMBDA_CHUNK 5
#define LITERAL_CHUNK 6
#define ATTR_CHUNK 7
#define COMPILE_CHUNK 8
#define LINE_CHUNK 9
#define UTF8_ATOM_CHUNK 10

#define NUM_CHUNK_TYPES (sizeof(chunk_types)/sizeof(chunk_types[0]))

/*
 * An array with all chunk types recognized by the loader.
 */

static Uint chunk_types[] = {
    /*
     * Atom chunk types -- Atom or AtU8 MUST be present.
     */
    MakeIffId('A', 't', 'o', 'm'), /* 0 */

    /*
     * Mandatory chunk types -- these MUST be present.
     */
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
    MakeIffId('L', 'i', 'n', 'e'), /* 9 */
    MakeIffId('A', 't', 'U', '8'), /* 10 */
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
    ErlHeapFragment* heap_frags;
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
 * This structure associates a code offset with a source code location.
 */

typedef struct {
    int pos;			/* Position in code */
    Uint32 loc;			/* Location in source code */
} LineInstr;

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
    } chunks[NUM_CHUNK_TYPES];

    /*
     * Used for code loading (mainly).
     */

    byte* code_start;		/* Start of code file. */
    unsigned code_size;		/* Size of code file. */
    int specific_op;		/* Specific opcode (-1 if not found). */
    unsigned int num_functions; /* Number of functions in module. */
    unsigned int num_labels;	/* Number of labels. */
    BeamCodeHeader* hdr;	/* Loaded code header */
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

#define GetTagAndValue(Stp, Tag, Val)					\
   do {									\
      BeamInstr __w;							\
      GetByte(Stp, __w);						\
      Tag = __w & 0x07;							\
      if ((__w & 0x08) == 0) {						\
	 Val = __w >> 4;						\
      } else if ((__w & 0x10) == 0) {					\
	 Val = ((__w >> 5) << 8);					\
	 GetByte(Stp, __w);						\
	 Val |= __w;							\
      } else {								\
	 int __res = get_tag_and_value(Stp, __w, (Tag), &(Val));	\
         if (__res < 0) goto load_error;				\
         Tag = (unsigned) __res;					\
      }									\
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
    }

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


static void free_loader_state(Binary* magic);
static ErlHeapFragment* new_literal_fragment(Uint size);
static void free_literal_fragment(ErlHeapFragment*);
static int loader_state_dtor(Binary* magic);
#ifdef HIPE
static Eterm stub_insert_new_code(Process *c_p, ErtsProcLocks c_p_locks,
				  Eterm group_leader, Eterm module,
				  BeamCodeHeader* code_hdr, Uint size,
				  HipeModule *hipe_code);
#endif
static int init_iff_file(LoaderState* stp, byte* code, Uint size);
static int scan_iff_file(LoaderState* stp, Uint* chunk_types,
			 Uint num_types);
static int verify_chunks(LoaderState* stp);
static int load_atom_table(LoaderState* stp, ErtsAtomEncoding enc);
static int load_import_table(LoaderState* stp);
static int read_export_table(LoaderState* stp);
static int is_bif(Eterm mod, Eterm func, unsigned arity);
static int read_lambda_table(LoaderState* stp);
static int read_literal_table(LoaderState* stp);
static int read_line_table(LoaderState* stp);
static int read_code_header(LoaderState* stp);
static int load_code(LoaderState* stp);
static GenOp* gen_element(LoaderState* stp, GenOpArg Fail, GenOpArg Index,
			  GenOpArg Tuple, GenOpArg Dst);
static GenOp* gen_split_values(LoaderState* stp, GenOpArg S,
			       GenOpArg TypeFail, GenOpArg Fail,
			       GenOpArg Size, GenOpArg* Rest);
static GenOp* gen_select_val(LoaderState* stp, GenOpArg S, GenOpArg Fail,
			     GenOpArg Size, GenOpArg* Rest);
static GenOp* gen_select_literals(LoaderState* stp, GenOpArg S,
				  GenOpArg Fail, GenOpArg Size,
				  GenOpArg* Rest);
static GenOp* const_select_val(LoaderState* stp, GenOpArg S, GenOpArg Fail,
			       GenOpArg Size, GenOpArg* Rest);

static GenOp* gen_get_map_element(LoaderState* stp, GenOpArg Fail, GenOpArg Src,
                                  GenOpArg Size, GenOpArg* Rest);

static int freeze_code(LoaderState* stp);

static void final_touch(LoaderState* stp, struct erl_module_instance* inst_p);
static void short_file(int line, LoaderState* stp, unsigned needed);
static void load_printf(int line, LoaderState* context, char *fmt, ...);
static int transform_engine(LoaderState* st);
static void id_to_string(Uint id, char* s);
static void new_genop(LoaderState* stp);
static int get_tag_and_value(LoaderState* stp, Uint len_code,
			     unsigned tag, BeamInstr* result);
static int new_label(LoaderState* stp);
static void new_literal_patch(LoaderState* stp, int pos);
static void new_string_patch(LoaderState* stp, int pos);
static Uint new_literal(LoaderState* stp, Eterm** hpp, Uint heap_size);
static int genopargcompare(GenOpArg* a, GenOpArg* b);
static Eterm get_module_info(Process* p, ErtsCodeIndex code_ix,
                             BeamCodeHeader*, Eterm module, Eterm what);
static Eterm exported_from_module(Process* p, ErtsCodeIndex code_ix,
                                  Eterm mod);
static Eterm functions_in_module(Process* p, BeamCodeHeader*);
static Eterm attributes_for_module(Process* p, BeamCodeHeader*);
static Eterm compilation_info_for_module(Process* p, BeamCodeHeader*);
static Eterm md5_of_module(Process* p, BeamCodeHeader*);
static Eterm has_native(BeamCodeHeader*);
static Eterm native_addresses(Process* p, BeamCodeHeader*);
static int safe_mul(UWord a, UWord b, UWord* resp);

static int must_swap_floats;

Uint erts_total_code_size;
/**********************************************************************/

void init_load(void)
{
    FloatDef f;

    erts_total_code_size = 0;

    beam_catches_init();

    f.fd = 1.0;
    must_swap_floats = (f.fw[0] == 0);

    erts_init_ranges();
}

static void
define_file(LoaderState* stp, char* name, int idx)
{
    stp->file_name = name;
    stp->file_p = stp->chunks[idx].start;
    stp->file_left = stp->chunks[idx].size;
}

Eterm
erts_preload_module(Process *c_p,
		 ErtsProcLocks c_p_locks,
		 Eterm group_leader, /* Group leader or NIL if none. */
		 Eterm* modp,	/*
				 * Module name as an atom (NIL to not check).
				 * On return, contains the actual module name.
				 */
		 byte* code,	/* Points to the code to load */
		 Uint size)	/* Size of code to load. */
{
    Binary* magic = erts_alloc_loader_state();
    Eterm retval;

    ASSERT(!erts_initialized);
    retval = erts_prepare_loading(magic, c_p, group_leader, modp,
				  code, size);
    if (retval != NIL) {
	return retval;
    }
    return erts_finish_loading(magic, c_p, c_p_locks, modp);
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


Eterm
erts_prepare_loading(Binary* magic, Process *c_p, Eterm group_leader,
		     Eterm* modp, byte* code, Uint unloaded_size)
{
    Eterm retval = am_badfile;
    LoaderState* stp;

    stp = ERTS_MAGIC_BIN_DATA(magic);
    stp->module = *modp;
    stp->group_leader = group_leader;

#if defined(LOAD_MEMORY_HARD_DEBUG) && defined(DEBUG)
    erts_fprintf(stderr,"Loading a module\n");
#endif

    /*
     * Scan the IFF file.
     */

    CHKALLOC();
    CHKBLK(ERTS_ALC_T_CODE,stp->code);
    if (!init_iff_file(stp, code, unloaded_size) ||
	!scan_iff_file(stp, chunk_types, NUM_CHUNK_TYPES) ||
	!verify_chunks(stp)) {
	goto load_error;
    }

    /*
     * Read the header for the code chunk.
     */

    CHKBLK(ERTS_ALC_T_CODE,stp->code);
    define_file(stp, "code chunk header", CODE_CHUNK);
    if (!read_code_header(stp)) {
	goto load_error;
    }

    /*
     * Initialize code area.
     */
    stp->codev_size = 2048 + stp->num_functions;
    stp->hdr = (BeamCodeHeader*) erts_alloc(ERTS_ALC_T_CODE,
                                            (offsetof(BeamCodeHeader,functions)
                                             + sizeof(BeamInstr) * stp->codev_size));

    stp->hdr->num_functions = stp->num_functions;

    /* Let the codev array start at functions[0] in order to index
     * both function pointers and the loaded code itself that follows.
     */
    stp->codev = (BeamInstr*) &stp->hdr->functions;
    stp->ci = stp->num_functions + 1;

    stp->hdr->attr_ptr = NULL;
    stp->hdr->attr_size = 0;
    stp->hdr->attr_size_on_heap = 0;
    stp->hdr->compile_ptr = NULL;
    stp->hdr->compile_size = 0;
    stp->hdr->compile_size_on_heap = 0;
    stp->hdr->literal_area = NULL;
    stp->hdr->md5_ptr = NULL;

    /*
     * Read the atom table.
     */

    CHKBLK(ERTS_ALC_T_CODE,stp->code);
    if (stp->chunks[UTF8_ATOM_CHUNK].size > 0) {
        define_file(stp, "utf8 atom table", UTF8_ATOM_CHUNK);
        if (!load_atom_table(stp, ERTS_ATOM_ENC_UTF8)) {
            goto load_error;
        }
    } else {
        define_file(stp, "atom table", ATOM_CHUNK);
        if (!load_atom_table(stp, ERTS_ATOM_ENC_LATIN1)) {
            goto load_error;
        }
    }

    /*
     * Read the import table.
     */

    CHKBLK(ERTS_ALC_T_CODE,stp->code);
    define_file(stp, "import table", IMP_CHUNK);
    if (!load_import_table(stp)) {
	goto load_error;
    }

    /*
     * Read the lambda (fun) table.
     */

    CHKBLK(ERTS_ALC_T_CODE,stp->code);
    if (stp->chunks[LAMBDA_CHUNK].size > 0) {
	define_file(stp, "lambda (fun) table", LAMBDA_CHUNK);
	if (!read_lambda_table(stp)) {
	    goto load_error;
	}
    }

    /*
     * Read the literal table.
     */

    CHKBLK(ERTS_ALC_T_CODE,stp->code);
    if (stp->chunks[LITERAL_CHUNK].size > 0) {
	define_file(stp, "literals table (constant pool)", LITERAL_CHUNK);
	if (!read_literal_table(stp)) {
	    goto load_error;
	}
    }

    /*
     * Read the line table (if present).
     */

    CHKBLK(ERTS_ALC_T_CODE,stp->code);
    if (stp->chunks[LINE_CHUNK].size > 0) {
	define_file(stp, "line table", LINE_CHUNK);
	if (!read_line_table(stp)) {
	    goto load_error;
	}
    }

    /*
     * Load the code chunk.
     */

    CHKBLK(ERTS_ALC_T_CODE,stp->code);
    stp->file_name = "code chunk";
    stp->file_p = stp->code_start;
    stp->file_left = stp->code_size;
    if (!load_code(stp)) {
	goto load_error;
    }
    CHKBLK(ERTS_ALC_T_CODE,stp->code);
    if (!freeze_code(stp)) {
	goto load_error;
    }


    /*
     * Read and validate the export table.  (This must be done after
     * loading the code, because it contains labels.)
     */
    
    CHKBLK(ERTS_ALC_T_CODE,stp->code);
    define_file(stp, "export table", EXP_CHUNK);
    if (!read_export_table(stp)) {
	goto load_error;
    }

    /*
     * Good so far.
     */

    retval = NIL;

 load_error:
    if (retval != NIL) {
	free_loader_state(magic);
    }
    return retval;
}

Eterm
erts_finish_loading(Binary* magic, Process* c_p,
		    ErtsProcLocks c_p_locks, Eterm* modp)
{
    Eterm retval = NIL;
    LoaderState* stp = ERTS_MAGIC_BIN_DATA(magic);
    Module* mod_tab_p;
    struct erl_module_instance* inst_p;
    Uint size;

    /*
     * No other process may run since we will update the export
     * table which is not protected by any locks.
     */

    ERTS_SMP_LC_ASSERT(erts_initialized == 0 || erts_has_code_write_permission() ||
		       erts_smp_thr_progress_is_blocking());
    /*
     * Make current code for the module old and insert the new code
     * as current.  This will fail if there already exists old code
     * for the module.
     */

    mod_tab_p = erts_put_module(stp->module);
    CHKBLK(ERTS_ALC_T_CODE,stp->code);
    if (!stp->on_load) {
	/*
	 * Normal case -- no -on_load() function.
	 */
	retval = beam_make_current_old(c_p, c_p_locks, stp->module);
	ASSERT(retval == NIL);
    } else {
	ErtsCodeIndex code_ix = erts_staging_code_ix();
	Eterm module = stp->module;
	int i, num_exps;

	/*
	 * There is an -on_load() function. We will keep the current
	 * code, but we must turn off any tracing.
	 */
        num_exps = export_list_size(code_ix);
	for (i = 0; i < num_exps; i++) {
	    Export *ep = export_list(i, code_ix);
	    if (ep == NULL || ep->info.mfa.module != module) {
		continue;
	    }
	    if (ep->addressv[code_ix] == ep->beam) {
		if (ep->beam[0] == (BeamInstr) em_apply_bif) {
		    continue;
		} else if (ep->beam[0] ==
			   (BeamInstr) BeamOp(op_i_generic_breakpoint)) {
		    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
		    ASSERT(mod_tab_p->curr.num_traced_exports > 0);
		    erts_clear_export_break(mod_tab_p, &ep->info);
		    ep->addressv[code_ix] = (BeamInstr *) ep->beam[1];
		    ep->beam[1] = 0;
		}
		ASSERT(ep->beam[1] == 0);
	    }
	}
	ASSERT(mod_tab_p->curr.num_breakpoints == 0);
	ASSERT(mod_tab_p->curr.num_traced_exports == 0);
    }

    /*
     * Update module table.
     */

    size = stp->loaded_size;
    erts_total_code_size += size;

    if (!stp->on_load) {
	inst_p = &mod_tab_p->curr;
    } else {
	mod_tab_p->on_load =
	    (struct erl_module_instance *)
	    erts_alloc(ERTS_ALC_T_PREPARED_CODE,
		       sizeof(struct erl_module_instance));
	inst_p = mod_tab_p->on_load;
        erts_module_instance_init(inst_p);
    }

    inst_p->code_hdr = stp->hdr;
    inst_p->code_length = size;

    /*
     * Update ranges (used for finding a function from a PC value).
     */

    erts_update_ranges((BeamInstr*)inst_p->code_hdr, size);

    /*
     * Ready for the final touch: fixing the export table entries for
     * exported and imported functions.  This can't fail.
     */
    
    CHKBLK(ERTS_ALC_T_CODE,stp->code);
    final_touch(stp, inst_p);

    /*
     * Loading succeded.
     */
    CHKBLK(ERTS_ALC_T_CODE,stp->code);
#if defined(LOAD_MEMORY_HARD_DEBUG) && defined(DEBUG)
    erts_fprintf(stderr,"Loaded %T\n",*modp);
#if 0
    debug_dump_code(stp->code,stp->ci);
#endif
#endif
    stp->hdr = NULL;		/* Prevent code from being freed. */
    stp->codev = NULL;
    *modp = stp->module;

    /*
     * If there is an on_load function, signal an error to
     * indicate that the on_load function must be run.
     */
    if (stp->on_load) {
	retval = am_on_load;
    }

    free_loader_state(magic);
    return retval;
}

Binary*
erts_alloc_loader_state(void)
{
    LoaderState* stp;
    Binary* magic;

    magic = erts_create_magic_binary(sizeof(LoaderState),
				     loader_state_dtor);
    erts_refc_inc(&magic->intern.refc, 1);
    stp = ERTS_MAGIC_BIN_DATA(magic);
    stp->bin = NULL;
    stp->function = THE_NON_VALUE; /* Function not known yet */
    stp->arity = 0;
    stp->specific_op = -1;
    stp->genop = NULL;
    stp->atom = NULL;
    stp->hdr = NULL;
    stp->codev = NULL;
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
    stp->may_load_nif = 0;
    stp->on_load = 0;
    stp->line_item = 0;
    stp->line_instr = 0;
    stp->func_line = 0;
    stp->fname = 0;
    return magic;
}

/*
 * Return the module name (a tagged atom) for the prepared code
 * in the magic binary, or NIL if the binary does not contain
 * prepared code.
 */
Eterm
erts_module_for_prepared_code(Binary* magic)
{
    LoaderState* stp;

    if (ERTS_MAGIC_BIN_DESTRUCTOR(magic) != loader_state_dtor) {
#ifdef HIPE
	HipeLoaderState *hipe_stp;
	if ((hipe_stp = hipe_get_loader_state(magic))
	    && hipe_stp->text_segment != 0) {
	    return hipe_stp->module;
	}
#endif
	return NIL;
    }
    stp = ERTS_MAGIC_BIN_DATA(magic);
    if (stp->hdr != 0) {
	return stp->module;
    } else {
	return NIL;
    }
}

/*
 * Return a non-zero value if the module has an on_load function,
 * or 0 if it does not.
 */

Eterm
erts_has_code_on_load(Binary* magic)
{
    LoaderState* stp;

    if (ERTS_MAGIC_BIN_DESTRUCTOR(magic) != loader_state_dtor) {
	return NIL;
    }
    stp = ERTS_MAGIC_BIN_DATA(magic);
    return stp->on_load ? am_true : am_false;
}

static void
free_loader_state(Binary* magic)
{
    loader_state_dtor(magic);
    erts_bin_release(magic);
}

static ErlHeapFragment* new_literal_fragment(Uint size)
{
    ErlHeapFragment* bp;
    bp = (ErlHeapFragment*) ERTS_HEAP_ALLOC(ERTS_ALC_T_PREPARED_CODE,
					    ERTS_HEAP_FRAG_SIZE(size));
    ERTS_INIT_HEAP_FRAG(bp, size, size);
    return bp;
}

static void free_literal_fragment(ErlHeapFragment* bp)
{
    ASSERT(bp != NULL);
    do {
	ErlHeapFragment* next_bp = bp->next;

	erts_cleanup_offheap(&bp->off_heap);
	ERTS_HEAP_FREE(ERTS_ALC_T_PREPARED_CODE, (void *) bp,
		       ERTS_HEAP_FRAG_SIZE(bp->size));
	bp = next_bp;
    }while (bp != NULL);
}

/*
 * This destructor function can safely be called multiple times.
 */
static int
loader_state_dtor(Binary* magic)
{
    LoaderState* stp = ERTS_MAGIC_BIN_DATA(magic);

    if (stp->bin != 0) {
	driver_free_binary(stp->bin);
	stp->bin = 0;
    }
    if (stp->hdr != 0) {
        if (stp->hdr->literal_area) {
	    erts_release_literal_area(stp->hdr->literal_area);
	    stp->hdr->literal_area = NULL;
        }
	erts_free(ERTS_ALC_T_CODE, stp->hdr);
	stp->hdr = 0;
        stp->codev = 0;
    }
    if (stp->labels != 0) {
	erts_free(ERTS_ALC_T_PREPARED_CODE, (void *) stp->labels);
	stp->labels = 0;
    }
    if (stp->atom != 0) {
	erts_free(ERTS_ALC_T_PREPARED_CODE, (void *) stp->atom);
	stp->atom = 0;
    }
    if (stp->import != 0) {
	erts_free(ERTS_ALC_T_PREPARED_CODE, (void *) stp->import);
	stp->import = 0;
    }
    if (stp->export != 0) {
	erts_free(ERTS_ALC_T_PREPARED_CODE, (void *) stp->export);
	stp->export = 0;
    }
    if (stp->lambdas != stp->def_lambdas) {
	erts_free(ERTS_ALC_T_PREPARED_CODE, (void *) stp->lambdas);
	stp->lambdas = stp->def_lambdas;
    }
    if (stp->literals != 0) {
	int i;
	for (i = 0; i < stp->num_literals; i++) {
	    if (stp->literals[i].heap_frags != 0) {
                free_literal_fragment(stp->literals[i].heap_frags);
		stp->literals[i].heap_frags = 0;
	    }
	}
	erts_free(ERTS_ALC_T_PREPARED_CODE, (void *) stp->literals);
	stp->literals = 0;
    }
    while (stp->literal_patches != 0) {
	LiteralPatch* next = stp->literal_patches->next;
	erts_free(ERTS_ALC_T_PREPARED_CODE, (void *) stp->literal_patches);
	stp->literal_patches = next;
    }
    while (stp->string_patches != 0) {
	StringPatch* next = stp->string_patches->next;
	erts_free(ERTS_ALC_T_PREPARED_CODE, (void *) stp->string_patches);
	stp->string_patches = next;
    }

    if (stp->line_item != 0) {
	erts_free(ERTS_ALC_T_PREPARED_CODE, stp->line_item);
	stp->line_item = 0;
    }

    if (stp->line_instr != 0) {
	erts_free(ERTS_ALC_T_PREPARED_CODE, stp->line_instr);
	stp->line_instr = 0;
    }

    if (stp->func_line != 0) {
	erts_free(ERTS_ALC_T_PREPARED_CODE, stp->func_line);
	stp->func_line = 0;
    }

    if (stp->fname != 0) {
	erts_free(ERTS_ALC_T_PREPARED_CODE, stp->fname);
	stp->fname = 0;
    }

    /*
     * The following data items should have been freed earlier.
     */

    ASSERT(stp->genop_blocks == 0);
    return 1;
}

#ifdef HIPE
static Eterm
stub_insert_new_code(Process *c_p, ErtsProcLocks c_p_locks,
		     Eterm group_leader, Eterm module,
		     BeamCodeHeader* code_hdr, Uint size,
		     HipeModule *hipe_code)
{
    Module* modp;
    Eterm retval;

    if ((retval = beam_make_current_old(c_p, c_p_locks, module)) != NIL) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp,
		      "Module %T must be purged before loading\n",
		      module);
	erts_send_error_to_logger(group_leader, dsbufp);
	return retval;
    }

    /*
     * Update module table.
     */

    erts_total_code_size += size;
    modp = erts_put_module(module);
    modp->curr.code_hdr = code_hdr;
    modp->curr.code_length = size;
    modp->curr.catches = BEAM_CATCHES_NIL; /* Will be filled in later. */
    DBG_TRACE_MFA(make_atom(modp->module), 0, 0, "insert_new_code "
                  "first_hipe_ref = %p", hipe_code->first_hipe_ref);
    modp->curr.hipe_code = hipe_code;

    /*
     * Update ranges (used for finding a function from a PC value).
     */

    erts_update_ranges((BeamInstr*)modp->curr.code_hdr, size);
    return NIL;
}
#endif

static int
init_iff_file(LoaderState* stp, byte* code, Uint size)
{
    Uint form_id = MakeIffId('F', 'O', 'R', '1');
    Uint id;
    Uint count;

    if (size < 4) {
	goto load_error;
    }

    /*
     * Check if the module is compressed (or possibly invalid/corrupted).
     */
    if (MakeIffId(code[0], code[1], code[2], code[3]) != form_id) {
	stp->bin = (ErlDrvBinary *) erts_gzinflate_buffer((char*)code, size);
	if (stp->bin == NULL) {
	    goto load_error;
	}
	code = (byte*)stp->bin->orig_bytes;
	size = stp->bin->orig_size;
	if (size < 4) {
	    goto load_error;
	}
    }

    /*
     * The binary must start with an IFF 'FOR1' chunk.
     */
    if (MakeIffId(code[0], code[1], code[2], code[3]) != form_id) {
	LoadError0(stp, "not a BEAM file: no IFF 'FOR1' chunk");
    }

    /*
     * Initialize our "virtual file system".
     */

    stp->file_name = "IFF header for Beam file";
    stp->file_p = code + 4;
    stp->file_left = size - 4;

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
    return 1;

 load_error:
    return 0;
}

/*
 * Scan the IFF file. The header should have been verified by init_iff_file().
 */
static int
scan_iff_file(LoaderState* stp, Uint* chunk_types, Uint num_types)
{
    Uint count;
    Uint id;
    int i;

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
    return 1;

 load_error:
    return 0;
}

/*
 * Verify that all mandatory chunks are present and calculate
 * MD5 for the module.
 */

static int
verify_chunks(LoaderState* stp)
{
    int i;
    MD5_CTX context;

    MD5Init(&context);

    if (stp->chunks[UTF8_ATOM_CHUNK].start != NULL) {
	MD5Update(&context, stp->chunks[UTF8_ATOM_CHUNK].start, stp->chunks[UTF8_ATOM_CHUNK].size);
    } else if (stp->chunks[ATOM_CHUNK].start != NULL) {
	MD5Update(&context, stp->chunks[ATOM_CHUNK].start, stp->chunks[ATOM_CHUNK].size);
    } else {
        LoadError0(stp, "mandatory chunk of type 'Atom' or 'AtU8' not found\n");
    }

    for (i = MIN_MANDATORY; i < MAX_MANDATORY; i++) {
	if (stp->chunks[i].start != NULL) {
	    MD5Update(&context, stp->chunks[i].start, stp->chunks[i].size);
	} else {
	    char sbuf[5];

	    id_to_string(chunk_types[i], sbuf);
	    LoadError1(stp, "mandatory chunk of type '%s' not found\n", sbuf);
	}
    }

    /*
     * If there is a lambda chunk, include parts of it in the MD5.
     */
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


    /*
     * If there is a literal chunk, include it in the MD5.
     */
    if (stp->chunks[LITERAL_CHUNK].start != 0) {
	MD5Update(&context, stp->chunks[LITERAL_CHUNK].start,
		  stp->chunks[LITERAL_CHUNK].size);
    }

    MD5Final(stp->mod_md5, &context);
    return 1;

 load_error:
    return 0;
}

static int
load_atom_table(LoaderState* stp, ErtsAtomEncoding enc)
{
    unsigned int i;

    GetInt(stp, 4, stp->num_atoms);
    stp->num_atoms++;
    stp->atom = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
			   stp->num_atoms*sizeof(Eterm));

    /*
     * Read all atoms.
     */

    for (i = 1; i < stp->num_atoms; i++) {
	byte* atom;
	Uint n;

	GetByte(stp, n);
	GetString(stp, atom, n);
	stp->atom[i] = erts_atom_put(atom, n, enc, 1);
    }

    /*
     * Check the module name if a module name was given.
     */

    if (is_nil(stp->module)) {
	stp->module = stp->atom[1];
    } else if (stp->atom[1] != stp->module) {
	char sbuf[MAX_ATOM_SZ_FROM_LATIN1];
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
    unsigned int i;

    GetInt(stp, 4, stp->num_imports);
    stp->import = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
			     stp->num_imports * sizeof(ImportEntry));
    for (i = 0; i < stp->num_imports; i++) {
	unsigned int n;
	Eterm mod;
	Eterm func;
	Uint arity;
	Export* e;

	GetInt(stp, 4, n);
	if (n >= stp->num_atoms) {
	    LoadError2(stp, "import entry %u: invalid atom number %u", i, n);
	}
	mod = stp->import[i].module = stp->atom[n];
	GetInt(stp, 4, n);
	if (n >= stp->num_atoms) {
	    LoadError2(stp, "import entry %u: invalid atom number %u", i, n);
	}
	func = stp->import[i].function = stp->atom[n];
	GetInt(stp, 4, arity);
	if (arity > MAX_REG) {
	    LoadError2(stp, "import entry %u: invalid arity %d", i, arity);
	}
	stp->import[i].arity = arity;
	stp->import[i].patches = 0;
	stp->import[i].bf = NULL;

	/*
	 * If the export entry refers to a BIF, get the pointer to
	 * the BIF function.
	 */
	if ((e = erts_active_export_entry(mod, func, arity)) != NULL) {
	    if (e->beam[0] == (BeamInstr) em_apply_bif) {
		stp->import[i].bf = (BifFunction) e->beam[1];
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
    unsigned int i;
    BeamInstr* address;

    GetInt(stp, 4, stp->num_exps);
    if (stp->num_exps > stp->num_functions) {
	LoadError2(stp, "%u functions exported; only %u functions defined",
		   stp->num_exps, stp->num_functions);
    }
    stp->export
	= (ExportEntry *) erts_alloc(ERTS_ALC_T_PREPARED_CODE,
				     (stp->num_exps * sizeof(ExportEntry)));

    for (i = 0; i < stp->num_exps; i++) {
	Uint n;
	Uint value;
	Eterm func;
	Uint arity;

	GetInt(stp, 4, n);
	GetAtom(stp, n, func);
	stp->export[i].function = func;
	GetInt(stp, 4, arity);
	if (arity > MAX_REG) {
	    LoadError2(stp, "export table entry %u: absurdly high arity %u", i, arity);
	}
	stp->export[i].arity = arity;
	GetInt(stp, 4, n);
	if (n >= stp->num_labels) {
	    LoadError3(stp, "export table entry %u: invalid label %u (highest defined label is %u)", i, n, stp->num_labels);
	}
	value = stp->labels[n].value;
	if (value == 0) {
	    LoadError2(stp, "export table entry %u: label %u not resolved", i, n);
	}
	stp->export[i].address = address = stp->codev + value;

	/*
	 * Find out if there is a BIF with the same name.
	 */

	if (!is_bif(stp->module, func, arity)) {
	    continue;
	}

	/*
	 * This is a stub for a BIF.
	 *
	 * It should not be exported, and the information in its
	 * func_info instruction should be invalidated so that it
	 * can be filtered out by module_info(functions) and by
	 * any other functions that walk through all local functions.
	 */

	if (stp->labels[n].patches >= 0) {
	    LoadError3(stp, "there are local calls to the stub for "
		       "the BIF %T:%T/%d",
		       stp->module, func, arity);
	}
	stp->export[i].address = NULL;
	address[-1] = 0;
	address[-2] = NIL;
	address[-3] = NIL;
    }
    return 1;

 load_error:
    return 0;
}


static int
is_bif(Eterm mod, Eterm func, unsigned arity)
{
    Export* e = erts_active_export_entry(mod, func, arity);
    if (e == NULL) {
	return 0;
    }
    if (e->beam[0] != (BeamInstr) em_apply_bif) {
	return 0;
    }
    if (mod == am_erlang && func == am_apply && arity == 3) {
	/*
	 * erlang:apply/3 is a special case -- it is implemented
	 * as an instruction and it is OK to redefine it.
	 */
	return 0;
    }
    return 1;
}

static int
read_lambda_table(LoaderState* stp)
{
    unsigned int i;

    GetInt(stp, 4, stp->num_lambdas);
    if (stp->num_lambdas > stp->lambdas_allocated) {
	ASSERT(stp->lambdas == stp->def_lambdas);
	stp->lambdas_allocated = stp->num_lambdas;
	stp->lambdas = (Lambda *) erts_alloc(ERTS_ALC_T_PREPARED_CODE,
					     stp->num_lambdas * sizeof(Lambda));
    }
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
	    LoadError2(stp, "lambda entry %u: absurdly high arity %u", i, arity);
	}
	stp->lambdas[i].arity = arity;
	GetInt(stp, 4, n);
	if (n >= stp->num_labels) {
	    LoadError3(stp, "lambda entry %u: invalid label %u (highest defined label is %u)",
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
    unsigned int i;
    uLongf uncompressed_sz;
    byte* uncompressed = 0;

    GetInt(stp, 4, uncompressed_sz);
    uncompressed = erts_alloc(ERTS_ALC_T_TMP, uncompressed_sz);
    if (erl_zlib_uncompress(uncompressed, &uncompressed_sz,
		   stp->file_p, stp->file_left) != Z_OK) {
	LoadError0(stp, "failed to uncompress literal table (constant pool)");
    }
    stp->file_p = uncompressed;
    stp->file_left = (unsigned) uncompressed_sz;
    GetInt(stp, 4, stp->num_literals);
    stp->literals = (Literal *) erts_alloc(ERTS_ALC_T_PREPARED_CODE,
					   stp->num_literals * sizeof(Literal));
    stp->allocated_literals = stp->num_literals;

    for (i = 0; i < stp->num_literals; i++) {
	stp->literals[i].heap_frags = 0;
    }

    for (i = 0; i < stp->num_literals; i++) {
	Uint sz;
	Sint heap_size;
	byte* p;
	Eterm val;
        ErtsHeapFactory factory;

	GetInt(stp, 4, sz);	/* Size of external term format. */
	GetString(stp, p, sz);
	if ((heap_size = erts_decode_ext_size(p, sz)) < 0) {
	    LoadError1(stp, "literal %u: bad external format", i);
	}

        if (heap_size > 0) {
            erts_factory_heap_frag_init(&factory,
					new_literal_fragment(heap_size));
	    factory.alloc_type = ERTS_ALC_T_PREPARED_CODE;
            val = erts_decode_ext(&factory, &p, 0);

            if (is_non_value(val)) {
                LoadError1(stp, "literal %u: bad external format", i);
            }
            erts_factory_close(&factory);
            stp->literals[i].heap_frags = factory.heap_frags;
            stp->total_literal_size += erts_used_frag_sz(factory.heap_frags);
        }
        else {
            erts_factory_dummy_init(&factory);
            val = erts_decode_ext(&factory, &p, 0);
            if (is_non_value(val)) {
                LoadError1(stp, "literal %u: bad external format", i);
            }
            ASSERT(is_immed(val));
            stp->literals[i].heap_frags = NULL;
        }
        stp->literals[i].term = val;

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
read_line_table(LoaderState* stp)
{
    unsigned version;
    ERTS_DECLARE_DUMMY(unsigned flags);
    unsigned int num_line_items;
    BeamInstr* lp;
    unsigned int i;
    BeamInstr fname_index;
    BeamInstr tag;

    /*
     * If the emulator flag ignoring the line information was given,
     * return immediately.
     */

    if (erts_no_line_info) {
	return 1;
    }

    /*
     * Check version of line table.
     */

    GetInt(stp, 4, version);
    if (version != 0) {
	/*
	 * Wrong version. Silently ignore the line number chunk.
	 */
	return 1;
    }

    /*
     * Read the remaining header words. The flag word is reserved
     * for possible future use; for the moment we ignore it.
     */
    GetInt(stp, 4, flags);
    GetInt(stp, 4, stp->num_line_instrs);
    GetInt(stp, 4, num_line_items);
    GetInt(stp, 4, stp->num_fnames);

    /*
     * Calculate space and allocate memory for the line item table.
     */

    num_line_items++;
    lp = (BeamInstr *) erts_alloc(ERTS_ALC_T_PREPARED_CODE,
				  num_line_items * sizeof(BeamInstr));
    stp->line_item = lp;
    stp->num_line_items = num_line_items;

    /*
     * The zeroth entry in the line item table is special.
     * It contains the undefined location.
     */

    *lp++ = LINE_INVALID_LOCATION;
    num_line_items--;

    /*
     * Read all the line items.
     */

    stp->loc_size = stp->num_fnames ? 4 : 2;
    fname_index = 0;
    while (num_line_items-- > 0) {
	BeamInstr val;
	BeamInstr loc;

	GetTagAndValue(stp, tag, val);
	if (tag == TAG_i) {
	    if (IS_VALID_LOCATION(fname_index, val)) {
		loc = MAKE_LOCATION(fname_index, val);
	    } else {
		/*
		 * Too many files or huge line number. Silently invalidate
		 * the location.
		 */
		loc = LINE_INVALID_LOCATION;
	    }
	    *lp++ = loc;
	    if (val > 0xFFFF) {
		stp->loc_size = 4;
	    }
	} else if (tag == TAG_a) {
	    if (val > stp->num_fnames) {
		LoadError2(stp, "file index overflow (%u/%u)",
			   val, stp->num_fnames);
	    }
	    fname_index = val;
	    num_line_items++;
	} else {
	    LoadError1(stp, "bad tag '%c' (expected 'a' or 'i')",
		       tag_to_letter[tag]);
	}
    }

    /*
     * Read all filenames.
     */

    if (stp->num_fnames != 0) {
	stp->fname = (Eterm *) erts_alloc(ERTS_ALC_T_PREPARED_CODE,
					      stp->num_fnames *
					      sizeof(Eterm));
	for (i = 0; i < stp->num_fnames; i++) {
	    byte* fname;
	    Uint n;

	    GetInt(stp, 2, n);
	    GetString(stp, fname, n);
	    stp->fname[i] = erts_atom_put(fname, n, ERTS_ATOM_ENC_LATIN1, 1);
	}
    }

    /*
     * Allocate the arrays to be filled while code is being loaded.
     */
    stp->line_instr = (LineInstr *) erts_alloc(ERTS_ALC_T_PREPARED_CODE,
					       stp->num_line_instrs *
					       sizeof(LineInstr));
    stp->current_li = 0;
    stp->func_line = (unsigned int *)  erts_alloc(ERTS_ALC_T_PREPARED_CODE,
						  stp->num_functions *
						  sizeof(int));

    return 1;

 load_error:
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
    if (head_size > stp->file_left) {
	LoadError2(stp, "invalid code header size %u; bytes left %u",
		   head_size, stp->file_left);
    }
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
	LoadError2(stp,
		   "This BEAM file was compiled for a later version"
		   " of the run-time system than " ERLANG_OTP_RELEASE ".\n"
		   "  To fix this, please recompile this module with an "
		   ERLANG_OTP_RELEASE " compiler.\n"
		   "  (Use of opcode %d; this emulator supports "
		   "only up to %d.)",
		   opcode_max, MAX_GENERIC_OPCODE);
    }

    GetInt(stp, 4, stp->num_labels);
    GetInt(stp, 4, stp->num_functions);

    /*
     * Initialize label table.
     */

    stp->labels = (Label *) erts_alloc(ERTS_ALC_T_PREPARED_CODE,
				       stp->num_labels * sizeof(Label));
    for (i = 0; i < stp->num_labels; i++) {
	stp->labels[i].value = 0;
	stp->labels[i].patches = -1;
#ifdef ERTS_SMP
	stp->labels[i].looprec_targeted = 0;
#endif
    }

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
    ASSERT(ci <= codev_size);					        \
    if (codev_size < ci+(w)) {					        \
        codev_size = 2*ci+(w);	                                        \
	stp->hdr = (BeamCodeHeader*) erts_realloc(ERTS_ALC_T_CODE,	\
            (void *) stp->hdr,	                                        \
            (offsetof(BeamCodeHeader,functions)                         \
             + codev_size * sizeof(BeamInstr)));                        \
        code = stp->codev = (BeamInstr*) &stp->hdr->functions;          \
    } 									\
} while (0)
    
#define TermWords(t) (((t) / (sizeof(BeamInstr)/sizeof(Eterm))) + !!((t) % (sizeof(BeamInstr)/sizeof(Eterm))))

static int
load_code(LoaderState* stp)
{
    int i;
    int ci;
    int last_func_start = 0;	/* Needed by nif loading and line instructions */
    char* sign;
    int arg;			/* Number of current argument. */
    int num_specific;		/* Number of specific ops for current. */
    BeamInstr* code;
    int codev_size;
    int specific;
    Uint last_label = 0;	/* Number of last label. */
    Uint function_number = 0;
    GenOp* last_op = NULL;
    GenOp** last_op_next = NULL;
    int arity;
    int retval = 1;

    /*
     * The size of the loaded func_info instruction is needed
     * by both the nif functionality and line instructions.
     */
    enum {
	FUNC_INFO_SZ = sizeof(ErtsCodeInfo) / sizeof(Eterm)
    };

    code = stp->codev;
    codev_size = stp->codev_size;
    ci = stp->ci;

    for (;;) {
	unsigned int new_op;
	GenOp* tmp_op;

	ASSERT(ci <= codev_size);

    get_next_instr:
	GetByte(stp, new_op);
	if (new_op >= NUM_GENERIC_OPS) {
	    LoadError1(stp, "invalid opcode %u", new_op);
	}
	if (gen_opc[new_op].name[0] == '\0') {
	    LoadError1(stp, "invalid opcode %u", new_op);
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

	for (arg = 0; arg < arity; arg++) {
	    GetTagAndValue(stp, last_op->a[arg].type, last_op->a[arg].val);
	    switch (last_op->a[arg].type) {
	    case TAG_i:
	    case TAG_u:
	    case TAG_q:
	    case TAG_o:
		break;
	    case TAG_x:
		if (last_op->a[arg].val >= MAX_REG) {
		    LoadError1(stp, "invalid x register number: %u",
			       last_op->a[arg].val);
		}
		break;
	    case TAG_y:
		if (last_op->a[arg].val >= MAX_REG) {
		    LoadError1(stp, "invalid y register number: %u",
			       last_op->a[arg].val);
		}
		last_op->a[arg].val += CP_SIZE;
		break;
	    case TAG_a:
		if (last_op->a[arg].val == 0) {
		    last_op->a[arg].type = TAG_n;
		} else if (last_op->a[arg].val >= stp->num_atoms) {
		    LoadError1(stp, "bad atom index: %d", last_op->a[arg].val);
		} else {
		    last_op->a[arg].val = stp->atom[last_op->a[arg].val];
		}
		break;
	    case TAG_f:
		if (last_op->a[arg].val == 0) {
		    last_op->a[arg].type = TAG_p;
		} else if (last_op->a[arg].val >= stp->num_labels) {
		    LoadError1(stp, "bad label: %d", last_op->a[arg].val);
		}
		break;
	    case TAG_h:
		if (last_op->a[arg].val > 65535) {
		    LoadError1(stp, "invalid range for character data type: %u",
			       last_op->a[arg].val);
		}
		break;
	    case TAG_z:
		{
		    unsigned tag;

		    switch (last_op->a[arg].val) {
		    case 0:
			/* Floating point number.
			 * Not generated by the compiler in R16B and later.
			 */
			{
			    Eterm* hp;
#if !defined(ARCH_64)
			    Uint high, low;
# endif
			    last_op->a[arg].val = new_literal(stp, &hp,
							      FLOAT_SIZE_OBJECT);
			    hp[0] = HEADER_FLONUM;
			    last_op->a[arg].type = TAG_q;
#if defined(ARCH_64)
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
			LoadError1(stp, "invalid extended tag %d",
				   last_op->a[arg].val);
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

	ASSERT(arity == last_op->arity);

    do_transform:
	ASSERT(stp->genop != NULL);
	if (gen_opc[stp->genop->op].transform != -1) {
	    if (stp->genop->next == NULL) {
		/*
		 * Simple heuristic: Most transformations requires
		 * at least two instructions, so make sure that
		 * there are. That will reduce the number of
		 * TE_SHORT_WINDOWs.
		 */
		goto get_next_instr;
	    }
	    switch (transform_engine(stp)) {
	    case TE_FAIL:
		/*
		 * No transformation found. stp->genop != NULL and
		 * last_op_next is still valid. Go ahead and load
		 * the instruction.
		 */
		break;
	    case TE_OK:
		/*
		 * Some transformation was applied. last_op_next is
		 * no longer valid and stp->genop may be NULL.
		 * Try to transform again.
		 */
		if (stp->genop == NULL) {
		    last_op_next = &stp->genop;
		    goto get_next_instr;
		}
		last_op_next = NULL;
		goto do_transform;
	    case TE_SHORT_WINDOW:
		/*
		 * No transformation applied. stp->genop != NULL and
		 * last_op_next is still valid. Fetch a new instruction
		 * before trying the transformation again.
		 */
		goto get_next_instr;
	    }
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

		    if (!opc[specific].involves_r) {
			break;	/* No complications - match */
		    }

		    /*
		     * The specific operation uses the 'r' operand,
		     * which is shorthand for x(0). Now things
		     * get complicated. First we must check whether
		     * all operands that should be of type 'r' use
		     * x(0) (as opposed to some other X register).
		     */
		    for (arg = 0; arg < arity; arg++) {
			if (opc[specific].involves_r & (1 << arg) &&
			    tmp_op->a[arg].type == TAG_x) {
			    if (tmp_op->a[arg].val != 0) {
				break; /* Other X register than 0 */
			    }
			}
		    }

		    if (arg == arity) {
			/*
			 * All 'r' operands use x(0) in the generic
			 * operation. That means a match. Now we
			 * will need to rewrite the generic instruction
			 * to actually use 'r' instead of 'x(0)'.
			 */
			for (arg = 0; arg < arity; arg++) {
			    if (opc[specific].involves_r & (1 << arg) &&
				tmp_op->a[arg].type == TAG_x) {
				tmp_op->a[arg].type = TAG_r;
			    }
			}
			break;	/* Match */
		    }
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

		/*
		 * Some generic instructions should have a special
		 * error message.
		 */
		switch (stp->genop->op) {
		case genop_too_old_compiler_0:
		    LoadError0(stp, "please re-compile this module with an "
			       ERLANG_OTP_RELEASE " compiler");
		case genop_unsupported_guard_bif_3:
		    {
			Eterm Mod = (Eterm) stp->genop->a[0].val;
			Eterm Name = (Eterm) stp->genop->a[1].val;
			Uint arity = (Uint) stp->genop->a[2].val;
			FREE_GENOP(stp, stp->genop);
			stp->genop = 0;
			LoadError3(stp, "unsupported guard BIF: %T:%T/%d\n",
				   Mod, Name, arity);
		    }
		default:
		    LoadError0(stp, "no specific operation found");
		}
	    }

	    stp->specific_op = specific;
	    CodeNeed(opc[stp->specific_op].sz+16); /* Extra margin for packing */
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
		case TAG_x:
		    code[ci++] = make_loader_x_reg(tmp_op->a[arg].val);
		    break;
		case TAG_y:
		    code[ci++] = make_loader_y_reg(tmp_op->a[arg].val);
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
		case TAG_q:
		    new_literal_patch(stp, ci);
		    code[ci++] = tmp_op->a[arg].val;
		    break;
		default:
		    LoadError1(stp, "bad tag %d for general source",
			       tmp_op->a[arg].type);
		    break;
		}
		break;
	    case 'd':	/* Destination (x(0), x(N), y(N) */
		switch (tag) {
		case TAG_x:
		    code[ci++] = tmp_op->a[arg].val * sizeof(Eterm);
		    break;
		case TAG_y:
		    code[ci++] = tmp_op->a[arg].val * sizeof(Eterm) + 1;
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
		    LoadError2(stp, "invalid label num %u (0 < label < %u)",
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
	    case 'P':		/* Byte offset into tuple or stack */
	    case 'Q':		/* Like 'P', but packable */
		VerifyTag(stp, tag, TAG_u);
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
#ifdef ARCH_64
		case 'w':	/* Shift 32 steps */
		    packed = (packed << BEAM_WIDE_SHIFT) | code[--ci];
		    break;
#endif
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
	    case TAG_x:
		CodeNeed(1);
		code[ci++] = make_loader_x_reg(tmp_op->a[arg].val);
		break;
	    case TAG_y:
		CodeNeed(1);
		code[ci++] = make_loader_y_reg(tmp_op->a[arg].val);
		break;
	    case TAG_n:
		CodeNeed(1);
		code[ci++] = NIL;
		break;
	    case TAG_q:
		CodeNeed(1);
		new_literal_patch(stp, ci);
		code[ci++] = tmp_op->a[arg].val;
		break;
	    default:
		LoadError1(stp, "unsupported primitive type '%c'",
			   tag_to_letter[tmp_op->a[arg].type]);
	    }
	}

	/*
	 * Handle a few special cases.
	 */
	switch (stp->specific_op) {
	case op_i_func_info_IaaI:
	    {
		Sint offset;
		if (function_number >= stp->num_functions) {
		    LoadError1(stp, "too many functions in module (header said %u)",
			       stp->num_functions); 
		}

		if (stp->may_load_nif) {
		    const int finfo_ix = ci - FUNC_INFO_SZ;
		    if (finfo_ix - last_func_start < BEAM_NIF_MIN_FUNC_SZ && last_func_start) {
			/* Must make room for call_nif op */
			int pad = BEAM_NIF_MIN_FUNC_SZ - (finfo_ix - last_func_start);
			ASSERT(pad > 0 && pad < BEAM_NIF_MIN_FUNC_SZ);
			CodeNeed(pad);
			sys_memmove(&code[finfo_ix+pad], &code[finfo_ix],
				    FUNC_INFO_SZ*sizeof(BeamInstr));
			sys_memset(&code[finfo_ix], 0, pad*sizeof(BeamInstr));
			ci += pad;
			stp->labels[last_label].value += pad;
		    }
		}
		last_func_start = ci;

		/*
		 * Save current offset of into the line instruction array.
		 */

		if (stp->func_line) {
		    stp->func_line[function_number] = stp->current_li;
		}

		/*
		 * Save context for error messages.
		 */
		stp->function = code[ci-2];
		stp->arity = code[ci-1];

                /* When this assert is triggered, it is normally a sign that
                   the size of the ops.tab i_func_info instruction is not
                   the same as FUNC_INFO_SZ */
		ASSERT(stp->labels[last_label].value == ci - FUNC_INFO_SZ);
		stp->hdr->functions[function_number] = (ErtsCodeInfo*) stp->labels[last_label].patches;
		offset = function_number;
		stp->labels[last_label].patches = offset;
		function_number++;
		if (stp->arity > MAX_ARG) {
		    LoadError1(stp, "too many arguments: %d", stp->arity);
		}
#ifdef DEBUG
		ASSERT(stp->labels[0].patches < 0); /* Should not be referenced. */
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

	case op_line_I:
	    if (stp->line_item) {
		BeamInstr item = code[ci-1];
		BeamInstr loc;
		unsigned int li;
		if (item >= stp->num_line_items) {
		    LoadError2(stp, "line instruction index overflow (%u/%u)",
			       item, stp->num_line_items);
		}
		li = stp->current_li;
		if (li >= stp->num_line_instrs) {
		    LoadError2(stp, "line instruction table overflow (%u/%u)",
			       li, stp->num_line_instrs);
		}
		loc = stp->line_item[item];

		if (ci - 2 == last_func_start) {
		    /*
		     * This line instruction directly follows the func_info
		     * instruction. Its address must be adjusted to point to
		     * func_info instruction.
		     */
		    stp->line_instr[li].pos = last_func_start - FUNC_INFO_SZ;
		    stp->line_instr[li].loc = stp->line_item[item];
		    stp->current_li++;
		} else if (li <= stp->func_line[function_number-1] ||
			   stp->line_instr[li-1].loc != loc) {
		    /*
		     * Only store the location if it is different
		     * from the previous location in the same function.
		     */
		    stp->line_instr[li].pos = ci - 2;
		    stp->line_instr[li].loc = stp->line_item[item];
		    stp->current_li++;
		}
	    }
	    ci -= 2;		/* Get rid of the instruction */
	    break;

	    /*
	     * End of code found.
	     */
	case op_int_code_end:
	    if (function_number != stp->num_functions) {
		LoadError2(stp, "too few functions (%u) in module (header said %u)",
			   function_number, stp->num_functions);
	    }
	    stp->codev_size = codev_size;
	    stp->ci = ci;
	    stp->function = THE_NON_VALUE;
	    stp->genop = NULL;
	    stp->specific_op = -1;
	    retval = 1;
	    goto cleanup;
	}

	/*
	 * Delete the generic instruction just loaded.
	 */
	{
	    GenOp* next = stp->genop->next;
	    FREE_GENOP(stp, stp->genop);
	    if ((stp->genop = next) == NULL) {
		last_op_next = &stp->genop;
		goto get_next_instr;
	    }
	    goto do_transform;
	}
    }
    
 load_error:
    retval = 0;

 cleanup:
    /*
     * Clean up everything that is not needed any longer.
     */

    while (stp->genop_blocks) {
	GenOpBlock* next = stp->genop_blocks->next;
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) stp->genop_blocks);
	stp->genop_blocks = next;
    }
    return retval;
}

#define succ(St, X, Y) ((X).type == (Y).type && (X).val + 1 == (Y).val)
#define succ2(St, X, Y) ((X).type == (Y).type && (X).val + 2 == (Y).val)
#define succ3(St, X, Y) ((X).type == (Y).type && (X).val + 3 == (Y).val)

#ifdef NO_FPE_SIGNALS 
#define no_fpe_signals(St) 1
#else
#define no_fpe_signals(St) 0
#endif

#define never(St) 0

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
 * Predicate to test whether all values in a table are either
 * floats or bignums.
 */

static int
floats_or_bignums(LoaderState* stp, GenOpArg Size, GenOpArg* Rest)
{
    int i;

    if (Size.val < 2 || Size.val % 2 != 0) {
	return 0;
    }

    for (i = 0; i < Size.val; i += 2) {
	if (Rest[i].type != TAG_q) {
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

static int
is_killed_apply(LoaderState* stp, GenOpArg Reg, GenOpArg Live)
{
    return Reg.type == TAG_x && Live.type == TAG_u &&
	Live.val+2 <= Reg.val;
}

static int
is_killed(LoaderState* stp, GenOpArg Reg, GenOpArg Live)
{
    return Reg.type == TAG_x && Live.type == TAG_u &&
	Live.val <= Reg.val;
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
    op->arity = 4;
    op->next = NULL;

    if (Index.type == TAG_i && Index.val > 0 &&
	(Tuple.type == TAG_x || Tuple.type == TAG_y)) {
	op->op = genop_i_fast_element_4;
	op->a[0] = Fail;
	op->a[1] = Tuple;
	op->a[2].type = TAG_u;
	op->a[2].val = Index.val;
	op->a[3] = Dst;
    } else {
	op->op = genop_i_element_4;
	op->a[0] = Fail;
	op->a[1] = Tuple;
	op->a[2] = Index;
	op->a[3] = Dst;
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
	op->op = genop_i_bs_get_integer_6;
	op->arity = 6;
	op->a[0] = Fail;
	op->a[1] = Live;
	op->a[2].type = TAG_u;
	op->a[2].val = (Unit.val << 3) | Flags.val;
	op->a[3] = Ms;
	op->a[4] = Size;
	op->a[5] = Dst;
	op->next = NULL;
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
    return Size.type == TAG_o ||
	(Size.type == TAG_u && ((Size.val >> (8*sizeof(Uint)-3)) != 0));
}

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
	/* Negative size must fail */
    if (Size.type == TAG_i) {
	op->op = genop_i_new_bs_put_integer_imm_4;
	op->arity = 4;
	op->a[0] = Fail;
	op->a[1].type = TAG_u;
	if (!safe_mul(Size.val, Unit.val, &op->a[1].val)) {
        error:
            op->op = genop_badarg_1;
            op->arity = 1;
            op->a[0] = Fail;
            op->next = NULL;
            return op;
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

static GenOp*
gen_increment(LoaderState* stp, GenOpArg Reg, GenOpArg Integer,
	      GenOpArg Live, GenOpArg Dst)
{
    GenOp* op;

    NEW_GENOP(stp, op);
    op->op = genop_i_increment_4;
    op->arity = 4;
    op->next = NULL;
    op->a[0] = Reg;
    op->a[1].type = TAG_u;
    op->a[1].val = Integer.val;
    op->a[2] = Live;
    op->a[3] = Dst;
    return op;
}

static GenOp*
gen_increment_from_minus(LoaderState* stp, GenOpArg Reg, GenOpArg Integer,
			 GenOpArg Live, GenOpArg Dst)
{
    GenOp* op;

    NEW_GENOP(stp, op);
    op->op = genop_i_increment_4;
    op->arity = 4;
    op->next = NULL;
    op->a[0] = Reg;
    op->a[1].type = TAG_u;
    op->a[1].val = -Integer.val;
    op->a[2] = Live;
    op->a[3] = Dst;
    return op;
}

/*
 * Test whether the negation of the given number is small.
 */
static int
negation_is_small(LoaderState* stp, GenOpArg Int)
{
    /* Check for the rare case of overflow in BeamInstr (UWord) -> Sint
     * Cast to the correct type before using IS_SSMALL (Sint) */
    return Int.type == TAG_i &&
           !(Int.val & ~((((BeamInstr)1) << ((sizeof(Sint)*8)-1))-1)) &&
           IS_SSMALL(-((Sint)Int.val));
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
#if defined(ARCH_64)
	(timeout >> 32) == 0
#else
	1
#endif
	) {
	op->a[1].val = timeout;
#if !defined(ARCH_64)
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
#if !defined(ARCH_64)
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
#if defined(ARCH_64)
	(timeout >> 32) == 0
#else
	1
#endif
	) {
	op->a[1].val = timeout;
#if !defined(ARCH_64)
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
#if !defined(ARCH_64)
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
    GenOpArg *tmp;
    int arity = Size.val + 3;
    int size = Size.val / 2;
    int i, j, align = 0;

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
     * Use a special-cased instruction if there are only two values.
     */
    if (size == 2) {
	NEW_GENOP(stp, op);
	op->next = NULL;
	op->op = genop_i_select_tuple_arity2_6;
	GENOP_ARITY(op, arity - 1);
	op->a[0] = S;
	op->a[1] = Fail;
	op->a[2].type = TAG_u;
	op->a[2].val  = Rest[0].val;
	op->a[3].type = TAG_u;
	op->a[3].val  = Rest[2].val;
	op->a[4] = Rest[1];
	op->a[5] = Rest[3];

	return op;
    }

    /*
     * Generate the generic instruction.
     * Assumption:
     *   Few different tuple arities to select on (fewer than 20).
     *   Use linear scan approach.
     */

    align = 1;

    arity += 2*align;
    size  += align;

    NEW_GENOP(stp, op);
    op->next = NULL;
    op->op = genop_i_select_tuple_arity_3;
    GENOP_ARITY(op, arity);
    op->a[0] = S;
    op->a[1] = Fail;
    op->a[2].type = TAG_u;
    op->a[2].val = size;

    tmp = (GenOpArg *) erts_alloc(ERTS_ALC_T_LOADER_TMP, sizeof(GenOpArg)*(arity-2*align));

    for (i = 3; i < arity - 2*align; i+=2) {
	tmp[i-3].type = TAG_v;
	tmp[i-3].val  = make_arityval(Rest[i-3].val);
	tmp[i-2]      = Rest[i-2];
    }

    /*
     * Sort the values to make them useful for a sentinel search
     */

    qsort(tmp, size - align, 2*sizeof(GenOpArg),
	    (int (*)(const void *, const void *)) genopargcompare);

    j = 3;
    for (i = 3; i < arity - 2*align; i += 2) {
	op->a[j]        = tmp[i-3];
	op->a[j + size] = tmp[i-2];
	j++;
    }

    erts_free(ERTS_ALC_T_LOADER_TMP, (void *) tmp);

    op->a[j].type = TAG_u;
    op->a[j].val  = ~((BeamInstr)0);
    op->a[j+size] = Fail;

    return op;
}

/*
 * Split a list consisting of both small and bignumbers into two
 * select_val instructions.
 */

static GenOp*
gen_split_values(LoaderState* stp, GenOpArg S, GenOpArg TypeFail,
		 GenOpArg Fail, GenOpArg Size, GenOpArg* Rest)

{
    GenOp* op1;
    GenOp* op2;
    GenOp* label;
    GenOp* is_integer;
    int i;

    ASSERT(Size.val >= 2 && Size.val % 2 == 0);

    NEW_GENOP(stp, is_integer);
    is_integer->op = genop_is_integer_2;
    is_integer->arity = 2;
    is_integer->a[0] = TypeFail;
    is_integer->a[1] = S;

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

    /*
     * Split the list.
     */

    ASSERT(Size.type == TAG_u);
    for (i = 0; i < Size.val; i += 2) {
	GenOp* op = (Rest[i].type == TAG_q) ? op2 : op1;
	int dst = 3 + op->a[2].val;

	ASSERT(Rest[i+1].type == TAG_f);
	op->a[dst] = Rest[i];
	op->a[dst+1] = Rest[i+1];
	op->arity += 2;
	op->a[2].val += 2;
    }
    ASSERT(op1->a[2].val > 0);
    ASSERT(op2->a[2].val > 0);

    /*
     * Order the instruction sequence appropriately.
     */

    if (TypeFail.val == Fail.val) {
	/*
	 * select_val L1 S ... (small numbers)
	 * label L1
	 * is_integer Fail S
	 * select_val Fail S ... (bignums)
	 */
	op1->next = label;
	label->next = is_integer;
	is_integer->next = op2;
    } else {
	/*
	 * is_integer TypeFail S
	 * select_val L1 S ... (small numbers)
	 * label L1
	 * select_val Fail S ... (bignums)
	 */
	is_integer->next = op1;
	op1->next = label;
	label->next = op2;
	op1 = is_integer;
    }
    op2->next = NULL;

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
     * If there is only one choice, don't generate a jump table.
     */
    if (Size.val == 2) {
	GenOp* jump;

	NEW_GENOP(stp, op);
	op->arity = 3;
	op->op = genop_is_ne_exact_3;
	op->a[0] = Rest[1];
	op->a[1] = S;
	op->a[2] = Rest[0];

	NEW_GENOP(stp, jump);
	jump->next = NULL;
	jump->arity = 1;
	jump->op = genop_jump_1;
	jump->a[0] = Fail;

	op->next = jump;
	return op;
    }

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
	Sint index;
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
 * Generate a select_val instruction.  We know that a jump table
 * is not suitable, and that all values are of the same type
 * (integer or atoms).
 */

static GenOp*
gen_select_val(LoaderState* stp, GenOpArg S, GenOpArg Fail,
	       GenOpArg Size, GenOpArg* Rest)
{
    GenOp* op;
    GenOpArg *tmp;
    int arity = Size.val + 3;
    int size = Size.val / 2;
    int i, j, align = 0;

    if (size == 2) {

	/*
	 * Use a special-cased instruction if there are only two values.
	 */

	NEW_GENOP(stp, op);
	op->next = NULL;
	op->op = genop_i_select_val2_6;
	GENOP_ARITY(op, arity - 1);
	op->a[0] = S;
	op->a[1] = Fail;
	op->a[2] = Rest[0];
	op->a[3] = Rest[2];
	op->a[4] = Rest[1];
	op->a[5] = Rest[3];

	return op;

    } else if (size > 10) {

	/* binary search instruction */

	NEW_GENOP(stp, op);
	op->next = NULL;
	op->op = genop_i_select_val_bins_3;
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

    /* linear search instruction */

    align = 1;

    arity += 2*align;
    size  += align;

    NEW_GENOP(stp, op);
    op->next = NULL;
    op->op = genop_i_select_val_lins_3;
    GENOP_ARITY(op, arity);
    op->a[0] = S;
    op->a[1] = Fail;
    op->a[2].type = TAG_u;
    op->a[2].val = size;

    tmp = (GenOpArg *) erts_alloc(ERTS_ALC_T_LOADER_TMP, sizeof(GenOpArg)*(arity-2*align));

    for (i = 3; i < arity - 2*align; i++) {
	tmp[i-3] = Rest[i-3];
    }

    /*
     * Sort the values to make them useful for a sentinel search
     */

    qsort(tmp, size - align, 2*sizeof(GenOpArg),
	    (int (*)(const void *, const void *)) genopargcompare);

    j = 3;
    for (i = 3; i < arity - 2*align; i += 2) {
	op->a[j]      = tmp[i-3];
	op->a[j+size] = tmp[i-2];
	j++;
    }

    erts_free(ERTS_ALC_T_LOADER_TMP, (void *) tmp);

    /* add sentinel */

    op->a[j].type = TAG_u;
    op->a[j].val  = ~((BeamInstr)0);
    op->a[j+size] = Fail;

#ifdef DEBUG
    for (i = 0; i < size - 1; i++) {
	ASSERT(op->a[i+3].val <= op->a[i+4].val);
    }
#endif

    return op;
}

/*
 * Generate a select_val instruction for big numbers.
 */

static GenOp*
gen_select_literals(LoaderState* stp, GenOpArg S, GenOpArg Fail,
	       GenOpArg Size, GenOpArg* Rest)
{
    GenOp* op;
    GenOp* jump;
    GenOp** prev_next = &op;

    int i;

    for (i = 0; i < Size.val; i += 2) {
	GenOp* op;
	ASSERT(Rest[i].type == TAG_q);

	NEW_GENOP(stp, op);
	op->op = genop_is_ne_exact_3;
	op->arity = 3;
	op->a[0] = Rest[i+1];
	op->a[1] = S;
	op->a[2] = Rest[i];
	*prev_next = op;
	prev_next = &op->next;
    }

    NEW_GENOP(stp, jump);
    jump->next = NULL;
    jump->op = genop_jump_1;
    jump->arity = 1;
    jump->a[0] = Fail;
    *prev_next = jump;
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

    NEW_GENOP(stp, op);
    op->next = NULL;
    op->op = genop_jump_1;
    op->arity = 1;

    /*
     * Search for a literal matching the controlling expression.
     */

    switch (S.type) {
    case TAG_q:
	{
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
	break;
    case TAG_i:
    case TAG_a:
	for (i = 0; i < Size.val; i += 2) {
	    if (Rest[i].val == S.val && Rest[i].type == S.type) {
		ASSERT(Rest[i+1].type == TAG_f);
		op->a[0] = Rest[i+1];
		return op;
	    }
	}
	break;
    }

    /*
     * No match.  Use the failure label.
     */

    op->a[0] = Fail;
    return op;
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

static GenOp*
translate_gc_bif(LoaderState* stp, GenOp* op, GenOpArg Bif)
{
    const ErtsGcBif* p;
    BifFunction bf;

    bf = stp->import[Bif.val].bf;
    for (p = erts_gc_bifs; p->bif != 0; p++) {
	if (p->bif == bf) {
	    op->a[1].type = TAG_u;
	    op->a[1].val = (BeamInstr) p->gc_bif;
	    return op;
	}
    }

    op->op = genop_unsupported_guard_bif_3;
    op->arity = 3;
    op->a[0].type = TAG_a;
    op->a[0].val = stp->import[Bif.val].module;
    op->a[1].type = TAG_a;
    op->a[1].val = stp->import[Bif.val].function;
    op->a[2].type = TAG_u;
    op->a[2].val = stp->import[Bif.val].arity;
    return op;
}

/*
 * Rewrite gc_bifs with one parameter (the common case).
 */
static GenOp*
gen_guard_bif1(LoaderState* stp, GenOpArg Fail, GenOpArg Live, GenOpArg Bif,
	      GenOpArg Src, GenOpArg Dst)
{
    GenOp* op;

    NEW_GENOP(stp, op);
    op->next = NULL;
    op->op = genop_i_gc_bif1_5;
    op->arity = 5;
    op->a[0] = Fail;
    /* op->a[1] is set by translate_gc_bif() */
    op->a[2] = Src;
    op->a[3] = Live;
    op->a[4] = Dst;
    return translate_gc_bif(stp, op, Bif);
}

/*
 * This is used by the ops.tab rule that rewrites gc_bifs with two parameters.
 */
static GenOp*
gen_guard_bif2(LoaderState* stp, GenOpArg Fail, GenOpArg Live, GenOpArg Bif,
	      GenOpArg S1, GenOpArg S2, GenOpArg Dst)
{
    GenOp* op;

    NEW_GENOP(stp, op);
    op->next = NULL;
    op->op = genop_i_gc_bif2_6;
    op->arity = 6;
    op->a[0] = Fail;
    /* op->a[1] is set by translate_gc_bif() */
    op->a[2] = Live;
    op->a[3] = S1;
    op->a[4] = S2;
    op->a[5] = Dst;
    return translate_gc_bif(stp, op, Bif);
}

/*
 * This is used by the ops.tab rule that rewrites gc_bifs with three parameters.
 */
static GenOp*
gen_guard_bif3(LoaderState* stp, GenOpArg Fail, GenOpArg Live, GenOpArg Bif,
	      GenOpArg S1, GenOpArg S2, GenOpArg S3, GenOpArg Dst)
{
    GenOp* op;

    NEW_GENOP(stp, op);
    op->next = NULL;
    op->op = genop_ii_gc_bif3_7;
    op->arity = 7;
    op->a[0] = Fail;
    /* op->a[1] is set by translate_gc_bif() */
    op->a[2] = Live;
    op->a[3] = S1;
    op->a[4] = S2;
    op->a[5] = S3;
    op->a[6] = Dst;
    return translate_gc_bif(stp, op, Bif);
}

static GenOp*
tuple_append_put5(LoaderState* stp, GenOpArg Arity, GenOpArg Dst,
		  GenOpArg* Puts, GenOpArg S1, GenOpArg S2, GenOpArg S3,
		  GenOpArg S4, GenOpArg S5)
{
    GenOp* op;
    int arity = Arity.val;	/* Arity of tuple, not the instruction */
    int i;

    NEW_GENOP(stp, op);
    op->next = NULL;
    GENOP_ARITY(op, arity+2+5);
    op->op = genop_i_put_tuple_2;
    op->a[0] = Dst;
    op->a[1].type = TAG_u;
    op->a[1].val = arity + 5;
    for (i = 0; i < arity; i++) {
	op->a[i+2] = Puts[i];
    }
    op->a[arity+2] = S1;
    op->a[arity+3] = S2;
    op->a[arity+4] = S3;
    op->a[arity+5] = S4;
    op->a[arity+6] = S5;
    return op;
}

static GenOp*
tuple_append_put(LoaderState* stp, GenOpArg Arity, GenOpArg Dst,
		 GenOpArg* Puts, GenOpArg S)
{
    GenOp* op;
    int arity = Arity.val;	/* Arity of tuple, not the instruction */
    int i;

    NEW_GENOP(stp, op);
    op->next = NULL;
    GENOP_ARITY(op, arity+2+1);
    op->op = genop_i_put_tuple_2;
    op->a[0] = Dst;
    op->a[1].type = TAG_u;
    op->a[1].val = arity + 1;
    for (i = 0; i < arity; i++) {
	op->a[i+2] = Puts[i];
    }
    op->a[arity+2] = S;
    return op;
}

/*
 * Predicate to test whether the given literal is a map.
 */

static int
literal_is_map(LoaderState* stp, GenOpArg Lit)
{
    Eterm term;

    ASSERT(Lit.type == TAG_q);
    term = stp->literals[Lit.val].term;
    return is_map(term);
}

/*
 * Predicate to test whether the given literal is an empty map.
 */

static int
is_empty_map(LoaderState* stp, GenOpArg Lit)
{
    Eterm term;

    if (Lit.type != TAG_q) {
	return 0;
    }
    term = stp->literals[Lit.val].term;
    return is_flatmap(term) && flatmap_get_size(flatmap_val(term)) == 0;
}

/*
 * Pseudo predicate map_key_sort that will sort the Rest operand for
 * map instructions as a side effect.
 */

typedef struct SortGenOpArg {
    Eterm term;			/* Term to use for comparing  */
    GenOpArg arg;		/* Original data */
} SortGenOpArg;

static int
genopargtermcompare(SortGenOpArg* a, SortGenOpArg* b)
{
    return CMP_TERM(a->term, b->term);
}

static int
map_key_sort(LoaderState* stp, GenOpArg Size, GenOpArg* Rest)
{
    SortGenOpArg* t;
    unsigned size = Size.val;
    unsigned i;

    if (size == 2) {
	return 1;		/* Already sorted. */
    }


    t = (SortGenOpArg *) erts_alloc(ERTS_ALC_T_TMP, size*sizeof(SortGenOpArg));

    /*
     * Copy original data and sort keys to a temporary array.
     */
    for (i = 0; i < size; i += 2) {
	t[i].arg = Rest[i];
	switch (Rest[i].type) {
	case TAG_a:
	    t[i].term = Rest[i].val;
	    ASSERT(is_atom(t[i].term));
	    break;
	case TAG_i:
	    t[i].term = make_small(Rest[i].val);
	    break;
	case TAG_n:
	    t[i].term = NIL;
	    break;
	case TAG_q:
	    t[i].term = stp->literals[Rest[i].val].term;
	    break;
	default:
	    /*
	     * Not a literal key. Not allowed. Only a single
	     * variable key is allowed in each map instruction.
	     */
	    erts_free(ERTS_ALC_T_TMP, (void *) t);
	    return 0;
	}
#ifdef DEBUG
	t[i+1].term = THE_NON_VALUE;
#endif
	t[i+1].arg = Rest[i+1];
    }

    /*
     * Sort the temporary array.
     */
    qsort((void *) t, size / 2, 2 * sizeof(SortGenOpArg),
	  (int (*)(const void *, const void *)) genopargtermcompare);

    /*
     * Copy back the sorted, original data.
     */
    for (i = 0; i < size; i++) {
	Rest[i] = t[i].arg;
    }

    erts_free(ERTS_ALC_T_TMP, (void *) t);
    return 1;
}

static int
hash_genop_arg(LoaderState* stp, GenOpArg Key, Uint32* hx)
{
    switch (Key.type) {
    case TAG_a:
	*hx = hashmap_make_hash(Key.val);
	return 1;
    case TAG_i:
	*hx = hashmap_make_hash(make_small(Key.val));
	return 1;
    case TAG_n:
	*hx = hashmap_make_hash(NIL);
	return 1;
    case TAG_q:
	*hx = hashmap_make_hash(stp->literals[Key.val].term);
	return 1;
    default:
	return 0;
    }
}

/*
 * Replace a get_map_elements with one key to an instruction with one
 * element.
 */

static GenOp*
gen_get_map_element(LoaderState* stp, GenOpArg Fail, GenOpArg Src,
		    GenOpArg Size, GenOpArg* Rest)
{
    GenOp* op;
    GenOpArg Key;
    Uint32 hx = 0;

    ASSERT(Size.type == TAG_u);

    NEW_GENOP(stp, op);
    op->next = NULL;
    op->a[0] = Fail;
    op->a[1] = Src;
    op->a[2] = Rest[0];

    Key = Rest[0];
    if (hash_genop_arg(stp, Key, &hx)) {
	op->arity = 5;
	op->op = genop_i_get_map_element_hash_5;
	op->a[3].type = TAG_u;
	op->a[3].val = (BeamInstr) hx;
	op->a[4] = Rest[1];
    } else {
	op->arity = 4;
	op->op = genop_i_get_map_element_4;
	op->a[3] = Rest[1];
    }
    return op;
}

static int
hash_internal_genop_arg(LoaderState* stp, GenOpArg Key, Uint32* hx)
{
    Eterm key_term;
    switch (Key.type) {
    case TAG_a:
	key_term = Key.val;
        break;
    case TAG_i:
	key_term = make_small(Key.val);
        break;
    case TAG_n:
        key_term = NIL;
        break;
    case TAG_q:
	key_term = stp->literals[Key.val].term;
	break;
    default:
	return 0;
    }
    *hx = erts_pd_make_hx(key_term);
    return 1;
}


static GenOp*
gen_get(LoaderState* stp, GenOpArg Src, GenOpArg Dst)
{
    GenOp* op;
    Uint32 hx = 0;

    NEW_GENOP(stp, op);
    op->next = NULL;
    if (hash_internal_genop_arg(stp, Src, &hx)) {
	op->arity = 3;
	op->op = genop_i_get_hash_3;
        op->a[0] = Src;
	op->a[1].type = TAG_u;
	op->a[1].val = (BeamInstr) hx;
	op->a[2] = Dst;
    } else {
	op->arity = 2;
	op->op = genop_i_get_2;
        op->a[0] = Src;
	op->a[1] = Dst;
    }
    return op;
}


static GenOp*
gen_get_map_elements(LoaderState* stp, GenOpArg Fail, GenOpArg Src,
		     GenOpArg Size, GenOpArg* Rest)
{
    GenOp* op;
    Uint32 hx;
    Uint i;
    GenOpArg* dst;
#ifdef DEBUG
    int good_hash;
#endif

    ERTS_UNDEF(hx, 0);
    ASSERT(Size.type == TAG_u);

    NEW_GENOP(stp, op);
    op->op = genop_i_get_map_elements_3;
    GENOP_ARITY(op, 3 + 3*(Size.val/2));
    op->next = NULL;
    op->a[0] = Fail;
    op->a[1] = Src;
    op->a[2].type = TAG_u;
    op->a[2].val = 3*(Size.val/2);

    dst = op->a+3;
    for (i = 0; i < Size.val / 2; i++) {
	dst[0] = Rest[2*i];
	dst[1] = Rest[2*i+1];
#ifdef DEBUG
	good_hash =
#endif
	    hash_genop_arg(stp, dst[0], &hx);
#ifdef DEBUG
	ASSERT(good_hash);
#endif
	dst[2].type = TAG_u;
	dst[2].val = (BeamInstr) hx;
	dst += 3;
    }
    return op;
}

static GenOp*
gen_has_map_fields(LoaderState* stp, GenOpArg Fail, GenOpArg Src,
		   GenOpArg Size, GenOpArg* Rest)
{
    GenOp* op;
    Uint i;
    Uint n;

    ASSERT(Size.type == TAG_u);
    n = Size.val;

    NEW_GENOP(stp, op);
    GENOP_ARITY(op, 3 + 2*n);
    op->next = NULL;
    op->op = genop_get_map_elements_3;

    op->a[0] = Fail;
    op->a[1] = Src;
    op->a[2].type = TAG_u;
    op->a[2].val = 2*n;

    for (i = 0; i < n; i++) {
	op->a[3+2*i] = Rest[i];
	op->a[3+2*i+1].type = TAG_x;
	op->a[3+2*i+1].val = SCRATCH_X_REG; /* Ignore result */
    }
    return op;
}

/*
 * Freeze the code in memory, move the string table into place,
 * resolve all labels.
 */

static int
freeze_code(LoaderState* stp)
{
    BeamCodeHeader* code_hdr = stp->hdr;
    BeamInstr* codev = (BeamInstr*) &stp->hdr->functions;
    int i;
    byte* str_table;
    unsigned strtab_size = stp->chunks[STR_CHUNK].size;
    unsigned attr_size = stp->chunks[ATTR_CHUNK].size;
    unsigned compile_size = stp->chunks[COMPILE_CHUNK].size;
    Uint size;
    Sint decoded_size;
    Uint line_size;

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
    if (stp->line_instr == 0) {
	line_size = 0;
    } else {
	line_size = (offsetof(BeamCodeLineTab,func_tab)
                     + (stp->num_functions + 1) * sizeof(BeamInstr**) /* func_tab */
                     + (stp->current_li + 1) * sizeof(BeamInstr*)     /* line items */
                     + stp->num_fnames * sizeof(Eterm)                /* fname table */
                     + (stp->current_li + 1) * stp->loc_size);        /* loc_tab */
    }
    size = offsetof(BeamCodeHeader,functions) + (stp->ci * sizeof(BeamInstr)) +
	strtab_size + attr_size + compile_size + MD5_SIZE + line_size;

    /*
     * Move the code to its final location.
     */

    code_hdr = (BeamCodeHeader*) erts_realloc(ERTS_ALC_T_CODE, (void *) code_hdr, size);
    codev = (BeamInstr*) &code_hdr->functions;
    CHKBLK(ERTS_ALC_T_CODE,code_hdr);
    /*
     * Place a pointer to the op_int_code_end instruction in the
     * function table in the beginning of the file.
     */

    code_hdr->functions[stp->num_functions] = (ErtsCodeInfo*)(codev + stp->ci - 1);
    CHKBLK(ERTS_ALC_T_CODE,code_hdr);

    /*
     * Store the pointer to the on_load function.
     */

    if (stp->on_load) {
	code_hdr->on_load_function_ptr = codev + stp->on_load;
    } else {
	code_hdr->on_load_function_ptr = NULL;
    }
    CHKBLK(ERTS_ALC_T_CODE,code_hdr);

    /*
     * Place the literals in their own allocated heap (for fast range check)
     * and fix up all instructions that refer to it.
     */
    {
	Eterm* ptr;
	LiteralPatch* lp;
        ErlOffHeap code_off_heap;
	ErtsLiteralArea *literal_area;
	Uint lit_asize;

        ERTS_INIT_OFF_HEAP(&code_off_heap);

	lit_asize = ERTS_LITERAL_AREA_ALLOC_SIZE(stp->total_literal_size);
	literal_area = erts_alloc(ERTS_ALC_T_LITERAL, lit_asize);
	ptr = &literal_area->start[0];
	literal_area->end = ptr + stp->total_literal_size;

	for (i = 0; i < stp->num_literals; i++) {
            if (is_not_immed(stp->literals[i].term)) {
                erts_move_multi_frags(&ptr, &code_off_heap,
				      stp->literals[i].heap_frags,
				      &stp->literals[i].term, 1, 1);
                ASSERT(erts_is_literal(stp->literals[i].term,
				       ptr_val(stp->literals[i].term)));
            }
	}
	literal_area->off_heap = code_off_heap.first;
	lp = stp->literal_patches;
	while (lp != 0) {
	    BeamInstr* op_ptr;
	    Literal* lit;

	    op_ptr = codev + lp->pos;
	    lit = &stp->literals[op_ptr[0]];
	    op_ptr[0] = lit->term;
	    lp = lp->next;
	}
	code_hdr->literal_area = literal_area;
    }
    CHKBLK(ERTS_ALC_T_CODE,code);

    /*
     * If there is line information, place it here.
     */
    if (stp->line_instr == 0) {
	code_hdr->line_table = NULL;
	str_table = (byte *) (codev + stp->ci);
    } else {
	BeamCodeLineTab* const line_tab = (BeamCodeLineTab *) (codev+stp->ci);
	const unsigned int ftab_size = stp->num_functions;
        const unsigned int num_instrs = stp->current_li;
        const BeamInstr** const line_items =
            (const BeamInstr**) &line_tab->func_tab[ftab_size + 1];

	code_hdr->line_table = line_tab;

	for (i = 0; i < ftab_size; i++) {
            line_tab->func_tab[i] = line_items + stp->func_line[i];
	}
	line_tab->func_tab[i] = line_items + num_instrs;

	for (i = 0; i < num_instrs; i++) {
            line_items[i] = codev + stp->line_instr[i].pos;
	}
	line_items[i] = codev + stp->ci - 1;

	line_tab->fname_ptr = (Eterm*) &line_items[i + 1];
	memcpy(line_tab->fname_ptr, stp->fname, stp->num_fnames*sizeof(Eterm));

	line_tab->loc_size = stp->loc_size;
	if (stp->loc_size == 2) {
            Uint16* locp = (Uint16 *) &line_tab->fname_ptr[stp->num_fnames];
            line_tab->loc_tab.p2 = locp;
            for (i = 0; i < num_instrs; i++) {
		*locp++ = (Uint16) stp->line_instr[i].loc;
            }
            *locp++ = LINE_INVALID_LOCATION;
            str_table = (byte *) locp;
	} else {
	    Uint32* locp = (Uint32 *) &line_tab->fname_ptr[stp->num_fnames];
            ASSERT(stp->loc_size == 4);
            line_tab->loc_tab.p4 = locp;
	    for (i = 0; i < num_instrs; i++) {
		*locp++ = stp->line_instr[i].loc;
	    }
	    *locp++ = LINE_INVALID_LOCATION;
            str_table = (byte *) locp;
	}
	CHKBLK(ERTS_ALC_T_CODE,code);
    }

    /*
     * Place the string table and, optionally, attributes here.
     */
    sys_memcpy(str_table, stp->chunks[STR_CHUNK].start, strtab_size);
    CHKBLK(ERTS_ALC_T_CODE,code);
    if (attr_size) {
	byte* attr = str_table + strtab_size;
	sys_memcpy(attr, stp->chunks[ATTR_CHUNK].start, stp->chunks[ATTR_CHUNK].size);
	code_hdr->attr_ptr = attr;
	code_hdr->attr_size = (BeamInstr) stp->chunks[ATTR_CHUNK].size;
	decoded_size = erts_decode_ext_size(attr, attr_size);
	if (decoded_size < 0) {
 	    LoadError0(stp, "bad external term representation of module attributes");
 	}
	code_hdr->attr_size_on_heap = decoded_size;
    }
    CHKBLK(ERTS_ALC_T_CODE,code);
    if (compile_size) {
	byte* compile_info = str_table + strtab_size + attr_size;
	CHKBLK(ERTS_ALC_T_CODE,code);
	sys_memcpy(compile_info, stp->chunks[COMPILE_CHUNK].start,
	       stp->chunks[COMPILE_CHUNK].size);

	CHKBLK(ERTS_ALC_T_CODE,code);
	code_hdr->compile_ptr = compile_info;
	CHKBLK(ERTS_ALC_T_CODE,code);
	code_hdr->compile_size = (BeamInstr) stp->chunks[COMPILE_CHUNK].size;
	CHKBLK(ERTS_ALC_T_CODE,code);
	decoded_size = erts_decode_ext_size(compile_info, compile_size);
	CHKBLK(ERTS_ALC_T_CODE,code);
	if (decoded_size < 0) {
 	    LoadError0(stp, "bad external term representation of compilation information");
 	}
	CHKBLK(ERTS_ALC_T_CODE,code);
	code_hdr->compile_size_on_heap = decoded_size;
    }
    CHKBLK(ERTS_ALC_T_CODE,code);
    {
	byte* md5_sum = str_table + strtab_size + attr_size + compile_size;
	CHKBLK(ERTS_ALC_T_CODE,code);
	sys_memcpy(md5_sum, stp->mod_md5, MD5_SIZE);
	CHKBLK(ERTS_ALC_T_CODE,code);
	code_hdr->md5_ptr = md5_sum;
	CHKBLK(ERTS_ALC_T_CODE,code);
    }
    CHKBLK(ERTS_ALC_T_CODE,code);

    /*
     * Make sure that we have not overflowed the allocated code space.
     */
    ASSERT(str_table + strtab_size + attr_size + compile_size + MD5_SIZE ==
	   ((byte *) code_hdr) + size);

    /*
     * Patch all instructions that refer to the string table.
     */
    {
	StringPatch* sp = stp->string_patches;

	while (sp != 0) {
	    BeamInstr* op_ptr;
	    byte* strp;

	    op_ptr = codev + sp->pos;
	    strp = str_table + op_ptr[0];
	    op_ptr[0] = (BeamInstr) strp;
	    sp = sp->next;
	}
    }
    CHKBLK(ERTS_ALC_T_CODE,code_hdr);

    /*
     * Resolve all labels.
     */

    for (i = 0; i < stp->num_labels; i++) {
	Sint this_patch;
	Sint next_patch;
	Uint value = stp->labels[i].value;
	
	if (value == 0 && stp->labels[i].patches >= 0) {
	    LoadError1(stp, "label %d not resolved", i);
	}
	ASSERT(value < stp->ci);
	this_patch = stp->labels[i].patches;
	while (this_patch >= 0) {
	    ASSERT(this_patch < stp->ci);
	    next_patch = codev[this_patch];
	    ASSERT(next_patch < stp->ci);
	    codev[this_patch] = (BeamInstr) (codev + value);
	    this_patch = next_patch;
	}
    }
    CHKBLK(ERTS_ALC_T_CODE,code_hdr);

    /*
     * Save the updated code pointer and code size.
     */

    stp->hdr = code_hdr;
    stp->codev = codev;
    stp->loaded_size = size;

    CHKBLK(ERTS_ALC_T_CODE,code_hdr);
    return 1;

 load_error:
    /*
     * Make sure that the caller frees the newly reallocated block, and
     * not the old one (in case it has moved).
     */
    stp->hdr = code_hdr;
    stp->codev = codev;
    return 0;
}

static void
final_touch(LoaderState* stp, struct erl_module_instance* inst_p)
{
    unsigned int i;
    int on_load = stp->on_load;
    unsigned catches;
    Uint index;
    BeamInstr* codev = stp->codev;

    /*
     * Allocate catch indices and fix up all catch_yf instructions.
     */

    index = stp->catches;
    catches = BEAM_CATCHES_NIL;
    while (index != 0) {
	BeamInstr next = codev[index];
	codev[index] = BeamOpCode(op_catch_yf);
	catches = beam_catches_cons((BeamInstr *)codev[index+2], catches);
	codev[index+2] = make_catch(catches);
	index = next;
    }
    inst_p->catches = catches;

    /*
     * Export functions.
     */

    for (i = 0; i < stp->num_exps; i++) {
	Export* ep;
	BeamInstr* address = stp->export[i].address;

	if (address == NULL) {
	    /* Skip stub for a BIF */
	    continue;
	}
	ep = erts_export_put(stp->module, stp->export[i].function,
			     stp->export[i].arity);
	if (on_load) {
	    /*
	     * on_load: Don't make any of the exported functions
	     * callable yet. Keep any function in the current
	     * code callable.
	     */
	    ep->beam[1] = (BeamInstr) address;
	}
        else
            ep->addressv[erts_staging_code_ix()] = address;
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
	    next = stp->codev[current];
	    stp->codev[current] = import;
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
	    BeamInstr* code_ptr = stp->codev + stp->labels[entry_label].value;

	    if (fe->address[0] != 0) {
		/*
		 * We are hiding a pointer into older code.
		 */
		erts_smp_refc_dec(&fe->refc, 1);
	    }
	    fe->address = code_ptr;
#ifdef HIPE
	    hipe_set_closure_stub(fe);
#endif
	}
    }
}

static int
transform_engine(LoaderState* st)
{
    Uint op;
    int ap;			/* Current argument. */
    const Uint* restart; /* Where to restart if current match fails. */
    GenOpArg var[TE_MAX_VARS];	/* Buffer for variables. */
    GenOpArg* rest_args = NULL;
    int num_rest_args = 0;
    int i;			/* General index. */
    Uint mask;
    GenOp* instr;
    GenOp* first = st->genop;
    GenOp* keep = NULL;
    const Uint* pc;
    static Uint restart_fail[1] = {TOP_fail};

    ASSERT(gen_opc[first->op].transform != -1);
    restart = op_transform + gen_opc[first->op].transform;

 restart:
    ASSERT(restart != NULL);
    pc = restart;
    ASSERT(*pc < NUM_TOPS);	/* Valid instruction? */
    instr = first;

#ifdef DEBUG
    restart = NULL;
#endif
    ap = 0;
    for (;;) {
	op = *pc++;

	switch (op) {
	case TOP_next_instr:
	    instr = instr->next;
	    ap = 0;
	    if (instr == NULL) {
		/*
		 * We'll need at least one more instruction to decide whether
		 * this combination matches or not.
		 */
		return TE_SHORT_WINDOW;
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
#if defined(TOP_is_eq)
	case TOP_is_eq:
	    ASSERT(ap < instr->arity);
	    if (*pc++ != instr->a[ap].val)
		goto restart;
	    break;
#endif
	case TOP_is_type_eq:
	    mask = *pc++;

	    ASSERT(ap < instr->arity);
	    ASSERT(instr->a[ap].type < BEAM_NUM_TAGS);
	    if (((1 << instr->a[ap].type) & mask) == 0)
		goto restart;
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
	    case TAG_n:
		break;
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
		    bif_export[bif_number]->beam[1] != (BeamInstr) st->import[i].bf) {
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
		int formal_arity = gen_opc[instr->op].arity;
		num_rest_args = instr->arity - formal_arity;
		rest_args = instr->a + formal_arity;
	    }
	    break;
#endif
	case TOP_next_arg:
	    ap++;
	    break;
	case TOP_commit:
	    instr = instr->next; /* The next_instr was optimized away. */
	    keep = instr;
	    st->genop = instr;
#ifdef DEBUG
	    instr = 0;
#endif
	    break;
#if defined(TOP_keep)
	case TOP_keep:
	    /* Keep the current instruction unchanged. */
	    keep = instr;
	    st->genop = instr;
#ifdef DEBUG
	    instr = 0;
#endif
	    break;
#endif
#if defined(TOP_call_end)
	case TOP_call_end:
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
		 
		keep = instr->next; /* The next_instr was optimized away. */
		*lastp = keep;
		st->genop = new_instr;
	    }
	    /* FALLTHROUGH */
#endif
	case TOP_end:
	    while (first != keep) {
		GenOp* next = first->next;
		FREE_GENOP(st, first);
		first = next;
	    }
	    return TE_OK;
	case TOP_new_instr:
	    /*
	     * Note that the instructions are generated in reverse order.
	     */
	    NEW_GENOP(st, instr);
	    instr->next = st->genop;
	    st->genop = instr;
	    instr->op = op = *pc++;
	    instr->arity = gen_opc[op].arity;
	    ap = 0;
	    break;
#ifdef TOP_rename
	case TOP_rename:
	    instr->op = op = *pc++;
	    instr->arity = gen_opc[op].arity;
	    return TE_OK;
#endif
	case TOP_store_type:
	    i = *pc++;
	    instr->a[ap].type = i;
	    instr->a[ap].val = 0;
	    break;
	case TOP_store_val:
	    i = *pc++;
	    instr->a[ap].val = i;
	    break;
	case TOP_store_var_next_arg:
	    i = *pc++;
	    ASSERT(i < TE_MAX_VARS);
	    instr->a[ap].type = var[i].type;
	    instr->a[ap].val = var[i].val;
	    ap++;
	    break;
#if defined(TOP_store_rest_args)
	case TOP_store_rest_args:
	    {
		GENOP_ARITY(instr, instr->arity+num_rest_args);
		memcpy(instr->a, instr->def_args, ap*sizeof(GenOpArg));
		memcpy(instr->a+ap, rest_args, num_rest_args*sizeof(GenOpArg));
		ap += num_rest_args;
	    }
	    break;
#endif
	case TOP_try_me_else:
	    restart = pc + 1;
	    restart += *pc++;
	    ASSERT(*pc < NUM_TOPS); /* Valid instruction? */
	    break;
	case TOP_try_me_else_fail:
	    restart = restart_fail;
	    break;
	case TOP_fail:
	    return TE_FAIL;
	default:
	    ASSERT(0);
	}
    }
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
get_tag_and_value(LoaderState* stp, Uint len_code,
		  unsigned tag, BeamInstr* result)
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
	unsigned sztag;
	UWord len_word;

	ASSERT(len_code == 7);
	GetTagAndValue(stp, sztag, len_word);
	VerifyTag(stp, sztag, TAG_u);
	count = len_word + 9;
    }

    /*
     * The value for tags except TAG_i must be an unsigned integer
     * fitting in an Uint. If it does not fit, we'll indicate overflow
     * by changing the tag to TAG_o.
     */

    if (tag != TAG_i) {
	if (count == sizeof(Uint)+1) {
	    Uint msb;

	    /*
	     * The encoded value has one more byte than an Uint.
	     * It will still fit in an Uint if the most significant
	     * byte is 0.
	     */
	    GetByte(stp, msb);
	    GetInt(stp, sizeof(Uint), *result);
	    if (msb != 0) {
		/* Overflow: Negative or too big. */
		return TAG_o;
	    }
	} else if (count == sizeof(Uint)) {
	    /*
	     * The value must be positive (or the encoded value would
	     * have been one byte longer).
	     */
	    GetInt(stp, count, *result);
	} else if (count < sizeof(Uint)) {
	    GetInt(stp, count, *result);

	    /*
	     * If the sign bit is set, the value is negative
	     * (not allowed).
	     */
	    if (*result & ((Uint)1 << (count*8-1))) {
		return TAG_o;
	    }
	} else {
	    GetInt(stp, count, *result);
	    return TAG_o;
	}
	return tag;
    }

    /*
     * TAG_i: First handle values up to the size of an Uint (i.e. either
     * a small or a bignum).
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
    if (is_nil(bytes_to_big(bigbuf, count, neg, hp)))
	goto load_error;

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
    unsigned int num = stp->num_labels;

    stp->num_labels++;
    stp->labels = (Label *) erts_realloc(ERTS_ALC_T_PREPARED_CODE,
					 (void *) stp->labels,
					 stp->num_labels * sizeof(Label));
    stp->labels[num].value = 0;
    stp->labels[num].patches = -1;
    return num;
}

static void
new_literal_patch(LoaderState* stp, int pos)
{
    LiteralPatch* p = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
				 sizeof(LiteralPatch));
    p->pos = pos;
    p->next = stp->literal_patches;
    stp->literal_patches = p;
}

static void
new_string_patch(LoaderState* stp, int pos)
{
    StringPatch* p = erts_alloc(ERTS_ALC_T_PREPARED_CODE, sizeof(StringPatch));
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
	stp->literals = (Literal *) erts_alloc(ERTS_ALC_T_PREPARED_CODE,
					       need);
    } else if (stp->allocated_literals <= stp->num_literals) {
	Uint need;

	stp->allocated_literals *= 2;
	need = stp->allocated_literals * sizeof(Literal);
	stp->literals = (Literal *) erts_realloc(ERTS_ALC_T_PREPARED_CODE,
						 (void *) stp->literals,
						 need);
    }

    stp->total_literal_size += heap_size;
    lit = stp->literals + stp->num_literals;
    lit->heap_frags = new_literal_fragment(heap_size);
    lit->term = make_boxed(lit->heap_frags->mem);
    *hpp = lit->heap_frags->mem;
    return stp->num_literals++;
}

Eterm
erts_module_info_0(Process* p, Eterm module)
{
    Module* modp;
    ErtsCodeIndex code_ix = erts_active_code_ix();
    BeamCodeHeader* code_hdr;
    Eterm *hp;
    Eterm list = NIL;
    Eterm tup;

    if (is_not_atom(module)) {
	return THE_NON_VALUE;
    }

    modp = erts_get_module(module, code_ix);
    if (modp == NULL) {
	return THE_NON_VALUE;
    }

    code_hdr = modp->curr.code_hdr;
    if (code_hdr == NULL) {
        return THE_NON_VALUE;
    }

#define BUILD_INFO(What) \
    tup = get_module_info(p, code_ix, code_hdr, module, What); \
    hp = HAlloc(p, 5); \
    tup = TUPLE2(hp, What, tup); \
    hp += 3; \
    list = CONS(hp, tup, list)

    BUILD_INFO(am_md5);
#ifdef HIPE
    BUILD_INFO(am_native);
#endif
    BUILD_INFO(am_compile);
    BUILD_INFO(am_attributes);
    BUILD_INFO(am_exports);
    BUILD_INFO(am_module);
#undef BUILD_INFO
    return list;
}

Eterm
erts_module_info_1(Process* p, Eterm module, Eterm what)
{
    Module* modp;
    ErtsCodeIndex code_ix = erts_active_code_ix();
    BeamCodeHeader* code_hdr;

    if (is_not_atom(module)) {
        return THE_NON_VALUE;
    }

    modp = erts_get_module(module, code_ix);
    if (modp == NULL) {
        return THE_NON_VALUE;
    }

    code_hdr = modp->curr.code_hdr;
    if (code_hdr == NULL) {
        return THE_NON_VALUE;
    }

    return get_module_info(p, code_ix, code_hdr, module, what);
}

static Eterm
get_module_info(Process* p, ErtsCodeIndex code_ix, BeamCodeHeader* code_hdr,
                Eterm module, Eterm what)
{
    if (what == am_module) {
	return module;
    } else if (what == am_md5) {
	return md5_of_module(p, code_hdr);
    } else if (what == am_exports) {
	return exported_from_module(p, code_ix, module);
    } else if (what == am_functions) {
	return functions_in_module(p, code_hdr);
    } else if (what == am_attributes) {
	return attributes_for_module(p, code_hdr);
    } else if (what == am_compile) {
	return compilation_info_for_module(p, code_hdr);
    } else if (what == am_native_addresses) {
	return native_addresses(p, code_hdr);
    } else if (what == am_native) {
	return has_native(code_hdr);
    }
    return THE_NON_VALUE;
}

/*
 * Builds a list of all functions in the given module:
 *     [{Name, Arity},...]
 */

Eterm
functions_in_module(Process* p, /* Process whose heap to use. */
		    BeamCodeHeader* code_hdr)
{
    int i;
    Uint num_functions;
    Uint need;
    Eterm* hp;
    Eterm* hp_end;
    Eterm result = NIL;

    num_functions = code_hdr->num_functions;
    need = 5*num_functions;
    hp = HAlloc(p, need);
    hp_end = hp + need;
    for (i = num_functions-1; i >= 0 ; i--) {
	ErtsCodeInfo* ci = code_hdr->functions[i];
	Eterm tuple;

	/*
	 * If the function name is [], this entry is a stub for
	 * a BIF that should be ignored.
	 */
	ASSERT(is_atom(ci->mfa.function) || is_nil(ci->mfa.function));
	if (is_atom(ci->mfa.function)) {
	    tuple = TUPLE2(hp, ci->mfa.function, make_small(ci->mfa.arity));
	    hp += 3;
	    result = CONS(hp, tuple, result);
	    hp += 2;
	}
    }
    HRelease(p, hp_end, hp);
    return result;
}

/*
 * Returns 'true' if mod has any native compiled functions, otherwise 'false'
 */

static Eterm
has_native(BeamCodeHeader *code_hdr)
{
    Eterm result = am_false;
#ifdef HIPE
    if (erts_is_module_native(code_hdr)) {
        result = am_true;
    }
#endif
    return result;
}

void
erts_release_literal_area(ErtsLiteralArea* literal_area)
{
    struct erl_off_heap_header* oh;

    if (!literal_area)
	return;

    oh = literal_area->off_heap;
	
    while (oh) {
	Binary* bptr;
	ASSERT(thing_subtag(oh->thing_word) == REFC_BINARY_SUBTAG);
	bptr = ((ProcBin*)oh)->val;
        erts_bin_release(bptr);
	oh = oh->next;
    }
    erts_free(ERTS_ALC_T_LITERAL, literal_area);
}

int
erts_is_module_native(BeamCodeHeader* code_hdr)
{
    Uint i, num_functions;

    /* Check NativeAdress of first real function in module */
    if (code_hdr != NULL) {
        num_functions = code_hdr->num_functions;
        for (i=0; i<num_functions; i++) {
            BeamInstr* func_info = (BeamInstr *) code_hdr->functions[i];
            Eterm name = (Eterm) func_info[3];
            if (is_atom(name)) {
                return func_info[1] != 0;
            }
            else ASSERT(is_nil(name)); /* ignore BIF stubs */
        }
    }
    return 0;
}

/*
 * Builds a list of all functions including native addresses.
 *     [{Name,Arity,NativeAddress},...]
 */

static Eterm
native_addresses(Process* p, BeamCodeHeader* code_hdr)
{
    Eterm result = NIL;
#ifdef HIPE
    int i;
    Eterm* hp;
    Uint num_functions;
    Uint need;
    Eterm* hp_end;

    num_functions = code_hdr->num_functions;
    need = (6+BIG_UINT_HEAP_SIZE)*num_functions;
    hp = HAlloc(p, need);
    hp_end = hp + need;
    for (i = num_functions-1; i >= 0 ; i--) {
	ErtsCodeInfo *ci = code_hdr->functions[i];
	Eterm tuple;

	ASSERT(is_atom(ci->mfa.function)
               || is_nil(ci->mfa.function)); /* [] if BIF stub */
	if (ci->u.ncallee != NULL) {
            Eterm addr;
	    ASSERT(is_atom(ci->mfa.function));
	    addr = erts_bld_uint(&hp, NULL, (Uint)ci->u.ncallee);
	    tuple = erts_bld_tuple(&hp, NULL, 3, ci->mfa.function,
                                   make_small(ci->mfa.arity), addr);
	    result = erts_bld_cons(&hp, NULL, tuple, result);
	}
    }
    HRelease(p, hp_end, hp);
#endif
    return result;
}

/*
 * Builds a list of all exported functions in the given module:
 *     [{Name, Arity},...]
 */

Eterm
exported_from_module(Process* p, /* Process whose heap to use. */
                     ErtsCodeIndex code_ix,
		     Eterm mod) /* Tagged atom for module. */
{
    int i, num_exps;
    Eterm* hp = NULL;
    Eterm* hend = NULL;
    Eterm result = NIL;

    num_exps = export_list_size(code_ix);
    for (i = 0; i < num_exps; i++) {
	Export* ep = export_list(i,code_ix);
	
	if (ep->info.mfa.module == mod) {
	    Eterm tuple;
	    
	    if (ep->addressv[code_ix] == ep->beam &&
		ep->beam[0] == (BeamInstr) em_call_error_handler) {
		/* There is a call to the function, but it does not exist. */ 
		continue;
	    }

	    if (hp == hend) {
		int need = 10 * 5;
		hp = HAlloc(p, need);
		hend = hp + need;
	    }
	    tuple = TUPLE2(hp, ep->info.mfa.function,
                           make_small(ep->info.mfa.arity));
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
 */

Eterm
attributes_for_module(Process* p, /* Process whose heap to use. */
                      BeamCodeHeader* code_hdr)
{
    byte* ext;
    Eterm result = NIL;

    ext = code_hdr->attr_ptr;
    if (ext != NULL) {
	ErtsHeapFactory factory;
	erts_factory_proc_prealloc_init(&factory, p, code_hdr->attr_size_on_heap);
	result = erts_decode_ext(&factory, &ext, 0);
	if (is_value(result)) {
	    erts_factory_close(&factory);
	}
    }
    return result;
}

/*
 * Returns a list containing compilation information.
 */

Eterm
compilation_info_for_module(Process* p, /* Process whose heap to use. */
                            BeamCodeHeader* code_hdr)
{
    byte* ext;
    Eterm result = NIL;

    ext = code_hdr->compile_ptr;
    if (ext != NULL) {
	ErtsHeapFactory factory;
	erts_factory_proc_prealloc_init(&factory, p, code_hdr->compile_size_on_heap);
	result = erts_decode_ext(&factory, &ext, 0);
	if (is_value(result)) {
	    erts_factory_close(&factory);
	}
    }
    return result;
}

/*
 * Returns the MD5 checksum for a module
 */

Eterm
md5_of_module(Process* p, /* Process whose heap to use. */
              BeamCodeHeader* code_hdr)
{
    return new_binary(p, code_hdr->md5_ptr, MD5_SIZE);
}

/*
 * Build a single {M,F,A,Loction} item to be part of
 * a stack trace.
 */
Eterm*
erts_build_mfa_item(FunctionInfo* fi, Eterm* hp, Eterm args, Eterm* mfa_p)
{
    Eterm loc = NIL;

    if (fi->loc != LINE_INVALID_LOCATION) {
	Eterm tuple;
	int line = LOC_LINE(fi->loc);
	int file = LOC_FILE(fi->loc);
	Eterm file_term = NIL;

	if (file == 0) {
	    Atom* ap = atom_tab(atom_val(fi->mfa->module));
	    file_term = buf_to_intlist(&hp, ".erl", 4, NIL);
	    file_term = buf_to_intlist(&hp, (char*)ap->name, ap->len, file_term);
	} else {
	    Atom* ap = atom_tab(atom_val((fi->fname_ptr)[file-1]));
	    file_term = buf_to_intlist(&hp, (char*)ap->name, ap->len, NIL);
	}

	tuple = TUPLE2(hp, am_line, make_small(line));
	hp += 3;
	loc = CONS(hp, tuple, loc);
	hp += 2;
	tuple = TUPLE2(hp, am_file, file_term);
	hp += 3;
	loc = CONS(hp, tuple, loc);
	hp += 2;
    }

    if (is_list(args) || is_nil(args)) {
	*mfa_p = TUPLE4(hp, fi->mfa->module, fi->mfa->function,
                        args, loc);
    } else {
	Eterm arity = make_small(fi->mfa->arity);
	*mfa_p = TUPLE4(hp, fi->mfa->module, fi->mfa->function,
                        arity, loc);
    }
    return hp + 5;
}

/*
 * Force setting of the current function in a FunctionInfo
 * structure. No source code location will be associated with
 * the function.
 */
void
erts_set_current_function(FunctionInfo* fi, ErtsCodeMFA* mfa)
{
    fi->mfa = mfa;
    fi->needed = 5;
    fi->loc = LINE_INVALID_LOCATION;
}


/*
 * Returns a pointer to {module, function, arity}, or NULL if not found.
 */
ErtsCodeMFA*
find_function_from_pc(BeamInstr* pc)
{
    FunctionInfo fi;

    erts_lookup_function_info(&fi, pc, 0);
    return fi.mfa;
}

/*
 * Read a specific chunk from a Beam binary.
 */

BIF_RETTYPE
code_get_chunk_2(BIF_ALIST_2)
{
    Process* p = BIF_P;
    Eterm Bin = BIF_ARG_1;
    Eterm Chunk = BIF_ARG_2;
    Binary* magic = 0;
    LoaderState* stp;
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

    magic = erts_alloc_loader_state();
    stp = ERTS_MAGIC_BIN_DATA(magic);
    if ((start = erts_get_aligned_binary_bytes(Bin, &temp_alloc)) == NULL) {
    error:
	erts_free_aligned_binary_bytes(temp_alloc);
	if (magic) {
	    free_loader_state(magic);
	}
	BIF_ERROR(p, BADARG);
    }
    stp->module = THE_NON_VALUE; /* Suppress diagnostics */
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
    if (!init_iff_file(stp, start, binary_size(Bin)) ||
	!scan_iff_file(stp, &chunk, 1) ||
	stp->chunks[0].start == NULL) {
	res = am_undefined;
	goto done;
    }
    ERTS_GET_REAL_BIN(Bin, real_bin, offset, bitoffs, bitsize);
    if (bitoffs) {
	res = new_binary(p, stp->chunks[0].start, stp->chunks[0].size);
    } else {
	sb = (ErlSubBin *) HAlloc(p, ERL_SUB_BIN_SIZE);
	sb->thing_word = HEADER_SUB_BIN;
	sb->orig = real_bin;
	sb->size = stp->chunks[0].size;
	sb->bitsize = 0;
	sb->bitoffs = 0;
	sb->offs = offset + (stp->chunks[0].start - start);
	sb->is_writable = 0;
	res = make_binary(sb);
    }

 done:
    erts_free_aligned_binary_bytes(temp_alloc);
    free_loader_state(magic);
    return res;
}

/*
 * Calculate the MD5 for a module.
 */
  
BIF_RETTYPE
code_module_md5_1(BIF_ALIST_1)
{
    Process* p = BIF_P;
    Eterm Bin = BIF_ARG_1;
    Binary* magic;
    LoaderState* stp;
    byte* bytes;
    byte* temp_alloc = NULL;
    Eterm res;

    magic = erts_alloc_loader_state();
    stp = ERTS_MAGIC_BIN_DATA(magic);
    if ((bytes = erts_get_aligned_binary_bytes(Bin, &temp_alloc)) == NULL) {
	free_loader_state(magic);
	BIF_ERROR(p, BADARG);
    }
    stp->module = THE_NON_VALUE; /* Suppress diagnostiscs */
    if (!init_iff_file(stp, bytes, binary_size(Bin)) ||
	!scan_iff_file(stp, chunk_types, NUM_CHUNK_TYPES) ||
	!verify_chunks(stp)) {
	res = am_undefined;
	goto done;
    }
    res = new_binary(p, stp->mod_md5, MD5_SIZE);

 done:
    erts_free_aligned_binary_bytes(temp_alloc);
    free_loader_state(magic);
    return res;
}

#ifdef HIPE
#define WORDS_PER_FUNCTION (sizeof(ErtsCodeInfo) / sizeof(UWord) + 1)

static BeamInstr*
make_stub(ErtsCodeInfo* info, Eterm mod, Eterm func, Uint arity, Uint native, BeamInstr OpCode)
{
    DBG_TRACE_MFA(mod,func,arity,"make beam stub at %p", erts_codeinfo_to_code(info));
    ASSERT(WORDS_PER_FUNCTION == 6);
    info->op = (BeamInstr) BeamOp(op_i_func_info_IaaI);
    info->u.ncallee = (void (*)(void)) native;
    info->mfa.module = mod;
    info->mfa.function = func;
    info->mfa.arity = arity;
    erts_codeinfo_to_code(info)[0] = OpCode;
    return erts_codeinfo_to_code(info)+1;
}

static byte*
stub_copy_info(LoaderState* stp,
	       int chunk,	/* Chunk: ATTR_CHUNK or COMPILE_CHUNK */
	       byte* info,	/* Where to store info. */
	       byte** ptr_word,	/* Where to store pointer into info. */
	       BeamInstr* size_word,	/* Where to store size into info. */
	       BeamInstr* size_on_heap_word) /* Where to store size on heap. */
{
    Sint decoded_size;
    Uint size = stp->chunks[chunk].size;
    if (size != 0) {
	memcpy(info, stp->chunks[chunk].start, size);
	*ptr_word = info;
	decoded_size = erts_decode_ext_size(info, size);
	if (decoded_size < 0) {
 	    return 0;
 	}
	*size_word = (BeamInstr) size;
	*size_on_heap_word = decoded_size;
    }
    return info + size;
}

static int
stub_read_export_table(LoaderState* stp)
{
    unsigned int i;

    GetInt(stp, 4, stp->num_exps);
    if (stp->num_exps > stp->num_functions) {
	LoadError2(stp, "%u functions exported; only %u functions defined",
		   stp->num_exps, stp->num_functions);
    }
    stp->export
	= (ExportEntry *) erts_alloc(ERTS_ALC_T_PREPARED_CODE,
				     stp->num_exps * sizeof(ExportEntry));

    for (i = 0; i < stp->num_exps; i++) {
	Uint n;

	GetInt(stp, 4, n);
	GetAtom(stp, n, stp->export[i].function);
	GetInt(stp, 4, n);
	if (n > MAX_REG) {
	    LoadError2(stp, "export table entry %u: absurdly high arity %u", i, n);
	}
	stp->export[i].arity = n;
	GetInt(stp, 4, n);	/* Ignore label */
    }
    return 1;

 load_error:
    return 0;
}

static void
stub_final_touch(LoaderState* stp, ErtsCodeInfo* ci)
{
    unsigned int i;
    unsigned int n = stp->num_exps;
    Lambda* lp;

    if (is_bif(ci->mfa.module, ci->mfa.function, ci->mfa.arity)) {
	ci->u.ncallee = NULL;
	ci->mfa.module = 0;
	ci->mfa.function = 0;
	ci->mfa.arity = 0;
	return;
    }

    /*
     * Test if the function should be exported.
     */

    for (i = 0; i < n; i++) {
	if (stp->export[i].function == ci->mfa.function &&
            stp->export[i].arity == ci->mfa.arity) {
	    Export* ep = erts_export_put(ci->mfa.module,
                                         ci->mfa.function,
                                         ci->mfa.arity);
	    ep->addressv[erts_staging_code_ix()] = erts_codeinfo_to_code(ci);
	    DBG_TRACE_MFA_P(&ci->mfa,"set beam stub at %p in export at %p (code_ix=%d)",
			    erts_codeinfo_to_code(ci), ep, erts_staging_code_ix());
	    return;
	}
    }

    /*
     * Must be a plain local function or a lambda local function.
     * Search the lambda table to find out which.
     */
    
    n = stp->num_lambdas;
    for (i = 0, lp = stp->lambdas; i < n; i++, lp++) {
        ErlFunEntry* fe = stp->lambdas[i].fe;
	if (lp->function == ci->mfa.function && lp->arity == ci->mfa.arity) {
	    *erts_codeinfo_to_code(ci) = (Eterm) BeamOpCode(op_hipe_trap_call_closure);
            fe->address = erts_codeinfo_to_code(ci);
	}
    }
    return;
}


/* Takes an erlang list of addresses:
   [{Adr, Patchtyppe} | Addresses]
   and the address of a fun_entry.
*/
static int
patch(Eterm Addresses, Uint fe) 
 {
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

  return 1;
}


static int
patch_funentries(Eterm Patchlist) 
 {
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

    erts_smp_refc_dec(&fe->refc, 1);

    if (!patch(Addresses, (Uint) fe))
      return 0;

  }
  return 1; /* Signal that all went well */
}

/*
 * Do a dummy load of a module. No threaded code will be loaded.
 * Used for loading native code.
 * Will also patch all references to fun_entries to point to 
 * the new fun_entries created.
 */
Eterm
erts_make_stub_module(Process* p, Eterm hipe_magic_bin, Eterm Beam, Eterm Info)
{
    Binary* magic;
    Binary* hipe_magic;
    LoaderState* stp;
    HipeLoaderState* hipe_stp;
    HipeModule *hipe_code;
    BeamInstr Funcs;
    BeamInstr Patchlist;
    Eterm MD5Bin;
    Eterm* tp;
    BeamCodeHeader* code_hdr;
    BeamInstr* code_base;
    BeamInstr* fp;
    byte* info;
    Sint n;
    int code_size;
    int rval;
    Sint i;
    byte* temp_alloc = NULL;
    byte* bytes;
    Uint size;

    /*
     * Must initialize stp->lambdas here because the error handling code
     * at label 'error' uses it.
     */
    magic = erts_alloc_loader_state();
    stp = ERTS_MAGIC_BIN_DATA(magic);
    hipe_code = erts_alloc(ERTS_ALC_T_HIPE_LL, sizeof(*hipe_code));

    if (!is_internal_magic_ref(hipe_magic_bin) ||
	!(hipe_magic = erts_magic_ref2bin(hipe_magic_bin),
	  hipe_stp = hipe_get_loader_state(hipe_magic)) ||
	hipe_stp->module == NIL || hipe_stp->text_segment == 0) {
	goto error;
    }
    if (is_not_tuple(Info)) {
	goto error;
    }
    tp = tuple_val(Info);
    if (tp[0] != make_arityval(3)) {
      goto error;
    }
    Funcs = tp[1];
    Patchlist = tp[2];
    MD5Bin = tp[3];
    if (is_not_binary(MD5Bin) || (binary_size(MD5Bin) != MD5_SIZE)) {
	goto error;
    }
    if ((n = erts_list_length(Funcs)) < 0) {
	goto error;
    }
    if ((bytes = erts_get_aligned_binary_bytes(Beam, &temp_alloc)) == NULL) {
	goto error;
    }
    size = binary_size(Beam);

    /*
     * Scan the Beam binary and read the interesting sections.
     */

    stp->module = hipe_stp->module;
    stp->group_leader = p->group_leader;
    stp->num_functions = n;
    if (!init_iff_file(stp, bytes, size)) {
	goto error;
    }
    if (!scan_iff_file(stp, chunk_types, NUM_CHUNK_TYPES) ||
	!verify_chunks(stp)) {
	goto error;
    }
    define_file(stp, "code chunk header", CODE_CHUNK);
    if (!read_code_header(stp)) {
	goto error;
    }
    if (stp->chunks[UTF8_ATOM_CHUNK].size > 0) {
        define_file(stp, "utf8 atom table", UTF8_ATOM_CHUNK);
        if (!load_atom_table(stp, ERTS_ATOM_ENC_UTF8)) {
            goto error;
        }
    } else {
        define_file(stp, "atom table", ATOM_CHUNK);
        if (!load_atom_table(stp, ERTS_ATOM_ENC_LATIN1)) {
            goto error;
        }
    }
    define_file(stp, "export table", EXP_CHUNK);
    if (!stub_read_export_table(stp)) {
	goto error;
    }
    
    if (stp->chunks[LAMBDA_CHUNK].size > 0) {
	define_file(stp, "lambda (fun) table", LAMBDA_CHUNK);
	if (!read_lambda_table(stp)) {
	    goto error;
	}
    }

    /*
     * Allocate memory for the stub module.
     */

    code_size = (offsetof(BeamCodeHeader,functions)
                 + ((n+1) * sizeof(BeamInstr*))
                 + (WORDS_PER_FUNCTION*n + 1) * sizeof(BeamInstr)
                 + stp->chunks[ATTR_CHUNK].size
                 + stp->chunks[COMPILE_CHUNK].size
                 + MD5_SIZE);
    code_hdr = erts_alloc_fnf(ERTS_ALC_T_CODE, code_size);
    if (!code_hdr) {
	goto error;
    }

    /*
     * Initialize code header.
     */

    code_hdr->num_functions = n;
    code_hdr->attr_ptr = NULL;
    code_hdr->attr_size = 0;
    code_hdr->attr_size_on_heap = 0;
    code_hdr->compile_ptr = NULL;
    code_hdr->compile_size = 0;
    code_hdr->compile_size_on_heap = 0;
    code_hdr->literal_area = NULL;
    code_hdr->on_load_function_ptr = NULL;
    code_hdr->line_table = NULL;
    code_hdr->md5_ptr = NULL;

    /*
     * Make stubs for all functions.
     */

    fp = code_base = (BeamInstr*) &code_hdr->functions[n+1];
    for (i = 0; i < n; i++) {
	Eterm* listp;
	Eterm tuple;
	Eterm* tp;
	Eterm func;
	Eterm arity_term;
	Sint arity;
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
	code_hdr->functions[i] = (ErtsCodeInfo*)fp;
	op = (Eterm) BeamOpCode(op_hipe_trap_call); /* Might be changed later. */
	fp = make_stub((ErtsCodeInfo*)fp, hipe_stp->module, func, arity,
                       (Uint)native_address, op);
    }

    /*
     * Insert the last pointer and the int_code_end instruction.
     */

    code_hdr->functions[i] = (ErtsCodeInfo*)fp;
    *fp++ = (BeamInstr) BeamOp(op_int_code_end);

    /*
     * Copy attributes and compilation information.
     */

    info = (byte *) fp;
    info = stub_copy_info(stp, ATTR_CHUNK, info,
			  &code_hdr->attr_ptr,
			  &code_hdr->attr_size,
			  &code_hdr->attr_size_on_heap);
    if (info == NULL) {
	goto error;
    }
    info = stub_copy_info(stp, COMPILE_CHUNK, info,
			  &code_hdr->compile_ptr,
			  &code_hdr->compile_size,
			  &code_hdr->compile_size_on_heap);
    if (info == NULL) {
	goto error;
    }
    {
      byte *tmp = NULL;
      byte *md5 = NULL;
      if ((md5 = erts_get_aligned_binary_bytes(MD5Bin, &tmp)) != NULL) {
        sys_memcpy(info, md5, MD5_SIZE);
        code_hdr->md5_ptr = info;
      }
      erts_free_aligned_binary_bytes(tmp);
    }

    /*
     * Initialise HiPE module
     */
    hipe_code->text_segment = hipe_stp->text_segment;
    hipe_code->text_segment_size = hipe_stp->text_segment_size;
    hipe_code->data_segment = hipe_stp->data_segment;
    hipe_code->first_hipe_ref = hipe_stp->new_hipe_refs;
    hipe_code->first_hipe_sdesc = hipe_stp->new_hipe_sdesc;

    /*
     * Insert the module in the module table.
     */

    rval = stub_insert_new_code(p, 0, p->group_leader, hipe_stp->module,
				code_hdr, code_size, hipe_code);
    if (rval != NIL) {
	goto error;
    }

    /*
     * Export all stub functions and insert the correct type of HiPE trap.
     */

    fp = code_base;
    for (i = 0; i < n; i++) {
	stub_final_touch(stp, (ErtsCodeInfo*)fp);
	fp += WORDS_PER_FUNCTION;
    }

    if (patch_funentries(Patchlist)) {
	Eterm mod = hipe_stp->module;
	/* Prevent code from being freed */
	hipe_stp->text_segment = 0;
	hipe_stp->data_segment = 0;
        hipe_stp->new_hipe_refs = NULL;
        hipe_stp->new_hipe_sdesc = NULL;

	erts_free_aligned_binary_bytes(temp_alloc);
	free_loader_state(magic);
	hipe_free_loader_state(hipe_stp);

	return mod;
    }

 error:
    erts_free(ERTS_ALC_T_HIPE_LL, hipe_code);
    erts_free_aligned_binary_bytes(temp_alloc);
    free_loader_state(magic);
    BIF_ERROR(p, BADARG);
}

int erts_commit_hipe_patch_load(Eterm hipe_magic_bin)
{
    Binary* hipe_magic;
    HipeLoaderState* hipe_stp;
    HipeModule *hipe_code;
    Module* modp;

    if (!is_internal_magic_ref(hipe_magic_bin) ||
	!(hipe_magic = erts_magic_ref2bin(hipe_magic_bin),
	  hipe_stp = hipe_get_loader_state(hipe_magic)) ||
	hipe_stp->module == NIL || hipe_stp->text_segment == 0) {
	return 0;
    }

    modp = erts_get_module(hipe_stp->module, erts_active_code_ix());
    if (!modp)
	return 0;

    /*
     * Initialise HiPE module
     */
    hipe_code = erts_alloc(ERTS_ALC_T_HIPE_LL, sizeof(*hipe_code));
    hipe_code->text_segment = hipe_stp->text_segment;
    hipe_code->text_segment_size = hipe_stp->text_segment_size;
    hipe_code->data_segment = hipe_stp->data_segment;
    hipe_code->first_hipe_ref = hipe_stp->new_hipe_refs;
    hipe_code->first_hipe_sdesc = hipe_stp->new_hipe_sdesc;

    modp->curr.hipe_code = hipe_code;

    /* Prevent code from being freed */
    hipe_stp->text_segment = 0;
    hipe_stp->data_segment = 0;
    hipe_stp->new_hipe_refs = NULL;
    hipe_stp->new_hipe_sdesc = NULL;

    return 1;
}

#undef WORDS_PER_FUNCTION
#endif /* HIPE */


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

#ifdef ENABLE_DBG_TRACE_MFA

#define MFA_MAX 10
Eterm dbg_trace_m[MFA_MAX];
Eterm dbg_trace_f[MFA_MAX];
Uint  dbg_trace_a[MFA_MAX];
unsigned int dbg_trace_ix = 0;

void dbg_set_traced_mfa(const char* m, const char* f, Uint a)
{
    unsigned i = dbg_trace_ix++;
    ASSERT(i < MFA_MAX);
    dbg_trace_m[i] = am_atom_put(m, strlen(m));
    dbg_trace_f[i] = am_atom_put(f, strlen(f));
    dbg_trace_a[i] = a;
}

int dbg_is_traced_mfa(Eterm m, Eterm f, Uint a)
{
    unsigned int i;
    for (i = 0; i < dbg_trace_ix; ++i) {
        if (m == dbg_trace_m[i] &&
            (!f || (f == dbg_trace_f[i] && a == dbg_trace_a[i]))) {

            return i+1;
        }
    }
    return 0;
}

void dbg_vtrace_mfa(unsigned ix, const char* format, ...)
{
    va_list arglist;
    va_start(arglist, format);
    ASSERT(--ix < MFA_MAX);
    erts_fprintf(stderr, "MFA TRACE %T:%T/%u: ",
                 dbg_trace_m[ix], dbg_trace_f[ix], (int)dbg_trace_a[ix]);

    erts_vfprintf(stderr, format, arglist);
    va_end(arglist);
}

#endif /* ENABLE_DBG_TRACE_MFA */
