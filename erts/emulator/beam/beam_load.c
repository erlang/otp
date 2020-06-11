/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2020. All Rights Reserved.
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
#include "erl_unicode.h"

#ifdef HIPE
#include "hipe_bif0.h"
#include "hipe_mode_switch.h"
#include "hipe_arch.h"
#include "hipe_load.h"
#endif

ErlDrvBinary* erts_gzinflate_buffer(char*, int);

#define CALLED    0
#define DEFINED   1
#define EXPORTED  2

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

/*
 * An array with all chunk types recognized by the loader.
 */

static Uint chunk_types[ERTS_BEAM_NUM_CHUNK_TYPES] = {
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

#define FREE_GENOP(Stp, Genop)                 \
 do { \
   if ((Genop)->a != (Genop)->def_args) { \
       erts_free(ERTS_ALC_T_LOADER_TMP, (Genop)->a); \
   } \
   (Genop)->next = (Stp)->free_genop; \
   (Stp)->free_genop = (Genop); \
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
static void init_label(Label* lp);
static int load_code(LoaderState* stp);

static int freeze_code(LoaderState* stp);

static void final_touch(LoaderState* stp, struct erl_module_instance* inst_p);
static void short_file(int line, LoaderState* stp, unsigned needed);
static void load_printf(int line, LoaderState* context, char *fmt, ...);
static void id_to_string(Uint id, char* s);
static int get_tag_and_value(LoaderState* stp, Uint len_code,
			     unsigned tag, BeamInstr* result);
static void new_literal_patch(LoaderState* stp, int pos);
static void new_string_patch(LoaderState* stp, int pos);
static Eterm get_module_info(Process* p, ErtsCodeIndex code_ix,
                             BeamCodeHeader*, Eterm module, Eterm what);
static Eterm exported_from_module(Process* p, ErtsCodeIndex code_ix,
                                  Eterm mod);
static Eterm functions_in_module(Process* p, BeamCodeHeader*);
static Eterm nifs_in_module(Process* p, Eterm module);
static Eterm attributes_for_module(Process* p, BeamCodeHeader*);
static Eterm compilation_info_for_module(Process* p, BeamCodeHeader*);
static Eterm md5_of_module(Process* p, BeamCodeHeader*);
static Eterm has_native(BeamCodeHeader*);
static Eterm native_addresses(Process* p, BeamCodeHeader*);

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
	!scan_iff_file(stp, chunk_types, ERTS_BEAM_NUM_CHUNK_TYPES) ||
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
     * Find out whether the code was compiled with OTP 20
     * or higher.
     */

    stp->otp_20_or_higher = stp->chunks[UTF8_ATOM_CHUNK].size > 0;

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

    ERTS_LC_ASSERT(erts_initialized == 0 || erts_has_code_write_permission() ||
		       erts_thr_progress_is_blocking());
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

            DBG_CHECK_EXPORT(ep, code_ix);

	    if (ep->addressv[code_ix] == ep->trampoline.raw) {
		if (BeamIsOpCode(ep->trampoline.op, op_i_generic_breakpoint)) {
		    ERTS_LC_ASSERT(erts_thr_progress_is_blocking());
		    ASSERT(mod_tab_p->curr.num_traced_exports > 0);

                    erts_clear_export_break(mod_tab_p, ep);

                    ep->addressv[code_ix] =
                        (BeamInstr*)ep->trampoline.breakpoint.address;
                    ep->trampoline.breakpoint.address = 0;

                    ASSERT(ep->addressv[code_ix] != ep->trampoline.raw);
		}
		ASSERT(ep->trampoline.breakpoint.address == 0);
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
        Uint num;
        for (num = 0; num < stp->num_labels; num++) {
            erts_free(ERTS_ALC_T_PREPARED_CODE, (void *) stp->labels[num].patches);
        }
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
	sys_memcpy(sbuf, ap->name, ap->len);
	sbuf[ap->len] = '\0';
	LoadError1(stp, "BEAM file exists but it defines a module named %s", sbuf);
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
	stp->import[i].bif = NULL;

	/*
	 * If the export entry refers to a BIF, save a pointer to the BIF entry.
	 */
	if ((e = erts_active_export_entry(mod, func, arity)) != NULL) {
	    if (e->bif_number != -1) {
		stp->import[i].bif = e;
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
    }
    return 1;

 load_error:
    return 0;
}

static int
is_bif(Eterm mod, Eterm func, unsigned arity)
{
    Export *e = erts_active_export_entry(mod, func, arity);

    if (e != NULL) {
        return e->bif_number != -1;
    }

    return 0;
}

static int
read_lambda_table(LoaderState* stp)
{
    unsigned int i;
    unsigned int otp_22_or_lower;

    /*
     * Determine whether this module was compiled with OTP 22 or lower
     * by looking at the max opcode number. The compiler in OTP 23 will
     * always set the max opcode to the opcode for `swap` (whether
     * actually used or not) so that a module compiled for OTP 23
     * cannot be loaded in earlier versions.
     */

    otp_22_or_lower = stp->max_opcode < genop_swap_2;

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

        /*
         * Fun entries are now keyed by the explicit ("new") index in
         * the fun entry. That allows multiple make_fun2 instructions
         * to share the same fun entry (when the `fun F/A` syntax is
         * used). Before OTP 23, fun entries were keyed by the old
         * index, which is the order of the entries in the fun
         * chunk. Each make_fun2 needed to refer to its own fun entry.
         *
         * Modules compiled before OTP 23 can safely be loaded if the
         * old index and the new index are equal. That is true for all
         * modules compiled with OTP R15 and later.
         */
        if (otp_22_or_lower && i != Index) {
            /*
             * Compiled with a compiler before OTP R15B. The new indices
             * are not reliable, so it is not safe to load this module.
             */
            LoadError2(stp, "please re-compile this module with an "
                       ERLANG_OTP_RELEASE " compiler "
                       "(old-style fun with indices: %d/%d)",
                       i, Index);
        }
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
	    stp->fname[i] = erts_atom_put(fname, n, ERTS_ATOM_ENC_UTF8, 1);
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
    GetInt(stp, 4, stp->max_opcode);
    if (stp->max_opcode > MAX_GENERIC_OPCODE) {
	LoadError2(stp,
		   "This BEAM file was compiled for a later version"
		   " of the run-time system than " ERLANG_OTP_RELEASE ".\n"
		   "  To fix this, please recompile this module with an "
		   ERLANG_OTP_RELEASE " compiler.\n"
		   "  (Use of opcode %d; this emulator supports "
		   "only up to %d.)",
		   stp->max_opcode, MAX_GENERIC_OPCODE);
    }

    GetInt(stp, 4, stp->num_labels);
    GetInt(stp, 4, stp->num_functions);

    /*
     * Initialize label table.
     */

    stp->labels = (Label *) erts_alloc(ERTS_ALC_T_PREPARED_CODE,
				       stp->num_labels * sizeof(Label));
    for (i = 0; i < stp->num_labels; i++) {
        init_label(&stp->labels[i]);
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

static void init_label(Label* lp)
{
    lp->value = 0;
    lp->looprec_targeted = 0;
    lp->num_patches = 0;
    lp->num_allocated = 4;
    lp->patches = erts_alloc(ERTS_ALC_T_PREPARED_CODE,
                             lp->num_allocated * sizeof(LabelPatch));
}

static void
register_label_patch(LoaderState* stp, Uint label, Uint ci, Uint offset)
{
    Label* lp;

    ASSERT(label < stp->num_labels);
    lp = &stp->labels[label];
    if (lp->num_allocated <= lp->num_patches) {
        lp->num_allocated *= 2;
        lp->patches = erts_realloc(ERTS_ALC_T_PREPARED_CODE,
                                   (void *) lp->patches,
                                   lp->num_allocated * sizeof(LabelPatch));
    }
    lp->patches[lp->num_patches].pos = ci;
    lp->patches[lp->num_patches].offset = offset;
    lp->patches[lp->num_patches].packed = 0;
    lp->num_patches++;
    stp->codev[ci] = label;
}

static int
load_code(LoaderState* stp)
{
    int i;
    Uint ci;
    Uint last_instr_start;       /* Needed for relative jumps */
    Uint last_func_start = 0;	/* Needed by nif loading and line instructions */
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
#if defined(BEAM_WIDE_SHIFT)
    int num_trailing_f;     /* Number of extra 'f' arguments in a list */
#endif

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
	ASSERT(arity <= ERTS_BEAM_MAX_OPARGS);

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
                         * (The literal pool is used instead.)
			 */
                        LoadError0(stp, "please re-compile this module with an "
                                   ERLANG_OTP_RELEASE " compiler");
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
			sys_memcpy(last_op->a, last_op->def_args,
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
	    switch (erts_transform_engine(stp)) {
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
            last_instr_start = ci + opc[stp->specific_op].adjust;
	    code[ci++] = BeamOpCodeAddr(stp->specific_op);
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
                    {
                        BeamInstr val = tmp_op->a[arg].val;
                        Eterm term = stp->literals[val].term;
                        new_literal_patch(stp, ci);
                        code[ci++] = val;
                        switch (loader_tag(term)) {
                        case LOADER_X_REG:
                        case LOADER_Y_REG:
                            LoadError1(stp, "the term '%T' would be confused "
                                       "with a register", term);
                        }
                    }
		    break;
		default:
		    LoadError1(stp, "bad tag %d for general source",
			       tmp_op->a[arg].type);
		    break;
		}
		break;
	    case 'd':	/* Destination (x(N), y(N) */
            case 'S':   /* Source (x(N), y(N)) */
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
	    case 't':	/* Small untagged integer (16 bits) -- can be packed. */
	    case 'I':	/* Untagged integer (32 bits) -- can be packed.  */
	    case 'W':	/* Untagged integer or pointer (machine word). */
#ifdef DEBUG
                switch (*sign) {
                case 't':
                    if (tmp_op->a[arg].val >> 16 != 0) {
                        load_printf(__LINE__, stp, "value %lu of type 't' does not fit in 16 bits",
                                    tmp_op->a[arg].val);
                        ASSERT(0);
                    }
                    break;
#ifdef ARCH_64
                case 'I':
                    if (tmp_op->a[arg].val >> 32 != 0) {
                        load_printf(__LINE__, stp, "value %lu of type 'I' does not fit in 32 bits",
                                    tmp_op->a[arg].val);
                        ASSERT(0);
                    }
                    break;
#endif
                }
#endif
		VerifyTag(stp, tag, TAG_u);
		code[ci++] = tmp_op->a[arg].val;
		break;
	    case 'A':	/* Arity value. */
		VerifyTag(stp, tag, TAG_u);
		code[ci++] = make_arityval(tmp_op->a[arg].val);
		break;
	    case 'f':		/* Destination label */
		VerifyTag(stp, tag_to_letter[tag], *sign);
                register_label_patch(stp, tmp_op->a[arg].val, ci, -last_instr_start);
		ci++;
		break;
	    case 'j':		/* 'f' or 'p' */
		if (tag == TAG_p) {
		    code[ci] = 0;
		} else if (tag == TAG_f) {
                    register_label_patch(stp, tmp_op->a[arg].val, ci, -last_instr_start);
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
		if (stp->import[i].bif == NULL) {
		    LoadError1(stp, "not a BIF: import table index %d", i);
		}
		{
		    int bif_index = stp->import[i].bif->bif_number;
		    BifEntry *bif_entry = &bif_table[bif_index];
		    code[ci++] = (BeamInstr) bif_entry->f;
		}
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
	    char* prog;            /* Program for packing engine. */
	    struct pack_stack {
                BeamInstr instr;
                Uint* patch_pos;
            } stack[8];            /* Stack. */
	    struct pack_stack* sp = stack; /* Points to next free position. */
	    BeamInstr packed = 0; /* Accumulator for packed operations. */
            LabelPatch* packed_label = 0;

	    for (prog = opc[stp->specific_op].pack; *prog; prog++) {
		switch (*prog) {
		case 'g':	/* Get operand and push on stack. */
                    ci--;
                    sp->instr = code[ci];
                    sp->patch_pos = 0;
                    sp++;
                    break;
		case 'f':	/* Get possible 'f' operand and push on stack. */
                    {
                        Uint w = code[--ci];
                        sp->instr = w;
                        sp->patch_pos = 0;

                        if (w != 0) {
                            LabelPatch* lbl_p;
                            int num_patches;
                            int patch;

                            ASSERT(w < stp->num_labels);
                            lbl_p = stp->labels[w].patches;
                            num_patches = stp->labels[w].num_patches;
                            for (patch = num_patches - 1; patch >= 0; patch--) {
                                if (lbl_p[patch].pos == ci) {
                                    sp->patch_pos = &lbl_p[patch].pos;
                                    break;
                                }
                            }
                            ASSERT(sp->patch_pos);
                        }
                        sp++;
                    }
                    break;
		case 'q':	/* Get possible 'q' operand and push on stack. */
                    {
                        LiteralPatch* lp;

                        ci--;
                        sp->instr = code[ci];
                        sp->patch_pos = 0;

                        for (lp = stp->literal_patches;
                             lp && lp->pos > ci - ERTS_BEAM_MAX_OPARGS;
                             lp = lp->next) {
                            if (lp->pos == ci) {
                                sp->patch_pos = &lp->pos;
                                break;
                            }
                        }
                        sp++;
                    }
                    break;
#ifdef ARCH_64
		case '1':	/* Tightest shift (always 10 bits) */
                    ci--;
                    ASSERT((code[ci] & ~0x1FF8ull) == 0); /* Fits in 10 bits */
                    packed = (packed << BEAM_TIGHTEST_SHIFT);
                    packed |= code[ci] >> 3;
                    if (packed_label) {
                        packed_label->packed++;
                    }
		    break;
#endif
		case '2':	/* Tight shift (10 or 16 bits) */
		    packed = (packed << BEAM_TIGHT_SHIFT) | code[--ci];
                    if (packed_label) {
                        packed_label->packed++;
                    }
		    break;
		case '3':	/* Loose shift (16 bits) */
		    packed = (packed << BEAM_LOOSE_SHIFT) | code[--ci];
                    if (packed_label) {
                        packed_label->packed++;
                    }
		    break;
#ifdef ARCH_64
		case '4':	/* Wide shift (32 bits) */
                    {
                        Uint w = code[--ci];

                        if (packed_label) {
                            packed_label->packed++;
                        }

                        /*
                         * 'w' can handle both labels ('f' and 'j'), as well
                         * as 'I'. Test whether this is a label.
                         */

                        if (w < stp->num_labels) {
                            /*
                             * Probably a label. Look for patch pointing to this
                             * position.
                             */
                            LabelPatch* lp = stp->labels[w].patches;
                            int num_patches = stp->labels[w].num_patches;
                            int patch;
                            for (patch = num_patches - 1; patch >= 0; patch--) {
                                if (lp[patch].pos == ci) {
                                    lp[patch].packed = 1;
                                    packed_label = &lp[patch];
                                    break;
                                }
                            }
                        }
                        packed = (packed << BEAM_WIDE_SHIFT) |
                            (code[ci] & BEAM_WIDE_MASK);
                    }
                    break;
#endif
		case 'p':	/* Put instruction (from stack). */
                    --sp;
                    code[ci] = sp->instr;
                    if (sp->patch_pos) {
                        *sp->patch_pos = ci;
                    }
                    ci++;
		    break;
		case 'P':	/* Put packed operands (on the stack). */
                    sp->instr = packed;
                    sp->patch_pos = 0;
                    if (packed_label) {
                        sp->patch_pos = &packed_label->pos;
                        packed_label = 0;
                    }
                    sp++;
		    packed = 0;
		    break;
#if defined(ARCH_64) && defined(CODE_MODEL_SMALL)
                case '#':       /* -1 */
                case '$':       /* -2 */
                case '%':       /* -3 */
                case '&':       /* -4 */
                case '\'':      /* -5 */
                case '(':       /* -6 */
                    /* Pack accumulator contents into instruction word. */
                    {
                        Sint pos = ci - (*prog - '#' + 1);
                        /* Are the high 32 bits of the instruction word zero? */
                        ASSERT((code[pos] & ~((1ull << BEAM_WIDE_SHIFT)-1)) == 0);
                        code[pos] |= packed << BEAM_WIDE_SHIFT;
                        if (packed_label) {
                            ASSERT(packed_label->packed == 1);
                            packed_label->pos = pos;
                            packed_label->packed = 2;
                            packed_label = 0;
                        }
                        packed >>= BEAM_WIDE_SHIFT;
                    }
		    break;
#endif
		default:
                    erts_exit(ERTS_ERROR_EXIT, "beam_load: invalid packing op: %c\n", *prog);
		}
	    }
	    ASSERT(sp == stack); /* Incorrect program? */
	}

	/*
	 * Load any list arguments using the primitive tags.
	 */

#if defined(BEAM_WIDE_SHIFT)
        num_trailing_f = 0;
#endif
	for ( ; arg < tmp_op->arity; arg++) {
#if defined(BEAM_WIDE_SHIFT)
	    if (tmp_op->a[arg].type == TAG_f) {
                num_trailing_f++;
            } else {
                num_trailing_f = 0;
            }
#endif
            CodeNeed(1);
	    switch (tmp_op->a[arg].type) {
	    case TAG_i:
		code[ci++] = make_small(tmp_op->a[arg].val);
		break;
	    case TAG_u:
	    case TAG_a:
	    case TAG_v:
		code[ci++] = tmp_op->a[arg].val;
		break;
	    case TAG_f:
                register_label_patch(stp, tmp_op->a[arg].val, ci, -last_instr_start);
		ci++;
		break;
	    case TAG_x:
		code[ci++] = make_loader_x_reg(tmp_op->a[arg].val);
		break;
	    case TAG_y:
		code[ci++] = make_loader_y_reg(tmp_op->a[arg].val);
		break;
	    case TAG_n:
		code[ci++] = NIL;
		break;
	    case TAG_q:
		new_literal_patch(stp, ci);
		code[ci++] = tmp_op->a[arg].val;
		break;
	    default:
		LoadError1(stp, "unsupported primitive type '%c'",
			   tag_to_letter[tmp_op->a[arg].type]);
	    }
	}

        /*
         * If all the extra arguments were 'f' operands,
         * and the wordsize is 64 bits, pack two 'f' operands
         * into each word.
         */

#if defined(BEAM_WIDE_SHIFT)
        if (num_trailing_f >= 1) {
            Uint src_index = ci - num_trailing_f;
            Uint src_limit = ci;
            Uint dst_limit = src_index + (num_trailing_f+1)/2;

            ci = src_index;
            while (ci < dst_limit) {
                Uint w[2];
                BeamInstr packed = 0;
                int wi;

                w[0] = code[src_index];
                if (src_index+1 < src_limit) {
                    w[1] = code[src_index+1];
                } else {
                    w[1] = 0;
                }
                for (wi = 0; wi < 2; wi++) {
                    Uint lbl = w[wi];
                    LabelPatch* lp = stp->labels[lbl].patches;
                    int num_patches = stp->labels[lbl].num_patches;

#if defined(WORDS_BIGENDIAN)
                    packed <<= BEAM_WIDE_SHIFT;
                    packed |= lbl & BEAM_WIDE_MASK;
#else
                    packed >>= BEAM_WIDE_SHIFT;
                    packed |= lbl << BEAM_WIDE_SHIFT;
#endif
                    while (num_patches-- > 0) {
                        if (lp->pos == src_index + wi) {
                            lp->pos = ci;
#if defined(WORDS_BIGENDIAN)
                            lp->packed = 2 - wi;
#else
                            lp->packed = wi + 1;
#endif
                            break;
                        }
                        lp++;
                    }
                }
                code[ci++] = packed;
                src_index += 2;
            }
        }
#endif

	/*
	 * Handle a few special cases.
	 */
	switch (stp->specific_op) {
	case op_i_func_info_IaaI:
	    {
                int padding_required;
		Sint offset;

		if (function_number >= stp->num_functions) {
		    LoadError1(stp, "too many functions in module (header said %u)",
			       stp->num_functions); 
		}

                /* Native function calls may be larger than their stubs, so
                 * we'll need to make sure any potentially-native function stub
                 * is padded with enough room.
                 *
                 * Note that the padding is applied for the previous function,
                 * not the current one, so we check whether the old F/A is
                 * a BIF. */
                padding_required = last_func_start && (stp->may_load_nif ||
                    is_bif(stp->module, stp->function, stp->arity));

		/*
		 * Save context for error messages.
		 */
		stp->function = code[ci-2];
		stp->arity = code[ci-1];

		/*
		 * Save current offset of into the line instruction array.
		 */
		if (stp->func_line) {
		    stp->func_line[function_number] = stp->current_li;
		}

		if (padding_required) {
		    const int finfo_ix = ci - FUNC_INFO_SZ;
		    if (finfo_ix - last_func_start < BEAM_NATIVE_MIN_FUNC_SZ) {
			/* Must make room for call_nif op */
			int pad = BEAM_NATIVE_MIN_FUNC_SZ - (finfo_ix - last_func_start);
			ASSERT(pad > 0 && pad < BEAM_NATIVE_MIN_FUNC_SZ);
			CodeNeed(pad);
			sys_memmove(&code[finfo_ix+pad], &code[finfo_ix],
				    FUNC_INFO_SZ*sizeof(BeamInstr));
			sys_memset(&code[finfo_ix], 0, pad*sizeof(BeamInstr));
			ci += pad;
			stp->labels[last_label].value += pad;
		    }
		}
		last_func_start = ci;

                /* When this assert is triggered, it is normally a sign that
                   the size of the ops.tab i_func_info instruction is not
                   the same as FUNC_INFO_SZ */
		ASSERT(stp->labels[last_label].value == ci - FUNC_INFO_SZ);
		offset = function_number;
                register_label_patch(stp, last_label, offset, 0);
		function_number++;
		if (stp->arity > MAX_ARG) {
		    LoadError1(stp, "too many arguments: %d", stp->arity);
		}
#ifdef DEBUG
		ASSERT(stp->labels[0].num_patches == 0); /* Should not be referenced. */
		for (i = 1; i < stp->num_labels; i++) {
                    ASSERT(stp->labels[i].num_patches <= stp->labels[i].num_allocated);
		}
#endif
	    }
	    break;
	case op_on_load:
	    ci--;		/* Get rid of the instruction */

	    /* Remember offset for the on_load function. */
	    stp->on_load = ci;
	    break;
	case op_bs_put_string_WW:
	case op_i_bs_match_string_xfWW:
	case op_i_bs_match_string_yfWW:
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
        if (stp->num_fnames)
            sys_memcpy(line_tab->fname_ptr, stp->fname,
                       stp->num_fnames*sizeof(Eterm));

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
	Uint patch;
	Uint value = stp->labels[i].value;

	if (value == 0 && stp->labels[i].num_patches != 0) {
	    LoadError1(stp, "label %d not resolved", i);
	}
	ASSERT(value < stp->ci);
        for (patch = 0; patch < stp->labels[i].num_patches; patch++) {
            LabelPatch* lp = &stp->labels[i].patches[patch];
            Uint pos = lp->pos;
	    ASSERT(pos < stp->ci);
            if (pos < stp->num_functions) {
                /*
                 * This is the array of pointers to the beginning of
                 * each function. The pointers must remain absolute.
                 */
                codev[pos] = (BeamInstr) (codev + value);
            } else {
#if defined(DEBUG) && defined(BEAM_WIDE_MASK)
                Uint w;
#endif
                Sint32 rel = lp->offset + value;
                switch (lp->packed) {
                case 0:         /* Not packed */
                    ASSERT(codev[pos] == i);
                    codev[pos] = rel;
                    break;
#ifdef BEAM_WIDE_MASK
                case 1:         /* Least significant word. */
#ifdef DEBUG
                    w = codev[pos] & BEAM_WIDE_MASK;
                    /* Correct label in least significant word? */
                    ASSERT(w == i);
#endif
                    codev[pos] = (codev[pos] & ~BEAM_WIDE_MASK) |
                        (rel & BEAM_WIDE_MASK);
                    break;
                case 2:         /* Most significant word */
#ifdef DEBUG
                    w = (codev[pos] >> BEAM_WIDE_SHIFT) & BEAM_WIDE_MASK;
                    /* Correct label in most significant word? */
                    ASSERT(w == i);
#endif
                    codev[pos] = ((Uint)rel << BEAM_WIDE_SHIFT) |
                        (codev[pos] & BEAM_WIDE_MASK);
                    break;
#endif
                default:
                    ASSERT(0);
                }
            }
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
        BeamInstr* abs_addr;
	codev[index] = BeamOpCodeAddr(op_catch_yf);
        /* We must make the address of the label absolute again. */
        abs_addr = (BeamInstr *)codev + index + codev[index+2];
	catches = beam_catches_cons(abs_addr, catches);
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

        ep = erts_export_put(stp->module,
                             stp->export[i].function,
                             stp->export[i].arity);

        /* Fill in BIF stubs with a proper call to said BIF. */
        if (ep->bif_number != -1) {
            erts_write_bif_wrapper(ep, address);
        }

        if (on_load) {
            /*
             * on_load: Don't make any of the exported functions
             * callable yet. Keep any function in the current
             * code callable.
             */
            ep->trampoline.not_loaded.deferred = (BeamInstr) address;
        } else {
            ep->addressv[erts_staging_code_ix()] = address;
        }
    }

#ifdef DEBUG
    /* Ensure that we've loaded stubs for all BIFs in this module. */
    for (i = 0; i < BIF_SIZE; i++) {
        BifEntry *entry = &bif_table[i];

        if (stp->module == entry->module) {
            Export *ep = erts_export_put(entry->module,
                                         entry->name,
                                         entry->arity);
            BeamInstr *addr = ep->addressv[erts_staging_code_ix()];

            if (!ErtsInArea(addr, stp->codev, stp->ci * sizeof(BeamInstr))) {
                erts_exit(ERTS_ABORT_EXIT,
                          "Module %T doesn't export BIF %T/%i\n",
                          entry->module,
                          entry->name,
                          entry->arity);
            }
        }
    }
#endif

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
		erts_refc_dec(&fe->refc, 1);
	    }
	    fe->address = code_ptr;
#ifdef HIPE
	    hipe_set_closure_stub(fe);
#endif
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
    byte default_byte_buf[128];
    byte* byte_buf = default_byte_buf;
    Eterm default_big_buf[128/sizeof(Eterm)];
    Eterm* big_buf = default_big_buf;
    Eterm tmp_big;
    byte* s;
    int i;
    int neg = 0;
    Uint words_needed;
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
            tmp_big = small_to_big(val, big_buf);
            if (!beam_load_find_literal(stp, tmp_big, result)) {
                *result = beam_load_new_literal(stp, &hp, BIG_UINT_HEAP_SIZE);
                sys_memcpy(hp, big_buf, BIG_UINT_HEAP_SIZE*sizeof(Eterm));
            }
	    return TAG_q;
	}
    }

    /*
     * Make sure that the number will fit in our temporary buffer
     * (including margin).
     */

    if (count+8 > sizeof(default_byte_buf)) {
	byte_buf = erts_alloc(ERTS_ALC_T_LOADER_TMP, count+8);
    }

    /*
     * Copy the number reversed to our temporary buffer.
     */

    GetString(stp, s, count);
    for (i = 0; i < count; i++) {
	byte_buf[count-i-1] = *s++;
    }

    /*
     * Check if the number is negative, and negate it if so.
     */

    if ((byte_buf[count-1] & 0x80) != 0) {
	unsigned carry = 1;

	neg = 1;
	for (i = 0; i < count; i++) {
	    byte_buf[i] = ~byte_buf[i] + carry;
	    carry = (byte_buf[i] == 0 && carry == 1);
	}
	ASSERT(carry == 0);
    }

    /*
     * Align to word boundary.
     */

    if (byte_buf[count-1] == 0) {
	count--;
    }
    if (byte_buf[count-1] == 0) {
	LoadError0(stp, "bignum not normalized");
    }
    while (count % sizeof(Eterm) != 0) {
	byte_buf[count++] = 0;
    }

    /*
     * Convert to a bignum.
     */

    words_needed = count/sizeof(Eterm) + 1;
    if (words_needed*sizeof(Eterm) > sizeof(default_big_buf)) {
        big_buf = erts_alloc(ERTS_ALC_T_LOADER_TMP, words_needed*sizeof(Eterm));
    }
    tmp_big = bytes_to_big(byte_buf, count, neg, big_buf);
    if (is_nil(tmp_big)) {
        goto load_error;
    }

    /*
     * Create a literal if there is no previous literal with the same value.
     */

    if (!beam_load_find_literal(stp, tmp_big, result)) {
        *result = beam_load_new_literal(stp, &hp, words_needed);
        sys_memcpy(hp, big_buf, words_needed*sizeof(Eterm));
    }

    if (byte_buf != default_byte_buf) {
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) byte_buf);
    }
    if (big_buf != default_big_buf) {
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) big_buf);
    }
    return TAG_q;

 load_error:
    if (byte_buf != default_byte_buf) {
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) byte_buf);
    }
    if (big_buf != default_big_buf) {
	erts_free(ERTS_ALC_T_LOADER_TMP, (void *) big_buf);
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

void
beam_load_new_genop(LoaderState* stp)
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

int
beam_load_new_label(LoaderState* stp)
{
    unsigned int num = stp->num_labels;

    stp->num_labels++;
    stp->labels = (Label *) erts_realloc(ERTS_ALC_T_PREPARED_CODE,
					 (void *) stp->labels,
					 stp->num_labels * sizeof(Label));
    init_label(&stp->labels[num]);
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

Uint
beam_load_new_literal(LoaderState* stp, Eterm** hpp, Uint heap_size)
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

int
beam_load_find_literal(LoaderState* stp, Eterm needle, Uint *idx)
{
    int i;

    /*
     * The search is done backwards since the most recent literals
     * allocated by the loader itself will be placed at the end
     */
    for (i = stp->num_literals - 1; i >= 0; i--) {
        if (EQ(needle, stp->literals[i].term)) {
            *idx = (Uint) i;
            return 1;
        }
    }
    return 0;
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
    } else if (what == am_nifs) {
	return nifs_in_module(p, module);
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
 * Builds a list of all NIFs in the given module:
 *     [{Name, Arity},...]
 */
Eterm
nifs_in_module(Process* p, Eterm module)
{
    Eterm nif_list, *hp;
    Module *mod;

    mod = erts_get_module(module, erts_active_code_ix());
    nif_list = NIL;

    if (mod->curr.nif != NULL) {
        int func_count, func_ix;
        ErlNifFunc *funcs;

        func_count = erts_nif_get_funcs(mod->curr.nif, &funcs);
        hp = HAlloc(p, func_count * 5);

        for (func_ix = func_count - 1; func_ix >= 0; func_ix--) {
            Eterm name, arity, pair;
            ErlNifFunc *func;

            func = &funcs[func_ix];

            name = am_atom_put(func->name, sys_strlen(func->name));
            arity = make_small(func->arity);

            pair = TUPLE2(hp, name, arity);
            hp += 3;

            nif_list = CONS(hp, pair, nif_list);
            hp += 2;
        }
    }

    return nif_list;
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
        switch (thing_subtag(oh->thing_word)) {
        case REFC_BINARY_SUBTAG:
            {
                Binary* bptr = ((ProcBin*)oh)->val;
                erts_bin_release(bptr);
                break;
            }
        case FUN_SUBTAG:
            {
                ErlFunEntry* fe = ((ErlFunThing*)oh)->fe;
                if (erts_refc_dectest(&fe->refc, 0) == 0) {
                    erts_erase_fun_entry(fe);
                }
                break;
            }
        case REF_SUBTAG:
            {
                ErtsMagicBinary *bptr;
                ASSERT(is_magic_ref_thing(oh));
                bptr = ((ErtsMRefThing *) oh)->mb;
                erts_bin_release((Binary *) bptr);
                break;
            }
        default:
            ASSERT(is_external_header(oh->thing_word));
            erts_deref_node_entry(((ExternalThing*)oh)->node,
                                  make_boxed(&oh->thing_word));
        }
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
            ErtsCodeInfo* ci = code_hdr->functions[i];
            if (is_atom(ci->mfa.function)) {
                return erts_is_function_native(ci);
            }
            else ASSERT(is_nil(ci->mfa.function)); /* ignore BIF stubs */
        }
    }
    return 0;
}

int
erts_is_function_native(ErtsCodeInfo *ci)
{
#ifdef HIPE
    ASSERT(BeamIsOpCode(ci->op, op_i_func_info_IaaI));
    return BeamIsOpCode(erts_codeinfo_to_code(ci)[0], op_hipe_trap_call) ||
	BeamIsOpCode(erts_codeinfo_to_code(ci)[0], op_hipe_trap_call_closure);
#else
    return 0;
#endif
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

            if (ep->addressv[code_ix] == ep->trampoline.raw &&
                BeamIsOpCode(ep->trampoline.op, op_call_error_handler)) {
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
            file_term = erts_atom_to_string(&hp, (fi->fname_ptr)[file-1]);
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
	!scan_iff_file(stp, chunk_types, ERTS_BEAM_NUM_CHUNK_TYPES) ||
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
    info->op = BeamOpCodeAddr(op_i_func_info_IaaI);
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
	sys_memcpy(info, stp->chunks[chunk].start, size);
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
	    *erts_codeinfo_to_code(ci) = BeamOpCodeAddr(op_hipe_trap_call_closure);
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

    erts_refc_dec(&fe->refc, 1);

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
    if (!scan_iff_file(stp, chunk_types, ERTS_BEAM_NUM_CHUNK_TYPES) ||
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
	op = BeamOpCodeAddr(op_hipe_trap_call); /* Might be changed later. */
	fp = make_stub((ErtsCodeInfo*)fp, hipe_stp->module, func, arity,
                       (Uint)native_address, op);
    }

    /*
     * Insert the last pointer and the int_code_end instruction.
     */

    code_hdr->functions[i] = (ErtsCodeInfo*)fp;
    *fp++ = BeamOpCodeAddr(op_int_code_end);

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

    hipe_redirect_to_module(modp);

    return 1;
}

#undef WORDS_PER_FUNCTION
#endif /* HIPE */


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
    dbg_trace_m[i] = am_atom_put(m, sys_strlen(m));
    dbg_trace_f[i] = am_atom_put(f, sys_strlen(f));
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
