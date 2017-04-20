/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
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

/*
 * Purpose: Basic debugging support.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_driver.h"
#include "bif.h"
#include "big.h"
#include "external.h"
#include "beam_load.h"
#include "beam_bp.h"
#include "erl_binary.h"
#include "erl_thr_progress.h"
#include "erl_nfunc_sched.h"

#ifdef ARCH_64
# define HEXF "%016bpX"
#else
# define HEXF "%08bpX"
#endif
#define TermWords(t) (((t) / (sizeof(BeamInstr)/sizeof(Eterm))) + !!((t) % (sizeof(BeamInstr)/sizeof(Eterm))))

void dbg_bt(Process* p, Eterm* sp);
void dbg_where(BeamInstr* addr, Eterm x0, Eterm* reg);

static int print_op(fmtfn_t to, void *to_arg, int op, int size, BeamInstr* addr);
static void print_bif_name(fmtfn_t to, void* to_arg, BifFunction bif);

BIF_RETTYPE
erts_debug_same_2(BIF_ALIST_2)
{
    return (BIF_ARG_1 == BIF_ARG_2) ? am_true : am_false;
}

BIF_RETTYPE
erts_debug_flat_size_1(BIF_ALIST_1)
{
    Process* p = BIF_P;
    Eterm term = BIF_ARG_1;
    Uint size = size_object(term);

    if (IS_USMALL(0, size)) {
	BIF_RET(make_small(size));
    } else {
	Eterm* hp = HAlloc(p, BIG_UINT_HEAP_SIZE);
	BIF_RET(uint_to_big(size, hp));
    }
}

BIF_RETTYPE
erts_debug_size_shared_1(BIF_ALIST_1)
{
    Process* p = BIF_P;
    Eterm term = BIF_ARG_1;
    Uint size = size_shared(term);

    if (IS_USMALL(0, size)) {
	BIF_RET(make_small(size));
    } else {
	Eterm* hp = HAlloc(p, BIG_UINT_HEAP_SIZE);
	BIF_RET(uint_to_big(size, hp));
    }
}

BIF_RETTYPE
erts_debug_copy_shared_1(BIF_ALIST_1)
{
    Process* p = BIF_P;
    Eterm term = BIF_ARG_1;
    Uint size;
    Eterm* hp;
    Eterm copy;
    erts_shcopy_t info;
    INITIALIZE_SHCOPY(info);

    size = copy_shared_calculate(term, &info);
    if (size > 0) {
      hp = HAlloc(p, size);
    }
    copy = copy_shared_perform(term, size, &info, &hp, &p->off_heap);
    DESTROY_SHCOPY(info);
    BIF_RET(copy);
}

BIF_RETTYPE
erts_debug_breakpoint_2(BIF_ALIST_2)
{
    Process* p = BIF_P;
    Eterm MFA = BIF_ARG_1;
    Eterm boolean = BIF_ARG_2;
    Eterm* tp;
    ErtsCodeMFA mfa;
    int i;
    int specified = 0;
    Eterm res;
    BpFunctions f;

    if (boolean != am_true && boolean != am_false)
	goto error;

    if (is_not_tuple(MFA)) {
	goto error;
    }
    tp = tuple_val(MFA);
    if (*tp != make_arityval(3)) {
	goto error;
    }
    if (!is_atom(tp[1]) || !is_atom(tp[2]) ||
	(!is_small(tp[3]) && tp[3] != am_Underscore)) {
	goto error;
    }
    for (i = 0; i < 3 && tp[i+1] != am_Underscore; i++, specified++) {
	/* Empty loop body */
    }
    for (i = specified; i < 3; i++) {
	if (tp[i+1] != am_Underscore) {
	    goto error;
	}
    }

    mfa.module = tp[1];
    mfa.function = tp[2];

    if (is_small(tp[3])) {
        mfa.arity = signed_val(tp[3]);
    }

    if (!erts_try_seize_code_write_permission(BIF_P)) {
	ERTS_BIF_YIELD2(bif_export[BIF_erts_debug_breakpoint_2],
			BIF_P, BIF_ARG_1, BIF_ARG_2);
    }
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
    erts_smp_thr_progress_block();

    erts_bp_match_functions(&f, &mfa, specified);
    if (boolean == am_true) {
	erts_set_debug_break(&f);
	erts_install_breakpoints(&f);
	erts_commit_staged_bp();
    } else {
	erts_clear_debug_break(&f);
	erts_commit_staged_bp();
	erts_uninstall_breakpoints(&f);
    }
    erts_consolidate_bp_data(&f, 1);
    res = make_small(f.matched);
    erts_bp_free_matched_functions(&f);

    erts_smp_thr_progress_unblock();
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    erts_release_code_write_permission();
    return res;

 error:
    BIF_ERROR(p, BADARG);
}

#if 0 /* Kept for conveninence when hard debugging. */
void debug_dump_code(BeamInstr *I, int num)
{
    BeamInstr *code_ptr = I;
    BeamInstr *end = code_ptr + num;
    erts_dsprintf_buf_t *dsbufp;
    BeamInstr instr;
    int i;

    dsbufp = erts_create_tmp_dsbuf(0);
    while (code_ptr < end) {
	erts_print(ERTS_PRINT_DSBUF, (void *) dsbufp, HEXF ": ", code_ptr);
	instr = (BeamInstr) code_ptr[0];
	for (i = 0; i < NUM_SPECIFIC_OPS; i++) {
	    if (instr == (BeamInstr) BeamOp(i) && opc[i].name[0] != '\0') {
		code_ptr += print_op(ERTS_PRINT_DSBUF, (void *) dsbufp,
				     i, opc[i].sz-1, code_ptr+1) + 1;
		break;
	    }
	}
	if (i >= NUM_SPECIFIC_OPS) {
	    erts_print(ERTS_PRINT_DSBUF, (void *) dsbufp,
		       "unknown " HEXF "\n", instr);
	    code_ptr++;
	}
    }
    dsbufp->str[dsbufp->str_len] = 0;
    erts_fprintf(stderr,"%s", dsbufp->str);
    erts_destroy_tmp_dsbuf(dsbufp);
}
#endif

BIF_RETTYPE
erts_debug_instructions_0(BIF_ALIST_0)
{
    int i = 0;
    Uint needed = num_instructions * 2;
    Eterm* hp;
    Eterm res = NIL;

    for (i = 0; i < num_instructions; i++) {
	needed += 2*strlen(opc[i].name);
    }
    hp = HAlloc(BIF_P, needed);
    for (i = num_instructions-1; i >= 0; i--) {
	Eterm s = erts_bld_string_n(&hp, 0, opc[i].name, strlen(opc[i].name));
	res = erts_bld_cons(&hp, 0, s, res);
    }
    return res;
}

BIF_RETTYPE
erts_debug_disassemble_1(BIF_ALIST_1)
{
    Process* p = BIF_P;
    Eterm addr = BIF_ARG_1;
    erts_dsprintf_buf_t *dsbufp;
    Eterm* hp;
    Eterm* tp;
    Eterm bin;
    Eterm mfa;
    ErtsCodeMFA *cmfa = NULL;
    BeamCodeHeader* code_hdr;
    BeamInstr *code_ptr;
    BeamInstr instr;
    BeamInstr uaddr;
    Uint hsz;
    int i;

    if (term_to_UWord(addr, &uaddr)) {
	code_ptr = (BeamInstr *) uaddr;
	if ((cmfa = find_function_from_pc(code_ptr)) == NULL) {
	    BIF_RET(am_false);
	}
    } else if (is_tuple(addr)) {
	ErtsCodeIndex code_ix;
	Module* modp;
	Eterm mod;
	Eterm name;
	Export* ep;
	Sint arity;
	int n;

	tp = tuple_val(addr);
	if (tp[0] != make_arityval(3)) {
	error:
	    BIF_ERROR(p, BADARG);
	}
	mod = tp[1];
	name = tp[2];
	if (!is_atom(mod) || !is_atom(name) || !is_small(tp[3])) {
	    goto error;
	}
	arity = signed_val(tp[3]);
	code_ix = erts_active_code_ix();
	modp = erts_get_module(mod, code_ix);

	/*
	 * Try the export entry first to allow disassembly of special functions
	 * such as erts_debug:apply/4.  Then search for it in the module.
	 */
	if ((ep = erts_find_function(mod, name, arity, code_ix)) != NULL) {
	    /* XXX: add "&& ep->address != ep->code" condition?
	     * Consider a traced function.
	     * Its ep will have ep->address == ep->code.
	     * erts_find_function() will return the non-NULL ep.
	     * Below we'll try to derive a code_ptr from ep->address.
	     * But this code_ptr will point to the start of the Export,
	     * not the function's func_info instruction. BOOM !?
	     */
	    cmfa = erts_code_to_codemfa(ep->addressv[code_ix]);
	} else if (modp == NULL || (code_hdr = modp->curr.code_hdr) == NULL) {
	    BIF_RET(am_undef);
	} else {
	    n = code_hdr->num_functions;
	    for (i = 0; i < n; i++) {
		cmfa = &code_hdr->functions[i]->mfa;
		if (cmfa->function == name && cmfa->arity == arity) {
		    break;
		}
	    }
	    if (i == n) {
		BIF_RET(am_undef);
	    }
	}
        code_ptr = (BeamInstr*)erts_code_to_codeinfo(erts_codemfa_to_code(cmfa));
    } else {
	goto error;
    }

    dsbufp = erts_create_tmp_dsbuf(0);
    erts_print(ERTS_PRINT_DSBUF, (void *) dsbufp, HEXF ": ", code_ptr);
    instr = (BeamInstr) code_ptr[0];
    for (i = 0; i < NUM_SPECIFIC_OPS; i++) {
	if (instr == (BeamInstr) BeamOp(i) && opc[i].name[0] != '\0') {
	    code_ptr += print_op(ERTS_PRINT_DSBUF, (void *) dsbufp,
				 i, opc[i].sz-1, code_ptr+1) + 1;
	    break;
	}
    }
    if (i >= NUM_SPECIFIC_OPS) {
	erts_print(ERTS_PRINT_DSBUF, (void *) dsbufp,
		   "unknown " HEXF "\n", instr);
	code_ptr++;
    }
    bin = new_binary(p, (byte *) dsbufp->str, dsbufp->str_len);
    erts_destroy_tmp_dsbuf(dsbufp);
    hsz = 4+4;
    (void) erts_bld_uword(NULL, &hsz, (BeamInstr) code_ptr);
    hp = HAlloc(p, hsz);
    addr = erts_bld_uword(&hp, NULL, (BeamInstr) code_ptr);
    ASSERT(is_atom(cmfa->module) || is_nil(cmfa->module));
    ASSERT(is_atom(cmfa->function) || is_nil(cmfa->function));
    mfa = TUPLE3(hp, cmfa->module, cmfa->function,
                 make_small(cmfa->arity));
    hp += 4;
    return TUPLE3(hp, addr, bin, mfa);
}

void
dbg_bt(Process* p, Eterm* sp)
{
    Eterm* stack = STACK_START(p);

    while (sp < stack) {
	if (is_CP(*sp)) {
	    ErtsCodeMFA* cmfa = find_function_from_pc(cp_val(*sp));
	    if (cmfa)
		erts_fprintf(stderr,
			     HEXF ": %T:%T/%bpu\n",
			     &cmfa->module, cmfa->module,
                             cmfa->function, cmfa->arity);
	}
	sp++;
    }
}

void
dbg_where(BeamInstr* addr, Eterm x0, Eterm* reg)
{
    ErtsCodeMFA* cmfa = find_function_from_pc(addr);

    if (cmfa == NULL) {
	erts_fprintf(stderr, "???\n");
    } else {
	int arity;
	int i;

	arity = cmfa->arity;
	erts_fprintf(stderr, HEXF ": %T:%T(", addr,
                     cmfa->module, cmfa->function);
	for (i = 0; i < arity; i++)
	    erts_fprintf(stderr, i ? ", %T" : "%T", i ? reg[i] : x0);
	erts_fprintf(stderr, ")\n");
    }
}

static int
print_op(fmtfn_t to, void *to_arg, int op, int size, BeamInstr* addr)
{
    int i;
    BeamInstr tag;
    char* sign;
    char* start_prog;		/* Start of program for packer. */
    char* prog;			/* Current position in packer program. */
    BeamInstr stack[8];		/* Stack for packer. */
    BeamInstr* sp = stack;		/* Points to next free position. */
    BeamInstr packed = 0;		/* Accumulator for packed operations. */
    BeamInstr args[8];		/* Arguments for this instruction. */
    BeamInstr* ap;			/* Pointer to arguments. */
    BeamInstr* unpacked;		/* Unpacked arguments */

    start_prog = opc[op].pack;

    if (start_prog[0] == '\0') {
	/*
	 * There is no pack program.
	 * Avoid copying because instructions containing bignum operands
	 * are bigger than actually declared.
	 */
	ap = (BeamInstr *) addr;
    } else {
	/*
	 * Copy all arguments to a local buffer for the unpacking.
	 */

	ASSERT(size <= sizeof(args)/sizeof(args[0]));
	ap = args;
	for (i = 0; i < size; i++) {
	    *ap++ = addr[i];
	}

	/*
	 * Undo any packing done by the loader.  This is easily done by running
	 * the packing program backwards and in reverse.
	 */

	prog = start_prog + strlen(start_prog);
	while (start_prog < prog) {
	    prog--;
	    switch (*prog) {
	    case 'g':
		*ap++ = *--sp;
		break;
	    case 'i':		/* Initialize packing accumulator. */
		*ap++ = packed;
		break;
	    case 's':
		*ap++ = packed & 0x3ff;
		packed >>= 10;
		break;
	    case '0':		/* Tight shift */
		*ap++ = packed & BEAM_TIGHT_MASK;
		packed >>= BEAM_TIGHT_SHIFT;
		break;
	    case '6':		/* Shift 16 steps */
		*ap++ = packed & BEAM_LOOSE_MASK;
		packed >>= BEAM_LOOSE_SHIFT;
		break;
#ifdef ARCH_64
	    case 'w':		/* Shift 32 steps */
		*ap++ = packed & BEAM_WIDE_MASK;
		packed >>= BEAM_WIDE_SHIFT;
		break;
#endif
	    case 'p':
		*sp++ = *--ap;
		break;
	    case 'P':
		packed = *--sp;
		break;
	    default:
		ASSERT(0);
	    }
	}
	ap = args;
    }

    /*
     * Print the name and all operands of the instructions.
     */
	
    erts_print(to, to_arg, "%s ", opc[op].name);
    sign = opc[op].sign;
    while (*sign) {
	switch (*sign) {
	case 'r':		/* x(0) */
	    erts_print(to, to_arg, "r(0)");
	    break;
	case 'x':		/* x(N) */
	    {
		Uint n = ap[0] / sizeof(Eterm);
		erts_print(to, to_arg, "x(%d)", n);
		ap++;
	    }
	    break;
	case 'y':		/* y(N) */
	    {
		Uint n = ap[0] / sizeof(Eterm) - CP_SIZE;
		erts_print(to, to_arg, "y(%d)", n);
		ap++;
	    }
	    break;
	case 'n':		/* Nil */
	    erts_print(to, to_arg, "[]");
	    break;
	case 's':		/* Any source (tagged constant or register) */
	    tag = loader_tag(*ap);
	    if (tag == LOADER_X_REG) {
		erts_print(to, to_arg, "x(%d)", loader_x_reg_index(*ap));
		ap++;
		break;
	    } else if (tag == LOADER_Y_REG) {
		erts_print(to, to_arg, "y(%d)", loader_y_reg_index(*ap) - CP_SIZE);
		ap++;
		break;
	    }
	    /*FALLTHROUGH*/
	case 'a':		/* Tagged atom */
	case 'i':		/* Tagged integer */
	case 'c':		/* Tagged constant */
	case 'q':		/* Tagged literal */
	    erts_print(to, to_arg, "%T", (Eterm) *ap);
	    ap++;
	    break;
	case 'A':
	    erts_print(to, to_arg, "%d", arityval( (Eterm) ap[0]));
	    ap++;
	    break;
	case 'd':		/* Destination (x(0), x(N), y(N)) */
	    if (*ap & 1) {
		erts_print(to, to_arg, "y(%d)",
			   *ap / sizeof(Eterm) - CP_SIZE);
	    } else {
		erts_print(to, to_arg, "x(%d)",
			   *ap / sizeof(Eterm));
	    }
	    ap++;
	    break;
	case 'I':		/* Untagged integer. */
	case 't':
	    switch (op) {
	    case op_i_gc_bif1_jIsId:
	    case op_i_gc_bif2_jIIssd:
	    case op_i_gc_bif3_jIIssd:
		{
		    const ErtsGcBif* p;
		    BifFunction gcf = (BifFunction) *ap;
		    for (p = erts_gc_bifs; p->bif != 0; p++) {
			if (p->gc_bif == gcf) {
			    print_bif_name(to, to_arg, p->bif);
			    break;
			}
		    }
		    if (p->bif == 0) {
			erts_print(to, to_arg, "%d", (Uint)gcf);
		    }
		    break;
		}
	    default:
		erts_print(to, to_arg, "%d", *ap);
	    }
	    ap++;
	    break;
	case 'f':		/* Destination label */
	    {
		ErtsCodeMFA* cmfa = find_function_from_pc((BeamInstr *)*ap);
		if (!cmfa || erts_codemfa_to_code(cmfa) != (BeamInstr *) *ap) {
		    erts_print(to, to_arg, "f(" HEXF ")", *ap);
		} else {
		    erts_print(to, to_arg, "%T:%T/%bpu", cmfa->module,
                               cmfa->function, cmfa->arity);
		}
		ap++;
	    }
	    break;
	case 'p':		/* Pointer (to label) */
	    {
		ErtsCodeMFA* cmfa = find_function_from_pc((BeamInstr *)*ap);
		if (!cmfa || erts_codemfa_to_code(cmfa) != (BeamInstr *) *ap) {
		    erts_print(to, to_arg, "p(" HEXF ")", *ap);
		} else {
		    erts_print(to, to_arg, "%T:%T/%bpu", cmfa->module,
                               cmfa->function, cmfa->arity);
		}
		ap++;
	    }
	    break;
	case 'j':		/* Pointer (to label) */
	    erts_print(to, to_arg, "j(" HEXF ")", *ap);
	    ap++;
	    break;
	case 'e':		/* Export entry */
	    {
		Export* ex = (Export *) *ap;
		erts_print(to, to_arg,
			   "%T:%T/%bpu", (Eterm) ex->info.mfa.module,
                           (Eterm) ex->info.mfa.function,
                           ex->info.mfa.arity);
		ap++;
	    }
	    break;
	case 'F':		/* Function definition */
	    break;
	case 'b':
	    print_bif_name(to, to_arg, (BifFunction) *ap);
	    ap++;
	    break;
	case 'P':	/* Byte offset into tuple (see beam_load.c) */
	case 'Q':	/* Like 'P', but packable */
	    erts_print(to, to_arg, "%d", (*ap / sizeof(Eterm)) - 1);
	    ap++;
	    break;
	case 'l':		/* fr(N) */
	    erts_print(to, to_arg, "fr(%d)", loader_reg_index(ap[0]));
	    ap++;
	    break;
	default:
	    erts_print(to, to_arg, "???");
	    ap++;
	    break;
	}
	erts_print(to, to_arg, " ");
	sign++;
    }

    /*
     * Print more information about certain instructions.
     */

    unpacked = ap;
    ap = addr + size;
    switch (op) {
    case op_i_select_val_lins_xfI:
    case op_i_select_val_lins_yfI:
	{
	    int n = ap[-1];
	    int ix = n;

	    while (ix--) {
		erts_print(to, to_arg, "%T ", (Eterm) ap[0]);
		ap++;
		size++;
	    }
	    ix = n;
	    while (ix--) {
		erts_print(to, to_arg, "f(" HEXF ") ", (Eterm) ap[0]);
		ap++;
		size++;
	    }
	}
	break;
    case op_i_select_val_bins_xfI:
    case op_i_select_val_bins_yfI:
	{
	    int n = ap[-1];

	    while (n > 0) {
		erts_print(to, to_arg, "%T f(" HEXF ") ", (Eterm) ap[0], ap[1]);
		ap += 2;
		size += 2;
		n--;
	    }
	}
	break;
    case op_i_select_tuple_arity_xfI:
    case op_i_select_tuple_arity_yfI:
        {
            int n = ap[-1];
            int ix = n - 1; /* without sentinel */

            while (ix--) {
                Uint arity = arityval(ap[0]);
                erts_print(to, to_arg, "{%d} ", arity, ap[1]);
                ap++;
                size++;
            }
            /* print sentinel */
            erts_print(to, to_arg, "{%T} ", ap[0], ap[1]);
            ap++;
            size++;
            ix = n;
            while (ix--) {
                erts_print(to, to_arg, "f(" HEXF ") ", ap[0]);
                ap++;
                size++;
            }
        }
        break;
    case op_i_jump_on_val_xfII:
    case op_i_jump_on_val_yfII:
	{
	    int n;
	    for (n = ap[-2]; n > 0; n--) {
		erts_print(to, to_arg, "f(" HEXF ") ", ap[0]);
		ap++;
		size++;
	    }
	}
	break;
    case op_i_jump_on_val_zero_xfI:
    case op_i_jump_on_val_zero_yfI:
	{
	    int n;
	    for (n = ap[-1]; n > 0; n--) {
		erts_print(to, to_arg, "f(" HEXF ") ", ap[0]);
		ap++;
		size++;
	    }
	}
	break;
    case op_i_put_tuple_xI:
    case op_i_put_tuple_yI:
    case op_new_map_dII:
    case op_update_map_assoc_jsdII:
    case op_update_map_exact_jsdII:
	{
	    int n = unpacked[-1];

	    while (n > 0) {
		switch (loader_tag(ap[0])) {
		case LOADER_X_REG:
		    erts_print(to, to_arg, " x(%d)", loader_x_reg_index(ap[0]));
		    break;
		case LOADER_Y_REG:
		    erts_print(to, to_arg, " x(%d)", loader_y_reg_index(ap[0]));
		    break;
		default:
		    erts_print(to, to_arg, " %T", (Eterm) ap[0]);
		    break;
		}
		ap++, size++, n--;
	    }
	}
	break;
    case op_i_get_map_elements_fsI:
	{
	    int n = unpacked[-1];

	    while (n > 0) {
		if (n % 3 == 1) {
		    erts_print(to, to_arg, " %X", ap[0]);
		} else {
		    switch (loader_tag(ap[0])) {
		    case LOADER_X_REG:
			erts_print(to, to_arg, " x(%d)", loader_x_reg_index(ap[0]));
			break;
		    case LOADER_Y_REG:
			erts_print(to, to_arg, " y(%d)", loader_y_reg_index(ap[0]));
			break;
		    default:
			erts_print(to, to_arg, " %T", (Eterm) ap[0]);
			break;
		    }
		}
		ap++, size++, n--;
	    }
	}
	break;
    }
    erts_print(to, to_arg, "\n");

    return size;
}

static void print_bif_name(fmtfn_t to, void* to_arg, BifFunction bif)
{
    int i;

    for (i = 0; i < BIF_SIZE; i++) {
	if (bif == bif_table[i].f) {
	    break;
	}
    }
    if (i == BIF_SIZE) {
	erts_print(to, to_arg, "b(%d)", (Uint) bif);
    } else {
	Eterm name = bif_table[i].name;
	unsigned arity = bif_table[i].arity;
	erts_print(to, to_arg, "%T/%u", name, arity);
    }
}

/*
 * Dirty BIF testing.
 *
 * The erts_debug:dirty_cpu/2, erts_debug:dirty_io/1, and
 * erts_debug:dirty/3 BIFs are used by the dirty_bif_SUITE
 * test suite.
 */

#ifdef ERTS_DIRTY_SCHEDULERS
static int ms_wait(Process *c_p, Eterm etimeout, int busy);
static int dirty_send_message(Process *c_p, Eterm to, Eterm tag);
#endif
static BIF_RETTYPE dirty_test(Process *c_p, Eterm type, Eterm arg1, Eterm arg2, UWord *I);

/*
 * erts_debug:dirty_cpu/2 is statically determined to execute on
 * a dirty CPU scheduler (see erts_dirty_bif.tab).
 */
BIF_RETTYPE
erts_debug_dirty_cpu_2(BIF_ALIST_2)
{
    return dirty_test(BIF_P, am_dirty_cpu, BIF_ARG_1, BIF_ARG_2, BIF_I);
}

/*
 * erts_debug:dirty_io/2 is statically determined to execute on
 * a dirty I/O scheduler (see erts_dirty_bif.tab).
 */
BIF_RETTYPE
erts_debug_dirty_io_2(BIF_ALIST_2)
{
    return dirty_test(BIF_P, am_dirty_io, BIF_ARG_1, BIF_ARG_2, BIF_I);
}

/*
 * erts_debug:dirty/3 executes on a normal scheduler.
 */
BIF_RETTYPE
erts_debug_dirty_3(BIF_ALIST_3)
{
#ifdef ERTS_DIRTY_SCHEDULERS
    Eterm argv[2];
    switch (BIF_ARG_1) {
    case am_normal:
	return dirty_test(BIF_P, am_normal, BIF_ARG_2, BIF_ARG_3, BIF_I);
    case am_dirty_cpu:
	argv[0] = BIF_ARG_2;
	argv[1] = BIF_ARG_3;
	return erts_schedule_bif(BIF_P,
				 argv,
				 BIF_I,
				 erts_debug_dirty_cpu_2,
				 ERTS_SCHED_DIRTY_CPU,
				 am_erts_debug,
				 am_dirty_cpu,
				 2);
    case am_dirty_io:
	argv[0] = BIF_ARG_2;
	argv[1] = BIF_ARG_3;
	return erts_schedule_bif(BIF_P,
				 argv,
				 BIF_I,
				 erts_debug_dirty_io_2,
				 ERTS_SCHED_DIRTY_IO,
				 am_erts_debug,
				 am_dirty_io,
				 2);
    default:
	BIF_ERROR(BIF_P, EXC_BADARG);
    }
#else
	BIF_ERROR(BIF_P, EXC_UNDEF);
#endif
}


static BIF_RETTYPE
dirty_test(Process *c_p, Eterm type, Eterm arg1, Eterm arg2, UWord *I)
{
    BIF_RETTYPE ret;
#ifdef ERTS_DIRTY_SCHEDULERS
    if (am_scheduler == arg1) {
	ErtsSchedulerData *esdp;
	if (arg2 != am_type) 
	    goto badarg;
	esdp = erts_proc_sched_data(c_p);
	if (!esdp)
            goto scheduler_type_error;
      
        switch (esdp->type) {
        case ERTS_SCHED_NORMAL:
	    ERTS_BIF_PREP_RET(ret, am_normal);
            break;
        case ERTS_SCHED_DIRTY_CPU:
	    ERTS_BIF_PREP_RET(ret, am_dirty_cpu);
            break;
        case ERTS_SCHED_DIRTY_IO:
	    ERTS_BIF_PREP_RET(ret, am_dirty_io);
            break;
        default:
        scheduler_type_error:
	    ERTS_BIF_PREP_RET(ret, am_error);
            break;
        }
    }
    else if (am_error == arg1) {
	switch (arg2) {
	case am_notsup:
	    ERTS_BIF_PREP_ERROR(ret, c_p, EXC_NOTSUP);
	    break;
	case am_undef:
	    ERTS_BIF_PREP_ERROR(ret, c_p, EXC_UNDEF);
	    break;
	case am_badarith:
	    ERTS_BIF_PREP_ERROR(ret, c_p, EXC_BADARITH);
	    break;
	case am_noproc:
	    ERTS_BIF_PREP_ERROR(ret, c_p, EXC_NOPROC);
	    break;
	case am_system_limit:
	    ERTS_BIF_PREP_ERROR(ret, c_p, SYSTEM_LIMIT);
	    break;
	case am_badarg:
	default:
	    goto badarg;
	}
    }
    else if (am_copy == arg1) {
	int i;
	Eterm res;

	for (res = NIL, i = 0; i < 1000; i++) {
	    Eterm *hp, sz;
	    Eterm cpy;
	    /* We do not want this to be optimized,
	       but rather the oposite... */
	    sz = size_object(arg2);
	    hp = HAlloc(c_p, sz);
	    cpy = copy_struct(arg2, sz, &hp, &c_p->off_heap);
	    hp = HAlloc(c_p, 2);
	    res = CONS(hp, cpy, res);
	}

	ERTS_BIF_PREP_RET(ret, res);
    }
    else if (am_send == arg1) {
	dirty_send_message(c_p, arg2, am_ok);
	ERTS_BIF_PREP_RET(ret, am_ok);
    }
    else if (ERTS_IS_ATOM_STR("wait", arg1)) {
	if (!ms_wait(c_p, arg2, type == am_dirty_cpu))
	    goto badarg;
	ERTS_BIF_PREP_RET(ret, am_ok);
    }
    else if (ERTS_IS_ATOM_STR("reschedule", arg1)) {
	/*
	 * Reschedule operation after decrement of two until we reach
	 * zero. Switch between dirty scheduler types when 'n' is
	 * evenly divided by 4. If the initial value wasn't evenly
	 * dividable by 2, throw badarg exception.
	 */
	Eterm next_type;
	Sint n;
	if (!term_to_Sint(arg2, &n) || n < 0)
	    goto badarg;
	if (n == 0)
	    ERTS_BIF_PREP_RET(ret, am_ok);
	else {
	    Eterm argv[3];
	    Eterm eint = erts_make_integer((Uint) (n - 2), c_p);
	    if (n % 4 != 0)
		next_type = type;
	    else {
		switch (type) {
		case am_dirty_cpu: next_type = am_dirty_io; break;
		case am_dirty_io: next_type = am_normal; break;
		case am_normal: next_type = am_dirty_cpu; break;
		default: goto badarg;
		}
	    }
	    switch (next_type) {
	    case am_dirty_io:
		argv[0] = arg1;
		argv[1] = eint;
		ret = erts_schedule_bif(c_p,
					argv,
					I,
					erts_debug_dirty_io_2,
					ERTS_SCHED_DIRTY_IO,
					am_erts_debug,
					am_dirty_io,
					2);
		break;
	    case am_dirty_cpu:
		argv[0] = arg1;
		argv[1] = eint;
		ret = erts_schedule_bif(c_p,
					argv,
					I,
					erts_debug_dirty_cpu_2,
					ERTS_SCHED_DIRTY_CPU,
					am_erts_debug,
					am_dirty_cpu,
					2);
		break;
	    case am_normal:
		argv[0] = am_normal;
		argv[1] = arg1;
		argv[2] = eint;
		ret = erts_schedule_bif(c_p,
					argv,
					I,
					erts_debug_dirty_3,
					ERTS_SCHED_NORMAL,
					am_erts_debug,
					am_dirty,
					3);
		break;
	    default:
		goto badarg;
	    }
	}
    }
    else if (ERTS_IS_ATOM_STR("ready_wait6_done", arg1)) {
	ERTS_DECL_AM(ready);
	ERTS_DECL_AM(done);
	dirty_send_message(c_p, arg2, AM_ready);
	ms_wait(c_p, make_small(6000), 0);
	dirty_send_message(c_p, arg2, AM_done);
	ERTS_BIF_PREP_RET(ret, am_ok);
    }
    else if (ERTS_IS_ATOM_STR("alive_waitexiting", arg1)) {
	Process *real_c_p = erts_proc_shadow2real(c_p);
	Eterm *hp, *hp2;
	Uint sz;
	int i;
	ErtsSchedulerData *esdp = erts_proc_sched_data(c_p);
        int dirty_io = esdp->type == ERTS_SCHED_DIRTY_IO;

	if (ERTS_PROC_IS_EXITING(real_c_p))
	    goto badarg;
	dirty_send_message(c_p, arg2, am_alive);

	/* Wait until dead */
	while (!ERTS_PROC_IS_EXITING(real_c_p)) {
            if (dirty_io)
                ms_wait(c_p, make_small(100), 0);
            else
                erts_thr_yield();
        }

	ms_wait(c_p, make_small(1000), 0);

	/* Should still be able to allocate memory */
	hp = HAlloc(c_p, 3); /* Likely on heap */
	sz = 10000;
	hp2 = HAlloc(c_p, sz); /* Likely in heap fragment */
	*hp2 = make_pos_bignum_header(sz);
	for (i = 1; i < sz; i++)
	    hp2[i] = (Eterm) 4711;
	ERTS_BIF_PREP_RET(ret, TUPLE2(hp, am_ok, make_big(hp2)));
    }
    else {
    badarg:
	ERTS_BIF_PREP_ERROR(ret, c_p, BADARG);
    }
#else
    ERTS_BIF_PREP_ERROR(ret, c_p, EXC_UNDEF);
#endif
    return ret;
}

#ifdef ERTS_DIRTY_SCHEDULERS

static int
dirty_send_message(Process *c_p, Eterm to, Eterm tag)
{
    ErtsProcLocks c_p_locks, rp_locks;
    Process *rp, *real_c_p;
    Eterm msg, *hp;
    ErlOffHeap *ohp;
    ErtsMessage *mp;

    ASSERT(is_immed(tag));

    real_c_p = erts_proc_shadow2real(c_p);
    if (real_c_p != c_p)
	c_p_locks = 0;
    else
	c_p_locks = ERTS_PROC_LOCK_MAIN;

    ASSERT(real_c_p->common.id == c_p->common.id);

    rp = erts_pid2proc_opt(real_c_p, c_p_locks,
			   to, 0,
			   ERTS_P2P_FLG_INC_REFC);

    if (!rp)
	return 0;

    rp_locks = 0;
    mp = erts_alloc_message_heap(rp, &rp_locks, 3, &hp, &ohp);

    msg = TUPLE2(hp, tag, c_p->common.id);
    erts_queue_message(rp, rp_locks, mp, msg, c_p->common.id);

    if (rp == real_c_p)
	rp_locks &= ~c_p_locks;
    if (rp_locks)
	erts_smp_proc_unlock(rp, rp_locks);

    erts_proc_dec_refc(rp);

    return 1;
}

static int
ms_wait(Process *c_p, Eterm etimeout, int busy)
{
    ErtsSchedulerData *esdp = erts_proc_sched_data(c_p);
    ErtsMonotonicTime time, timeout_time;
    Sint64 ms;

    if (!term_to_Sint64(etimeout, &ms))
	return 0;

    time = erts_get_monotonic_time(esdp);

    if (ms < 0)
	timeout_time = time;
    else
	timeout_time = time + ERTS_MSEC_TO_MONOTONIC(ms);

    while (time < timeout_time) {
	if (busy)
	    erts_thr_yield();
	else {
	    ErtsMonotonicTime timeout = timeout_time - time;

#ifdef __WIN32__
	    Sleep((DWORD) ERTS_MONOTONIC_TO_MSEC(timeout));
#else
	    {
		ErtsMonotonicTime to = ERTS_MONOTONIC_TO_USEC(timeout);
		struct timeval tv;

		tv.tv_sec = (long) to / (1000*1000);
		tv.tv_usec = (long) to % (1000*1000);

		select(0, NULL, NULL, NULL, &tv);
	    }
#endif
	}

	time = erts_get_monotonic_time(esdp);
    }
    return 1;
}

#endif /* ERTS_DIRTY_SCHEDULERS */

#ifdef ERTS_SMP
#  define ERTS_STACK_LIMIT ((char *) ethr_get_stacklimit())
#else
#  define ERTS_STACK_LIMIT ((char *) erts_scheduler_stack_limit)
#endif

/*
 * The below functions is for testing of the stack
 * limit functionality. They are intentionally
 * written body recursive in order to prevent
 * last call optimization...
 */

UWord
erts_check_stack_recursion_downwards(char *start_c)
{
    char *limit = ERTS_STACK_LIMIT;
    char c;
    UWord res;
    if (erts_check_below_limit(&c, limit + 1024))
        return (char *) erts_ptr_id(start_c) - (char *) erts_ptr_id(&c);
    res = erts_check_stack_recursion_downwards(start_c);
    erts_ptr_id(&c);
    return res;
}

UWord
erts_check_stack_recursion_upwards(char *start_c)
{
    char *limit = ERTS_STACK_LIMIT;
    char c;
    UWord res;
    if (erts_check_above_limit(&c, limit - 1024))
        return (char *) erts_ptr_id(&c) - (char *) erts_ptr_id(start_c);
    res = erts_check_stack_recursion_upwards(start_c);
    erts_ptr_id(&c);
    return res;
}

int
erts_is_above_stack_limit(char *ptr)
{
    return (char *) ptr > ERTS_STACK_LIMIT;
}
