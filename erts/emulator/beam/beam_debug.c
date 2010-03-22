/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2010. All Rights Reserved.
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

#ifdef ARCH_64
# define HEXF "%016bpX"
#else
# define HEXF "%08bpX"
#endif
#define TermWords(t) (((t) / (sizeof(BeamInstr)/sizeof(Eterm))) + !!((t) % (sizeof(BeamInstr)/sizeof(Eterm))))

void dbg_bt(Process* p, Eterm* sp);
void dbg_where(BeamInstr* addr, Eterm x0, Eterm* reg);

static void print_big(int to, void *to_arg, Eterm* addr);
static int print_op(int to, void *to_arg, int op, int size, BeamInstr* addr);
Eterm
erts_debug_same_2(Process* p, Eterm term1, Eterm term2)
{
    return (term1 == term2) ? am_true : am_false;
}

Eterm
erts_debug_flat_size_1(Process* p, Eterm term)
{
    Uint size = size_object(term);

    if (IS_USMALL(0, size)) {
	BIF_RET(make_small(size));
    } else {
	Eterm* hp = HAlloc(p, BIG_UINT_HEAP_SIZE);
	BIF_RET(uint_to_big(size, hp));
    }
}

Eterm
erts_debug_breakpoint_2(Process* p, Eterm MFA, Eterm bool)
{
    Eterm* tp;
    Eterm mfa[3];
    int i;
    int specified = 0;
    Eterm res;

    if (bool != am_true && bool != am_false)
	goto error;

    if (is_not_tuple(MFA)) {
	goto error;
    }
    tp = tuple_val(MFA);
    if (*tp != make_arityval(3)) {
	goto error;
    }
    mfa[0] = tp[1];
    mfa[1] = tp[2];
    mfa[2] = tp[3];
    if (!is_atom(mfa[0]) || !is_atom(mfa[1]) ||
	(!is_small(mfa[2]) && mfa[2] != am_Underscore)) {
	goto error;
    }
    for (i = 0; i < 3 && mfa[i] != am_Underscore; i++, specified++) {
	/* Empty loop body */
    }
    for (i = specified; i < 3; i++) {
	if (mfa[i] != am_Underscore) {
	    goto error;
	}
    }
    if (is_small(mfa[2])) {
	mfa[2] = signed_val(mfa[2]);
    }

    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
    erts_smp_block_system(0);

    if (bool == am_true) {
	res = make_small(erts_set_debug_break(mfa, specified));
    } else {
	res = make_small(erts_clear_debug_break(mfa, specified));
    }

    erts_smp_release_system();
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);

    return res;

 error:
    BIF_ERROR(p, BADARG);
}

#if 0 /* XXX:PaN - not used */
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

Eterm
erts_debug_disassemble_1(Process* p, Eterm addr)
{
    erts_dsprintf_buf_t *dsbufp;
    Eterm* hp;
    Eterm* tp;
    Eterm bin;
    Eterm mfa;
    BeamInstr* funcinfo = NULL;	/* Initialized to eliminate warning. */
    BeamInstr* code_base;
    BeamInstr* code_ptr = NULL;	/* Initialized to eliminate warning. */
    BeamInstr instr;
    BeamInstr uaddr;
    Uint hsz;
    int i;

    if (term_to_UWord(addr, &uaddr)) {
	code_ptr = (BeamInstr *) uaddr;
	if ((funcinfo = find_function_from_pc(code_ptr)) == NULL) {
	    BIF_RET(am_false);
	}
    } else if (is_tuple(addr)) {
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
	modp = erts_get_module(mod);

	/*
	 * Try the export entry first to allow disassembly of special functions
	 * such as erts_debug:apply/4.  Then search for it in the module.
	 */

	if ((ep = erts_find_function(mod, name, arity)) != NULL) {
	    /* XXX: add "&& ep->address != ep->code+3" condition?
	     * Consider a traced function.
	     * Its ep will have ep->address == ep->code+3.
	     * erts_find_function() will return the non-NULL ep.
	     * Below we'll try to derive a code_ptr from ep->address.
	     * But this code_ptr will point to the start of the Export,
	     * not the function's func_info instruction. BOOM !?
	     */
	    code_ptr = ((BeamInstr *) ep->address) - 5;
	    funcinfo = code_ptr+2;
	} else if (modp == NULL || (code_base = modp->code) == NULL) {
	    BIF_RET(am_undef);
	} else {
	    n = code_base[MI_NUM_FUNCTIONS];
	    for (i = 0; i < n; i++) {
		code_ptr = (BeamInstr *) code_base[MI_FUNCTIONS+i];
		if (code_ptr[3] == name && code_ptr[4] == arity) {
		    funcinfo = code_ptr+2;
		    break;
		}
	    }
	    if (i == n) {
		BIF_RET(am_undef);
	    }
	}
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
    bin = new_binary(p, (byte *) dsbufp->str, (int) dsbufp->str_len);
    erts_destroy_tmp_dsbuf(dsbufp);
    hsz = 4+4;
    (void) erts_bld_uword(NULL, &hsz, (BeamInstr) code_ptr);
    hp = HAlloc(p, hsz);
    addr = erts_bld_uword(&hp, NULL, (BeamInstr) code_ptr);
    ASSERT(is_atom(funcinfo[0]));
    ASSERT(is_atom(funcinfo[1]));
    mfa = TUPLE3(hp, (Eterm) funcinfo[0], (Eterm) funcinfo[1], make_small((Eterm) funcinfo[2]));
    hp += 4;
    return TUPLE3(hp, addr, bin, mfa);
}

void
dbg_bt(Process* p, Eterm* sp)
{
    Eterm* stack = STACK_START(p);

    while (sp < stack) {
	if (is_CP(*sp)) {
	    BeamInstr* addr = find_function_from_pc(cp_val(*sp));
	    if (addr)
		erts_fprintf(stderr,
			     HEXF ": %T:%T/%bpu\n",
			     addr, (Eterm) addr[0], (Eterm) addr[1], (Uint) addr[2]);
	}
	sp++;
    }
}

void
dbg_where(BeamInstr* addr, Eterm x0, Eterm* reg)
{
    BeamInstr* f = find_function_from_pc(addr);

    if (f == NULL) {
	erts_fprintf(stderr, "???\n");
    } else {
	int arity;
	int i;

	addr = f;
	arity = addr[2];
	erts_fprintf(stderr, HEXF ": %T:%T(", addr, (Eterm) addr[0], (Eterm) addr[1]);
	for (i = 0; i < arity; i++)
	    erts_fprintf(stderr, i ? ", %T" : "%T", i ? reg[i] : x0);
	erts_fprintf(stderr, ")\n");
    }
}

static int
print_op(int to, void *to_arg, int op, int size, BeamInstr* addr)
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
		*ap++ = packed & (BEAM_TIGHT_MASK / sizeof(Eterm));
		packed >>= BEAM_TIGHT_SHIFT;
		break;
	    case '6':		/* Shift 16 steps */
		*ap++ = packed & BEAM_LOOSE_MASK;
		packed >>= BEAM_LOOSE_SHIFT;
		break;
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
	    erts_print(to, to_arg, "x(0)");
	    break;
	case 'x':		/* x(N) */
	    if (reg_index(ap[0]) == 0) {
		erts_print(to, to_arg, "X[0]");
	    } else {
		erts_print(to, to_arg, "x(%d)", reg_index(ap[0]));
	    }
	    ap++;
	    break;
	case 'y':		/* y(N) */
	    erts_print(to, to_arg, "y(%d)", reg_index(ap[0]) - CP_SIZE);
	    ap++;
	    break;
	case 'n':		/* Nil */
	    erts_print(to, to_arg, "[]");
	    break;
	case 's':		/* Any source (tagged constant or register) */
	    tag = beam_reg_tag(*ap);
	    if (tag == X_REG_DEF) {
		if (reg_index(*ap) == 0) {
		    erts_print(to, to_arg, "x[0]");
		} else {
		    erts_print(to, to_arg, "x(%d)", reg_index(*ap));
		}
		ap++;
		break;
	    } else if (tag == Y_REG_DEF) {
		erts_print(to, to_arg, "y(%d)", reg_index(*ap) - CP_SIZE);
		ap++;
		break;
	    } else if (tag == R_REG_DEF) {
		erts_print(to, to_arg, "x(0)");
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
	    switch (beam_reg_tag(*ap)) {
	    case X_REG_DEF:
		if (reg_index(*ap) == 0) {
		    erts_print(to, to_arg, "x[0]");
		} else {
		    erts_print(to, to_arg, "x(%d)", reg_index(*ap));
		}
		break;
	    case Y_REG_DEF:
		erts_print(to, to_arg, "y(%d)", reg_index(*ap) - CP_SIZE);
		break;
	    case R_REG_DEF:
		erts_print(to, to_arg, "x(0)");
		break;
	    }
	    ap++;
	    break;
	case 'I':		/* Untagged integer. */
	case 't':
	    erts_print(to, to_arg, "%d", *ap);
	    ap++;
	    break;
	case 'f':		/* Destination label */
	    {
		BeamInstr* f = find_function_from_pc((BeamInstr *)*ap);
		if (f+3 != (BeamInstr *) *ap) {
		    erts_print(to, to_arg, "f(" HEXF ")", *ap);
		} else {
		    erts_print(to, to_arg, "%T:%T/%bpu", (Eterm) f[0], (Eterm) f[1], (Eterm) f[2]);
		}
		ap++;
	    }
	    break;
	case 'p':		/* Pointer (to label) */
	    {
		BeamInstr* f = find_function_from_pc((BeamInstr *)*ap);
		if (f+3 != (BeamInstr *) *ap) {
		    erts_print(to, to_arg, "p(" HEXF ")", *ap);
		} else {
		    erts_print(to, to_arg, "%T:%T/%bpu", (Eterm) f[0], (Eterm) f[1], (Eterm) f[2]);
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
			   "%T:%T/%bpu", (Eterm) ex->code[0], (Eterm) ex->code[1], (Uint) ex->code[2]);
		ap++;
	    }
	    break;
	case 'F':		/* Function definition */
	    break;
	case 'b':
	    for (i = 0; i < BIF_SIZE; i++) {
		BifFunction bif = (BifFunction) *ap;
		if (bif == bif_table[i].f) {
		    break;
		}
	    }
	    if (i == BIF_SIZE) {
		erts_print(to, to_arg, "b(%d)", (Uint) *ap);
	    } else {
		Eterm name = bif_table[i].name;
		unsigned arity = bif_table[i].arity;
		erts_print(to, to_arg, "%T/%u", name, arity);
	    }
	    ap++;
	    break;
	case 'P':	/* Byte offset into tuple (see beam_load.c) */
	    erts_print(to, to_arg, "%d", (*ap / sizeof(Eterm)) - 1);
	    ap++;
	    break;
	case 'l':		/* fr(N) */
	    erts_print(to, to_arg, "fr(%d)", reg_index(ap[0]));
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

    ap = addr + size;
    switch (op) {
    case op_i_select_val_sfI:
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
    case op_i_jump_on_val_sfII:
	{
	    int n;
	    for (n = ap[-2]; n > 0; n--) {
		erts_print(to, to_arg, "f(" HEXF ") ", ap[0]);
		ap++;
		size++;
	    }
	}
	break;
    case op_i_select_big_sf:
	while (ap[0]) {
	    Eterm *bigp = (Eterm *) ap;
	    int arity = thing_arityval(*bigp);
	    print_big(to, to_arg, bigp);
	    size += TermWords(arity+1);
	    ap += TermWords(arity+1);
	    erts_print(to, to_arg, " f(" HEXF ") ", ap[0]);
	    ap++;
	    size++;
	}
	ap++;
	size++;
	break;
    }
    erts_print(to, to_arg, "\n");

    return size;
}

static void
print_big(int to, void *to_arg, Eterm* addr)
{
    int i;
    int k;

    i = BIG_SIZE(addr);
    if (BIG_SIGN(addr))
	erts_print(to, to_arg, "-#integer(%d) = {", i);
    else
	erts_print(to, to_arg, "#integer(%d) = {", i);
    erts_print(to, to_arg, "0x%x", BIG_DIGIT(addr, 0));
    for (k = 1; k < i; k++)
	erts_print(to, to_arg, ",0x%x", BIG_DIGIT(addr, k));
    erts_print(to, to_arg, "}");
}
