/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2011. All Rights Reserved.
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
 * hipe_debug.c
 *
 * TODO:
 * - detect mode-switch native return addresses (ARCH-specific)
 * - map user-code native return addresses to symbolic names
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <stddef.h>	/* offsetof() */
#include <stdio.h>
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "beam_catches.h"
#include "beam_load.h"
#include "hipe_mode_switch.h"
#include "hipe_debug.h"

static const char dashes[2*sizeof(long)+5] = {
    [0 ... 2*sizeof(long)+3] = '-'
};

static const char dots[2*sizeof(long)+5] = {
    [0 ... 2*sizeof(long)+3] = '.'
};

static const char stars[2*sizeof(long)+5] = {
    [0 ... 2*sizeof(long)+3] = '*'
};

extern Uint beam_apply[];

static void print_beam_pc(BeamInstr *pc)
{
    if (pc == hipe_beam_pc_return) {
	printf("return-to-native");
    } else if (pc == hipe_beam_pc_throw) {
	printf("throw-to-native");
    } else if (pc == &beam_apply[1]) {
	printf("normal-process-exit");
    } else {
	BeamInstr *mfa = find_function_from_pc(pc);
	if (mfa)
	    erts_printf("%T:%T/%bpu + 0x%bpx",
			mfa[0], mfa[1], mfa[2], pc - &mfa[3]);
	else
	    printf("?");
    }
}

static void catch_slot(Eterm *pos, Eterm val)
{
    BeamInstr *pc = catch_pc(val);
    printf(" | 0x%0*lx | 0x%0*lx | CATCH 0x%0*lx (BEAM ",
	   2*(int)sizeof(long), (unsigned long)pos,
	   2*(int)sizeof(long), (unsigned long)val,
	   2*(int)sizeof(long), (unsigned long)pc);
    print_beam_pc(pc);
    printf(")\r\n");
}

static void print_beam_cp(Eterm *pos, Eterm val)
{
    printf(" |%s|%s| BEAM ACTIVATION RECORD\r\n", dashes, dashes);
    printf(" | 0x%0*lx | 0x%0*lx | BEAM PC ",
	   2*(int)sizeof(long), (unsigned long)pos,
	   2*(int)sizeof(long), (unsigned long)val);
    print_beam_pc(cp_val(val));
    printf("\r\n");
}

static void print_catch(Eterm *pos, Eterm val)
{
    printf(" |%s|%s| BEAM CATCH FRAME\r\n", dots, dots);
    catch_slot(pos, val);
    printf(" |%s|%s|\r\n", stars, stars);
}

static void print_stack(Eterm *sp, Eterm *end)
{
    printf(" | %*s | %*s |\r\n",
	   2+2*(int)sizeof(long), "Address",
	   2+2*(int)sizeof(long), "Contents");
    while (sp < end) {
	Eterm val = sp[0];
	if (is_CP(val))
	    print_beam_cp(sp, val);
	else if (is_catch(val))
	    print_catch(sp, val);
	else {
	    printf(" | 0x%0*lx | 0x%0*lx | ",
		   2*(int)sizeof(long), (unsigned long)sp,
		   2*(int)sizeof(long), (unsigned long)val);
	    erts_printf("%.30T", val);
	    printf("\r\n");
	}
	sp += 1;
    }
    printf(" |%s|%s|\r\n", dashes, dashes);
}

void hipe_print_estack(Process *p)
{
    printf(" |       BEAM  STACK       |\r\n");
    print_stack(p->stop, STACK_START(p));
}

static void print_heap(Eterm *pos, Eterm *end)
{
    printf("From: 0x%0*lx to 0x%0*lx\n\r",
	   2*(int)sizeof(long), (unsigned long)pos,
	   2*(int)sizeof(long), (unsigned long)end);
    printf(" |         H E A P         |\r\n");
    printf(" | %*s | %*s |\r\n",
	   2+2*(int)sizeof(long), "Address",
	   2+2*(int)sizeof(long), "Contents");
    printf(" |%s|%s|\r\n", dashes, dashes);
    while (pos < end) {
	Eterm val = pos[0];
	printf(" | 0x%0*lx | 0x%0*lx | ",
	       2*(int)sizeof(long), (unsigned long)pos,
	       2*(int)sizeof(long), (unsigned long)val);
	++pos;
	if (is_arity_value(val))
	    printf("Arity(%lu)", arityval(val));
	else if (is_thing(val)) {
	    unsigned int ari = thing_arityval(val);
	    printf("Thing Arity(%u) Tag(%lu)", ari, thing_subtag(val));
	    while (ari) {
		printf("\r\n | 0x%0*lx | 0x%0*lx | THING",
		       2*(int)sizeof(long), (unsigned long)pos,
		       2*(int)sizeof(long), (unsigned long)*pos);
		++pos;
		--ari;
	    }
	} else
	    erts_printf("%.30T", val);
	printf("\r\n");
    }
    printf(" |%s|%s|\r\n", dashes, dashes);
}

void hipe_print_heap(Process *p)
{
    print_heap(p->heap, p->htop);
}

void hipe_print_pcb(Process *p)
{
    printf("P: 0x%0*lx\r\n", 2*(int)sizeof(long), (unsigned long)p);
    printf("-----------------------------------------------\r\n");
    printf("Offset| Name        | Value      | *Value     |\r\n");
#define U(n,x) \
    printf(" % 4d | %s | 0x%0*lx |            |\r\n", (int)offsetof(Process,x), n, 2*(int)sizeof(long), (unsigned long)p->x)
#define P(n,x) \
    printf(" % 4d | %s | 0x%0*lx | 0x%0*lx |\r\n", (int)offsetof(Process,x), n, 2*(int)sizeof(long), (unsigned long)p->x, 2*(int)sizeof(long), p->x ? (unsigned long)*(p->x) : -1UL)

    U("htop       ", htop);
    U("hend       ", hend);
    U("heap       ", heap);
    U("heap_sz    ", heap_sz);
    U("stop       ", stop);
    U("gen_gcs    ", gen_gcs);
    U("max_gen_gcs", max_gen_gcs);
    U("high_water ", high_water);
    U("old_hend   ", old_hend);
    U("old_htop   ", old_htop);
    U("old_head   ", old_heap);
    U("min_heap_..", min_heap_size);
    U("status     ", status);
    U("rstatus    ", rstatus);
    U("rcount     ", rcount);
    U("id         ", id);
    U("prio       ", prio);
    U("reds       ", reds);
    U("tracer_pr..", tracer_proc);
    U("trace_fla..", trace_flags);
    U("group_lea..", group_leader);
    U("flags      ", flags);
    U("fvalue     ", fvalue);
    U("freason    ", freason);
    U("fcalls     ", fcalls);
    /*XXX: ErlTimer tm; */
    U("next       ", next);
    /*XXX: ErlOffHeap off_heap; */
    U("reg        ", reg);
    U("nlinks     ", nlinks);
    /*XXX: ErlMessageQueue msg; */
    U("mbuf       ", mbuf);
    U("mbuf_sz    ", mbuf_sz);
    U("dictionary ", dictionary);
    U("seq..clock ", seq_trace_clock);
    U("seq..astcnt", seq_trace_lastcnt);
    U("seq..token ", seq_trace_token);
    U("intial[0]  ", initial[0]);
    U("intial[1]  ", initial[1]);
    U("intial[2]  ", initial[2]);
    P("current    ", current);
    P("cp         ", cp);
    P("i          ", i);
    U("catches    ", catches);
    U("arity      ", arity);
    P("arg_reg    ", arg_reg);
    U("max_arg_reg", max_arg_reg);
    U("def..reg[0]", def_arg_reg[0]);
    U("def..reg[1]", def_arg_reg[1]);
    U("def..reg[2]", def_arg_reg[2]);
    U("def..reg[3]", def_arg_reg[3]);
    U("def..reg[4]", def_arg_reg[4]);
    U("def..reg[5]", def_arg_reg[5]);
#ifdef HIPE
    U("nsp        ", hipe.nsp);
    U("nstack     ", hipe.nstack);
    U("nstend     ", hipe.nstend);
    U("ncallee    ", hipe.ncallee);
    hipe_arch_print_pcb(&p->hipe);
#endif	/* HIPE */
#undef U
#undef P
    printf("-----------------------------------------------\r\n");
}
