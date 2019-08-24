/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2018. All Rights Reserved.
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
#include "erl_map.h"

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
	ErtsCodeMFA *cmfa = find_function_from_pc(pc);
	if (cmfa) {
	    fflush(stdout);
	    erts_printf("%T:%T/%bpu + 0x%bpx",
			cmfa->module, cmfa->function,
                        cmfa->arity,
                        pc - erts_codemfa_to_code(cmfa));
	} else
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
	    fflush(stdout);
	    erts_printf("%.30T", val);
	    printf("\r\n");
	}
	sp += 1;
    }
    printf(" |%s|%s|\r\n", dashes, dashes);
}

void hipe_print_estack(Process *p)
{
    printf(" | %*s BEAM   STACK %*s |\r\n",
	   2*(int)sizeof(long)-3, "",
	   2*(int)sizeof(long)-4, "");
    print_stack(p->stop, STACK_START(p));
}

static void print_heap(Eterm *pos, Eterm *end)
{
    printf("From: 0x%0*lx to 0x%0*lx\n\r",
	   2*(int)sizeof(long), (unsigned long)pos,
	   2*(int)sizeof(long), (unsigned long)end);
    printf(" | %*s H E A P %*s |\r\n",
	   2*(int)sizeof(long)-1, "",
	   2*(int)sizeof(long)-1, "");
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
	} else {
	    fflush(stdout);
	    erts_printf("%.30T", val);
	}
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
    printf("%.*s\r\n",
           6+1+13+1+2*(int)sizeof(long)+4+1+2*(int)sizeof(long)+4+1,
           "---------------------------------------------------------------");
    printf("Offset| Name        | Value %*s | *Value %*s |\r\n",
           2*(int)sizeof(long)-4, "",
           2*(int)sizeof(long)-5, "");
#undef U
#define U(n,x) \
    printf(" % 4d | %s | 0x%0*lx | %*s |\r\n", (int)offsetof(Process,x), n, 2*(int)sizeof(long), (unsigned long)p->x, 2*(int)sizeof(long)+2, "")
#undef P
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
    U("rcount     ", rcount);
    U("id         ", common.id);
    U("reds       ", reds);
    U("tracer     ", common.tracer);
    U("trace_fla..", common.trace_flags);
    U("group_lea..", group_leader);
    U("flags      ", flags);
    U("fvalue     ", fvalue);
    U("freason    ", freason);
    U("fcalls     ", fcalls);
    /*XXX: ErlTimer tm; */
    U("next       ", next);
    /*XXX: ErlOffHeap off_heap; */
    U("reg        ", common.u.alive.reg);
    U("nlinks     ", common.u.alive.links);
    /*XXX: ErlMessageQueue msg; */
    U("mbuf       ", mbuf);
    U("mbuf_sz    ", mbuf_sz);
    U("dictionary ", dictionary);
    U("seq..clock ", seq_trace_clock);
    U("seq..astcnt", seq_trace_lastcnt);
    U("seq..token ", seq_trace_token);
    U("intial.mod ", u.initial.module);
    U("intial.fun ", u.initial.function);
    U("intial.ari ", u.initial.arity);
    U("current    ", current);
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
    U("ncallee    ", hipe.u.ncallee);
    hipe_arch_print_pcb(&p->hipe);
#endif	/* HIPE */
#undef U
#undef P
    printf("%.*s\r\n",
           6+1+14+1+2*(int)sizeof(long)+4+1+2*(int)sizeof(long)+4+1,
           "---------------------------------------------------------------");
}
