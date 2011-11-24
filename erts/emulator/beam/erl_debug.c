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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "erl_nmgc.h"
#include "big.h"
#include "bif.h"
#include "beam_catches.h"
#include "erl_debug.h"

#define WITHIN(ptr, x, y) ((x) <= (ptr) && (ptr) < (y))

#if defined(HYBRID)
#if defined(INCREMENTAL)
/* Hybrid + Incremental */
#define IN_HEAP(p, ptr)                                                 \
    (WITHIN((ptr), p->heap, p->hend) ||                                 \
     (OLD_HEAP(p) && WITHIN((ptr), OLD_HEAP(p), OLD_HEND(p))) ||        \
     WITHIN((ptr), global_heap, global_hend) ||                         \
     (inc_fromspc && WITHIN((ptr), inc_fromspc, inc_fromend)) ||        \
     WITHIN((ptr), global_old_heap, global_old_hend))

#define IN_MA(ptr)                                                      \
    (WITHIN((ptr), global_heap, global_hend) ||                         \
     (inc_fromspc && WITHIN((ptr), inc_fromspc, inc_fromend)) ||        \
     WITHIN((ptr), global_old_heap, global_old_hend))
#else
/* Hybrid */
#define IN_HEAP(p, ptr)                                                 \
    (WITHIN((ptr), p->heap, p->hend) ||                                 \
     (OLD_HEAP(p) && WITHIN((ptr), OLD_HEAP(p), OLD_HEND(p))) ||        \
     WITHIN((ptr), global_heap, global_hend) ||                         \
     (global_old_heap && WITHIN((ptr),global_old_heap,global_old_hend)))
#endif
#else
/* Private */
#define IN_HEAP(p, ptr)                                                 \
    (WITHIN((ptr), p->heap, p->hend) ||                                 \
     (OLD_HEAP(p) && WITHIN((ptr), OLD_HEAP(p), OLD_HEND(p))))
#endif


#ifdef __GNUC__
/*
 * Does not work in Microsoft C. Since this is debugging code that will
 * hardly be used on Windows, get rid of it unless we have Gnu compiler.
 */
#define PTR_SIZE 2*(int)sizeof(long)

static const char dashes[PTR_SIZE+3] = {
    [0 ... PTR_SIZE+1] = '-'
};
#endif

#if defined(DEBUG) && defined(__GNUC__)

/*
 * This file defines functions for use within a debugger like gdb
 * and the declarations below is just to make gcc quiet.
 */

void pps(Process*, Eterm*);
void ptd(Process*, Eterm);
void paranoid_display(int, void*, Process*, Eterm);
static int dcount;

static int pdisplay1(int to, void *to_arg, Process* p, Eterm obj);

void ptd(Process* p, Eterm x) 
{
    pdisplay1(ERTS_PRINT_STDERR, NULL, p, x);
    erts_putc(ERTS_PRINT_STDERR, NULL, '\n');
}

/*
 * Paranoid version of display which doesn't crasch as easily if there
 * are errors in the data structures.
 */

void
paranoid_display(int to, void *to_arg, Process* p, Eterm obj)
{
    dcount = 100000;
    pdisplay1(to, to_arg, p, obj);
}

static int
pdisplay1(int to, void *to_arg, Process* p, Eterm obj)
{
    int i, k;
    Eterm* nobj;

    if (dcount-- <= 0)
	return(1);

    if (is_CP(obj)) {
	erts_print(to, to_arg, "<cp/header:%0*lX",PTR_SIZE,obj);
	return 0;
    }

    switch (tag_val_def(obj)) {
    case NIL_DEF:
	erts_print(to, to_arg, "[]");
	break;
    case ATOM_DEF:
	erts_print(to, to_arg, "%T", obj);
	break;
    case SMALL_DEF:
	erts_print(to, to_arg, "%ld", signed_val(obj));
	break;

    case BIG_DEF:
	nobj = big_val(obj);
	if (!IN_HEAP(p, nobj)) {
	    erts_print(to, to_arg, "#<bad big %X>#", obj);
	    return 1;
	}

	i = BIG_SIZE(nobj);
	if (BIG_SIGN(nobj))
	    erts_print(to, to_arg, "-#integer(%d) = {", i);
	else
	    erts_print(to, to_arg, "#integer(%d) = {", i);
	erts_print(to, to_arg, "%d", BIG_DIGIT(nobj, 0));
	for (k = 1; k < i; k++)
	    erts_print(to, to_arg, ",%d", BIG_DIGIT(nobj, k));
	erts_putc(to, to_arg, '}');
	break;
    case REF_DEF:
    case EXTERNAL_REF_DEF: {
	Uint32 *ref_num;
	erts_print(to, to_arg, "#Ref<%lu", ref_channel_no(obj));
	ref_num = ref_numbers(obj);
	for (i = ref_no_of_numbers(obj)-1; i >= 0; i--)
	    erts_print(to, to_arg, ",%lu", ref_num[i]);
	erts_print(to, to_arg, ">");
	break;
    }
    case PID_DEF:
    case EXTERNAL_PID_DEF:
	erts_print(to, to_arg, "<%lu.%lu.%lu>",
		   pid_channel_no(obj),
		   pid_number(obj),
		   pid_serial(obj));
	break;
    case PORT_DEF:
    case EXTERNAL_PORT_DEF:
	erts_print(to, to_arg, "#Port<%lu.%lu>",
		   port_channel_no(obj),
		   port_number(obj));
	break;
    case LIST_DEF:
	erts_putc(to, to_arg, '[');
	nobj = list_val(obj);
	while (1) {
	    if (!IN_HEAP(p, nobj)) {
		erts_print(to, to_arg, "#<bad list %X>", obj);
		return 1;
	    }
	    if (pdisplay1(to, to_arg, p, *nobj++) != 0)
		return(1);
	    if (is_not_list(*nobj))
		break;
	    erts_putc(to, to_arg, ',');
	    nobj = list_val(*nobj);
	}
	if (is_not_nil(*nobj)) {
	    erts_putc(to, to_arg, '|');
	    if (pdisplay1(to, to_arg, p, *nobj) != 0)
		return(1);
	}
	erts_putc(to, to_arg, ']');
	break;
    case TUPLE_DEF:
	nobj = tuple_val(obj);	/* pointer to arity */
	i = arityval(*nobj);	/* arity */
	erts_putc(to, to_arg, '{');
	while (i--) {
	    if (pdisplay1(to, to_arg, p, *++nobj) != 0) return(1);
	    if (i >= 1) erts_putc(to, to_arg, ',');
	}
	erts_putc(to, to_arg, '}');
	break;
    case FLOAT_DEF: {
	    FloatDef ff;
	    GET_DOUBLE(obj, ff);
	    erts_print(to, to_arg, "%.20e", ff.fd);
	}
	break;
    case BINARY_DEF:
	erts_print(to, to_arg, "#Bin");
	break;
    default:
	erts_print(to, to_arg, "unknown object %x", obj);
    }
    return(0);
}

void
pps(Process* p, Eterm* stop)
{
    int to = ERTS_PRINT_STDOUT;
    void *to_arg = NULL;
    Eterm* sp = STACK_START(p) - 1;

    if (stop <= STACK_END(p)) {
        stop = STACK_END(p) + 1;
    }

    while(sp >= stop) {
	erts_print(to, to_arg, "%0*lx: ", PTR_SIZE, (UWord) sp);
	if (is_catch(*sp)) {
	    erts_print(to, to_arg, "catch %ld", (UWord)catch_pc(*sp));
	} else {
	    paranoid_display(to, to_arg, p, *sp);
	}
	erts_putc(to, to_arg, '\n');
	sp--;
    }
}

#endif /* DEBUG */

static int verify_eterm(Process *p,Eterm element);
static int verify_eterm(Process *p,Eterm element)
{
    Eterm *ptr;
    ErlHeapFragment* mbuf;

    switch (primary_tag(element)) {
        case TAG_PRIMARY_LIST: ptr = list_val(element); break;
        case TAG_PRIMARY_BOXED: ptr = boxed_val(element); break;
        default: /* Immediate or header/cp */ return 1;
    }

    if (p) {
        if (IN_HEAP(p, ptr))
            return 1;

        for (mbuf = p->mbuf; mbuf; mbuf = mbuf->next) {
            if (WITHIN(ptr, &mbuf->mem[0], &mbuf->mem[0] + mbuf->used_size)) {
                return 1;
            }
        }
    }
#ifdef INCREMENTAL
    else {
        if (IN_MA(ptr))
            return 1;
    }
#endif

    return 0;
}

void erts_check_stack(Process *p)
{
    Eterm *elemp;
    Eterm *stack_start = p->heap + p->heap_sz;
    Eterm *stack_end = p->htop;

    if (p->stop > stack_start)
	erl_exit(1,
		 "<%lu.%lu.%lu>: Stack underflow\n",
		 internal_pid_channel_no(p->id),
		 internal_pid_number(p->id),
		 internal_pid_serial(p->id));

    if (p->stop < stack_end)
	erl_exit(1,
		 "<%lu.%lu.%lu>: Stack overflow\n",
		 internal_pid_channel_no(p->id),
		 internal_pid_number(p->id),
		 internal_pid_serial(p->id));

    for (elemp = p->stop; elemp < stack_start; elemp++) {
	int in_mbuf = 0;
	Eterm *ptr;
	ErlHeapFragment* mbuf;
	switch (primary_tag(*elemp)) {
	case TAG_PRIMARY_LIST: ptr = list_val(*elemp); break;
	case TAG_PRIMARY_BOXED: ptr = boxed_val(*elemp); break;
	default: /* Immediate or cp */ continue;
	}
	if (IN_HEAP(p, ptr))
	    continue;
	for (mbuf = p->mbuf; mbuf; mbuf = mbuf->next)
	    if (WITHIN(ptr, &mbuf->mem[0], &mbuf->mem[0] + mbuf->used_size)) {
		in_mbuf = 1;
		break;
	    }
	if (in_mbuf)
	    continue;

	erl_exit(1,
		 "<%lu.%lu.%lu>: Wild stack pointer\n",
		 internal_pid_channel_no(p->id),
		 internal_pid_number(p->id),
		 internal_pid_serial(p->id));
    }

}

#if defined(CHECK_FOR_HOLES)
static void check_memory(Eterm *start, Eterm *end);

void erts_check_for_holes(Process* p)
{
    ErlHeapFragment* hf;
    Eterm* start;

    start = p->last_htop ? p->last_htop : HEAP_START(p);
    check_memory(start, HEAP_TOP(p));
    p->last_htop = HEAP_TOP(p);

    for (hf = MBUF(p); hf != 0; hf = hf->next) {
	if (hf == p->last_mbuf) {
	    break;
	}
	check_memory(hf->mem, hf->mem+hf->used_size);
    }
    p->last_mbuf = MBUF(p);
}

static void check_memory(Eterm *start, Eterm *end)
{
    Eterm *pos = start;

    while (pos < end) {
        Eterm hval = *pos++;

        if (hval == ERTS_HOLE_MARKER) {
            erts_fprintf(stderr,"%s, line %d: ERTS_HOLE_MARKER found at 0x%0*lx\n",
                         __FILE__, __LINE__,PTR_SIZE,(unsigned long)(pos-1));
            print_untagged_memory(start,end); /* DEBUGSTUFF */
	    abort();
	} else if (is_thing(hval)) {
	    pos += (thing_arityval(hval));
	}
    }
}
#endif

#ifdef __GNUC__

/*
 * erts_check_heap and erts_check_memory will run through the heap
 * silently if everything is ok.  If there are strange (untagged) data
 * in the heap or wild pointers, the system will be halted with an
 * error message.
 */
void erts_check_heap(Process *p)
{
    ErlHeapFragment* bp = MBUF(p);

    erts_check_memory(p,HEAP_START(p),HEAP_TOP(p));
    if (OLD_HEAP(p) != NULL) {
        erts_check_memory(p,OLD_HEAP(p),OLD_HTOP(p));
    }

    while (bp) {
	erts_check_memory(p,bp->mem,bp->mem + bp->used_size);
        bp = bp->next;
    }
}

void erts_check_memory(Process *p, Eterm *start, Eterm *end)
{
    Eterm *pos = start;

    while (pos < end) {
        Eterm hval = *pos++;

#ifdef DEBUG
        if (hval == DEBUG_BAD_WORD) {
            print_untagged_memory(start, end);
            erl_exit(1, "Uninitialized HAlloc'ed memory found @ 0x%0*lx!\n",
                     PTR_SIZE,(unsigned long)(pos - 1));
        }
#endif

        if (is_thing(hval)) {
            pos += thing_arityval(hval);
            continue;
        }

        if (verify_eterm(p,hval))
            continue;

        erl_exit(1, "Wild pointer found @ 0x%0*lx!\n",
                 PTR_SIZE,(unsigned long)(pos - 1));
    }
}

void verify_process(Process *p)
{
#define VERIFY_AREA(name,ptr,sz) {                                      \
    int n = (sz);							\
    while (n--) if(!verify_eterm(p,*(ptr+n)))				\
        erl_exit(1,"Wild pointer found in " name " of %T!\n",p->id); }

#define VERIFY_ETERM(name,eterm) {                                      \
    if(!verify_eterm(p,eterm))                                          \
        erl_exit(1,"Wild pointer found in " name " of %T!\n",p->id); }


    ErlMessage* mp = p->msg.first;

    VERBOSE(DEBUG_MEMORY,("Verify process: %T...\n",p->id));

    while (mp != NULL) {
        VERIFY_ETERM("message term",ERL_MESSAGE_TERM(mp));
        VERIFY_ETERM("message token",ERL_MESSAGE_TOKEN(mp));
        mp = mp->next;
    }

    erts_check_stack(p);
    erts_check_heap(p);

    if (p->dictionary)
        VERIFY_AREA("dictionary",p->dictionary->data, p->dictionary->used);
    VERIFY_ETERM("seq trace token",p->seq_trace_token);
    VERIFY_ETERM("group leader",p->group_leader);
    VERIFY_ETERM("fvalue",p->fvalue);
    VERIFY_ETERM("ftrace",p->ftrace);

#ifdef HYBRID
    VERIFY_AREA("rrma",p->rrma,p->nrr);
#endif

    VERBOSE(DEBUG_MEMORY,("...done\n"));

#undef VERIFY_AREA
#undef VERIFY_ETERM
}

void verify_everything()
{
#ifdef HYBRID
    Uint i;
    Uint n = erts_num_active_procs;

#ifdef INCREMENTAL_FREE_SIZES_NEEDS_TO_BE_TAGGED_AS_HEADERS_WITH_ARITY
    INC_Page *page = inc_used_mem;
#endif

    for (i = 0; i < n; i++) {
	verify_process(erts_active_procs[i]);
    }

    erts_check_memory(NULL,global_heap,global_htop);

#ifdef INCREMENTAL_FREE_SIZES_NEEDS_TO_BE_TAGGED_AS_HEADERS_WITH_ARITY
    while (page)
    {
	Eterm *end = page + INC_PAGE_SIZE;
	Eterm *pos = page->start;

	while( pos <  end) {
	    Eterm val = *pos++;
	    if(is_header(val))
		pos += thing_arityval(val);
	    else
		verify_eterm(NULL,val);
	}
	page = page->next;
    }
#endif
#endif /* HYBRID */
}

/*
 * print_untagged_memory will print the contents of given memory area.
 */
void print_untagged_memory(Eterm *pos, Eterm *end)
{
    int i = 0;
    erts_printf("| %*s   | Range: 0x%0*lx - 0x%0*lx%*s|\n",
                PTR_SIZE, "",
                PTR_SIZE,(unsigned long)pos,
                PTR_SIZE,(unsigned long)(end - 1),2 * PTR_SIZE - 2,"");
    erts_printf("| %-*s | %-*s |\n",PTR_SIZE+2,"Address",
                4*PTR_SIZE+11,"Contents");
    erts_printf("|-%s-|-%s-%s-%s-%s-|\n",dashes,dashes,dashes,dashes,dashes);
    while( pos < end ) {
        if (i == 0)
            erts_printf("| 0x%0*lx | ", PTR_SIZE, (unsigned long)pos);
        erts_printf("0x%0*lx ",PTR_SIZE,(unsigned long)*pos);
        pos++; i++;
        if (i == 4) {
            erts_printf("|\n");
            i = 0;
        }
    }
    while (i && i < 4) {
        erts_printf("%*s",PTR_SIZE+3,"");
        i++;
    }
    if (i != 0)
        erts_printf("|\n");
    erts_printf("+-%s-+-%s-%s-%s-%s-+\n",dashes,dashes,dashes,dashes,dashes);
}

/*
 * print_tagged_memory will print contents of given memory area and
 * display it as if it was tagged Erlang terms (which it hopefully
 * is).  This function knows about forwarding pointers to be able to
 * print a heap during garbage collection. erts_printf("%T",val)
 * do not know about forwarding pointers though, so it will still
 * crash if they are encoutered...
 */
void print_tagged_memory(Eterm *pos, Eterm *end)
{
    erts_printf("+-%s-+-%s-+\n",dashes,dashes);
    erts_printf("| 0x%0*lx - 0x%0*lx |\n",
                PTR_SIZE,(unsigned long)pos,
                PTR_SIZE,(unsigned long)(end - 1));
    erts_printf("| %-*s   | %-*s   |\n",PTR_SIZE,"Address",PTR_SIZE,"Contents");
    erts_printf("|-%s-|-%s-|\n",dashes,dashes);
    while( pos < end ) {
	Eterm val = pos[0];
	erts_printf("| 0x%0*lx | 0x%0*lx | ",
                    PTR_SIZE,(unsigned long)pos, PTR_SIZE,(unsigned long)val);
	++pos;
        if( is_arity_value(val) ) {
	    erts_printf("Arity(%lu)", arityval(val));
	} else if( is_thing(val) ) {
	    unsigned int ari = thing_arityval(val);
	    erts_printf("Thing Arity(%u) Tag(%lu)", ari, thing_subtag(val));
	    while( ari ) {
		erts_printf("\n| 0x%0*lx | 0x%0*lx | THING",
                            PTR_SIZE, (unsigned long)pos,
                            PTR_SIZE, (unsigned long)*pos);
		++pos;
		--ari;
	    }
	} else {
            switch (primary_tag(val)) {
            case TAG_PRIMARY_BOXED:
                if (!is_header(*boxed_val(val))) {
                    erts_printf("Moved -> 0x%0*lx\n",PTR_SIZE,
                                (unsigned long)*boxed_val(val));
                    continue;
                }
                break;

            case TAG_PRIMARY_LIST:
                if (is_non_value(*list_val(val))) {
                    erts_printf("Moved -> 0x%0*lx\n",PTR_SIZE,
                                (unsigned long)*(list_val(val) + 1));
                    continue;
                }
                break;
            }
            erts_printf("%.30T", val);
        }
	erts_printf("\n");
    }
    erts_printf("+-%s-+-%s-+\n",dashes,dashes);
}

#ifdef HYBRID
void print_ma_info(void)
{
    erts_printf("Message Area (start - top - end): "
                "0x%0*lx - 0x%0*lx - 0x%0*lx\n",
                PTR_SIZE, (unsigned long)global_heap,
                PTR_SIZE, (unsigned long)global_htop,
                PTR_SIZE, (unsigned long)global_hend);
#ifndef INCREMENTAL
    erts_printf("  High water: 0x%0*lx   "
                "Old gen: 0x%0*lx - 0x%0*lx - 0x%0*lx\n",
                PTR_SIZE, (unsigned long)global_high_water,
                PTR_SIZE, (unsigned long)global_old_heap,
                PTR_SIZE, (unsigned long)global_old_htop,
                PTR_SIZE, (unsigned long)global_old_hend);
#endif
}

void print_message_area(void)
{
    Eterm *pos = global_heap;
    Eterm *end = global_htop;

    erts_printf("From: 0x%0*lx  to  0x%0*lx\n",
                PTR_SIZE,(unsigned long)pos,PTR_SIZE,(unsigned long)end);
    erts_printf("(Old generation: 0x%0*lx  to 0x%0*lx\n",
                PTR_SIZE, (unsigned long)global_old_heap,
                PTR_SIZE, (unsigned long)global_old_hend);
    erts_printf("| %-*s | %-*s |\n",PTR_SIZE,"Address",PTR_SIZE,"Contents");
    erts_printf("|-%s-|-%s-|\n",dashes,dashes);
    while( pos < end ) {
	Eterm val = pos[0];
	erts_printf("| 0x%0*lx | 0x%0*lx | ",
                    PTR_SIZE,(unsigned long)pos,PTR_SIZE,(unsigned long)val);
	++pos;
	if( is_arity_value(val) ) {
	    erts_printf("Arity(%lu)", arityval(val));
	} else if( is_thing(val) ) {
	    unsigned int ari = thing_arityval(val);
	    erts_printf("Thing Arity(%u) Tag(%lu)", ari, thing_subtag(val));
	    while( ari ) {
		erts_printf("\n| 0x%0*lx | 0x%0*lx | THING",
                            PTR_SIZE, (unsigned long)pos,
                            PTR_SIZE, (unsigned long)*pos);
		++pos;
		--ari;
	    }
	} else
	    erts_printf("%.30T", val);
	erts_printf("\n");
    }
    erts_printf("+-%s-+-%s-+\n",dashes,dashes);
}

void check_message_area()
{
    Eterm *pos = global_heap;
    Eterm *end = global_htop;

    while( pos < end ) {
	Eterm val = *pos++;
	if(is_header(val))
	    pos += thing_arityval(val);
	else if(!is_immed(val))
	    if ((ptr_val(val) < global_heap || ptr_val(val) >= global_htop) &&
                (ptr_val(val) < global_old_heap ||
                 ptr_val(val) >= global_old_hend))
            {
		erts_printf("check_message_area: Stray pointer found\n");
                print_message_area();
                erts_printf("Crashing to make it look real...\n");
                pos = 0;
            }
    }
}
#endif /* HYBRID */

static void print_process_memory(Process *p);
static void print_process_memory(Process *p)
{
    ErlHeapFragment* bp = MBUF(p);

    erts_printf("==============================\n");
    erts_printf("|| Memory info for %T ||\n",p->id);
    erts_printf("==============================\n");

    erts_printf("-- %-*s ---%s-%s-%s-%s--\n",
                PTR_SIZE, "PCB", dashes, dashes, dashes, dashes);

    if (p->msg.first != NULL) {
        ErlMessage* mp;
        erts_printf("  Message Queue:\n");
        mp = p->msg.first;
        while (mp != NULL) {
            erts_printf("| 0x%0*lx | 0x%0*lx |\n",PTR_SIZE,
                        ERL_MESSAGE_TERM(mp),PTR_SIZE,ERL_MESSAGE_TOKEN(mp));
            mp = mp->next;
        }
    }

    if (p->dictionary != NULL) {
        int n = p->dictionary->used;
        Eterm *ptr = p->dictionary->data;
        erts_printf("  Dictionary: ");
        while (n--) erts_printf("0x%0*lx ",PTR_SIZE,(unsigned long)ptr++);
        erts_printf("\n");
    }

    if (p->arity > 0) {
        int n = p->arity;
        Eterm *ptr = p->arg_reg;
        erts_printf("  Argument Registers: ");
        while (n--) erts_printf("0x%0*lx ",PTR_SIZE,(unsigned long)*ptr++);
        erts_printf("\n");
    }

    erts_printf("  Trace Token: 0x%0*lx\n",PTR_SIZE,p->seq_trace_token);
    erts_printf("  Group Leader: 0x%0*lx\n",PTR_SIZE,p->group_leader);
    erts_printf("  Fvalue: 0x%0*lx\n",PTR_SIZE,p->fvalue);
    erts_printf("  Ftrace: 0x%0*lx\n",PTR_SIZE,p->ftrace);

#ifdef HYBRID
    if (p->nrr > 0) {
        int i;
        erts_printf("  Remembered Roots:\n");
        for (i = 0; i < p->nrr; i++)
            if (p->rrsrc[i] != NULL)
                erts_printf("0x%0*lx -> 0x%0*lx\n",
                            PTR_SIZE, (unsigned long)p->rrsrc[i],
                            PTR_SIZE, (unsigned long)p->rrma[i]);
        erts_printf("\n");
    }
#endif

    erts_printf("+- %-*s -+ 0x%0*lx 0x%0*lx %s-%s-+\n",
                PTR_SIZE, "Stack",
                PTR_SIZE, (unsigned long)STACK_TOP(p),
                PTR_SIZE, (unsigned long)STACK_START(p),
                dashes, dashes);
    print_untagged_memory(STACK_TOP(p),STACK_START(p));

    erts_printf("+- %-*s -+ 0x%0*lx 0x%0*lx 0x%0*lx 0x%0*lx +\n",
                PTR_SIZE, "Heap",
                PTR_SIZE, (unsigned long)HEAP_START(p),
                PTR_SIZE, (unsigned long)HIGH_WATER(p),
                PTR_SIZE, (unsigned long)HEAP_TOP(p),
                PTR_SIZE, (unsigned long)HEAP_END(p));
    print_untagged_memory(HEAP_START(p),HEAP_TOP(p));

    if (OLD_HEAP(p)) {
        erts_printf("+- %-*s -+ 0x%0*lx 0x%0*lx 0x%0*lx %s-+\n",
                    PTR_SIZE, "Old Heap",
                    PTR_SIZE, (unsigned long)OLD_HEAP(p),
                    PTR_SIZE, (unsigned long)OLD_HTOP(p),
                    PTR_SIZE, (unsigned long)OLD_HEND(p),
                    dashes);
        print_untagged_memory(OLD_HEAP(p),OLD_HTOP(p));
    }

    if (bp)
        erts_printf("+- %-*s -+-%s-%s-%s-%s-+\n",
                    PTR_SIZE, "heap fragments",
                    dashes, dashes, dashes, dashes);
    while (bp) {
	print_untagged_memory(bp->mem,bp->mem + bp->used_size);
        bp = bp->next;
    }
}


void print_memory(Process *p)
{
    if (p != NULL) {
        print_process_memory(p);
    }
#ifdef HYBRID
    else {
        Uint i;
        Uint n = erts_num_active_procs;

        for (i = 0; i < n; i++) {
            Process *p = erts_active_procs[i];
            print_process_memory(p);
        }

        erts_printf("==================\n");
        erts_printf("|| Message area ||\n");
        erts_printf("==================\n");
        erts_printf("+-%s-+-%s-%s-%s-%s-+\n",
                    dashes,dashes,dashes,dashes,dashes);
        erts_printf("| %-*s   | 0x%0*lx - 0x%0*lx - 0x%0*lx%*s|\n",
                    PTR_SIZE, "Young",
                    PTR_SIZE, (unsigned long)global_heap,
                    PTR_SIZE, (unsigned long)global_htop,
                    PTR_SIZE, (unsigned long)global_hend,
                    PTR_SIZE, "");
        erts_printf("+-%s-+-%s-%s-%s-%s-+\n",
                    dashes,dashes,dashes,dashes,dashes);

        print_untagged_memory(global_heap,global_htop);


        erts_printf("+-%s-+-%s-%s-%s-%s-+\n",
                    dashes,dashes,dashes,dashes,dashes);
        erts_printf("| %-*s   | 0x%0*lx - 0x%0*lx %*s    |\n",
                    PTR_SIZE, "Old",
                    PTR_SIZE, (unsigned long)global_old_heap,
                    PTR_SIZE, (unsigned long)global_old_hend,
                    2 * PTR_SIZE, "");
        erts_printf("+-%s-+-%s-%s-%s-%s-+\n",
                    dashes,dashes,dashes,dashes,dashes);

#ifdef INCREMENTAL
        {
            INC_Page *page = inc_used_mem;
            /* Genom att g� igenom fri-listan f�rst kan vi markera de
               omr�den som inte �r allokerade och bara skriva ut de som
               lever.
               char markarea[INC_PAGESIZE];
            */

            while (page) {
                Eterm *ptr = (Eterm*)page->start;
                Eterm *end = (Eterm*)page->start + INC_PAGESIZE;

                erts_printf("| %*s   | This: 0x%0*lx   Next: 0x%0*lx %*s|\n",
                            PTR_SIZE, "",
                            PTR_SIZE, (unsigned long)page,
                            PTR_SIZE, (unsigned long)page->next,
                            2 * PTR_SIZE - 8, "");
                print_untagged_memory(ptr,end);
                page = page->next;
            }
        }

        {
            INC_MemBlock *this = inc_free_list;

            erts_printf("-- %-*s --%s-%s-%s-%s-\n",PTR_SIZE+2,"Free list",
                        dashes,dashes,dashes,dashes);
            while (this) {
                erts_printf("Block @ 0x%0*lx sz: %8d prev: 0x%0*lx next: 0x%0*lx\n",
                            PTR_SIZE, (unsigned long)this,this->size,
                            PTR_SIZE, (unsigned long)this->prev,
                            PTR_SIZE, (unsigned long)this->next);
                this = this->next;
            }
            erts_printf("--%s---%s-%s-%s-%s--\n",
                        dashes,dashes,dashes,dashes,dashes);
        }

        if (inc_fromspc != NULL) {
            erts_printf("-- fromspace - 0x%0*lx 0x%0*lx "
                        "------------------------------\n",
                        PTR_SIZE, (unsigned long)inc_fromspc,
                        PTR_SIZE, (unsigned long)inc_fromend);
            print_untagged_memory(inc_fromspc,inc_fromend);
        }
#endif /* INCREMENTAL */
    }
#endif /* HYBRID */
}

void print_memory_info(Process *p)
{
    if (p != NULL) {
        erts_printf("======================================\n");
        erts_printf("|| Memory info for %-12T ||\n",p->id);
        erts_printf("======================================\n");
        erts_printf("+- local heap ----%s-%s-%s-%s-+\n",
                    dashes,dashes,dashes,dashes);
        erts_printf("| Young | 0x%0*lx - (0x%0*lx) - 0x%0*lx - 0x%0*lx |\n",
                    PTR_SIZE, (unsigned long)HEAP_START(p),
                    PTR_SIZE, (unsigned long)HIGH_WATER(p),
                    PTR_SIZE, (unsigned long)HEAP_TOP(p),
                    PTR_SIZE, (unsigned long)HEAP_END(p));
        if (OLD_HEAP(p) != NULL)
            erts_printf("| Old   | 0x%0*lx - 0x%0*lx - 0x%0*lx   %*s     |\n",
                        PTR_SIZE, (unsigned long)OLD_HEAP(p),
                        PTR_SIZE, (unsigned long)OLD_HTOP(p),
                        PTR_SIZE, (unsigned long)OLD_HEND(p),
                        PTR_SIZE, "");
    } else {
        erts_printf("=================\n");
        erts_printf("|| Memory info ||\n");
        erts_printf("=================\n");
    }
#ifdef HYBRID
    erts_printf("|- message area --%s-%s-%s-%s-|\n",
                dashes,dashes,dashes,dashes);
    erts_printf("| Young | 0x%0*lx - 0x%0*lx - 0x%0*lx   %*s     |\n",
                PTR_SIZE, (unsigned long)global_heap,
                PTR_SIZE, (unsigned long)global_htop,
                PTR_SIZE, (unsigned long)global_hend,
                PTR_SIZE, "");
    erts_printf("| Old   | 0x%0*lx - 0x%0*lx      %*s       |\n",
                PTR_SIZE, (unsigned long)global_old_heap,
                PTR_SIZE, (unsigned long)global_old_hend,
                2 * PTR_SIZE, "");
#endif
#ifdef INCREMENTAL
    if (inc_fromspc != NULL)
        erts_printf("| Frmsp | 0x%0*lx - 0x%0*lx      %*s  |\n",
                    PTR_SIZE, (unsigned long)inc_fromspc,
                    PTR_SIZE, (unsigned long)inc_fromend,
                    2 * PTR_SIZE, "");
#endif
    erts_printf("+-----------------%s-%s-%s-%s-+\n",dashes,dashes,dashes,dashes);
}
#if !HEAP_ON_C_STACK && defined(DEBUG)
Eterm *erts_debug_allocate_tmp_heap(int size, Process *p)
{
    ErtsSchedulerData *sd = ((p == NULL) ? erts_get_scheduler_data() : ERTS_PROC_GET_SCHDATA(p));
    int offset = sd->num_tmp_heap_used;

    ASSERT(offset+size <= TMP_HEAP_SIZE);
    return (sd->tmp_heap)+offset;
}
void erts_debug_use_tmp_heap(int size, Process *p)
{
    ErtsSchedulerData *sd = ((p == NULL) ? erts_get_scheduler_data() : ERTS_PROC_GET_SCHDATA(p));

    sd->num_tmp_heap_used += size;
    ASSERT(sd->num_tmp_heap_used <= TMP_HEAP_SIZE);
}
void erts_debug_unuse_tmp_heap(int size, Process *p)
{
    ErtsSchedulerData *sd = ((p == NULL) ? erts_get_scheduler_data() : ERTS_PROC_GET_SCHDATA(p));

    sd->num_tmp_heap_used -= size;
    ASSERT(sd->num_tmp_heap_used >= 0);
}
#endif
#endif

