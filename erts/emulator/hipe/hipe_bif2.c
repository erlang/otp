/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
 * hipe_bif2.c
 *
 * Miscellaneous add-ons.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "error.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "bif.h"
#include "big.h"
#include "erl_map.h"
#include "hipe_debug.h"
#include "hipe_mode_switch.h"
#include "hipe_arch.h"
#include "hipe_stack.h"

static void proc_unlock(Process* c_p, Process* rp)
{
    ErtsProcLocks locks = ERTS_PROC_LOCKS_ALL;
    if (rp == c_p) {
	locks &= ~ERTS_PROC_LOCK_MAIN;
    }
    if (rp && locks) {
	erts_smp_proc_unlock(rp, locks);
    }
}

BIF_RETTYPE hipe_bifs_show_estack_1(BIF_ALIST_1)
{    
    Process *rp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
				BIF_ARG_1, ERTS_PROC_LOCKS_ALL);
    if (!rp)
	BIF_ERROR(BIF_P, BADARG);
    hipe_print_estack(rp);
    proc_unlock(BIF_P, rp);
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_show_heap_1(BIF_ALIST_1)
{
    Process *rp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
				BIF_ARG_1, ERTS_PROC_LOCKS_ALL);
    if (!rp)
	BIF_ERROR(BIF_P, BADARG);
    hipe_print_heap(rp);
    proc_unlock(BIF_P, rp);
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_show_nstack_1(BIF_ALIST_1)
{
    Process *rp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
				BIF_ARG_1, ERTS_PROC_LOCKS_ALL);
    if (!rp)
	BIF_ERROR(BIF_P, BADARG);
    hipe_print_nstack(rp);
    proc_unlock(BIF_P, rp);
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_nstack_used_size_0(BIF_ALIST_0)
{
    BIF_RET(make_small(hipe_nstack_used(BIF_P)));
}

BIF_RETTYPE hipe_bifs_show_pcb_1(BIF_ALIST_1)
{
    Process *rp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
				BIF_ARG_1, ERTS_PROC_LOCKS_ALL);
    if (!rp)
	BIF_ERROR(BIF_P, BADARG);
    hipe_print_pcb(rp);
    proc_unlock(BIF_P, rp);
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_show_term_1(BIF_ALIST_1)
{
    Eterm obj = BIF_ARG_1;

    printf("0x%0*lx\r\n", 2*(int)sizeof(long), obj);
    do {
	Eterm *objp;
	int i, ary;

	if (is_list(obj)) {
	    objp = list_val(obj);
	    ary = 2;
	} else if (is_boxed(obj)) {
	    Eterm header;

	    objp = boxed_val(obj);
	    header = objp[0];
	    if (is_thing(header))
		ary = thing_arityval(header);
	    else if (is_arity_value(header))
		ary = arityval(header);
	    else {
		printf("bad header %#lx\r\n", header);
		break;
	    }
	    ary += 1;
	} else
	    break;
	for (i = 0; i < ary; ++i)
	    printf("0x%0*lx: 0x%0*lx\r\n",
		   2*(int)sizeof(long), (unsigned long)&objp[i],
		   2*(int)sizeof(long), objp[i]);
    } while (0);
    erts_printf("%T", obj);
    printf("\r\n");
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_in_native_0(BIF_ALIST_0)
{
    BIF_RET(am_false);
}

BIF_RETTYPE hipe_bifs_modeswitch_debug_on_0(BIF_ALIST_0)
{
    hipe_modeswitch_debug = 1;
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_modeswitch_debug_off_0(BIF_ALIST_0)
{
    hipe_modeswitch_debug = 0;
    BIF_RET(am_true);
}

#if defined(ERTS_ENABLE_LOCK_CHECK) && defined(ERTS_SMP)

BIF_RETTYPE hipe_debug_bif_wrapper(BIF_ALIST_1);

#    define ERTS_SMP_REQ_PROC_MAIN_LOCK(P) \
       if ((P)) erts_proc_lc_require_lock((P), ERTS_PROC_LOCK_MAIN,\
					  __FILE__, __LINE__)
#    define ERTS_SMP_UNREQ_PROC_MAIN_LOCK(P) \
        if ((P)) erts_proc_lc_unrequire_lock((P), ERTS_PROC_LOCK_MAIN)

BIF_RETTYPE hipe_debug_bif_wrapper(BIF_ALIST_1)
{
    typedef BIF_RETTYPE Bif(BIF_ALIST_1);
    Bif* fp = (Bif*) (BIF_P->hipe.bif_callee);
    BIF_RETTYPE res;
    ERTS_SMP_UNREQ_PROC_MAIN_LOCK(BIF_P);
    res = (*fp)(BIF_P, BIF__ARGS);
    ERTS_SMP_REQ_PROC_MAIN_LOCK(BIF_P);
    return res;
}

#endif /* ERTS_ENABLE_LOCK_CHECK && ERTS_SMP */


BIF_RETTYPE hipe_bifs_debug_native_called_2(BIF_ALIST_2)
{
    erts_printf("hipe_debug_native_called: %T(%T)\r\n", BIF_ARG_1, BIF_ARG_2);
    BIF_RET(am_ok);
}

/* Stub-BIF for LLVM:
 * Reloads BP, SP (in llvm unwind label) */

BIF_RETTYPE hipe_bifs_llvm_fix_pinned_regs_0(BIF_ALIST_0)
{
    BIF_RET(am_ok);
}
