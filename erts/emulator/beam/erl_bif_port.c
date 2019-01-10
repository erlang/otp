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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <ctype.h>

#define ERTS_WANT_EXTERNAL_TAGS
#include "sys.h"
#include "erl_vm.h"
#include "erl_sys_driver.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"
#include "erl_binary.h"
#include "erl_db_util.h"
#include "register.h"
#include "external.h"
#include "packet_parser.h"
#include "erl_bits.h"
#include "erl_bif_unique.h"
#include "dtrace-wrapper.h"
#include "erl_proc_sig_queue.h"

static Port *open_port(Process* p, Eterm name, Eterm settings, int *err_typep, int *err_nump);
static int merge_global_environment(erts_osenv_t *env, Eterm key_value_pairs);
static char **convert_args(Eterm);
static void free_args(char **);

char *erts_default_arg0 = "default";

BIF_RETTYPE erts_internal_open_port_2(BIF_ALIST_2)
{
    BIF_RETTYPE ret;
    Port *port;
    Eterm res;
    char *str;
    int err_type, err_num;
    ErtsLinkData *ldp;
    ErtsLink *lnk;

    port = open_port(BIF_P, BIF_ARG_1, BIF_ARG_2, &err_type, &err_num);
    if (!port) {
	if (err_type == -3) {
	    ASSERT(err_num == BADARG || err_num == SYSTEM_LIMIT);
	    if (err_num == BADARG)
                res = am_badarg;
            else if (err_num == SYSTEM_LIMIT)
                res = am_system_limit;
            else
                /* this is only here to silence gcc, it should not happen */
                BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
	} else if (err_type == -2) {
	    str = erl_errno_id(err_num);
            res = erts_atom_put((byte *) str, sys_strlen(str), ERTS_ATOM_ENC_LATIN1, 1);
	} else {
	    res = am_einval;
	}
        BIF_RET(res);
    }

    ldp = erts_link_create(ERTS_LNK_TYPE_PORT, BIF_P->common.id, port->common.id);
    ASSERT(ldp->a.other.item == port->common.id);
    ASSERT(ldp->b.other.item == BIF_P->common.id);
    /*
     * This link should not already be present, but can potentially
     * due to id wrapping...
     */
    lnk = erts_link_tree_lookup_insert(&ERTS_P_LINKS(BIF_P), &ldp->a);
    erts_link_tree_insert(&ERTS_P_LINKS(port), &ldp->b);

    if (port->drv_ptr->flags & ERL_DRV_FLAG_USE_INIT_ACK) {

        /* Copied from erl_port_task.c */
        port->async_open_port = erts_alloc(ERTS_ALC_T_PRTSD,
                                           sizeof(*port->async_open_port));
        erts_make_ref_in_array(port->async_open_port->ref);
        port->async_open_port->to = BIF_P->common.id;

        /*
         * We unconditionaly *must* do a receive on a message
         * containing the reference after this...
         */
        ERTS_RECV_MARK_SAVE(BIF_P);
        ERTS_RECV_MARK_SET(BIF_P);

        res = erts_proc_store_ref(BIF_P, port->async_open_port->ref);
    } else {
        res = port->common.id;
    }

    if (IS_TRACED_FL(BIF_P, F_TRACE_PROCS))
        trace_proc(BIF_P, ERTS_PROC_LOCK_MAIN, BIF_P,
                   am_link, port->common.id);

    ERTS_BIF_PREP_RET(ret, res);

    erts_port_release(port);

    if (lnk)
        erts_link_release(lnk);

    return ret;
}

static ERTS_INLINE Port *
lookup_port(Process *c_p, Eterm id_or_name, Uint32 invalid_flags)
{
    /* TODO: Implement nicer lookup in register... */
    Eterm id;
    if (is_atom(id_or_name))
	id = erts_whereis_name_to_id(c_p, id_or_name);
    else
	id = id_or_name;
    return erts_port_lookup(id, invalid_flags);
}

static ERTS_INLINE Port *
sig_lookup_port(Process *c_p, Eterm id_or_name)
{
    return lookup_port(c_p, id_or_name, ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP);
}

/* Non-inline copy of sig_lookup_port to be exported */
Port *erts_sig_lookup_port(Process *c_p, Eterm id_or_name)
{
    return lookup_port(c_p, id_or_name, ERTS_PORT_SFLGS_INVALID_DRIVER_LOOKUP);
}

static ERTS_INLINE Port *
data_lookup_port(Process *c_p, Eterm id_or_name)
{
    return lookup_port(c_p, id_or_name, ERTS_PORT_SFLGS_INVALID_LOOKUP);
}

/*
 * erts_internal:port_command/3 is used by the
 * erlang:port_command/2 and erlang:port_command/3
 * BIFs.
 */

BIF_RETTYPE erts_internal_port_command_3(BIF_ALIST_3)
{
    BIF_RETTYPE res;
    Port *prt;
    int flags = 0;
    Eterm ref;

    if (is_not_nil(BIF_ARG_3)) {
	Eterm l = BIF_ARG_3;
	while (is_list(l)) {
	    Eterm* cons = list_val(l);
	    Eterm car = CAR(cons);
	    if (car == am_force)
		flags |= ERTS_PORT_SIG_FLG_FORCE;
	    else if (car == am_nosuspend)
		flags |= ERTS_PORT_SIG_FLG_NOSUSPEND;
	    else
		BIF_RET(am_badarg);
	    l = CDR(cons);
	}
	if (!is_nil(l))
	    BIF_RET(am_badarg);
    }

    prt = sig_lookup_port(BIF_P, BIF_ARG_1);
    if (!prt)
	BIF_RET(am_badarg);

    if (flags & ERTS_PORT_SIG_FLG_FORCE) {
	if (!(prt->drv_ptr->flags & ERL_DRV_FLAG_SOFT_BUSY))
	    BIF_RET(am_notsup);
    }

#ifdef DEBUG
    ref = NIL;
#endif

    switch (erts_port_output(BIF_P, flags, prt, prt->common.id, BIF_ARG_2, &ref)) {
    case ERTS_PORT_OP_BADARG:
    case ERTS_PORT_OP_DROPPED:
 	ERTS_BIF_PREP_RET(res, am_badarg);
	break;
    case ERTS_PORT_OP_BUSY:
	ASSERT(!(flags & ERTS_PORT_SIG_FLG_FORCE));
	if (flags & ERTS_PORT_SIG_FLG_NOSUSPEND)
	    ERTS_BIF_PREP_RET(res, am_false);
	else {
	    erts_suspend(BIF_P, ERTS_PROC_LOCK_MAIN, prt);
	    ERTS_BIF_PREP_YIELD3(res, bif_export[BIF_erts_internal_port_command_3],
				 BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
	}
	break;
    case ERTS_PORT_OP_BUSY_SCHEDULED:
	ASSERT(!(flags & ERTS_PORT_SIG_FLG_FORCE));
	/* Fall through... */
    case ERTS_PORT_OP_SCHEDULED:
	ASSERT(is_internal_ordinary_ref(ref));
	ERTS_BIF_PREP_RET(res, ref);
	break;
    case ERTS_PORT_OP_DONE:
	ERTS_BIF_PREP_RET(res, am_true);
	break;
    default:
	ERTS_INTERNAL_ERROR("Unexpected erts_port_output() result");
	break;
    }

    if (ERTS_PROC_IS_EXITING(BIF_P)) {
	KILL_CATCHES(BIF_P);	/* Must exit */
	ERTS_BIF_PREP_ERROR(res, BIF_P, EXC_ERROR);
    }

    return res;
}

BIF_RETTYPE erts_internal_port_call_3(BIF_ALIST_3)
{
    Port* prt;
    Eterm retval;
    Uint uint_op;
    unsigned int op;
    erts_aint32_t state;

    prt = sig_lookup_port(BIF_P, BIF_ARG_1);
    if (!prt)
	BIF_RET(am_badarg);

    if (!term_to_Uint(BIF_ARG_2, &uint_op))
	BIF_RET(am_badarg);

    if (uint_op > (Uint) UINT_MAX)
	BIF_RET(am_badarg);

    op = (unsigned int) uint_op;

    switch (erts_port_call(BIF_P, prt, op, BIF_ARG_3, &retval)) {
    case ERTS_PORT_OP_DROPPED:
    case ERTS_PORT_OP_BADARG:
	retval = am_badarg;
	break;
    case ERTS_PORT_OP_SCHEDULED:
	ASSERT(is_internal_ordinary_ref(retval));
	break;
    case ERTS_PORT_OP_DONE:
	ASSERT(is_not_internal_ref(retval));
	break;
    default:
	ERTS_INTERNAL_ERROR("Unexpected erts_port_call() result");
	retval = am_internal_error;
	break;
    }

    state = erts_atomic32_read_acqb(&BIF_P->state);
    if (state & ERTS_PSFLG_EXITING)
	ERTS_BIF_EXITED(BIF_P);

    BIF_RET(retval);
}

BIF_RETTYPE erts_internal_port_control_3(BIF_ALIST_3)
{
    Port* prt;
    Eterm retval;
    Uint uint_op;
    unsigned int op;
    erts_aint32_t state;

    prt = sig_lookup_port(BIF_P, BIF_ARG_1);
    if (!prt)
	BIF_RET(am_badarg);

    if (!term_to_Uint(BIF_ARG_2, &uint_op))
	BIF_RET(am_badarg);

    if (uint_op > (Uint) UINT_MAX)
	BIF_RET(am_badarg);

    op = (unsigned int) uint_op;

    switch (erts_port_control(BIF_P, prt, op, BIF_ARG_3, &retval)) {
    case ERTS_PORT_OP_BADARG:
    case ERTS_PORT_OP_DROPPED:
	retval = am_badarg;
	break;
    case ERTS_PORT_OP_SCHEDULED:
	ASSERT(is_internal_ordinary_ref(retval));
	break;
    case ERTS_PORT_OP_DONE:
	ASSERT(is_not_internal_ref(retval));
	break;
    default:
	ERTS_INTERNAL_ERROR("Unexpected erts_port_control() result");
	retval = am_internal_error;
	break;
    }

    state = erts_atomic32_read_acqb(&BIF_P->state);
    if (state & ERTS_PSFLG_EXITING)
	ERTS_BIF_EXITED(BIF_P);

    BIF_RET(retval);
}

/*
 * erts_internal:port_close/1 is used by the
 * erlang:port_close/1 BIF.
 */
BIF_RETTYPE erts_internal_port_close_1(BIF_ALIST_1)
{
    Eterm ref;
    Port *prt;

#ifdef DEBUG
    ref = NIL;
#endif

    prt = sig_lookup_port(BIF_P, BIF_ARG_1);
    if (!prt)
	BIF_RET(am_badarg);

    switch (erts_port_exit(BIF_P, 0, prt, BIF_P->common.id, am_normal, &ref)) {
    case ERTS_PORT_OP_BADARG:
    case ERTS_PORT_OP_DROPPED:
	BIF_RET(am_badarg);
    case ERTS_PORT_OP_SCHEDULED:
	ASSERT(is_internal_ordinary_ref(ref));
	BIF_RET(ref);
    case ERTS_PORT_OP_DONE:
	BIF_RET(am_true);
    default:
	ERTS_INTERNAL_ERROR("Unexpected erts_port_exit() result");
	BIF_RET(am_internal_error);
    }
}

/*
 * erts_internal:port_connect/2 is used by the
 * erlang:port_connect/2 BIF.
 */
BIF_RETTYPE erts_internal_port_connect_2(BIF_ALIST_2)
{
    Eterm ref;
    Port* prt;

    prt = sig_lookup_port(BIF_P, BIF_ARG_1);
    if (!prt)
	BIF_RET(am_badarg);

#ifdef DEBUG
    ref = NIL;
#endif

    switch (erts_port_connect(BIF_P, 0, prt, BIF_P->common.id, BIF_ARG_2, &ref)) {
    case ERTS_PORT_OP_BADARG:
    case ERTS_PORT_OP_DROPPED:
	BIF_RET(am_badarg);
    case ERTS_PORT_OP_SCHEDULED:
	ASSERT(is_internal_ordinary_ref(ref));
	BIF_RET(ref);
	break;
    case ERTS_PORT_OP_DONE:
	BIF_RET(am_true);
	break;
    default:
	ERTS_INTERNAL_ERROR("Unexpected erts_port_connect() result");
	BIF_RET(am_internal_error);
    }
}

BIF_RETTYPE erts_internal_port_info_1(BIF_ALIST_1)
{
    Eterm retval;
    Port* prt;

    if (is_internal_port(BIF_ARG_1) || is_atom(BIF_ARG_1)) {
	prt = sig_lookup_port(BIF_P, BIF_ARG_1);
	if (!prt)
	    BIF_RET(am_undefined);
    }
    else if (is_external_port(BIF_ARG_1)) {
	if (external_port_dist_entry(BIF_ARG_1) == erts_this_dist_entry)
	    BIF_RET(am_undefined);
	else
	    BIF_RET(am_badarg);
    }
    else {
	BIF_RET(am_badarg);
    }

    switch (erts_port_info(BIF_P, prt, THE_NON_VALUE, &retval)) {
    case ERTS_PORT_OP_BADARG:
	BIF_RET(am_badarg);
    case ERTS_PORT_OP_DROPPED:
	BIF_RET(am_undefined);
    case ERTS_PORT_OP_SCHEDULED:
	ASSERT(is_internal_ordinary_ref(retval));
	BIF_RET(retval);
    case ERTS_PORT_OP_DONE:
	ASSERT(is_not_internal_ref(retval));
	BIF_RET(retval);
    default:
	ERTS_INTERNAL_ERROR("Unexpected erts_port_info() result");
	BIF_RET(am_internal_error);
    }
}


BIF_RETTYPE erts_internal_port_info_2(BIF_ALIST_2)
{
    Eterm retval;
    Port* prt;

    if (is_internal_port(BIF_ARG_1) || is_atom(BIF_ARG_1)) {
	prt = sig_lookup_port(BIF_P, BIF_ARG_1);
	if (!prt)
	    BIF_RET(am_undefined);
    }
    else if (is_external_port(BIF_ARG_1)) {
	if (external_port_dist_entry(BIF_ARG_1) == erts_this_dist_entry)
	    BIF_RET(am_undefined);
	else
	    BIF_RET(am_badarg);
    }
    else {
	BIF_RET(am_badarg);
    }

    switch (erts_port_info(BIF_P, prt, BIF_ARG_2, &retval)) {
    case ERTS_PORT_OP_BADARG:
	BIF_RET(am_badarg);
    case ERTS_PORT_OP_DROPPED:
	BIF_RET(am_undefined);
    case ERTS_PORT_OP_SCHEDULED:
	ASSERT(is_internal_ordinary_ref(retval));
	BIF_RET(retval);
    case ERTS_PORT_OP_DONE:
	ASSERT(is_not_internal_ref(retval));
	BIF_RET(retval);
    default:
	ERTS_INTERNAL_ERROR("Unexpected erts_port_info() result");
	BIF_RET(am_internal_error);
    }
}

/*
 * The erlang:port_set_data()/erlang:port_get_data() operations should
 * be viewed as operations on a table (inet_db) with data values
 * associated with port identifier keys. That is, these operations are
 * *not* signals to/from ports.
 */

#if (TAG_PRIMARY_IMMED1 & 0x3) == 0
# error "erlang:port_set_data()/erlang:port_get_data() needs to be rewritten!"
#endif

typedef struct {
    ErtsThrPrgrLaterOp later_op;
    Uint hsize;
    Eterm data;
    ErlOffHeap off_heap;
    Eterm heap[1];
} ErtsPortDataHeap;

static void
free_port_data_heap(void *vpdhp)
{
    erts_cleanup_offheap(&((ErtsPortDataHeap *) vpdhp)->off_heap);
    erts_free(ERTS_ALC_T_PORT_DATA_HEAP, vpdhp);
}

static ERTS_INLINE void
cleanup_old_port_data(erts_aint_t data)
{
    if ((data & 0x3) != 0) {
	ASSERT(is_immed((Eterm) data));
    }
    else {
	ErtsPortDataHeap *pdhp = (ErtsPortDataHeap *) data;
	size_t size;
	ERTS_THR_DATA_DEPENDENCY_READ_MEMORY_BARRIER;
	size = sizeof(ErtsPortDataHeap) + (pdhp->hsize-1)*sizeof(Eterm);
	erts_schedule_thr_prgr_later_cleanup_op(free_port_data_heap,
						(void *) pdhp,
						&pdhp->later_op,
						size);
    }
}

void
erts_init_port_data(Port *prt)
{
    erts_atomic_init_nob(&prt->data, (erts_aint_t) am_undefined);
}

void
erts_cleanup_port_data(Port *prt)
{
    ASSERT(erts_atomic32_read_nob(&prt->state) & ERTS_PORT_SFLGS_INVALID_LOOKUP);
    cleanup_old_port_data(erts_atomic_xchg_nob(&prt->data,
						   (erts_aint_t) NULL));
}

Uint
erts_port_data_size(Port *prt)
{
    erts_aint_t data = erts_atomic_read_ddrb(&prt->data);

    if ((data & 0x3) != 0) {
	ASSERT(is_immed((Eterm) (UWord) data));
	return (Uint) 0;
    }
    else {
	ErtsPortDataHeap *pdhp = (ErtsPortDataHeap *) data;
	return (Uint) sizeof(ErtsPortDataHeap) + (pdhp->hsize-1)*sizeof(Eterm);
    }
}

ErlOffHeap *
erts_port_data_offheap(Port *prt)
{
    erts_aint_t data = erts_atomic_read_ddrb(&prt->data);

    if ((data & 0x3) != 0) {
	ASSERT(is_immed((Eterm) (UWord) data));
	return NULL;
    }
    else {
	ErtsPortDataHeap *pdhp = (ErtsPortDataHeap *) data;
	return &pdhp->off_heap;
    }
}

BIF_RETTYPE port_set_data_2(BIF_ALIST_2)
{
    /*
     * This is not a signal. See comment above.
     */
    erts_aint_t data;
    Port* prt;

    prt = data_lookup_port(BIF_P, BIF_ARG_1);
    if (!prt)
        BIF_ERROR(BIF_P, BADARG);

    if (is_immed(BIF_ARG_2)) {
	data = (erts_aint_t) BIF_ARG_2;
	ASSERT((data & 0x3) != 0);
    }
    else {
	ErtsPortDataHeap *pdhp;
	Uint hsize;
	Eterm *hp;

	hsize = size_object(BIF_ARG_2);
	pdhp = erts_alloc(ERTS_ALC_T_PORT_DATA_HEAP,
			  sizeof(ErtsPortDataHeap) + (hsize-1)*sizeof(Eterm));
	hp = &pdhp->heap[0];
	pdhp->off_heap.first = NULL;
	pdhp->off_heap.overhead = 0;
	pdhp->hsize = hsize;
	pdhp->data = copy_struct(BIF_ARG_2, hsize, &hp, &pdhp->off_heap);
	data = (erts_aint_t) pdhp;
	ASSERT((data & 0x3) == 0);
    }

    data = erts_atomic_xchg_wb(&prt->data, data);

    if (data == (erts_aint_t)NULL) {
	/* Port terminated by racing thread */
	data = erts_atomic_xchg_wb(&prt->data, data);
	ASSERT(data != (erts_aint_t)NULL);
	cleanup_old_port_data(data);
	BIF_ERROR(BIF_P, BADARG);
    }
    cleanup_old_port_data(data);
    BIF_RET(am_true);
}


BIF_RETTYPE port_get_data_1(BIF_ALIST_1)
{
    /*
     * This is not a signal. See comment above.
     */
    Eterm res;
    erts_aint_t data;
    Port* prt;

    prt = data_lookup_port(BIF_P, BIF_ARG_1);
    if (!prt)
        BIF_ERROR(BIF_P, BADARG);

    data = erts_atomic_read_ddrb(&prt->data);
    if (data == (erts_aint_t)NULL)
        BIF_ERROR(BIF_P, BADARG);  /* Port terminated by racing thread */

    if ((data & 0x3) != 0) {
	res = (Eterm) (UWord) data;
	ASSERT(is_immed(res));
    }
    else {
	ErtsPortDataHeap *pdhp = (ErtsPortDataHeap *) data;
	Eterm *hp = HAlloc(BIF_P, pdhp->hsize);
	res = copy_struct(pdhp->data, pdhp->hsize, &hp, &MSO(BIF_P));
    }

    BIF_RET(res);
}

Eterm erts_port_data_read(Port* prt)
{
    Eterm res;
    erts_aint_t data;

    data = erts_atomic_read_ddrb(&prt->data);
    if (data == (erts_aint_t)NULL)
        return am_undefined;  /* Port terminated by racing thread */

    if ((data & 0x3) != 0) {
	res = (Eterm) (UWord) data;
	ASSERT(is_immed(res));
    }
    else {
	ErtsPortDataHeap *pdhp = (ErtsPortDataHeap *) data;
	res = pdhp->data;
    }
    return res;
}


/* 
 * Open a port. Most of the work is not done here but rather in
 * the file io.c.
 * Error returns: -1 or -2 returned from open_driver (-2 implies
 * that *err_nump contains the error code; -1 means we don't really know what happened),
 * -3 if argument parsing failed or we are out of ports (*err_nump should contain
 * either BADARG or SYSTEM_LIMIT).
 */

static Port *
open_port(Process* p, Eterm name, Eterm settings, int *err_typep, int *err_nump)
{
    int merged_environment = 0;
    Sint i;
    Eterm option;
    Uint arity;
    Eterm* tp;
    Uint* nargs;
    erts_driver_t* driver;
    char* name_buf = NULL;
    SysDriverOpts opts;
    Sint linebuf;
    Eterm edir = NIL;
    byte dir[MAXPATHLEN];
    erts_aint32_t sflgs = 0;
    Port *port;

    /* These are the defaults */
    opts.packet_bytes = 0;
    opts.use_stdio = 1;
    opts.redir_stderr = 0;
    opts.read_write = 0;
    opts.hide_window = 0;
    opts.wd = NULL;
    opts.exit_status = 0;
    opts.overlapped_io = 0; 
    opts.spawn_type = ERTS_SPAWN_ANY; 
    opts.argv = NULL;
    opts.parallelism = erts_port_parallelism;
    erts_osenv_init(&opts.envir);

    linebuf = 0;

    *err_nump = 0;

    if (is_not_list(settings) && is_not_nil(settings)) {
	goto badarg;
    }
    /*
     * Parse the settings.
     */

    if (is_not_nil(settings)) {
	nargs = list_val(settings);
	while (1) {
	    if (is_tuple_arity(*nargs, 2)) {
		tp = tuple_val(*nargs);
		arity = *tp++;
		option = *tp++;
		if (option == am_packet) {
		    if (is_not_small(*tp)) {
			goto badarg;
		    }
		    opts.packet_bytes = signed_val(*tp);
		    switch (opts.packet_bytes) {
		    case 1:
		    case 2:
		    case 4:
			break;
		    default:
			goto badarg;
		   }
		} else if (option == am_line) {
		    if (is_not_small(*tp)) {
			goto badarg;
		    }
		    linebuf = signed_val(*tp);
		    if (linebuf <= 0) {
			goto badarg;
		    }
		} else if (option == am_env) {
		    if (merged_environment) {
		        /* Ignore previous env option */
		        erts_osenv_clear(&opts.envir);
		    }

		    merged_environment = 1;

		    if (merge_global_environment(&opts.envir, *tp)) {
		        goto badarg;
		    }
		} else if (option == am_args) {
		    char **av;
		    char **oav = opts.argv;
		    if ((av = convert_args(*tp)) == NULL) {
			goto badarg;
		    }
		    opts.argv = av;
		    if (oav) {
			opts.argv[0] = oav[0];
			oav[0] = erts_default_arg0;
			free_args(oav);
		    }

		} else if (option == am_arg0) {
		    char *a0;

		    if ((a0 = erts_convert_filename_to_native(*tp, NULL, 0, ERTS_ALC_T_TMP, 1, 1, NULL)) == NULL) {
			goto badarg;
		    }
		    if (opts.argv == NULL) {
			opts.argv = erts_alloc(ERTS_ALC_T_TMP, 
					       2 * sizeof(char **));
			opts.argv[0] = a0;
			opts.argv[1] = NULL;
		    } else {
			if (opts.argv[0] != erts_default_arg0) {
			    erts_free(ERTS_ALC_T_TMP, opts.argv[0]);
			}
			opts.argv[0] = a0;
		    }
		} else if (option == am_cd) {
		    edir = *tp;
		} else if (option == am_parallelism) {
		    if (*tp == am_true)
			opts.parallelism = 1;
		    else if (*tp == am_false)
			opts.parallelism = 0;
		    else
			goto badarg;
		} else {
		    goto badarg;
		}
	    } else if (*nargs == am_stream) {
		opts.packet_bytes = 0;
	    } else if (*nargs == am_use_stdio) {
		opts.use_stdio = 1;
	    } else if (*nargs == am_stderr_to_stdout) {
		opts.redir_stderr = 1;
	    } else if (*nargs == am_line) {
		linebuf = 512;
	    } else if (*nargs == am_nouse_stdio) {
		opts.use_stdio = 0;
	    } else if (*nargs == am_binary) {
		sflgs |= ERTS_PORT_SFLG_BINARY_IO;
	    } else if (*nargs == am_in) {
		opts.read_write |= DO_READ;
	    } else if (*nargs == am_out) {
		opts.read_write |= DO_WRITE;
	    } else if (*nargs == am_eof) {
		sflgs |= ERTS_PORT_SFLG_SOFT_EOF;
	    } else if (*nargs == am_hide) {
		opts.hide_window = 1;
	    } else if (*nargs == am_exit_status) {
		opts.exit_status = 1;
	    } else if (*nargs == am_overlapped_io) {
		opts.overlapped_io = 1;
	    } else {
		goto badarg;
	    }
	    if (is_nil(*++nargs)) 
		break;
	    if (is_not_list(*nargs)) {
		goto badarg;
	    }
	    nargs = list_val(*nargs);
	}
    }
    if (opts.read_write == 0)	/* implement default */
	opts.read_write = DO_READ|DO_WRITE;

    /* Mutually exclusive arguments. */
    if((linebuf && opts.packet_bytes) || 
       (opts.redir_stderr && !opts.use_stdio)) {
	goto badarg;
}

    /* If we lacked an env option, fill in the global environment without
     * changes. */
    if (!merged_environment) {
        merge_global_environment(&opts.envir, NIL);
    }

    /*
     * Parse the first argument and start the appropriate driver.
     */
    
    if (is_atom(name) || (i = is_string(name))) {
	/* a vanilla port */
	if (is_atom(name)) {
	    name_buf = (char *) erts_alloc(ERTS_ALC_T_TMP,
					   atom_tab(atom_val(name))->len+1);
	    sys_memcpy((void *) name_buf,
		       (void *) atom_tab(atom_val(name))->name, 
		       atom_tab(atom_val(name))->len);
	    name_buf[atom_tab(atom_val(name))->len] = '\0';
	} else {
	    name_buf = (char *) erts_alloc(ERTS_ALC_T_TMP, i + 1);
	    if (intlist_to_buf(name, name_buf, i) != i)
		erts_exit(ERTS_ERROR_EXIT, "%s:%d: Internal error\n", __FILE__, __LINE__);
	    name_buf[i] = '\0';
	}
	driver = &vanilla_driver;
    } else {   
	if (is_not_tuple(name)) {
	    goto badarg;		/* Not a process or fd port */
	}
	tp = tuple_val(name);
	arity = *tp++;

	if (arity == make_arityval(0)) {
	    goto badarg;
	}
    
	if (*tp == am_spawn || *tp == am_spawn_driver || *tp == am_spawn_executable) {	/* A process port */
	    int encoding;
	    if (arity != make_arityval(2)) {
		goto badarg;
	    }
	    name = tp[1];
	    encoding = erts_get_native_filename_encoding();
	    /* Do not convert the command to utf-16le yet, do that in win32 specific code */
	    /* since the cmd is used for comparsion with drivers names and copied to port info */
	    if (encoding == ERL_FILENAME_WIN_WCHAR) {
		encoding = ERL_FILENAME_UTF8;
	    }
	    if ((name_buf = erts_convert_filename_to_encoding(name, NULL, 0, ERTS_ALC_T_TMP,0,1, encoding, NULL, 0))
		== NULL) {
		goto badarg;
	    }

	    if (*tp == am_spawn_driver) {
		opts.spawn_type = ERTS_SPAWN_DRIVER;
	    } else if (*tp == am_spawn_executable) {
		opts.spawn_type = ERTS_SPAWN_EXECUTABLE;
	    }

	    driver = &spawn_driver;
	} else if (*tp == am_fd) { /* An fd port */
	    if (arity != make_arityval(3)) {
		goto badarg;
	    }
	    if (is_not_small(tp[1]) || is_not_small(tp[2])) {
		goto badarg;
	    }
	    opts.ifd = unsigned_val(tp[1]);
	    opts.ofd = unsigned_val(tp[2]);

            /* Syntesize name from input and output descriptor. */
            name_buf = erts_alloc(ERTS_ALC_T_TMP, 256);
            erts_snprintf(name_buf, 256, "%i/%i", opts.ifd, opts.ofd);

	    driver = &fd_driver;
	} else {
	    goto badarg;
	}
    }

    if ((driver != &spawn_driver && opts.argv != NULL) ||
	(driver == &spawn_driver && 
	 opts.spawn_type != ERTS_SPAWN_EXECUTABLE && 
	 opts.argv != NULL)) {
	/* Argument vector only if explicit spawn_executable */
	goto badarg;
    }

    if (edir != NIL) {
	if ((opts.wd = erts_convert_filename_to_native(edir, NULL, 0, ERTS_ALC_T_TMP,0,1,NULL)) == NULL) {
	    goto badarg;
	}
    }

    if (driver != &spawn_driver && opts.exit_status) {
	goto badarg;
    }
    
    if (IS_TRACED_FL(p, F_TRACE_SCHED_PROCS)) {
        trace_sched(p, ERTS_PROC_LOCK_MAIN, am_out);
    }
    

    erts_proc_unlock(p, ERTS_PROC_LOCK_MAIN);

    port = erts_open_driver(driver, p->common.id, name_buf, &opts, err_typep, err_nump);
#ifdef USE_VM_PROBES
    if (port && DTRACE_ENABLED(port_open)) {
        DTRACE_CHARBUF(process_str, DTRACE_TERM_BUF_SIZE);
        DTRACE_CHARBUF(port_str, DTRACE_TERM_BUF_SIZE);

        dtrace_proc_str(p, process_str);
        erts_snprintf(port_str, sizeof(DTRACE_CHARBUF_NAME(port_str)), "%T", port->common.id);
        DTRACE3(port_open, process_str, name_buf, port_str);
    }
#endif

    if (port && IS_TRACED_FL(port, F_TRACE_PORTS))
        trace_port(port, am_getting_linked, p->common.id);

    erts_proc_lock(p, ERTS_PROC_LOCK_MAIN);

    if (IS_TRACED_FL(p, F_TRACE_SCHED_PROCS)) {
        trace_sched(p, ERTS_PROC_LOCK_MAIN, am_in);
    }

    if (!port) {
	DEBUGF(("open_driver returned (%d:%d)\n",
		err_typep ? *err_typep : 4711,
		err_nump ? *err_nump : 4711));
	goto do_return;
    }

    if (linebuf && port->linebuf == NULL){
	port->linebuf = allocate_linebuf(linebuf);
	sflgs |= ERTS_PORT_SFLG_LINEBUF_IO;
    }

    if (sflgs)
	erts_atomic32_read_bor_relb(&port->state, sflgs);
 
 do_return:
    erts_osenv_clear(&opts.envir);
    if (name_buf)
	erts_free(ERTS_ALC_T_TMP, (void *) name_buf);
    if (opts.argv) {
	free_args(opts.argv);
    }
    if (opts.wd && opts.wd != ((char *)dir)) {
	erts_free(ERTS_ALC_T_TMP, (void *) opts.wd);
    }
    return port;
    
 badarg:
    if (err_typep)
	*err_typep = -3;
    if (err_nump)
	*err_nump = BADARG;
    port = NULL;
    goto do_return;
}

/* Merges the the global environment and the given {Key, Value} list into env,
 * unsetting all keys whose value is either 'false' or NIL. The behavior on
 * NIL is undocumented and perhaps surprising, but the previous implementation
 * worked in this manner. */
static int merge_global_environment(erts_osenv_t *env, Eterm key_value_pairs) {
    const erts_osenv_t *global_env = erts_sys_rlock_global_osenv();
    erts_osenv_merge(env, global_env, 0);
    erts_sys_runlock_global_osenv();

    while (is_list(key_value_pairs)) {
        Eterm *cell, *tuple;

        cell = list_val(key_value_pairs);

        if(!is_tuple_arity(CAR(cell), 2)) {
            return -1;
        }

        tuple = tuple_val(CAR(cell));
        key_value_pairs = CDR(cell);

        if(is_nil(tuple[2]) || tuple[2] == am_false) {
            if(erts_osenv_unset_term(env, tuple[1]) < 0) {
                return -1;
            }
        } else {
            if(erts_osenv_put_term(env, tuple[1], tuple[2]) < 0) {
                return -1;
            }
        }
    }

    if(!is_nil(key_value_pairs)) {
        return -1;
    }

    return 0;
}

/* Arguments can be given i unicode and as raw binaries, convert filename is used to convert */
static char **convert_args(Eterm l)
{
    char **pp;
    char *b;
    Sint n;
    Sint i = 0;
    Eterm str;
    if (is_not_list(l) && is_not_nil(l)) {
	return NULL;
    }

    n = erts_list_length(l);
    if (n < 0)
        return NULL;
    /* We require at least one element in argv[0] + NULL at end */
    pp = erts_alloc(ERTS_ALC_T_TMP, (n + 2) * sizeof(char **));
    pp[i++] = erts_default_arg0;
    while (is_list(l)) {
	str = CAR(list_val(l));
	if ((b = erts_convert_filename_to_native(str,NULL,0,ERTS_ALC_T_TMP,1,1,NULL)) == NULL) {
	    int j;
	    for (j = 1; j < i; ++j)
		erts_free(ERTS_ALC_T_TMP, pp[j]);
	    erts_free(ERTS_ALC_T_TMP, pp);
	    return NULL;
	}	    
	pp[i++] = b;
	l = CDR(list_val(l));
    }
    pp[i] = NULL;
    return pp;
}

static void free_args(char **av)
{
    int i;
    if (av == NULL)
	return;
    for (i = 0; av[i] != NULL; ++i) {
	if (av[i] != erts_default_arg0) {
	    erts_free(ERTS_ALC_T_TMP, av[i]);
	}
    }
    erts_free(ERTS_ALC_T_TMP, av);
}

/* ------------ decode_packet() and friends: */

struct packet_callback_args
{
    Process* p;  /* In */
    Eterm res;   /* Out */
    int string_as_bin; /* return strings as binaries (http_bin): */
    byte* aligned_ptr;
    Uint bin_sz;
    Eterm orig;
    Uint bin_offs;
    byte bin_bitoffs;
};

#define in_area(ptr,start,nbytes) \
    ((UWord)((char*)(ptr) - (char*)(start)) < (nbytes))

static Eterm
http_bld_string(struct packet_callback_args* pca, Uint **hpp, Uint *szp,
		const char *str, Sint len)
{
    Eterm res = THE_NON_VALUE;
    Uint size;
    int make_subbin;

    if (pca->string_as_bin) {
	size = heap_bin_size(len);
	make_subbin = (size > ERL_SUB_BIN_SIZE
		       && in_area(str, pca->aligned_ptr, pca->bin_sz));
	if (szp) {
	    *szp += make_subbin ? ERL_SUB_BIN_SIZE : size;
	}
	if (hpp) {
	    res = make_binary(*hpp);
	    if (make_subbin) {
		ErlSubBin* bin = (ErlSubBin*) *hpp;
		bin->thing_word = HEADER_SUB_BIN;
		bin->size = len;
		bin->offs = pca->bin_offs + ((byte*)str - pca->aligned_ptr);
		bin->orig = pca->orig;
		bin->bitoffs = pca->bin_bitoffs;
		bin->bitsize = 0;
		bin->is_writable = 0;
		*hpp += ERL_SUB_BIN_SIZE;
	    }
	    else {
		ErlHeapBin* bin = (ErlHeapBin*) *hpp;
		bin->thing_word = header_heap_bin(len);
		bin->size = len;
		sys_memcpy(bin->data, str, len);
		*hpp += size;
	    }
	}
    }
    else {
	res = erts_bld_string_n(hpp, szp, str, len);
    }
    return res;
}

static int http_response_erl(void *arg, int major, int minor,
                             int status, const char* phrase, int phrase_len)
{
    /* {http_response,{Major,Minor},Status,"Phrase"} */
    struct packet_callback_args* pca = (struct packet_callback_args*) arg;    
    Eterm phrase_term, ver;
    Uint hsize = 3 + 5;
    Eterm* hp;
#ifdef DEBUG
    Eterm* hend;
#endif

    http_bld_string(pca, NULL, &hsize, phrase, phrase_len);
    hp = HAlloc(pca->p, hsize);
#ifdef DEBUG
    hend = hp + hsize;
#endif
    phrase_term = http_bld_string(pca, &hp, NULL, phrase, phrase_len);
    ver = TUPLE2(hp, make_small(major), make_small(minor));
    hp += 3;
    pca->res = TUPLE4(hp, am_http_response, ver, make_small(status), phrase_term);
    ASSERT(hp+5==hend);
    return 1;
}   
    
static Eterm http_bld_uri(struct packet_callback_args* pca,
			  Eterm** hpp, Uint* szp, const PacketHttpURI* uri)
{
    Eterm s1, s2;
    if (uri->type == URI_STAR) {
        return am_Times; /* '*' */
    }

    s1 = http_bld_string(pca, hpp, szp, uri->s1_ptr, uri->s1_len);

    switch (uri->type) {
    case URI_ABS_PATH:
        return erts_bld_tuple(hpp, szp, 2, am_abs_path, s1);
    case URI_HTTP:
    case URI_HTTPS:
        s2 = http_bld_string(pca, hpp, szp, uri->s2_ptr, uri->s2_len);
        return erts_bld_tuple
            (hpp, szp, 5, am_absoluteURI, 
             ((uri->type==URI_HTTP) ? am_http : am_https),
             s1, 
             ((uri->port==0) ? am_undefined : make_small(uri->port)),
             s2);
        
    case URI_STRING:
        return s1;
    case URI_SCHEME:
        s2 = http_bld_string(pca, hpp, szp, uri->s2_ptr, uri->s2_len);
        return erts_bld_tuple(hpp, szp, 3, am_scheme, s1, s2);
                              
    default:
        erts_exit(ERTS_ERROR_EXIT, "%s, line %d: type=%u\n", __FILE__, __LINE__, uri->type);
    }
}

static int http_request_erl(void* arg, const http_atom_t* meth,
                            const char* meth_ptr, int meth_len,
                            const PacketHttpURI* uri, int major, int minor)
{
    struct packet_callback_args* pca = (struct packet_callback_args*) arg;    
    Eterm meth_term, uri_term, ver_term;
    Uint sz = 0;
    Uint* szp = &sz;
    Eterm* hp;
    Eterm** hpp = NULL;

    /* {http_request,Meth,Uri,Version} */

    for (;;) {
        meth_term = (meth!=NULL) ? meth->atom :
	    http_bld_string(pca, hpp, szp, meth_ptr, meth_len);
        uri_term = http_bld_uri(pca, hpp, szp, uri);
        ver_term = erts_bld_tuple(hpp, szp, 2,
                                  make_small(major), make_small(minor));
        pca->res = erts_bld_tuple(hpp, szp, 4, am_http_request, meth_term,
                                  uri_term, ver_term); 
        if (hpp != NULL) break;
        hpp = &hp;
        hp = HAlloc(pca->p, sz);
        szp = NULL;        
    }
    return 1;
}

static int
http_header_erl(void* arg, const http_atom_t* name, const char* name_ptr,
                int name_len, const char* value_ptr, int value_len)
{
    struct packet_callback_args* pca = (struct packet_callback_args*) arg;    
    Eterm bit_term, name_term, val_term;
    Uint sz = 6;
    Eterm* hp;
#ifdef DEBUG
    Eterm* hend;
#endif
    
    /* {http_header,Bit,Name,IValue,Value} */

    if (name == NULL) {
	http_bld_string(pca, NULL, &sz, name_ptr, name_len);
    }
    http_bld_string(pca, NULL, &sz, value_ptr, value_len);

    hp = HAlloc(pca->p, sz);
#ifdef DEBUG
    hend = hp + sz;
#endif	

    if (name != NULL) {
	bit_term = make_small(name->index+1);
	name_term = name->atom;
    }
    else {	
	bit_term = make_small(0);	
	name_term = http_bld_string(pca, &hp,NULL,name_ptr,name_len);
    }

    val_term = http_bld_string(pca, &hp, NULL, value_ptr, value_len);
    pca->res = TUPLE5(hp, am_http_header, bit_term, name_term, am_undefined, val_term);
    ASSERT(hp+6==hend);
    return 1;
}   

static int http_eoh_erl(void* arg)
{
    /* http_eoh */
    struct packet_callback_args* pca = (struct packet_callback_args*) arg;    
    pca->res = am_http_eoh;
    return 1;
}

static int http_error_erl(void* arg, const char* buf, int len)
{
    /* {http_error,Line} */
    struct packet_callback_args* pca = (struct packet_callback_args*) arg;
    Uint sz = 3;
    Eterm* hp;
#ifdef DEBUG
    Eterm* hend;
#endif

    http_bld_string(pca, NULL, &sz, buf, len);

    hp = HAlloc(pca->p, sz);
#ifdef DEBUG
    hend = hp + sz;
#endif
    pca->res = erts_bld_tuple(&hp, NULL, 2, am_http_error,
			      http_bld_string(pca, &hp, NULL, buf, len));
    ASSERT(hp==hend);
    return 1;
}

static
int ssl_tls_erl(void* arg, unsigned type, unsigned major, unsigned minor,
		const char* buf, int len, const char* prefix, int plen)
{
    struct packet_callback_args* pca = (struct packet_callback_args*) arg;
    Eterm* hp;
    Eterm ver;
    Eterm bin = new_binary(pca->p, NULL, plen+len);
    byte* bin_ptr = binary_bytes(bin);

    sys_memcpy(bin_ptr+plen, buf, len);
    if (plen) {
        sys_memcpy(bin_ptr, prefix, plen);
    }

    /* {ssl_tls,NIL,ContentType,{Major,Minor},Bin} */
    hp = HAlloc(pca->p, 3+6);
    ver = TUPLE2(hp, make_small(major), make_small(minor));
    hp += 3;
    pca->res = TUPLE5(hp, am_ssl_tls, NIL, make_small(type), ver, bin);
    return 1;
}


PacketCallbacks packet_callbacks_erl = {
    http_response_erl,
    http_request_erl,
    http_eoh_erl,
    http_header_erl,
    http_error_erl,
    ssl_tls_erl
};

/*
 decode_packet(Type,Bin,Options)
 Returns:
     {ok, PacketBodyBin, RestBin}
     {more, PacketSz | undefined}
     {error, invalid}
*/
BIF_RETTYPE decode_packet_3(BIF_ALIST_3)
{
    unsigned max_plen = 0;   /* Packet max length, 0=no limit */
    unsigned trunc_len = 0;  /* Truncate lines if longer, 0=no limit */
    int http_state = 0;      /* 0=request/response 1=header */
    int packet_sz;           /*-------Binaries involved: ------------------*/
    byte* bin_ptr;           /*| orig: original binary                     */
    byte bin_bitsz;          /*| bin: BIF_ARG_2, may be sub-binary of orig */
	                     /*| packet: prefix of bin                     */
    char* body_ptr;          /*| body: part of packet to return            */
    int body_sz;             /*| rest: bin without packet                  */
    struct packet_callback_args pca;
    enum PacketParseType type;
    Eterm* hp;
    Eterm* hend;
    ErlSubBin* rest;
    Eterm res;
    Eterm options;
    int   code;
    char  delimiter = '\n';

    if (!is_binary(BIF_ARG_2) || 
        (!is_list(BIF_ARG_3) && !is_nil(BIF_ARG_3))) {
        BIF_ERROR(BIF_P, BADARG);
    }
    switch (BIF_ARG_1) {
    case make_small(0): case am_raw: type = TCP_PB_RAW; break;
    case make_small(1): type = TCP_PB_1; break;
    case make_small(2): type = TCP_PB_2; break;
    case make_small(4): type = TCP_PB_4; break;
    case am_asn1: type = TCP_PB_ASN1; break;
    case am_sunrm: type = TCP_PB_RM; break;
    case am_cdr: type = TCP_PB_CDR; break;
    case am_fcgi: type = TCP_PB_FCGI; break;
    case am_line: type = TCP_PB_LINE_LF; break;
    case am_tpkt: type = TCP_PB_TPKT; break;
    case am_http: type = TCP_PB_HTTP; break;
    case am_httph: type = TCP_PB_HTTPH; break;
    case am_http_bin: type = TCP_PB_HTTP_BIN; break;
    case am_httph_bin: type = TCP_PB_HTTPH_BIN; break;
    case am_ssl_tls: type = TCP_PB_SSL_TLS; break;
    default:
        BIF_ERROR(BIF_P, BADARG);
    }

    options = BIF_ARG_3;
    while (!is_nil(options)) {
        Eterm* cons = list_val(options);
        if (is_tuple(CAR(cons))) {
            Eterm* tpl = tuple_val(CAR(cons));
            Uint val;
            if (tpl[0] == make_arityval(2) &&
		term_to_Uint(tpl[2],&val) && val <= UINT_MAX) {
                switch (tpl[1]) {
                case am_packet_size:
                    max_plen = val;
                    goto next_option;
                case am_line_length:
                    trunc_len = val;
                    goto next_option;
                case am_line_delimiter:
                    if (type == TCP_PB_LINE_LF && val <= 255) {
                        delimiter = (char)val;
                        goto next_option;
                    }
                }
            }
        }
        BIF_ERROR(BIF_P, BADARG);

    next_option:       
        options = CDR(cons);
    }


    pca.bin_sz = binary_size(BIF_ARG_2);
    ERTS_GET_BINARY_BYTES(BIF_ARG_2, bin_ptr, pca.bin_bitoffs, bin_bitsz);  
    if (pca.bin_bitoffs != 0) {
        pca.aligned_ptr = erts_alloc(ERTS_ALC_T_TMP, pca.bin_sz);
        erts_copy_bits(bin_ptr, pca.bin_bitoffs, 1, pca.aligned_ptr, 0, 1, pca.bin_sz*8);
    }
    else {
        pca.aligned_ptr = bin_ptr;
    }
    packet_sz = packet_get_length(type, (char*)pca.aligned_ptr, pca.bin_sz,
                                  max_plen, trunc_len, delimiter, &http_state);
    if (!(packet_sz > 0 && packet_sz <= pca.bin_sz)) {
        if (packet_sz < 0) {
	    goto error;
        }
        else { /* not enough data */
            Eterm plen = (packet_sz==0) ? am_undefined : 
                erts_make_integer(packet_sz, BIF_P);
            Eterm* hp = HAlloc(BIF_P,3);        
            res = TUPLE2(hp, am_more, plen);
            goto done;
        }
    }
    /* We got a whole packet */

    body_ptr = (char*) pca.aligned_ptr;
    body_sz = packet_sz;
    packet_get_body(type, (const char**) &body_ptr, &body_sz);

    ERTS_GET_REAL_BIN(BIF_ARG_2, pca.orig, pca.bin_offs, pca.bin_bitoffs, bin_bitsz);
    pca.p = BIF_P;
    pca.res = THE_NON_VALUE;
    pca.string_as_bin = (type == TCP_PB_HTTP_BIN || type == TCP_PB_HTTPH_BIN);
    code = packet_parse(type, (char*)pca.aligned_ptr, packet_sz, &http_state,
			&packet_callbacks_erl, &pca);
    if (code == 0) { /* no special packet parsing, make plain binary */
        ErlSubBin* body;
        Uint hsz = 2*ERL_SUB_BIN_SIZE + 4;
        hp = HAlloc(BIF_P, hsz);
        hend = hp + hsz;

        body = (ErlSubBin *) hp;
        body->thing_word = HEADER_SUB_BIN;
        body->size = body_sz;
        body->offs = pca.bin_offs + (body_ptr - (char*)pca.aligned_ptr);
        body->orig = pca.orig;
        body->bitoffs = pca.bin_bitoffs;
        body->bitsize = 0;
        body->is_writable = 0;
        hp += ERL_SUB_BIN_SIZE;
        pca.res = make_binary(body);
    }
    else if (code > 0) {
	Uint hsz = ERL_SUB_BIN_SIZE + 4;
	ASSERT(pca.res != THE_NON_VALUE);
	hp = HAlloc(BIF_P, hsz);
	hend = hp + hsz;
    }
    else {
error:
	hp = HAlloc(BIF_P,3);        
	res = TUPLE2(hp, am_error, am_invalid);
	goto done;
    }

    rest = (ErlSubBin *) hp;
    rest->thing_word = HEADER_SUB_BIN;
    rest->size = pca.bin_sz - packet_sz;
    rest->offs = pca.bin_offs + packet_sz;
    rest->orig = pca.orig;
    rest->bitoffs = pca.bin_bitoffs;
    rest->bitsize = bin_bitsz;   /* The extra bits go into the rest. */
    rest->is_writable = 0;
    hp += ERL_SUB_BIN_SIZE;
    res = TUPLE3(hp, am_ok, pca.res, make_binary(rest));
    hp += 4;
    ASSERT(hp==hend); (void)hend;

done:
    if (pca.aligned_ptr != bin_ptr) {
        erts_free(ERTS_ALC_T_TMP, pca.aligned_ptr);
    }
    BIF_RET(res);
}

