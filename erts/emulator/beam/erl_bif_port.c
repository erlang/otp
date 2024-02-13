/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2024. All Rights Reserved.
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
#include "erl_osenv.h"

#define HALLOC_EXTRA 200

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
    ErtsLink *proc_lnk, *port_lnk;

    port = open_port(BIF_P, BIF_ARG_1, BIF_ARG_2, &err_type, &err_num);
    if (!port) {
	if (err_type == -4) {
            /* Invalid settings arguments. */
            return am_badopt;
        } else if (err_type == -3) {
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

    proc_lnk = erts_link_internal_create(ERTS_LNK_TYPE_PORT, port->common.id);
    port_lnk = erts_link_internal_create(ERTS_LNK_TYPE_PORT, BIF_P->common.id);
    /*
     * This link should not already be present, but can potentially
     * due to id wrapping...
     */
    if (!!erts_link_tree_lookup_insert(&ERTS_P_LINKS(BIF_P), proc_lnk))
        erts_link_internal_release(proc_lnk);
    erts_link_tree_insert(&ERTS_P_LINKS(port), port_lnk);

    if (port->drv_ptr->flags & ERL_DRV_FLAG_USE_INIT_ACK) {

        /* Copied from erl_port_task.c */
        port->async_open_port = erts_alloc(ERTS_ALC_T_PRTSD,
                                           sizeof(*port->async_open_port));
        erts_make_ref_in_array(port->async_open_port->ref);
        port->async_open_port->to = BIF_P->common.id;

        /*
         * We unconditionally *must* do a receive on a message
         * containing the reference after this...
         */
        erts_msgq_set_save_end(BIF_P);

        res = erts_proc_store_ref(BIF_P, port->async_open_port->ref);
    } else {
        res = port->common.id;
    }

    if (ERTS_IS_P_TRACED_FL(BIF_P, F_TRACE_PROCS))
        trace_proc(BIF_P, ERTS_PROC_LOCK_MAIN, BIF_P,
                   am_link, port->common.id);

    ERTS_BIF_PREP_RET(ret, res);

    erts_port_release(port);

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
    erts_aint32_t state;

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
		BIF_RET(am_badopt);
	    l = CDR(cons);
	}
	if (!is_nil(l))
	    BIF_RET(am_badopt);
    }

    prt = sig_lookup_port(BIF_P, BIF_ARG_1);
    if (!prt)
        ERTS_BIF_PREP_RET(res, am_badarg);
    else if ((flags & ERTS_PORT_SIG_FLG_FORCE)
             && !(prt->drv_ptr->flags & ERL_DRV_FLAG_SOFT_BUSY)) {
        ERTS_BIF_PREP_RET(res, am_notsup);
    }
    else {
        Eterm ref;
#ifdef DEBUG
        ref = NIL;
#endif

	switch (erts_port_output(BIF_P, flags, prt, prt->common.id,
				 BIF_ARG_2, &ref)) {
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
		ERTS_BIF_YIELD3(BIF_TRAP_EXPORT(BIF_erts_internal_port_command_3),
				BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
	    }
	    break;
	case ERTS_PORT_OP_BUSY_SCHEDULED:
	    ASSERT(!(flags & ERTS_PORT_SIG_FLG_FORCE));
	    /* Fall through... */
	case ERTS_PORT_OP_SCHEDULED:
	    ASSERT(is_internal_ordinary_ref(ref));
	    /* Signal order preserved by reply... */
	    BIF_RET(ref);
	    break;
	case ERTS_PORT_OP_DONE:
	    ERTS_BIF_PREP_RET(res, am_true);
	    break;
	default:
	    ERTS_INTERNAL_ERROR("Unexpected erts_port_output() result");
	    break;
	}
    }

    state = erts_atomic32_read_acqb(&BIF_P->state);
    if (state & ERTS_PSFLG_EXITING) {
	KILL_CATCHES(BIF_P);	/* Must exit */
	ERTS_BIF_PREP_ERROR(res, BIF_P, EXC_ERROR);
    }
    else {
        /* Ensure signal order is preserved... */
        if (state & (ERTS_PSFLG_SIG_Q
                     | ERTS_PSFLG_NMSG_SIG_IN_Q
                     | ERTS_PSFLG_MSG_SIG_IN_Q)) {
            Eterm from;
            if (is_internal_port(BIF_ARG_1))
                from = BIF_ARG_1;
            else if (prt)
                from = prt->common.id;
            else
                from = NIL;
            ERTS_BIF_PREP_HANDLE_SIGNALS_FROM_RETURN(res, BIF_P,
                                                     from, res);
        }
    }

    return res;
}

BIF_RETTYPE erts_internal_port_call_3(BIF_ALIST_3)
{
    Port* prt;
    Eterm retval;
    Uint uint_op;
    erts_aint32_t state;

    prt = sig_lookup_port(BIF_P, BIF_ARG_1);
    if (!prt)
	retval = am_badarg;
    else if (!term_to_Uint(BIF_ARG_2, &uint_op))
	retval = am_badarg;
    else if (uint_op > (Uint) UINT_MAX)
	retval = am_badarg;
    else {
        unsigned int op = (unsigned int) uint_op;

        switch (erts_port_call(BIF_P, prt, op, BIF_ARG_3, &retval)) {
        case ERTS_PORT_OP_DROPPED:
        case ERTS_PORT_OP_BADARG:
            retval = am_badarg;
            break;
        case ERTS_PORT_OP_SCHEDULED:
            ASSERT(is_internal_ordinary_ref(retval));
            /* Signal order preserved by reply... */
            BIF_RET(retval);
            break;
        case ERTS_PORT_OP_DONE:
            ASSERT(is_not_internal_ref(retval));
            break;
        default:
            ERTS_INTERNAL_ERROR("Unexpected erts_port_call() result");
            retval = am_internal_error;
            break;
        }
    }

    state = erts_atomic32_read_acqb(&BIF_P->state);
    if (state & ERTS_PSFLG_EXITING)
	ERTS_BIF_EXITED(BIF_P);
    else {
        /* Ensure signal order is preserved... */
        if (state & (ERTS_PSFLG_SIG_Q
                     | ERTS_PSFLG_NMSG_SIG_IN_Q
                     | ERTS_PSFLG_MSG_SIG_IN_Q)) {
            Eterm from;
            if (is_internal_port(BIF_ARG_1))
                from = BIF_ARG_1;
            else if (prt)
                from = prt->common.id;
            else
                from = NIL;
            ERTS_BIF_HANDLE_SIGNALS_FROM_RETURN(BIF_P, from, retval);
        }
    }

    BIF_RET(retval);
}

BIF_RETTYPE erts_internal_port_control_3(BIF_ALIST_3)
{
    Port* prt;
    Eterm retval;
    Uint uint_op;
    erts_aint32_t state;

    prt = sig_lookup_port(BIF_P, BIF_ARG_1);
    if (!prt)
	retval = am_badarg;
    else if (!term_to_Uint(BIF_ARG_2, &uint_op))
	retval = am_badarg;
    else if (uint_op > (Uint) UINT_MAX)
	retval = am_badarg;
    else {
        unsigned int op = (unsigned int) uint_op;

        switch (erts_port_control(BIF_P, prt, op, BIF_ARG_3, &retval)) {
        case ERTS_PORT_OP_BADARG:
        case ERTS_PORT_OP_DROPPED:
            retval = am_badarg;
            break;
        case ERTS_PORT_OP_SCHEDULED:
            ASSERT(is_internal_ordinary_ref(retval));
            /* Signal order preserved by reply... */
            BIF_RET(retval);
        case ERTS_PORT_OP_DONE:
            ASSERT(is_not_internal_ref(retval));
            break;
        default:
            ERTS_INTERNAL_ERROR("Unexpected erts_port_control() result");
            retval = am_internal_error;
            break;
        }
    }

    state = erts_atomic32_read_acqb(&BIF_P->state);
    if (state & ERTS_PSFLG_EXITING)
	ERTS_BIF_EXITED(BIF_P);
    else {
        /* Ensure signal order is preserved... */
        if (state & (ERTS_PSFLG_SIG_Q
                     | ERTS_PSFLG_NMSG_SIG_IN_Q
                     | ERTS_PSFLG_MSG_SIG_IN_Q)) {
            Eterm from;
            if (is_internal_port(BIF_ARG_1))
                from = BIF_ARG_1;
            else if (prt)
                from = prt->common.id;
            else
                from = NIL;
            ERTS_BIF_HANDLE_SIGNALS_FROM_RETURN(BIF_P, from, retval);
        }
    }

    BIF_RET(retval);
}

/*
 * erts_internal:port_close/1 is used by the
 * erlang:port_close/1 BIF.
 */
BIF_RETTYPE erts_internal_port_close_1(BIF_ALIST_1)
{
    Eterm retval;
    Port *prt;

    prt = sig_lookup_port(BIF_P, BIF_ARG_1);
    if (!prt)
	retval = am_badarg;
    else {

#ifdef DEBUG
        retval = NIL;
#endif

        switch (erts_port_exit(BIF_P, 0, prt, BIF_P->common.id,
                               am_normal, &retval)) {
        case ERTS_PORT_OP_BADARG:
        case ERTS_PORT_OP_DROPPED:
            retval = am_badarg;
            break;
        case ERTS_PORT_OP_SCHEDULED:
            ASSERT(is_internal_ordinary_ref(retval));
            /* Signal order preserved by reply... */
            BIF_RET(retval);
            break;
        case ERTS_PORT_OP_DONE:
            retval = am_true;
            break;
        default:
            ERTS_INTERNAL_ERROR("Unexpected erts_port_exit() result");
            retval = am_internal_error;
            break;
        }
    }

    /* Ensure signal order is preserved... */
    if (ERTS_PROC_HAS_INCOMING_SIGNALS(BIF_P)) {
        Eterm from;
        if (is_internal_port(BIF_ARG_1))
            from = BIF_ARG_1;
        else if (prt)
            from = prt->common.id;
        else
            from = NIL;
        ERTS_BIF_HANDLE_SIGNALS_FROM_RETURN(BIF_P, from, retval);
    }
    
    BIF_RET(retval);
}

/*
 * erts_internal:port_connect/2 is used by the
 * erlang:port_connect/2 BIF.
 */
BIF_RETTYPE erts_internal_port_connect_2(BIF_ALIST_2)
{
    Eterm retval;
    Port* prt;

    prt = sig_lookup_port(BIF_P, BIF_ARG_1);
    if (!prt)
	retval = am_badarg;
    else {
#ifdef DEBUG
        retval = NIL;
#endif

        switch (erts_port_connect(BIF_P, 0, prt, BIF_P->common.id,
                                  BIF_ARG_2, &retval)) {
        case ERTS_PORT_OP_BADARG:
        case ERTS_PORT_OP_DROPPED:
            retval = am_badarg;
            break;
        case ERTS_PORT_OP_SCHEDULED:
            ASSERT(is_internal_ordinary_ref(retval));
            /* Signal order preserved by reply... */
            BIF_RET(retval);
        case ERTS_PORT_OP_DONE:
            retval = am_true;
            break;
        default:
            ERTS_INTERNAL_ERROR("Unexpected erts_port_connect() result");
            retval = am_internal_error;
            break;
        }
    }

    /* Ensure signal order is preserved... */
    if (ERTS_PROC_HAS_INCOMING_SIGNALS(BIF_P)) {
        Eterm from;
        if (is_internal_port(BIF_ARG_1))
            from = BIF_ARG_1;
        else if (prt)
            from = prt->common.id;
        else
            from = NIL;
        ERTS_BIF_HANDLE_SIGNALS_FROM_RETURN(BIF_P, from, retval);
    }
    
    BIF_RET(retval);
}

BIF_RETTYPE erts_internal_port_info_1(BIF_ALIST_1)
{
    Eterm retval;
    Port* prt = NULL;

    if (is_internal_port(BIF_ARG_1) || is_atom(BIF_ARG_1)) {
	prt = sig_lookup_port(BIF_P, BIF_ARG_1);
	if (!prt)
	    retval = am_undefined;
        else {
            switch (erts_port_info(BIF_P, prt, THE_NON_VALUE, &retval)) {
            case ERTS_PORT_OP_BADARG:
                retval = am_badarg;
                break;
            case ERTS_PORT_OP_DROPPED:
                retval = am_undefined;
                break;
            case ERTS_PORT_OP_SCHEDULED:
                ASSERT(is_internal_ordinary_ref(retval));
                /* Signal order preserved by reply... */
                BIF_RET(retval);
            case ERTS_PORT_OP_DONE:
                ASSERT(is_not_internal_ref(retval));
                break;
            default:
                ERTS_INTERNAL_ERROR("Unexpected erts_port_info() result");
                retval = am_internal_error;
                break;
            }
        }
    }
    else if (is_external_port(BIF_ARG_1)) {
	if (external_port_dist_entry(BIF_ARG_1) == erts_this_dist_entry)
	    retval = am_undefined;
	else
	    retval = am_badarg;
    }
    else {
	retval = am_badarg;
    }

    /* Ensure signal order is preserved... */
    if (ERTS_PROC_HAS_INCOMING_SIGNALS(BIF_P)) {
        Eterm from;
        if (is_internal_port(BIF_ARG_1))
            from = BIF_ARG_1;
        else if (prt)
            from = prt->common.id;
        else
            from = NIL;
        ERTS_BIF_HANDLE_SIGNALS_FROM_RETURN(BIF_P, from, retval);
    }
    
    BIF_RET(retval);
}


BIF_RETTYPE erts_internal_port_info_2(BIF_ALIST_2)
{
    Eterm retval;
    Port* prt = NULL;

    if (is_internal_port(BIF_ARG_1) || is_atom(BIF_ARG_1)) {
	prt = sig_lookup_port(BIF_P, BIF_ARG_1);
	if (!prt)
	    retval = am_undefined;
        else {
            switch (erts_port_info(BIF_P, prt, BIF_ARG_2, &retval)) {
            case ERTS_PORT_OP_BADARG:
                retval = am_badarg;
                break;
            case ERTS_PORT_OP_DROPPED:
                retval = am_undefined;
                break;
            case ERTS_PORT_OP_SCHEDULED:
                ASSERT(is_internal_ordinary_ref(retval));
                /* Signal order preserved by reply... */
                BIF_RET(retval);
            case ERTS_PORT_OP_DONE:
                ASSERT(is_not_internal_ref(retval));
                break;
            default:
                ERTS_INTERNAL_ERROR("Unexpected erts_port_info() result");
                retval = am_internal_error;
                break;
            }
        }
    }
    else if (is_external_port(BIF_ARG_1)) {
	if (external_port_dist_entry(BIF_ARG_1) == erts_this_dist_entry)
	    retval = am_undefined;
	else
	    retval = am_badtype;
    }
    else {
	retval = am_badtype;
    }

    /* Ensure signal order is preserved... */
    if (ERTS_PROC_HAS_INCOMING_SIGNALS(BIF_P)) {
        Eterm from;
        if (is_internal_port(BIF_ARG_1))
            from = BIF_ARG_1;
        else if (prt)
            from = prt->common.id;
        else
            from = NIL;
        ERTS_BIF_HANDLE_SIGNALS_FROM_RETURN(BIF_P, from, retval);
    }

    BIF_RET(retval);
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
 * either BADARG or SYSTEM_LIMIT),
 * -4 if port settings were invalid.
 */

static Port *
open_port(Process* p, Eterm name, Eterm settings, int *err_typep, int *err_nump)
{
    int merged_environment = 0;
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
    opts.high_watermark = 8192;
    opts.low_watermark = opts.high_watermark / 2;
    opts.port_watermarks_set = 0;
    opts.msgq_watermarks_set = 0;
    erts_osenv_init(&opts.envir);

    linebuf = 0;

    *err_nump = 0;

    if (is_not_list(settings) && is_not_nil(settings)) {
	goto bad_settings;
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
			goto bad_settings;
		    }
		    opts.packet_bytes = signed_val(*tp);
		    switch (opts.packet_bytes) {
		    case 1:
		    case 2:
		    case 4:
			break;
		    default:
			goto bad_settings;
		   }
		} else if (option == am_line) {
		    if (is_not_small(*tp)) {
			goto bad_settings;
		    }
		    linebuf = signed_val(*tp);
		    if (linebuf <= 0) {
			goto bad_settings;
		    }
		} else if (option == am_env) {
		    if (merged_environment) {
		        /* Ignore previous env option */
		        erts_osenv_clear(&opts.envir);
		    }

		    merged_environment = 1;

		    if (merge_global_environment(&opts.envir, *tp)) {
		        goto bad_settings;
		    }
		} else if (option == am_args) {
		    char **av;
		    char **oav = opts.argv;
		    if ((av = convert_args(*tp)) == NULL) {
			goto bad_settings;
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
			goto bad_settings;
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
			goto bad_settings;
                } else if (option == am_busy_limits_port) {
                    Uint high, low;
                    if (*tp == am_disabled)
                        low = high = ERL_DRV_BUSY_MSGQ_DISABLED;
                    else if (!is_tuple_arity(*tp, 2))
                        goto bad_settings;
                    else {
                        Eterm *wtp = tuple_val(*tp);
                        if (!term_to_Uint(wtp[1], &low))
                            goto bad_settings;
                        if (!term_to_Uint(wtp[2], &high))
                            goto bad_settings;
                        if (high < ERL_DRV_BUSY_MSGQ_LIM_MIN)
                            goto bad_settings;
                        if (high > ERL_DRV_BUSY_MSGQ_LIM_MAX)
                            goto bad_settings;
                        if (low < ERL_DRV_BUSY_MSGQ_LIM_MIN)
                            goto bad_settings;
                        if (low > ERL_DRV_BUSY_MSGQ_LIM_MAX)
                            goto bad_settings;
                        if (high == ~((Uint) 0) || low == ~((Uint) 0))
                            goto bad_settings;
                        if (low > high)
                            low = high;
                    }
                    opts.low_watermark = low;
                    opts.high_watermark = high;
                    opts.port_watermarks_set = !0;
                } else if (option == am_busy_limits_msgq) {
                    Uint high, low;
                    if (*tp == am_disabled)
                        low = high = ERL_DRV_BUSY_MSGQ_DISABLED;
                    else if (!is_tuple_arity(*tp, 2))
                        goto bad_settings;
                    else {
                        Eterm *wtp = tuple_val(*tp);
                        if (!term_to_Uint(wtp[1], &low))
                            goto bad_settings;
                        if (!term_to_Uint(wtp[2], &high))
                            goto bad_settings;
                        if (high < ERL_DRV_BUSY_MSGQ_LIM_MIN)
                            goto bad_settings;
                        if (high > ERL_DRV_BUSY_MSGQ_LIM_MAX)
                            goto bad_settings;
                        if (low < ERL_DRV_BUSY_MSGQ_LIM_MIN)
                            goto bad_settings;
                        if (low > ERL_DRV_BUSY_MSGQ_LIM_MAX)
                            goto bad_settings;
                        if (high == ~((Uint) 0) || low == ~((Uint) 0))
                            goto bad_settings;
                        if (low > high)
                            low = high;
                    }
                    opts.low_msgq_watermark = low;
                    opts.high_msgq_watermark = high;
                    opts.msgq_watermarks_set = !0;
		} else {
		    goto bad_settings;
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
		goto bad_settings;
	    }
	    if (is_nil(*++nargs)) 
		break;
	    if (is_not_list(*nargs)) {
		goto bad_settings;
	    }
	    nargs = list_val(*nargs);
	}
    }

    if (opts.read_write == 0)	/* implement default */
	opts.read_write = DO_READ|DO_WRITE;

    /* Mutually exclusive arguments. */
    if((linebuf && opts.packet_bytes) || 
       (opts.redir_stderr && !opts.use_stdio)) {
	goto bad_settings;
    }

    /* If we lacked an env option, fill in the global environment without
     * changes. */
    if (!merged_environment) {
        merge_global_environment(&opts.envir, NIL);
    }

    /*
     * Parse the first argument and start the appropriate driver.
     */
	if (is_not_tuple(name)) {
	    goto badarg;		/* Not a process or fd port */
	}
	tp = tuple_val(name);
	arity = *tp++;

	if (arity == make_arityval_zero()) {
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
	    /* since the cmd is used for comparison with drivers names and copied to port info */
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
    
    if (ERTS_IS_P_TRACED_FL(p, F_TRACE_SCHED_PROCS)) {
        trace_sched(p, ERTS_PROC_LOCK_MAIN, am_out, F_TRACE_SCHED_PROCS);
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

    if (port && ERTS_IS_P_TRACED_FL(port, F_TRACE_PORTS))
        trace_port(port, am_getting_linked, p->common.id, F_TRACE_PORTS);

    erts_proc_lock(p, ERTS_PROC_LOCK_MAIN);

    if (ERTS_IS_P_TRACED_FL(p, F_TRACE_SCHED_PROCS)) {
        trace_sched(p, ERTS_PROC_LOCK_MAIN, am_in, F_TRACE_SCHED_PROCS);
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

 bad_settings:
    if (err_typep)
	*err_typep = -4;
    port = NULL;
    goto do_return;

 badarg:
    if (err_typep)
	*err_typep = -3;
    if (err_nump)
	*err_nump = BADARG;
    port = NULL;
    goto do_return;
}

/* Merges the global environment and the given {Key, Value} list into env,
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
    ErtsHeapFactory factory;
    int string_as_bin; /* return strings as binaries (http_bin): */
    const byte *aligned_ptr;
    Eterm orig;
    Uint bin_sz;
};

#define in_area(ptr,start,nbytes) \
    ((UWord)((char*)(ptr) - (char*)(start)) < (nbytes))

static Eterm
http_bld_string(struct packet_callback_args* pca, const char *str, Sint len)
{
    if (pca->string_as_bin) {
        if (!ErtsInArea(str, pca->aligned_ptr, pca->bin_sz)) {
            return erts_new_binary_from_data(pca->p, len, (byte*)str);
        }

        return erts_make_sub_binary(pca->p,
                                    pca->orig,
                                    ((byte*)str - pca->aligned_ptr),
                                    len);
    } else {
        Eterm *hp = erts_produce_heap(&pca->factory, len * 2, HALLOC_EXTRA);
        return erts_bld_string_n(&hp, NULL, str, len);
    }
}

static int http_response_erl(void *arg, int major, int minor,
                             int status, const char* phrase, int phrase_len)
{
    /* {http_response,{Major,Minor},Status,"Phrase"} */
    struct packet_callback_args* pca = (struct packet_callback_args*) arg;    
    Eterm phrase_term, ver;
    Eterm *hp;

    phrase_term = http_bld_string(pca, phrase, phrase_len);

    hp = erts_produce_heap(&pca->factory, 3 + 5, HALLOC_EXTRA);
    ver = TUPLE2(&hp[0], make_small(major), make_small(minor));
    pca->res = TUPLE4(&hp[3],
                      am_http_response,
                      ver,
                      make_small(status),
                      phrase_term);

    return 1;
}

static Eterm http_bld_uri(struct packet_callback_args* pca, const PacketHttpURI* uri)
{
    Eterm s1, s2;

    if (uri->type == URI_STAR) {
        return am_Times; /* '*' */
    }

    s1 = http_bld_string(pca, uri->s1_ptr, uri->s1_len);

    switch (uri->type) {
    case URI_ABS_PATH:
        {
            Eterm *hp = erts_produce_heap(&pca->factory, 3, HALLOC_EXTRA);
            return TUPLE2(hp, am_abs_path, s1);
        }
    case URI_HTTP:
    case URI_HTTPS:
        {
            Eterm *hp;
            s2 = http_bld_string(pca, uri->s2_ptr, uri->s2_len);
            hp = erts_produce_heap(&pca->factory, 6, HALLOC_EXTRA);
            return TUPLE5(hp,
                          am_absoluteURI,
                          ((uri->type==URI_HTTP) ? am_http : am_https),
                          s1,
                          ((uri->port==0) ? am_undefined : make_small(uri->port)),
                          s2);
        }
    case URI_STRING:
        return s1;
    case URI_SCHEME:
        {
            Eterm *hp;
            s2 = http_bld_string(pca, uri->s2_ptr, uri->s2_len);
            hp = erts_produce_heap(&pca->factory, 4, HALLOC_EXTRA);
            return TUPLE3(hp, am_scheme, s1, s2);
        }
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
    Eterm *hp;

    /* {http_request,Meth,Uri,Version} */
    meth_term = (meth != NULL) ?
        meth->atom :
        http_bld_string(pca, meth_ptr, meth_len);
    uri_term = http_bld_uri(pca, uri);

    hp = erts_produce_heap(&pca->factory, 3 + 5, HALLOC_EXTRA);
    ver_term = TUPLE2(&hp[0], make_small(major), make_small(minor));
    pca->res = TUPLE4(&hp[3], am_http_request, meth_term, uri_term, ver_term);

    return 1;
}

static int
http_header_erl(void* arg, const http_atom_t* name,
                const char* name_ptr, int name_len,
                const char* oname_ptr, int oname_len,
                const char* value_ptr, int value_len)
{
    struct packet_callback_args* pca = (struct packet_callback_args*) arg;    
    Eterm bit_term, name_term, oname_term, val_term;
    Eterm *hp;

    /* {http_header,Bit,Name,Oname,Value} */
    if (name != NULL) {
        bit_term = make_small(name->index+1);
        name_term = name->atom;
    } else {
        bit_term = make_small(0);
        name_term = http_bld_string(pca, name_ptr, name_len);
    }

    oname_term = http_bld_string(pca, oname_ptr, oname_len);
    val_term = http_bld_string(pca, value_ptr, value_len);

    hp = erts_produce_heap(&pca->factory, 6, HALLOC_EXTRA);
    pca->res = TUPLE5(hp,
                      am_http_header,
                      bit_term,
                      name_term,
                      oname_term,
                      val_term);

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
    Eterm* hp;

    hp = erts_produce_heap(&pca->factory, 3, HALLOC_EXTRA);
    pca->res = TUPLE2(hp, am_http_error, http_bld_string(pca, buf, len));

    return 1;
}

static
int ssl_tls_erl(void* arg, unsigned type, unsigned major, unsigned minor,
		const char* buf, int len, const char* prefix, int plen)
{
    struct packet_callback_args* pca = (struct packet_callback_args*) arg;
    Eterm* hp;
    Eterm ver;
    byte* bin_ptr;

    Eterm bin = erts_hfact_new_bitstring(&pca->factory,
                                         0,
                                         NBITS(plen + len),
                                         &bin_ptr);
    sys_memcpy(bin_ptr + plen, buf, len);

    if (plen) {
        sys_memcpy(bin_ptr, prefix, plen);
    }

    /* {ssl_tls,NIL,ContentType,{Major,Minor},Bin} */
    hp = erts_produce_heap(&pca->factory, 3 + 6, HALLOC_EXTRA);
    ver = TUPLE2(&hp[0], make_small(major), make_small(minor));
    pca->res = TUPLE5(&hp[3], am_ssl_tls, NIL, make_small(type), ver, bin);

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
    int packet_sz;
    char* body_ptr;
    int body_sz;
    struct packet_callback_args pca;
    enum PacketParseType type;
    const byte *temp_alloc = NULL;
    Eterm res;
    int   code;
    char  delimiter = '\n';

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
        BIF_P->fvalue = am_badopt;
        BIF_ERROR(BIF_P, BADARG | EXF_HAS_EXT_INFO);
    }

    if (!is_bitstring(BIF_ARG_2) ||
        (!is_list(BIF_ARG_3) && !is_nil(BIF_ARG_3))) {
        BIF_ERROR(BIF_P, BADARG);
    }

    for (Eterm options = BIF_ARG_3, *cell = NULL;
         !is_nil(options);
         options = CDR(cell)) {
        if (is_list(options)) {
            cell = list_val(options);

            if (is_tuple(CAR(cell))) {
                Eterm *tpl = tuple_val(CAR(cell));
                Uint val;

                if (tpl[0] == make_arityval(2) &&
                    term_to_Uint(tpl[2], &val) && val <= UINT_MAX) {
                    switch (tpl[1]) {
                    case am_packet_size:
                        max_plen = val;
                        continue;
                    case am_line_length:
                        trunc_len = val;
                        continue;
                    case am_line_delimiter:
                        if (type == TCP_PB_LINE_LF && val <= 255) {
                            delimiter = (char)val;
                            continue;
                        }
                    }
                }
            }
        }

        BIF_ERROR(BIF_P, BADARG);
    }

    pca.orig = BIF_ARG_2;
    pca.aligned_ptr = erts_get_aligned_binary_bytes(pca.orig,
                                                    &pca.bin_sz,
                                                    &temp_alloc);
    if (pca.aligned_ptr == NULL) {
        BIF_ERROR(BIF_P, BADARG);
    }

    packet_sz = packet_get_length(type, (char*)pca.aligned_ptr, pca.bin_sz,
                                  max_plen, trunc_len, delimiter, &http_state);

    if (packet_sz < 0) {
        Eterm *hp = HAlloc(BIF_P, 3);
        res = TUPLE2(hp, am_error, am_invalid);
    } else if (packet_sz == 0 || packet_sz > pca.bin_sz) {
        Eterm* hp = HAlloc(BIF_P, 3);
        Eterm required_size = (packet_sz == 0) ?
            am_undefined :
            erts_make_integer(packet_sz, BIF_P);
        res = TUPLE2(hp, am_more, required_size);
    } else {
        /* We got a whole packet, parse it */
        body_ptr = (char*) pca.aligned_ptr;
        body_sz = packet_sz;
        packet_get_body(type, (const char**) &body_ptr, &body_sz);
        ASSERT(body_sz <= packet_sz);

        pca.p = BIF_P;
        pca.res = THE_NON_VALUE;
        pca.string_as_bin = (type == TCP_PB_HTTP_BIN ||
                             type == TCP_PB_HTTPH_BIN);

        erts_factory_proc_init(&pca.factory, BIF_P);
        code = packet_parse(type, (char*)pca.aligned_ptr, packet_sz,
                            &http_state, &packet_callbacks_erl, &pca);

        if (code < 0) {
            Eterm *hp = erts_produce_heap(&pca.factory, 3, 0);
            res = TUPLE2(hp, am_error, am_invalid);
        } else {
            Eterm *hp = erts_produce_heap(&pca.factory, 4, 0);
            Eterm rest;

            if (code == 0) {
                /* no special packet parsing, make plain binary */
                pca.res = erts_make_sub_binary(BIF_P, BIF_ARG_2,
                                               (body_ptr - (char*)pca.aligned_ptr),
                                               body_sz);
            } else {
                ASSERT(pca.res != THE_NON_VALUE);
            }

            rest = erts_make_sub_binary(BIF_P, BIF_ARG_2,
                                        packet_sz,
                                        pca.bin_sz - packet_sz);

            res = TUPLE3(hp, am_ok, pca.res, rest);
        }

        erts_factory_close(&pca.factory);
    }

    if (temp_alloc != NULL) {
        erts_free_aligned_binary_bytes(temp_alloc);
    }

    BIF_RET(res);
}

