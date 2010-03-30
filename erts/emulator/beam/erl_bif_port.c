/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2010. All Rights Reserved.
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

#ifdef _OSE_
#  include "ose.h"
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

static int open_port(Process* p, Eterm name, Eterm settings, int *err_nump);
static byte* convert_environment(Process* p, Eterm env);
static char **convert_args(Eterm);
static void free_args(char **);

char *erts_default_arg0 = "default";

BIF_RETTYPE open_port_2(BIF_ALIST_2)
{
    int port_num;
    Eterm port_val;
    char *str;
    int err_num;

    if ((port_num = open_port(BIF_P, BIF_ARG_1, BIF_ARG_2, &err_num)) < 0) {
	if (port_num == -3) {
	    ASSERT(err_num == BADARG || err_num == SYSTEM_LIMIT);
	    BIF_ERROR(BIF_P, err_num);
	} else if (port_num == -2) {
	    str = erl_errno_id(err_num);
	} else {
	    str = "einval";
	}
	BIF_P->fvalue = am_atom_put(str, strlen(str));
	BIF_ERROR(BIF_P, EXC_ERROR);
    }

    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_LINK);

    port_val = erts_port[port_num].id;
    erts_add_link(&(erts_port[port_num].nlinks), LINK_PID, BIF_P->id);
    erts_add_link(&(BIF_P->nlinks), LINK_PID, port_val);

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_LINK);

    erts_port_release(&erts_port[port_num]);

    BIF_RET(port_val);
}

/****************************************************************************

  PORT BIFS:

           port_command/2   -- replace Port ! {..., {command, Data}}
               port_command(Port, Data) -> true
               when port(Port), io-list(Data)

           port_control/3   -- new port_control(Port, Ctl, Data) -> Reply
	      port_control(Port, Ctl, Data) -> Reply
              where integer(Ctl), io-list(Data), io-list(Reply)

           port_close/1     -- replace Port ! {..., close}
             port_close(Port) -> true
             when port(Port)

           port_connect/2   -- replace Port ! {..., {connect, Pid}}
              port_connect(Port, Pid) 
              when port(Port), pid(Pid)

 ***************************************************************************/

static Port*
id_or_name2port(Process *c_p, Eterm id)
{
    Port *port;
    if (is_not_atom(id))
	port = erts_id2port(id, c_p, ERTS_PROC_LOCK_MAIN);
    else
	erts_whereis_name(c_p, ERTS_PROC_LOCK_MAIN, id, NULL, 0, 0, &port);
    return port;
}

#define ERTS_PORT_COMMAND_FLAG_FORCE		(((Uint32) 1) << 0)
#define ERTS_PORT_COMMAND_FLAG_NOSUSPEND	(((Uint32) 1) << 1)

static BIF_RETTYPE do_port_command(Process *BIF_P,
				   Eterm BIF_ARG_1,
				   Eterm BIF_ARG_2,
				   Eterm BIF_ARG_3,
				   Uint32 flags)
{
    BIF_RETTYPE res;
    Port *p;

    /* Trace sched out before lock check wait */    
    if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
	trace_virtual_sched(BIF_P, am_out);
    }
    
    if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	profile_runnable_proc(BIF_P, am_inactive);
    }

    p = id_or_name2port(BIF_P, BIF_ARG_1);
    if (!p) {
    	if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
	    trace_virtual_sched(BIF_P, am_in);
	}
    	if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	    profile_runnable_proc(BIF_P, am_active);
    	}
	BIF_ERROR(BIF_P, BADARG);
    }
    
    /* Trace port in, id_or_name2port causes wait */

    if (IS_TRACED_FL(p, F_TRACE_SCHED_PORTS)) {
	trace_sched_ports_where(p, am_in, am_command);
    }
    if (erts_system_profile_flags.runnable_ports && !erts_port_is_scheduled(p)) {
    	profile_runnable_port(p, am_active);
    }

    ERTS_BIF_PREP_RET(res, am_true);

    if ((flags & ERTS_PORT_COMMAND_FLAG_FORCE)
	&& !(p->drv_ptr->flags & ERL_DRV_FLAG_SOFT_BUSY)) {
	ERTS_BIF_PREP_ERROR(res, BIF_P, EXC_NOTSUP);
    }
    else if (!(flags & ERTS_PORT_COMMAND_FLAG_FORCE)
	     && p->status & ERTS_PORT_SFLG_PORT_BUSY) {
	if (flags & ERTS_PORT_COMMAND_FLAG_NOSUSPEND) {
	    ERTS_BIF_PREP_RET(res, am_false);
	}
	else {
	    erts_suspend(BIF_P, ERTS_PROC_LOCK_MAIN, p);
	    if (erts_system_monitor_flags.busy_port) {
		monitor_generic(BIF_P, am_busy_port, p->id);
	    }
	    ERTS_BIF_PREP_YIELD3(res, bif_export[BIF_port_command_3], BIF_P,
				 BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);    
	}
    } else {
	int wres;
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	ERTS_SMP_CHK_NO_PROC_LOCKS;
	wres = erts_write_to_port(BIF_P->id, p, BIF_ARG_2);
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	if (wres != 0) {
	    ERTS_BIF_PREP_ERROR(res, BIF_P, BADARG);
	}
    }
    
    if (IS_TRACED_FL(p, F_TRACE_SCHED_PORTS)) {
	trace_sched_ports_where(p, am_out, am_command);
    }
    if (erts_system_profile_flags.runnable_ports && !erts_port_is_scheduled(p)) {
    	profile_runnable_port(p, am_inactive);
    }

    erts_port_release(p);
    /* Trace sched in after port release */
    if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
	trace_virtual_sched(BIF_P, am_in);
    }
    if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	profile_runnable_proc(BIF_P, am_active);
    }
    
    if (ERTS_PROC_IS_EXITING(BIF_P)) {
	KILL_CATCHES(BIF_P);	/* Must exit */
	ERTS_BIF_PREP_ERROR(res, BIF_P, EXC_ERROR);
    }
    return res;
}

BIF_RETTYPE port_command_2(BIF_ALIST_2)
{
    return do_port_command(BIF_P, BIF_ARG_1, BIF_ARG_2, NIL, 0);
}

BIF_RETTYPE port_command_3(BIF_ALIST_3)
{
    Eterm l = BIF_ARG_3;
    Uint32 flags = 0;
    while (is_list(l)) {
	Eterm* cons = list_val(l);
	Eterm car = CAR(cons);
	if (car == am_force) {
	    flags |= ERTS_PORT_COMMAND_FLAG_FORCE;
	} else if (car == am_nosuspend) {
	    flags |= ERTS_PORT_COMMAND_FLAG_NOSUSPEND;
	} else {
	    BIF_ERROR(BIF_P, BADARG);
	}
	l = CDR(cons);
    }
    if(!is_nil(l)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    return do_port_command(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, flags);
}

BIF_RETTYPE port_call_2(BIF_ALIST_2)
{
    return port_call_3(BIF_P,BIF_ARG_1,make_small(0),BIF_ARG_2);
}

BIF_RETTYPE port_call_3(BIF_ALIST_3)
{
    Uint op;
    Port *p;
    Uint size;
    byte *bytes;
    byte *endp;
    size_t real_size;
    erts_driver_t *drv;
    byte port_input[256];	/* Default input buffer to encode in */
    byte port_result[256];	/* Buffer for result from port. */
    byte* port_resp;		/* Pointer to result buffer. */
    char *prc;
    int ret;
    Eterm res;
    Sint result_size;
    Eterm *hp;
    Eterm *hp_end;              /* To satisfy hybrid heap architecture */
    unsigned ret_flags = 0U;
    int fpe_was_unmasked;

    bytes = &port_input[0];
    port_resp = port_result;
    /* trace of port scheduling with virtual process descheduling
     * lock wait 
     */
    if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
    	trace_virtual_sched(BIF_P, am_out);
    }

    if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	profile_runnable_proc(BIF_P, am_inactive);
    }

    p = id_or_name2port(BIF_P, BIF_ARG_1);
    if (!p) {
    error:
	if (port_resp != port_result && 
	    !(ret_flags & DRIVER_CALL_KEEP_BUFFER)) {
	    driver_free(port_resp);
	}
	if (bytes != &port_input[0])
	    erts_free(ERTS_ALC_T_PORT_CALL_BUF, bytes);
	/* Need to virtual schedule in the process if there
	 * was an error.
	 */
    	if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
    	    trace_virtual_sched(BIF_P, am_in);
    	}

	if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	    profile_runnable_proc(BIF_P, am_active);
    	}

	if (p)
	    erts_port_release(p);
#ifdef ERTS_SMP
	ERTS_SMP_BIF_CHK_PENDING_EXIT(BIF_P, ERTS_PROC_LOCK_MAIN);
#else
	ERTS_BIF_CHK_EXITED(BIF_P);
#endif
	BIF_ERROR(BIF_P, BADARG);
    }

    if ((drv = p->drv_ptr) == NULL) {
	goto error;
    }
    if (drv->call == NULL) {
	goto error;
    }
    if (!term_to_Uint(BIF_ARG_2, &op)) {
	goto error;
    }
    p->caller = BIF_P->id;
    
    /* Lock taken, virtual schedule of port */
    if (IS_TRACED_FL(p, F_TRACE_SCHED_PORTS)) {
    	trace_sched_ports_where(p, am_in, am_call);
    }
    
    if (erts_system_profile_flags.runnable_ports && !erts_port_is_scheduled(p)) {
    	profile_runnable_port(p, am_active);
    }
    size = erts_encode_ext_size(BIF_ARG_3);
    if (size > sizeof(port_input))
	bytes = erts_alloc(ERTS_ALC_T_PORT_CALL_BUF, size);

    endp = bytes;
    erts_encode_ext(BIF_ARG_3, &endp);

    real_size = endp - bytes;
    if (real_size > size) {
	erl_exit(1, "%s, line %d: buffer overflow: %d word(s)\n",
		 __FILE__, __LINE__, endp - (bytes + size));
    }
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    prc  = (char *) port_resp;
    fpe_was_unmasked = erts_block_fpe();
    ret = drv->call((ErlDrvData)p->drv_data, 
		    (unsigned) op,
		    (char *) bytes, 
		    (int) real_size,
		    &prc, 
		    (int) sizeof(port_result),
		    &ret_flags);
    erts_unblock_fpe(fpe_was_unmasked);
    if (IS_TRACED_FL(p, F_TRACE_SCHED_PORTS)) {
    	trace_sched_ports_where(p, am_out, am_call);
    }
    
    if (erts_system_profile_flags.runnable_ports && !erts_port_is_scheduled(p)) {
    	profile_runnable_port(p, am_inactive);
    }
   
    port_resp = (byte *) prc;
    p->caller = NIL;
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
#ifdef HARDDEBUG
    { 
	int z;
	printf("real_size = %ld,%d, ret = %d\r\n",real_size, 
	       (int) real_size, ret);
	printf("[");
	for(z = 0; z < real_size; ++z) {
	    printf("%d, ",(int) bytes[z]);
	}
	printf("]\r\n");
	printf("[");
	for(z = 0; z < ret; ++z) {
	    printf("%d, ",(int) port_resp[z]);
	}
	printf("]\r\n");
    }
#endif
    if (ret <= 0 || port_resp[0] != VERSION_MAGIC) { 
	/* Error or a binary without magic/ with wrong magic */
	goto error;
    }
    result_size = erts_decode_ext_size(port_resp, ret, 0);
    if (result_size < 0) {
	goto error;
    }
    hp = HAlloc(BIF_P, result_size);
    hp_end = hp + result_size;
    endp = port_resp;
    res = erts_decode_ext(&hp, &MSO(BIF_P), &endp);
    if (res == THE_NON_VALUE) {
	goto error;
    }
    HRelease(BIF_P, hp_end, hp);
    if (port_resp != port_result && !(ret_flags & DRIVER_CALL_KEEP_BUFFER)) {
	driver_free(port_resp);
    }
    if (bytes != &port_input[0])
	erts_free(ERTS_ALC_T_PORT_CALL_BUF, bytes);
    if (p)
	erts_port_release(p);
#ifdef ERTS_SMP
    ERTS_SMP_BIF_CHK_PENDING_EXIT(BIF_P, ERTS_PROC_LOCK_MAIN);
#else
    ERTS_BIF_CHK_EXITED(BIF_P);
#endif
    if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
    	trace_virtual_sched(BIF_P, am_in);
    }

    if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	profile_runnable_proc(BIF_P, am_active);
    }
  
    return res;
}
    
BIF_RETTYPE port_control_3(BIF_ALIST_3)
{
    Port* p;
    Uint op;
    Eterm res = THE_NON_VALUE;
    
    /* Virtual schedule out calling process before lock wait */
    if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
	trace_virtual_sched(BIF_P, am_out);
    }

    if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	profile_runnable_proc(BIF_P, am_inactive);
    }

    p = id_or_name2port(BIF_P, BIF_ARG_1);
    if (!p) {
    	/* Schedule the process before exiting */
    	if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
	    trace_virtual_sched(BIF_P, am_in);
    	}
	
	if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	    profile_runnable_proc(BIF_P, am_active);
	}
	
	BIF_ERROR(BIF_P, BADARG);
    }

    /* Trace the port for scheduling in */
    if (IS_TRACED_FL(p, F_TRACE_SCHED_PORTS)) {
    	trace_sched_ports_where(p, am_in, am_control);
    }
    
    if (erts_system_profile_flags.runnable_ports && !erts_port_is_scheduled(p)) {
    	profile_runnable_port(p, am_active);
    }

    if (term_to_Uint(BIF_ARG_2, &op))
	res = erts_port_control(BIF_P, p, op, BIF_ARG_3);
    
    /* Trace the port for scheduling out */
    if (IS_TRACED_FL(p, F_TRACE_SCHED_PORTS)) {
    	trace_sched_ports_where(p, am_out, am_control);
    }

    if (erts_system_profile_flags.runnable_ports && !erts_port_is_scheduled(p)) {
    	profile_runnable_port(p, am_inactive);
    }

    erts_port_release(p);
#ifdef ERTS_SMP
    ERTS_SMP_BIF_CHK_PENDING_EXIT(BIF_P, ERTS_PROC_LOCK_MAIN);
#else
    ERTS_BIF_CHK_EXITED(BIF_P);
#endif
    
    if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
	trace_virtual_sched(BIF_P, am_in);
    }
    
    if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	profile_runnable_proc(BIF_P, am_active);
    }
    
    if (is_non_value(res)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(res);
}

BIF_RETTYPE port_close_1(BIF_ALIST_1)
{
    Port* p;
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    p = id_or_name2port(NULL, BIF_ARG_1);
    if (!p) {
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	BIF_ERROR(BIF_P, BADARG);
    }
    erts_do_exit_port(p, p->connected, am_normal);
    /* if !ERTS_SMP: since we terminate port with reason normal 
       we SHOULD never get an exit signal ourselves
       */
    erts_port_release(p);
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    BIF_RET(am_true);
}

BIF_RETTYPE port_connect_2(BIF_ALIST_2)
{
    Port* prt;
    Process* rp;
    Eterm pid = BIF_ARG_2;

    if (is_not_internal_pid(pid)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    prt = id_or_name2port(BIF_P, BIF_ARG_1);
    if (!prt) {
	goto error;
    }

    rp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
		       pid, ERTS_PROC_LOCK_LINK);
    if (!rp) {
	erts_smp_port_unlock(prt);
	ERTS_SMP_ASSERT_IS_NOT_EXITING(BIF_P);
	goto error;
    }

    erts_add_link(&(rp->nlinks), LINK_PID, prt->id);
    erts_add_link(&(prt->nlinks), LINK_PID, pid);

    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);

    prt->connected = pid; /* internal pid */
    erts_smp_port_unlock(prt);
    BIF_RET(am_true);
}

BIF_RETTYPE port_set_data_2(BIF_ALIST_2)
{
    Port* prt;
    Eterm portid = BIF_ARG_1;
    Eterm data   = BIF_ARG_2;

    prt = id_or_name2port(BIF_P, portid);
    if (!prt) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (prt->bp != NULL) {
	free_message_buffer(prt->bp);
	prt->bp = NULL;
    }
    if (IS_CONST(data)) {
	prt->data = data;
    } else {
	Uint size;
	ErlHeapFragment* bp;
	Eterm* hp;

	size = size_object(data);
	prt->bp = bp = new_message_buffer(size);
	hp = bp->mem;
	prt->data = copy_struct(data, size, &hp, &bp->off_heap);
    }
    erts_smp_port_unlock(prt);
    BIF_RET(am_true);
}


BIF_RETTYPE port_get_data_1(BIF_ALIST_1)
{
    BIF_RETTYPE res;
    Port* prt;
    Eterm portid = BIF_ARG_1;

    prt = id_or_name2port(BIF_P, portid);
    if (!prt) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (prt->bp == NULL) {	/* MUST be CONST! */
	res = prt->data;
    } else {
	Eterm* hp = HAlloc(BIF_P, prt->bp->size);
	res = copy_struct(prt->data, prt->bp->size, &hp, &MSO(BIF_P));
    }
    erts_smp_port_unlock(prt);
    BIF_RET(res);
}

/* 
 * Open a port. Most of the work is not done here but rather in
 * the file io.c.
 * Error returns: -1 or -2 returned from open_driver (-2 implies
 * that *err_nump contains the error code; -1 means we don't really know what happened),
 * -3 if argument parsing failed or we are out of ports (*err_nump should contain
 * either BADARG or SYSTEM_LIMIT).
 */

static int
open_port(Process* p, Eterm name, Eterm settings, int *err_nump)
{
#define OPEN_PORT_ERROR(VAL) do { port_num = (VAL); goto do_return; } while (0)
    int i, port_num;
    Eterm option;
    Uint arity;
    Eterm* tp;
    Uint* nargs;
    erts_driver_t* driver;
    char* name_buf = NULL;
    SysDriverOpts opts;
    int binary_io;
    int soft_eof;
    Sint linebuf;
    byte dir[MAXPATHLEN];

    /* These are the defaults */
    opts.packet_bytes = 0;
    opts.use_stdio = 1;
    opts.redir_stderr = 0;
    opts.read_write = 0;
    opts.hide_window = 0;
    opts.wd = NULL;
    opts.envir = NULL;
    opts.exit_status = 0;
    opts.overlapped_io = 0; 
    opts.spawn_type = ERTS_SPAWN_ANY; 
    opts.argv = NULL;
    binary_io = 0;
    soft_eof = 0;
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
		    byte* bytes;
		    if ((bytes = convert_environment(p, *tp)) == NULL) {
			goto badarg;
		    }
		    opts.envir = (char *) bytes;
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
		    int n;
		    if (is_nil(*tp)) {
			n = 0;
		    } else if( (n = is_string(*tp)) == 0) {
			goto badarg;
		    }
		    a0 = (char *) erts_alloc(ERTS_ALC_T_TMP, 
					    (n + 1) * sizeof(byte));
		    if (intlist_to_buf(*tp, a0, n) != n) {
			erl_exit(1, "%s:%d: Internal error\n",
				 __FILE__, __LINE__);
		    }
		    a0[n] = '\0';		    
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
		    Eterm iolist;
		    DeclareTmpHeap(heap,4,p);
		    int r;

		    UseTmpHeap(4,p);
		    heap[0] = *tp;
		    heap[1] = make_list(heap+2);
		    heap[2] = make_small(0);
		    heap[3] = NIL;
		    iolist = make_list(heap);
		    r = io_list_to_buf(iolist, (char*) dir, MAXPATHLEN);
		    UnUseTmpHeap(4,p);
		    if (r < 0) {
			goto badarg;
		    }
		    opts.wd = (char *) dir;
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
		binary_io = 1;
	    } else if (*nargs == am_in) {
		opts.read_write |= DO_READ;
	    } else if (*nargs == am_out) {
		opts.read_write |= DO_WRITE;
	    } else if (*nargs == am_eof) {
		soft_eof = 1;
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
		erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);
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
    
	if (*tp == am_spawn || *tp == am_spawn_driver) {	/* A process port */
	    if (arity != make_arityval(2)) {
		goto badarg;
	    }
	    name = tp[1];
	    if (is_atom(name)) {
		name_buf = (char *) erts_alloc(ERTS_ALC_T_TMP,
					       atom_tab(atom_val(name))->len+1);
		sys_memcpy((void *) name_buf,
			   (void *) atom_tab(atom_val(name))->name, 
			   atom_tab(atom_val(name))->len);
		name_buf[atom_tab(atom_val(name))->len] = '\0';
	    } else if ((i = is_string(name))) {
		name_buf = (char *) erts_alloc(ERTS_ALC_T_TMP, i + 1);
		if (intlist_to_buf(name, name_buf, i) != i)
		    erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);
		name_buf[i] = '\0';
	    } else {
		goto badarg;
	    }
	    if (*tp == am_spawn_driver) {
		opts.spawn_type = ERTS_SPAWN_DRIVER;
	    }
	    driver = &spawn_driver;
	} else if (*tp == am_spawn_executable) {	/* A program */
	    /*
	     * {spawn_executable,Progname}
	     */
	    
	    if (arity != make_arityval(2)) {
		goto badarg;
	    }
	    name = tp[1];
	    if (is_atom(name)) {
		name_buf = (char *) erts_alloc(ERTS_ALC_T_TMP,
					       atom_tab(atom_val(name))->len+1);
		sys_memcpy((void *) name_buf,
			   (void *) atom_tab(atom_val(name))->name, 
			   atom_tab(atom_val(name))->len);
		name_buf[atom_tab(atom_val(name))->len] = '\0';
	    } else if ((i = is_string(name))) {
		name_buf = (char *) erts_alloc(ERTS_ALC_T_TMP, i + 1);
		if (intlist_to_buf(name, name_buf, i) != i)
		    erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);
		name_buf[i] = '\0';
	    } else {
		goto badarg;
	    }
	    opts.spawn_type = ERTS_SPAWN_EXECUTABLE;
	    driver = &spawn_driver;
	} else if (*tp == am_fd) { /* An fd port */
	    int n;
	    struct Sint_buf sbuf;
	    char* p;

	    if (arity != make_arityval(3)) {
		goto badarg;
	    }
	    if (is_not_small(tp[1]) || is_not_small(tp[2])) {
		goto badarg;
	    }
	    opts.ifd = unsigned_val(tp[1]);
	    opts.ofd = unsigned_val(tp[2]);

	    /* Syntesize name from input and output descriptor. */
	    name_buf = erts_alloc(ERTS_ALC_T_TMP,
				  2*sizeof(struct Sint_buf) + 2); 
	    p = Sint_to_buf(opts.ifd, &sbuf);
	    n = sys_strlen(p);
	    sys_strncpy(name_buf, p, n);
	    name_buf[n] = '/';
	    p = Sint_to_buf(opts.ofd, &sbuf);
	    sys_strcpy(name_buf+n+1, p);

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
	

    if (driver != &spawn_driver && opts.exit_status) {
	goto badarg;
    }
    
    if (IS_TRACED_FL(p, F_TRACE_SCHED_PROCS)) {
        trace_virtual_sched(p, am_out);
    }
    

    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);

    port_num = erts_open_driver(driver, p->id, name_buf, &opts, err_nump);

    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);

    if (port_num < 0) {
	DEBUGF(("open_driver returned %d(%d)\n", port_num, *err_nump));
    	if (IS_TRACED_FL(p, F_TRACE_SCHED_PROCS)) {
            trace_virtual_sched(p, am_in);
    	}
	OPEN_PORT_ERROR(port_num);
    }
    
    if (IS_TRACED_FL(p, F_TRACE_SCHED_PROCS)) {
        trace_virtual_sched(p, am_in);
    }

    if (binary_io) {
	erts_port_status_bor_set(&erts_port[port_num],
				 ERTS_PORT_SFLG_BINARY_IO);
    }
    if (soft_eof) {
	erts_port_status_bor_set(&erts_port[port_num],
				 ERTS_PORT_SFLG_SOFT_EOF);
    }
    if (linebuf && erts_port[port_num].linebuf == NULL){
	erts_port[port_num].linebuf = allocate_linebuf(linebuf); 
	erts_port_status_bor_set(&erts_port[port_num],
				 ERTS_PORT_SFLG_LINEBUF_IO);
    }
 
 do_return:
    if (name_buf)
	erts_free(ERTS_ALC_T_TMP, (void *) name_buf);
    if (opts.argv) {
	free_args(opts.argv);
    }
    return port_num;
    
 badarg:
    *err_nump = BADARG;
    OPEN_PORT_ERROR(-3);
    goto do_return;
#undef OPEN_PORT_ERROR
}

static char **convert_args(Eterm l)
{
    char **pp;
    char *b;
    int n;
    int i = 0;
    Eterm str;
    /* We require at least one element in list (argv[0]) */
    if (is_not_list(l) && is_not_nil(l)) {
	return NULL;
    }
    n = list_length(l);
    pp = erts_alloc(ERTS_ALC_T_TMP, (n + 2) * sizeof(char **));
    pp[i++] = erts_default_arg0;
    while (is_list(l)) {
	str = CAR(list_val(l));

	if (is_nil(str)) {
	    n = 0;
	} else if( (n = is_string(str)) == 0) {
	    /* Not a string... */
	    int j;
	    for (j = 1; j < i; ++j)
		erts_free(ERTS_ALC_T_TMP, pp[j]);
	    erts_free(ERTS_ALC_T_TMP, pp);
	    return NULL;
	}
	b = (char *) erts_alloc(ERTS_ALC_T_TMP, (n + 1) * sizeof(byte));
	pp[i++] = (char *) b;
	if (intlist_to_buf(str, b, n) != n)
	    erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);
	b[n] = '\0';
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
    

static byte* convert_environment(Process* p, Eterm env)
{
    Eterm all;
    Eterm* temp_heap;
    Eterm* hp;
    Uint heap_size;
    int n;
    byte* bytes;

    if ((n = list_length(env)) < 0) {
	return NULL;
    }
    heap_size = 2*(5*n+1);
    temp_heap = hp = (Eterm *) erts_alloc(ERTS_ALC_T_TMP, heap_size*sizeof(Eterm));
    bytes = NULL;		/* Indicating error */

    /*
     * All errors below are handled by jumping to 'done', to ensure that the memory
     * gets deallocated. Do NOT return directly from this function.
     */

    all = CONS(hp, make_small(0), NIL);
    hp += 2;

    while(is_list(env)) {
	Eterm tmp;
	Eterm* tp;

	tmp = CAR(list_val(env));
	if (is_not_tuple_arity(tmp, 2)) {
	    goto done;
	}
	tp = tuple_val(tmp);
	tmp = CONS(hp, make_small(0), NIL);
	hp += 2;
	if (tp[2] != am_false) {
	    tmp = CONS(hp, tp[2], tmp);
	    hp += 2;
	}
	tmp = CONS(hp, make_small('='), tmp);
	hp += 2;
	tmp = CONS(hp, tp[1], tmp);
	hp += 2;
	all = CONS(hp, tmp, all);
	hp += 2;
	env = CDR(list_val(env));
    }
    if (is_not_nil(env)) {
	goto done;
    }
    if ((n = io_list_len(all)) < 0) {
	goto done;
    }

    /*
     * Put the result in a binary (no risk for a memory leak that way).
     */
    (void) erts_new_heap_binary(p, NULL, n, &bytes);
    io_list_to_buf(all, (char*)bytes, n);

 done:
    erts_free(ERTS_ALC_T_TMP, temp_heap);
    return bytes;
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
    ((unsigned long)((char*)(ptr) - (char*)(start)) < (nbytes))

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
		memcpy(bin->data, str, len);
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
        erl_exit(1, "%s, line %d: type=%u\n", __FILE__, __LINE__, uri->type);
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

    memcpy(bin_ptr+plen, buf, len);
    if (plen) {
        memcpy(bin_ptr, prefix, plen);
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
    int code;

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
                                  max_plen, trunc_len, &http_state);
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

