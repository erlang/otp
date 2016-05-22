/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2006-2016. All Rights Reserved.
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
 * BIFs belonging to the 'erl_ddll' module together with utility
 * functions for dynamic loading. The actual loading is done in
 * erl_sys_ddll.c in respective system dependent directory.  The
 * driver structure contains a handle to the actual loaded "module" as
 * well as record keeping information about processes having loaded
 * the driver and processes monitoring the driver. A process in any
 * way involved in ddll-drivers, get a special flag, which triggers
 * cleenup at process exit.
 */


#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define ERL_SYS_DRV

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_driver.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"
#include "erl_bif_unique.h"
#include "dtrace-wrapper.h"
#include "lttng-wrapper.h"

#ifdef ERTS_SMP
#define DDLL_SMP 1
#else
#define DDLL_SMP 0
#endif


/*
 * Local types
 */

typedef struct {
    Eterm pid;
    Process *proc;
    Uint status;
    Uint count;
} ProcEntryInfo;

/*
 * Forward
 */
static char *pick_list_or_atom(Eterm name_term);
static erts_driver_t *lookup_driver(char *name);
static Eterm mkatom(char *str);
static void add_proc_loaded(DE_Handle *dh, Process *proc); 
static void add_proc_loaded_deref(DE_Handle *dh, Process *proc);
static void set_driver_reloading(DE_Handle *dh, Process *proc, char *path, char *name, Uint flags);
static int load_driver_entry(DE_Handle **dhp, char *path, char *name);
static int do_unload_driver_entry(DE_Handle *dh, Eterm *save_name);
static int do_load_driver_entry(DE_Handle *dh, char *path, char *name);
#if 0
static void unload_driver_entry(DE_Handle *dh);
#endif
static int reload_driver_entry(DE_Handle *dh);
static int build_proc_info(DE_Handle *dh, ProcEntryInfo **out_pei, Uint filter);
static int is_last_user(DE_Handle *dh, Process *proc);
static DE_ProcEntry *find_proc_entry(DE_Handle *dh, Process *proc, Uint status);
static void remove_proc_entry(DE_Handle *dh, DE_ProcEntry *pe);
static int num_procs(DE_Handle *dh, Uint status);
/*static int num_entries(DE_Handle *dh, Process *proc, Uint status);*/
static void notify_proc(Process *proc, Eterm ref, Eterm driver_name, 
			Eterm type, Eterm tag, int errcode);
static void notify_all(DE_Handle *dh, char *name, Uint awaiting, Eterm type, Eterm tag);
static int load_error_need(int code);
static Eterm build_load_error_hp(Eterm *hp, int code);
static Eterm build_load_error(Process *p, int code);
static int errdesc_to_code(Eterm errdesc, int *code /* out */);
static Eterm add_monitor(Process *p, DE_Handle *dh, Uint status);
static Eterm notify_when_loaded(Process *p, Eterm name_term, char *name,
				ErtsProcLocks plocks);
static Eterm notify_when_unloaded(Process *p, Eterm name_term, char *name,
				  ErtsProcLocks plocks, Uint flag);
static void first_ddll_reference(DE_Handle *dh);
static void dereference_all_processes(DE_Handle *dh);
static void restore_process_references(DE_Handle *dh);
static void ddll_no_more_references(void *vdh);

#define lock_drv_list() erts_smp_rwmtx_rwlock(&erts_driver_list_lock)
#define unlock_drv_list() erts_smp_rwmtx_rwunlock(&erts_driver_list_lock)
#define assert_drv_list_locked() \
    ERTS_SMP_LC_ASSERT(erts_smp_lc_rwmtx_is_rwlocked(&erts_driver_list_lock) \
		       || erts_smp_lc_rwmtx_is_rlocked(&erts_driver_list_lock))
#define assert_drv_list_rwlocked() \
    ERTS_SMP_LC_ASSERT(erts_smp_lc_rwmtx_is_rwlocked(&erts_driver_list_lock))
#define assert_drv_list_rlocked() \
    ERTS_SMP_LC_ASSERT(erts_smp_lc_rwmtx_is_rlocked(&erts_driver_list_lock))
#define assert_drv_list_not_locked() \
    ERTS_SMP_LC_ASSERT(!erts_smp_lc_rwmtx_is_rwlocked(&erts_driver_list_lock) \
		       && !erts_smp_lc_rwmtx_is_rlocked(&erts_driver_list_lock))


#define FREE_PORT_FLAGS (ERTS_PORT_SFLGS_DEAD & (~ERTS_PORT_SFLG_INITIALIZING))

static void
kill_ports_driver_unloaded(DE_Handle *dh)
{
    int ix, max = erts_ptab_max(&erts_port);

    for (ix = 0; ix < max; ix++) {
	erts_aint32_t state;
	Port* prt = erts_pix2port(ix);
	if (!prt)
	    continue;

	ERTS_SMP_DATA_DEPENDENCY_READ_MEMORY_BARRIER;

	state = erts_atomic32_read_nob(&prt->state);
	if (state & FREE_PORT_FLAGS)
	    continue;

	erts_smp_port_lock(prt);

	state = erts_atomic32_read_nob(&prt->state);
	if (!(state & ERTS_PORT_SFLGS_DEAD) && prt->drv_ptr->handle == dh)
	    driver_failure_atom(ERTS_Port2ErlDrvPort(prt), "driver_unloaded");

	erts_port_release(prt);
    }
}

/*
 *    try_load(Path, Name, OptionList) -> {ok,Status} | 
 *                                        {ok, PendingStatus, Ref} | 
 *                                        {error, ErrorDesc}
 *       Path = Name = string() | atom()
 *	 OptionList = [ Option ]
 *	 Option = {driver_options, DriverOptionList} | 
 *                {monitor,MonitorOption} | 
 *                {reload, ReloadOption}
 *	 DriverOptionList = [ DriverOption ]
 *	 DriverOption = kill_ports
 *	 MonitorOption = pending_driver | pending
 *	 ReloadOption = pending_driver | pending
 *	 Status = loaded | already_loaded | PendingStatus
 *	 PendingStatus = pending_driver | pending_process
 *	 Ref = ref()
 *	 ErrorDesc = ErrorAtom | OpaqueError
 *	 ErrorAtom = linked_in_driver | inconsistent |
 *	             permanent | pending
 */
/*
 * Try to load. If the driver is OK, add as LOADED.  If the driver is
 * UNLOAD, possibly change to reload and add as LOADED, 
 * there should be no other
 * LOADED tagged pid's.  If the driver is RELOAD then add/increment as
 * LOADED (should be some LOADED pid).  If the driver is not present,
 * really load and add as LOADED {ok,loaded} {ok,pending_driver}
 * {error, permanent} {error,load_error()}
 */
BIF_RETTYPE erl_ddll_try_load_3(BIF_ALIST_3)
{
    Eterm path_term = BIF_ARG_1;
    Eterm name_term = BIF_ARG_2;
    Eterm options = BIF_ARG_3;
    char *path = NULL;
    Sint path_len;
    char *name = NULL;
    DE_Handle *dh;
    erts_driver_t *drv;
    int res;
    Eterm soft_error_term = NIL;
    Eterm ok_term = NIL;
    Eterm *hp;
    Eterm t;
    int monitor = 0;
    int reload = 0;
    Eterm l;
    Uint flags = 0;
    int kill_ports = 0;
    int do_build_load_error = 0;
    int build_this_load_error = 0;
    int encoding;

    for(l = options; is_list(l); l =  CDR(list_val(l))) {
	Eterm opt = CAR(list_val(l));
	Eterm *tp;
	if (is_not_tuple(opt)) {
	    goto error;
	}
	tp = tuple_val(opt);
	if (*tp != make_arityval(2) || is_not_atom(tp[1])) {
	    goto error;
	}
	switch (tp[1]) {
	case am_driver_options:
	    {
		Eterm ll;
		for(ll = tp[2]; is_list(ll); ll = CDR(list_val(ll))) {
		    Eterm dopt = CAR(list_val(ll));
		    if (dopt == am_kill_ports) {
			flags |= ERL_DE_FL_KILL_PORTS;
		    } else {
			goto error;
		    }
		}
		if (is_not_nil(ll)) {
		    goto error;
		}
	    }
	    break;
	case am_monitor:
	    if (tp[2] == am_pending_driver) {
		monitor = 1;
	    } else if (tp[2] == am_pending ) {
		monitor = 2;
	    } else {
		goto error;
	    }
	    break;
	case am_reload:
	    if (tp[2] == am_pending_driver) {
		reload = 1;
	    } else if (tp[2] == am_pending ) {
		reload = 2;
	    } else {
		goto error;
	    }
	    break;
	default:
	    goto error;
	}
    }
    if (is_not_nil(l)) {
	goto error;
    }


    if ((name = pick_list_or_atom(name_term)) == NULL) {
	goto error;
    }

    encoding = erts_get_native_filename_encoding();
    if (encoding == ERL_FILENAME_WIN_WCHAR) {
        /* Do not convert the lib name to utf-16le yet, do that in win32 specific code */
        /* since lib_name is used in error messages */
        encoding = ERL_FILENAME_UTF8;
    }
    path = erts_convert_filename_to_encoding(path_term, NULL, 0,
					     ERTS_ALC_T_DDLL_TMP_BUF, 1, 0,
					     encoding, &path_len,
					     sys_strlen(name) + 2); /* might need path separator */
    if (!path) {
	goto error;
    }
    ASSERT(path_len > 0 && path[path_len-1] == 0);
    while (--path_len > 0 && (path[path_len-1] == '\\' || path[path_len-1] == '/'))
	;
    path[path_len++] = '/';
    sys_strcpy(path+path_len,name);

#if DDLL_SMP
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    lock_drv_list();
#endif
    if ((drv = lookup_driver(name)) != NULL) {
	if (drv->handle == NULL) {
	    /* static_driver */
	    soft_error_term = am_linked_in_driver;
	    goto soft_error;
	} else {
	    dh = drv->handle;
	    if (dh->status == ERL_DE_OK) {
		int is_last = is_last_user(dh, BIF_P);
		if (reload == 1 && !is_last) {
		    /*Want reload if no other users, 
		      but there are others...*/
		    soft_error_term = am_pending_process;
		    goto soft_error;
		} 
		if (reload != 0) {
		    DE_ProcEntry *old;
		    if ((dh->flags & ERL_FL_CONSISTENT_MASK) != 
			(flags &  ERL_FL_CONSISTENT_MASK)) {
			soft_error_term = am_inconsistent;
			goto soft_error;
		    }
		    if ((old = find_proc_entry(dh, BIF_P,
					       ERL_DE_PROC_LOADED)) ==
			NULL) {
			soft_error_term = am_not_loaded_by_this_process;
			goto soft_error;
		    } else {
			remove_proc_entry(dh, old);
			erts_ddll_dereference_driver(dh);
			erts_free(ERTS_ALC_T_DDLL_PROCESS, old);
		    }
		    /* Reload requested and granted */
		    dereference_all_processes(dh);
		    set_driver_reloading(dh, BIF_P, path, name, flags);
		    if (dh->flags & ERL_DE_FL_KILL_PORTS) {
			kill_ports = 1;
		    }
		    ok_term = (reload == 1) ? am_pending_driver : 
			am_pending_process;
		} else {
		    /* Already loaded and healthy (might be by me) */
		    if (sys_strcmp(dh->full_path, path) || 
			(dh->flags & ERL_FL_CONSISTENT_MASK) != 
			(flags &  ERL_FL_CONSISTENT_MASK)) {
			soft_error_term = am_inconsistent;
			goto soft_error;
		    }
		    add_proc_loaded(dh, BIF_P);
		    erts_ddll_reference_driver(dh);
		    monitor = 0;
		    ok_term = mkatom("already_loaded");
		}
	    } else if (dh->status == ERL_DE_UNLOAD ||
		       dh->status == ERL_DE_FORCE_UNLOAD) {
		/* pending driver */
		if (reload != 0) {
		    soft_error_term = am_not_loaded_by_this_process;
		    goto soft_error;
		} 
		if (sys_strcmp(dh->full_path, path) || 
		    (dh->flags & ERL_FL_CONSISTENT_MASK) != 
		    (flags &  ERL_FL_CONSISTENT_MASK)) {
		    soft_error_term = am_inconsistent;
		    goto soft_error;
		}
		dh->status = ERL_DE_OK;
		notify_all(dh, drv->name, 
			   ERL_DE_PROC_AWAIT_UNLOAD, am_UP, 
			   am_unload_cancelled);
		add_proc_loaded(dh, BIF_P);
		erts_ddll_reference_driver(dh);
		monitor = 0;
		ok_term = mkatom("already_loaded");
	    } else if (dh->status == ERL_DE_RELOAD ||
		       dh->status == ERL_DE_FORCE_RELOAD) {
		if (reload != 0) {
		    soft_error_term = am_pending_reload;
		    goto soft_error;
		}
		if (sys_strcmp(dh->reload_full_path, path) || 
		    (dh->reload_flags & ERL_FL_CONSISTENT_MASK) != 
		        (flags &  ERL_FL_CONSISTENT_MASK)) {
		    soft_error_term = am_inconsistent;
		    goto soft_error;
		}
		/* Load of granted unload... */
		/* Don't reference, will happen after reload */
		add_proc_loaded_deref(dh, BIF_P);
		++monitor;
		ok_term = am_pending_driver;
	    } else { /* ERL_DE_PERMANENT */
		soft_error_term = am_permanent;
		goto soft_error;
	    }
	}
    } else { /* driver non-existing */
	if (reload != 0) {
	    soft_error_term = am_not_loaded;
	    goto soft_error;
	} 
	if ((res = load_driver_entry(&dh, path, name)) !=  ERL_DE_NO_ERROR) {
	    build_this_load_error = res;
	    do_build_load_error = 1;
	    soft_error_term = am_undefined;
	    goto soft_error;
	} else {
	    dh->flags = flags;
	    add_proc_loaded(dh, BIF_P);
	    first_ddll_reference(dh);
	    monitor = 0;
	    ok_term = mkatom("loaded");
	}
    }
    assert_drv_list_rwlocked();
    if (kill_ports) {
 	/* Avoid closing the driver by referencing it */
	erts_ddll_reference_driver(dh);
	ASSERT(dh->status == ERL_DE_RELOAD);
	dh->status = ERL_DE_FORCE_RELOAD;
#if DDLL_SMP
	unlock_drv_list();
#endif
	kill_ports_driver_unloaded(dh);
	/* Dereference, eventually causing driver destruction */
#if DDLL_SMP
	lock_drv_list(); 
#endif
	erts_ddll_dereference_driver(dh);
    } 

#if DDLL_SMP
    erts_ddll_reference_driver(dh);
    unlock_drv_list();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    lock_drv_list();
    erts_ddll_dereference_driver(dh);
#endif

    BIF_P->flags |= F_USING_DDLL;
    if (monitor) {
	Eterm mref = add_monitor(BIF_P, dh, ERL_DE_PROC_AWAIT_LOAD);
	hp = HAlloc(BIF_P, 4);
	t = TUPLE3(hp, am_ok, ok_term, mref);
    } else {
	hp = HAlloc(BIF_P, 3);
	t = TUPLE2(hp, am_ok, ok_term);
    }
#if DDLL_SMP
    unlock_drv_list();
#endif
    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) path);
    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(BIF_P));
    BIF_RET(t);
 soft_error:
#if DDLL_SMP
    unlock_drv_list();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
#endif
    if (do_build_load_error) {
	soft_error_term = build_load_error(BIF_P, build_this_load_error);
    }

    hp = HAlloc(BIF_P, 3);
    t = TUPLE2(hp, am_error, soft_error_term);
    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) path);
    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(BIF_P));
    BIF_RET(t);
 error:
    assert_drv_list_not_locked();
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(BIF_P));
    if (path != NULL) {
	erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) path);
    }
    if (name != NULL) {
	erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    }
    BIF_ERROR(BIF_P, BADARG);
}

/*
 * try_unload(Name, OptionList) -> {ok,Status} | 
 *                                 {ok,PendingStatus, Ref} | 
 *                                 {error, ErrorAtom}
 *        Name = string() | atom()
 *	  OptionList = [ Option ]
 *	  Option = {monitor,MonitorOption} | kill_ports
 *	  MonitorOption = pending_driver | pending
 *	  Status = unloaded | PendingStatus 
 *	  PendingStatus = pending_driver | pending_process
 *	  Ref = ref()
 *	  ErrorAtom = linked_in_driver | not_loaded | 
 *	              not_loaded_by_this_process | permanent
 */

/* 
   You have to have loaded the driver and the pid state 
   is LOADED or AWAIT_LOAD. You will be removed from the list
   regardless of driver state.
   If the driver is loaded by someone else to, return is
   {ok, pending_process}
   If the driver is loaded but locked by a port, return is
   {ok, pending_driver}
   If the driver is loaded and free to unload (you're the last holding it)
   {ok, unloaded}
   If it's not loaded or not loaded by you
   {error, not_loaded} or {error, not_loaded_by_you}

   Internally, if its in state UNLOADING, just return {ok, pending_driver} and
   remove/decrement this pid (which should be an LOADED tagged one).
   If the state is RELOADING, this pid should be in list as LOADED tagged, 
   only AWAIT_LOAD would be possible but not allowed for unloading, remove it 
   and, if the last LOADED tagged, change from RELOAD to UNLOAD and notify
   any AWAIT_LOAD-waiters with {'DOWN', ref(), driver, name(), load_cancelled}
   If the driver made itself permanent, {'UP', ref(), driver, name(), permanent}
*/
Eterm erl_ddll_try_unload_2(BIF_ALIST_2)
{
    Eterm name_term = BIF_ARG_1;
    Eterm options = BIF_ARG_2;
    char *name = NULL;
    Eterm ok_term = NIL;
    Eterm soft_error_term = NIL;
    erts_driver_t *drv;
    DE_Handle *dh;
    DE_ProcEntry *pe;
    Eterm *hp;
    Eterm t;
    int monitor = 0;
    Eterm l;
    int kill_ports = 0;

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);

    for(l = options; is_list(l); l =  CDR(list_val(l))) {
	Eterm opt = CAR(list_val(l));
	Eterm *tp;
	if (is_not_tuple(opt)) {
	    if (opt == am_kill_ports) {
		kill_ports = 1;
		continue;
	    } else {
		goto error;
	    }
	}
	tp = tuple_val(opt);
	if (*tp != make_arityval(2) || tp[1] != am_monitor) {
	    goto error;
	}
	if (tp[2] == am_pending_driver) { 
	    monitor = 1;
	} else if (tp[2] == am_pending) {
	    monitor = 2;
	} else {
	    goto error;
	}
    }
    if (is_not_nil(l)) {
	goto error;
    }

    if ((name = pick_list_or_atom(name_term)) == NULL) {
	goto error;
    }

#if DDLL_SMP
    lock_drv_list();
#endif

    if ((drv = lookup_driver(name)) == NULL) {
	soft_error_term = am_not_loaded;
	goto soft_error;
    }

    if (drv->handle == NULL) {
	soft_error_term = am_linked_in_driver;
	goto soft_error;
    } else if (drv->handle->status == ERL_DE_PERMANENT) {
	soft_error_term = am_permanent;
	goto soft_error;
    }	
    dh = drv->handle;
    if (dh->flags & ERL_DE_FL_KILL_PORTS) {
	kill_ports = 1;
    }
    if ((pe = find_proc_entry(dh, BIF_P, ERL_DE_PROC_LOADED)) == NULL) {
	if (num_procs(dh, ERL_DE_PROC_LOADED) > 0) {
	    soft_error_term = am_not_loaded_by_this_process;
	    goto soft_error;
	}
    } else {
	remove_proc_entry(dh, pe);
	if (!(pe->flags & ERL_DE_FL_DEREFERENCED)) {
	    erts_ddll_dereference_driver(dh);
	}
	erts_free(ERTS_ALC_T_DDLL_PROCESS, pe);
    }
    if (num_procs(dh, ERL_DE_PROC_LOADED) > 0) {
	ok_term = am_pending_process;
	--monitor;
	goto done;
    }
    if (dh->status == ERL_DE_RELOAD ||
	dh->status == ERL_DE_FORCE_RELOAD) {
	notify_all(dh, drv->name, 
		   ERL_DE_PROC_AWAIT_LOAD, am_DOWN, am_load_cancelled);
	erts_free(ERTS_ALC_T_DDLL_HANDLE,dh->reload_full_path);
	erts_free(ERTS_ALC_T_DDLL_HANDLE,dh->reload_driver_name);
	dh->reload_full_path = dh->reload_driver_name = NULL; 
	dh->reload_flags = 0;
    } 
    if (erts_smp_atomic32_read_nob(&dh->port_count) > 0) {
	++kill_ports;
    }
    dh->status = ERL_DE_UNLOAD;
    ok_term = am_pending_driver;
done:
    assert_drv_list_rwlocked();
    if (kill_ports > 1) {
	/* Avoid closing the driver by referencing it */
	erts_ddll_reference_driver(dh);
	dh->status = ERL_DE_FORCE_UNLOAD;
#if DDLL_SMP
	unlock_drv_list();
#endif
	kill_ports_driver_unloaded(dh);
#if DDLL_SMP
	lock_drv_list(); 
#endif
	erts_ddll_dereference_driver(dh);
    } 

#if DDLL_SMP
    erts_ddll_reference_driver(dh);
    unlock_drv_list();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    lock_drv_list();
    erts_ddll_dereference_driver(dh);
#endif
    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    BIF_P->flags |= F_USING_DDLL;
    if (monitor > 0) {
	Eterm mref = add_monitor(BIF_P, dh, ERL_DE_PROC_AWAIT_UNLOAD);
	hp = HAlloc(BIF_P, 4);
	t = TUPLE3(hp, am_ok, ok_term, mref);
    } else {
	hp = HAlloc(BIF_P, 3);
	t = TUPLE2(hp, am_ok, ok_term);
    }
    if (kill_ports > 1) {
	ERTS_BIF_CHK_EXITED(BIF_P); /* May be exited by port killing */
    }
#if DDLL_SMP
    unlock_drv_list();
#endif
    BIF_RET(t);
 
soft_error:
#if DDLL_SMP
    unlock_drv_list();
#endif
    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    hp = HAlloc(BIF_P, 3);
    t = TUPLE2(hp, am_error, soft_error_term);
    BIF_RET(t);
 
 error: /* No lock fiddling before going here */
    assert_drv_list_not_locked();
    if (name != NULL) {
	erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    }
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    BIF_ERROR(BIF_P, BADARG);
}


/* 
 * A shadow of the "real" demonitor BIF
 */
BIF_RETTYPE erl_ddll_demonitor_1(BIF_ALIST_1)
{
   if (is_not_internal_ref(BIF_ARG_1)) {
       BIF_ERROR(BIF_P, BADARG);
   }
   if (BIF_P->flags & F_USING_DDLL) {
       erts_ddll_remove_monitor(BIF_P, BIF_ARG_1, ERTS_PROC_LOCK_MAIN);
   }
   BIF_RET(am_true);
}

/* 
 * A shadow of the "real" monitor BIF
 */
BIF_RETTYPE erl_ddll_monitor_2(BIF_ALIST_2)
{
    if (BIF_ARG_1 != am_driver) {
	BIF_ERROR(BIF_P, BADARG);
    }
    return erts_ddll_monitor_driver(BIF_P, BIF_ARG_2, ERTS_PROC_LOCK_MAIN);
}

/* 
 * Return list of loaded drivers {ok,[string()]} 
 */
BIF_RETTYPE erl_ddll_loaded_drivers_0(BIF_ALIST_0)
{
    Eterm *hp;
    int need = 3;
    Eterm res = NIL;
    erts_driver_t *drv;
#if DDLL_SMP
    lock_drv_list();
#endif
    for (drv = driver_list; drv; drv = drv->next) {
	need += sys_strlen(drv->name)*2+2;
    }
    hp = HAlloc(BIF_P, need);
    for (drv = driver_list; drv; drv = drv->next) {
	Eterm l;
	l = buf_to_intlist(&hp, drv->name, sys_strlen(drv->name), NIL);
	res = CONS(hp,l,res);
	hp += 2;
    }
    res = TUPLE2(hp,am_ok,res);
    /* hp += 3 */
#if DDLL_SMP
    unlock_drv_list();
#endif
    BIF_RET(res);
}

/* 
 * More detailed info about loaded drivers: 
 * item is processes, driver_options, port_count, linked_in_driver, 
 * permanent, awaiting_load, awaiting_unload 
 */
BIF_RETTYPE erl_ddll_info_2(BIF_ALIST_2)
{
    Process *p = BIF_P;
    Eterm name_term = BIF_ARG_1;
    Eterm item = BIF_ARG_2;
    char *name = NULL;
    Eterm res = NIL;
    erts_driver_t *drv;
    ProcEntryInfo *pei = NULL;
    int num_pei;
    Eterm *hp;
    int i;
    Uint filter;
#if DDLL_SMP
    int have_lock = 0;
#endif

    if ((name = pick_list_or_atom(name_term)) == NULL) {
	goto error;
    }

    if (!is_atom(item)) {
	goto error;
    }

#if DDLL_SMP 
    lock_drv_list();
    have_lock = 1;
#endif
    if ((drv = lookup_driver(name)) == NULL) {
	goto error;
    }
    
    switch (item) {
    case am_processes:
	filter = ERL_DE_PROC_LOADED;
	break;
    case am_driver_options:
	if (drv->handle == NULL) {
	    res = am_linked_in_driver;
	} else {
	    Uint start_flags = drv->handle->flags & ERL_FL_CONSISTENT_MASK;
	    /* Cheating, only one flag for now... */
	    if (start_flags & ERL_DE_FL_KILL_PORTS) {
		Eterm *myhp;
		myhp = HAlloc(p,2);
		res = CONS(myhp,am_kill_ports,NIL);
	    } else {
		res = NIL;
	    }
	}
	goto done;
    case am_port_count:
	if (drv->handle == NULL) {
	    res = am_linked_in_driver;
	} else if (drv->handle->status == ERL_DE_PERMANENT) {
	    res = am_permanent;
	} else {
	    res = make_small(erts_smp_atomic32_read_nob(&drv->handle->port_count));
	}
	goto done;
    case am_linked_in_driver:
	if (drv->handle == NULL){
	    res = am_true;
	} else {
	    res = am_false;
	}
	goto done;
    case am_permanent:
	if (drv->handle != NULL && drv->handle->status == ERL_DE_PERMANENT) {
	    res = am_true;
	} else {
	    res = am_false;
	}
	goto done;
    case am_awaiting_load:
	filter = ERL_DE_PROC_AWAIT_LOAD;
	break;
    case am_awaiting_unload:
	filter = ERL_DE_PROC_AWAIT_UNLOAD;
	break;
    default:
	goto error;
    }

    if (drv->handle == NULL) {
	res = am_linked_in_driver;
	goto done;
    } else if (drv->handle->status == ERL_DE_PERMANENT) {
	res = am_permanent;
	goto done;
    }
    num_pei = build_proc_info(drv->handle, &pei, filter);
    if (!num_pei) {
	goto done;
    }
    hp = HAlloc(p,num_pei * (2+3));
    for (i = 0; i < num_pei; ++ i) {
	Eterm tpl = TUPLE2(hp,pei[i].pid,make_small(pei[i].count));
	hp += 3;
	res = CONS(hp,tpl,res);
	hp += 2;
    }    
 done:    
#if DDLL_SMP
    unlock_drv_list();
#endif
    if (pei)
	erts_free(ERTS_ALC_T_DDLL_TMP_BUF, pei);
    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    BIF_RET(res);
 error:
    if (name != NULL) {
	erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    }
#if DDLL_SMP
    if (have_lock) {
	unlock_drv_list();
    }
#endif
    BIF_ERROR(p,BADARG);
}

/*
 * Backend for erl_ddll:format_error, handles all "soft" errors returned by builtins,
 * possibly by calling the system specific error handler
 */
BIF_RETTYPE erl_ddll_format_error_int_1(BIF_ALIST_1)
{
    Process *p = BIF_P;
    Eterm code_term = BIF_ARG_1;
    char *errstring = NULL;
    int errint;
    int len;
    Eterm ret = NIL;
    Eterm *hp;

    /* These errors can only appear in the erlang interface, not in the interface provided
       to drivers... */
    switch (code_term) {
    case am_inconsistent:
	errstring = "Driver name and/or driver options are inconsistent with "
	    "currently loaded driver";
	break;
    case am_linked_in_driver:
	errstring = "Driver is statically linked and "
	    "cannot be loaded/unloaded";
	break;
    case am_permanent:
	errstring = "DDLL driver is permanent an can not be unloaded/loaded";
	break;
    case am_not_loaded:
	errstring = "DDLL driver is not loaded";
	break;
    case am_not_loaded_by_this_process:
	errstring = "DDLL driver was not loaded by this process";
	break;
    case am_not_pending:
	errstring = "DDLL load not pending for this driver name";
	break;
    case am_already_loaded:
	errstring = "DDLL driver is already loaded successfully";
	break;
    case am_pending_reload:
	errstring = "Driver reloading is already pending";
	break;
    case am_pending_process:
	errstring = "Driver is loaded by others when attempting "
	    "option {reload, pending_driver}";
	break;
    default:
	/* A "real" error, we translate the atom to a code and translate the code 
	   to a string in the same manner as in the interface provided to drivers... */
	if (errdesc_to_code(code_term,&errint) != 0) {
	    goto error;
	}
#if DDLL_SMP 
	lock_drv_list();
#endif
	errstring = erts_ddll_error(errint);
#if DDLL_SMP
	unlock_drv_list();
#endif
	break;
    }
    if (errstring == NULL) {
	goto error;
    }
    len = sys_strlen(errstring);
    hp = HAlloc(p, 2 * len);
    ret = buf_to_intlist(&hp, errstring, len, NIL);
    BIF_RET(ret);
 error:
    BIF_ERROR(p,BADARG);
} 

void erts_ddll_init(void)
{
    erl_sys_ddll_init();
}

/* Return value as a bif, called by erlang:monitor */
Eterm erts_ddll_monitor_driver(Process *p,
			       Eterm description,
			       ErtsProcLocks plocks) 
{
    Eterm *tp;
    Eterm ret;
    char *name;

    if (is_not_tuple(description)) {
	BIF_ERROR(p,BADARG);
    }
    tp = tuple_val(description);
    if (*tp != make_arityval(2)) {
	BIF_ERROR(p,BADARG);
    }
    if ((name = pick_list_or_atom(tp[1])) == NULL) {
	BIF_ERROR(p,BADARG);
    }
    switch (tp[2]) {
    case am_loaded:
	ERTS_BIF_PREP_RET(ret, notify_when_loaded(p,tp[1],name,plocks));
	break;
    case am_unloaded:
	ERTS_BIF_PREP_RET(ret, notify_when_unloaded(p,tp[1],name,plocks,
						    ERL_DE_PROC_AWAIT_UNLOAD));
	break;
    case am_unloaded_only:
	ERTS_BIF_PREP_RET(ret, 
			  notify_when_unloaded(p,tp[1],name,plocks,
					       ERL_DE_PROC_AWAIT_UNLOAD_ONLY));
	break;
    default:
	ERTS_BIF_PREP_ERROR(ret,p,BADARG);
	break;
    }

    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    return ret;
}

void erts_ddll_remove_monitor(Process *p, Eterm ref, ErtsProcLocks plocks)
{ 
    erts_driver_t *drv;
    erts_smp_proc_unlock(p, plocks);
    lock_drv_list();
    drv = driver_list;
    while (drv != NULL) {
	if (drv->handle != NULL && drv->handle->status != ERL_DE_PERMANENT) {
	    DE_ProcEntry **pe = &(drv->handle->procs);
	    while ((*pe) != NULL) {
		if ((*pe)->proc == p && 
		    ((*pe)->awaiting_status == ERL_DE_PROC_AWAIT_LOAD ||
		     (*pe)->awaiting_status == ERL_DE_PROC_AWAIT_UNLOAD ||
		     (*pe)->awaiting_status == 
		     ERL_DE_PROC_AWAIT_UNLOAD_ONLY) &&
		    eq(make_internal_ref(&((*pe)->heap)),ref)) {
		    DE_ProcEntry *r = *pe;
		    *pe = r->next;
		    erts_free(ERTS_ALC_T_DDLL_PROCESS, (void *) r);
		    goto done;
		} 
		pe = &((*pe)->next);
	    }
	}
	drv = drv->next;
    }
 done:
    unlock_drv_list();
    erts_smp_proc_lock(p, plocks);
}

/* 
 * Called from erl_process.c.
 */
void erts_ddll_proc_dead(Process *p, ErtsProcLocks plocks) 
{
    erts_driver_t *drv;
    erts_smp_proc_unlock(p, plocks);
    lock_drv_list();
    drv = driver_list;
    while (drv != NULL) {
	if (drv->handle != NULL && drv->handle->status != ERL_DE_PERMANENT) {
	    DE_ProcEntry **pe = &(drv->handle->procs);
	    int kill_ports = (drv->handle->flags & ERL_DE_FL_KILL_PORTS);
	    int left = 0;
	    while ((*pe) != NULL) {
		if ((*pe)->proc == p) {
		    DE_ProcEntry *r = *pe;
		    *pe = r->next;
		    if (!(r->flags & ERL_DE_FL_DEREFERENCED) && 
			r->awaiting_status == ERL_DE_PROC_LOADED) {
			erts_ddll_dereference_driver(drv->handle);
		    }
		    erts_free(ERTS_ALC_T_DDLL_PROCESS, (void *) r);
		} else {
		    if ((*pe)->awaiting_status == ERL_DE_PROC_LOADED) {
			++left;
		    }
		    pe = &((*pe)->next);
		}
	    }
	    if (!left) {
		DE_Handle *dh = drv->handle;
		if (dh->status == ERL_DE_RELOAD ||
		    dh->status == ERL_DE_FORCE_RELOAD) {
		    notify_all(dh, drv->name, 
			       ERL_DE_PROC_AWAIT_LOAD, am_DOWN, am_load_cancelled);
		    erts_free(ERTS_ALC_T_DDLL_HANDLE,dh->reload_full_path);
		    erts_free(ERTS_ALC_T_DDLL_HANDLE,dh->reload_driver_name);
		    dh->reload_full_path = dh->reload_driver_name = NULL; 
		    dh->reload_flags = 0;
		} 
		dh->status = ERL_DE_UNLOAD;
	    }
	    if (!left
		&& erts_smp_atomic32_read_nob(&drv->handle->port_count) > 0) {
		if (kill_ports) {
		    DE_Handle *dh = drv->handle;
		    erts_ddll_reference_driver(dh);
		    dh->status = ERL_DE_FORCE_UNLOAD;
#if DDLL_SMP
		    unlock_drv_list();
#endif
		    kill_ports_driver_unloaded(dh);
#if DDLL_SMP
		    lock_drv_list(); /* Needed for future list operations */
#endif
		    drv = drv->next; /* before allowing destruction */
		    erts_ddll_dereference_driver(dh);
		} else {
		    drv = drv->next;
		}
	    } else {
		drv = drv->next;
	    }
	} else {
	    drv = drv->next;
	}
    }
    unlock_drv_list();
    erts_smp_proc_lock(p, plocks);
}
void erts_ddll_lock_driver(DE_Handle *dh, char *name)
{
    DE_ProcEntry *p,*q;
    assert_drv_list_rwlocked();
    notify_all(dh, name, 
	       ERL_DE_PROC_AWAIT_LOAD, am_UP, am_permanent);
    notify_all(dh, name, 
	       ERL_DE_PROC_AWAIT_UNLOAD, am_UP, am_permanent);
    notify_all(dh, name, 
	       ERL_DE_PROC_AWAIT_UNLOAD_ONLY, am_UP, am_permanent);
    
    p = dh->procs; 
    while(p != NULL) {
	q = p;
	p = p->next;
	erts_free(ERTS_ALC_T_DDLL_PROCESS, (void *) q);
    }
    dh->procs = NULL;
    erts_ddll_reference_driver(dh);
    dh->status = ERL_DE_PERMANENT;
}


void erts_ddll_increment_port_count(DE_Handle *dh) 
{
    assert_drv_list_locked();
    erts_smp_atomic32_inc_nob(&dh->port_count);
}

void erts_ddll_decrement_port_count(DE_Handle *dh)
{
    assert_drv_list_locked();
#if DEBUG
    ASSERT(erts_smp_atomic32_dec_read_nob(&dh->port_count) >= 0);
#else
    erts_smp_atomic32_dec_nob(&dh->port_count);
#endif
}

static void first_ddll_reference(DE_Handle *dh) 
{
    assert_drv_list_rwlocked();
    erts_refc_init(&(dh->refc),1);
}

void erts_ddll_reference_driver(DE_Handle *dh)
{
    assert_drv_list_locked();
    if (erts_refc_inctest(&(dh->refc),1) == 1) {
	erts_refc_inc(&(dh->refc),2); /* add a reference for the scheduled operation */
    }
}

void erts_ddll_reference_referenced_driver(DE_Handle *dh)
{
    erts_refc_inc(&(dh->refc),2);
}

void erts_ddll_dereference_driver(DE_Handle *dh)
{
    if (erts_refc_dectest(&(dh->refc),0) == 0) {
	/* No lock here, but if the driver is referenced again,
	   the scheduled deletion is added as a reference too, see above */
	erts_schedule_misc_op(ddll_no_more_references, (void *) dh);
    }
}
static void dereference_all_processes(DE_Handle *dh) 
{
    DE_ProcEntry *p;
    assert_drv_list_rwlocked();
    for(p  = dh->procs;p != NULL; p = p->next) {
	if (p->awaiting_status == ERL_DE_PROC_LOADED) {
	    ASSERT(!(p->flags & ERL_DE_FL_DEREFERENCED));
	    erts_ddll_dereference_driver(dh);
	    p->flags |= ERL_DE_FL_DEREFERENCED;
	}
    }
}

static void restore_process_references(DE_Handle *dh) 
{
    DE_ProcEntry *p;
    assert_drv_list_rwlocked();
    ASSERT(erts_refc_read(&(dh->refc),0) == 0);
    for(p  = dh->procs;p != NULL; p = p->next) {
	if (p->awaiting_status == ERL_DE_PROC_LOADED) {
	    ASSERT(p->flags & ERL_DE_FL_DEREFERENCED);
	    erts_refc_inc(&(dh->refc),1);
	    p->flags &= ~ERL_DE_FL_DEREFERENCED;
	}
    }
}
    

int erts_ddll_driver_ok(DE_Handle *dh) 
{
    assert_drv_list_locked();
    return ((dh == NULL) || (dh->status != ERL_DE_FORCE_UNLOAD &&
			     dh->status != ERL_DE_FORCE_RELOAD));
}


static void ddll_no_more_references(void *vdh)
{
    DE_Handle *dh = (DE_Handle *) vdh;
    erts_aint_t x;

    lock_drv_list();

    x = erts_refc_read(&(dh->refc),0);
    if (x > 0) {
	x = erts_refc_dectest(&(dh->refc),0); /* delete the reference added for me */
    }


    if (x == 0) {
	DE_ProcEntry **p = &(dh->procs);
	Eterm save_driver_name = am_undefined;
	ASSERT(dh->status != ERL_DE_OK);
	do_unload_driver_entry(dh,&save_driver_name);
	while (*p != NULL) {
	    DE_ProcEntry *q;
	    if ((*p)->awaiting_status == ERL_DE_PROC_AWAIT_UNLOAD ||
		(*p)->awaiting_status == ERL_DE_PROC_AWAIT_UNLOAD_ONLY) {
		notify_proc((*p)->proc, 
			    make_internal_ref(&((*p)->heap)),
			    save_driver_name,am_DOWN,am_unloaded, 0);
		q = *p;
		*p = q->next;
		erts_free(ERTS_ALC_T_DDLL_PROCESS, (void *) q);
	    } else {
		ASSERT(dh->status == ERL_DE_RELOAD ||
		       dh->status == ERL_DE_FORCE_RELOAD);
		p = &((*p)->next);
	    }
	}

	if (dh->status == ERL_DE_UNLOAD || dh->status == ERL_DE_FORCE_UNLOAD) {
	    ASSERT(dh->full_path != NULL);
	    erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) dh->full_path);
	    erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) dh);
	} else { /* ERL_DE_RELOAD || ERL_DE_FORCE_RELOAD */
	    int reload_res =
		reload_driver_entry(dh);
	    p = &(dh->procs);
	    while (*p != NULL) {
		DE_ProcEntry *q;
		if ((*p)->awaiting_status == ERL_DE_PROC_AWAIT_LOAD) {
		    if (reload_res == 0) {
			notify_proc((*p)->proc, 
				    make_internal_ref(&((*p)->heap)),
				    save_driver_name, am_UP, am_loaded, 0);
		    } else {
			notify_proc((*p)->proc, 
				    make_internal_ref(&((*p)->heap)),
				    save_driver_name, am_DOWN, am_load_failure, reload_res);
		    }
		    q = *p;
		    *p = q->next;
		    erts_free(ERTS_ALC_T_DDLL_PROCESS, (void *) q);
		} else {
		    if (reload_res != 0) {
			DE_ProcEntry *q = *p;
			*p = q->next;
			erts_free(ERTS_ALC_T_DDLL_PROCESS, (void *) q);
		    } else {
			p = &((*p)->next);
		    }
		}
	    }
	    if (reload_res != 0) {
		ASSERT(dh->full_path == NULL);
		erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) dh);
	    }
	}
    }
    unlock_drv_list();
}    

char *erts_ddll_error(int code) {
    switch (code) {
    case ERL_DE_NO_ERROR:
	return "No error";
    case ERL_DE_LOAD_ERROR_NO_INIT:
	return "No driver init in dynamic library";
    case  ERL_DE_LOAD_ERROR_FAILED_INIT:
	return "Driver init failed";
    case ERL_DE_LOAD_ERROR_BAD_NAME:
	return "Bad driver name";
    case ERL_DE_LOAD_ERROR_NAME_TO_LONG:
	return "Driver name to long";
    case ERL_DE_LOAD_ERROR_INCORRECT_VERSION:
	return "Driver compiled with incorrect version of erl_driver.h";
    case ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY:
	return "DDLL functionality not available on this platform";
    case ERL_DE_ERROR_UNSPECIFIED:
	return "Unspecified dynamic library error";
    case ERL_DE_LOOKUP_ERROR_NOT_FOUND:
	return "Symbol not found in dynamic library";
    default:
	return erts_sys_ddll_error(code);
    }
}

/*
 * Utilities
 */
static Eterm notify_when_loaded(Process *p, Eterm name_term, char *name, ErtsProcLocks plocks)
{ 
    Eterm r = NIL;
    Eterm immediate_tag = NIL;
    Eterm immediate_type = NIL;
    erts_driver_t *drv;

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & plocks);
#if DDLL_SMP 
    lock_drv_list();
#endif
    if ((drv = lookup_driver(name)) == NULL) {
	immediate_tag = am_unloaded;
	immediate_type = am_DOWN;
	goto immediate;
    }
    if (drv->handle == NULL || drv->handle->status == ERL_DE_PERMANENT) {
	immediate_tag = am_permanent;
	immediate_type = am_UP;
	goto immediate;
    }	

    switch (drv->handle->status) {
    case ERL_DE_OK:
	immediate_tag = am_loaded;
	immediate_type = am_UP;
	goto immediate;
    case ERL_DE_UNLOAD:
    case ERL_DE_FORCE_UNLOAD:
	immediate_tag = am_load_cancelled;
	immediate_type = am_DOWN;
	goto immediate;
    case ERL_DE_RELOAD:
    case ERL_DE_FORCE_RELOAD:
	break;
    default:
	erts_exit(ERTS_ERROR_EXIT,"Internal error, unknown state %u in dynamic driver.", drv->handle->status);
    }
    p->flags |= F_USING_DDLL;
    r = add_monitor(p, drv->handle, ERL_DE_PROC_AWAIT_LOAD);
#if DDLL_SMP
    unlock_drv_list();
#endif
    BIF_RET(r);
 immediate:
    r =  erts_make_ref(p);
#if DDLL_SMP 
    erts_smp_proc_unlock(p, plocks);
#endif
    notify_proc(p, r, name_term, immediate_type, immediate_tag, 0);
#if DDLL_SMP
    unlock_drv_list();
    erts_smp_proc_lock(p, plocks);
#endif
    BIF_RET(r);
}

static Eterm notify_when_unloaded(Process *p, Eterm name_term, char *name, ErtsProcLocks plocks, Uint flag)
{ 
    Eterm r = NIL;
    Eterm immediate_tag = NIL;
    Eterm immediate_type = NIL;
    erts_driver_t *drv;

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & plocks);
#if DDLL_SMP 
    lock_drv_list();
#endif
    if ((drv = lookup_driver(name)) == NULL) {
	immediate_tag = am_unloaded;
	immediate_type = am_DOWN;
	goto immediate;
    }
    if (drv->handle == NULL || drv->handle->status == ERL_DE_PERMANENT) {
	immediate_tag = am_permanent;
	immediate_type = am_UP;
	goto immediate;
    }	

    p->flags |= F_USING_DDLL;
    r = add_monitor(p, drv->handle, flag);
#if DDLL_SMP
    unlock_drv_list();
#endif
    BIF_RET(r);
 immediate:
    r =  erts_make_ref(p);
#if DDLL_SMP 
    erts_smp_proc_unlock(p, plocks);
#endif
    notify_proc(p, r, name_term, immediate_type, immediate_tag, 0);
#if DDLL_SMP
    unlock_drv_list();
    erts_smp_proc_lock(p, plocks);
#endif
    BIF_RET(r);
}


static int is_last_user(DE_Handle *dh, Process *proc) {
    DE_ProcEntry *p = dh->procs;
    int found = 0;

    assert_drv_list_rwlocked();

    while (p != NULL) {
	if (p->proc == proc && p->awaiting_status == ERL_DE_PROC_LOADED) {
	    if (found == 0) {
		found = 1;
	    } else {
		return 0;
	    }
	} else if (p->awaiting_status == ERL_DE_PROC_LOADED) {
	    return 0;
	}
	p = p->next;
    }
    return found;
}
    
static DE_ProcEntry *find_proc_entry(DE_Handle *dh, Process *proc, Uint status)
{
    DE_ProcEntry *p = dh->procs;

    assert_drv_list_rwlocked();

    while (p != NULL) {
	if (p->proc == proc && p->awaiting_status == status) {
	    return p;
	}
	p = p->next;
    }
    return NULL;
}

static void remove_proc_entry(DE_Handle *dh, DE_ProcEntry *pe)
{
    DE_ProcEntry **p = &(dh->procs);

    while (*p != NULL && *p != pe) {
	p = &((*p)->next);
    }
    if ((*p) != NULL) {
	*p = (*p)->next;
    }
}

static int num_procs(DE_Handle *dh, Uint status) {
    DE_ProcEntry *p = dh->procs;
    int i = 0;

    assert_drv_list_rwlocked();

    while (p != NULL) {
	if (p->awaiting_status == status) {
	    ++i;
	}
	p = p->next;
    }
    return i;
}
/*
static int num_entries(DE_Handle *dh, Process *proc, Uint status) {    
    DE_ProcEntry *p = dh->procs;
    int i = 0;

    assert_drv_list_rwlocked();
    while (p != NULL) {
	if (p->awaiting_status == status && p->proc == proc) {
	    ++i;
	}
	p = p->next;
    }
    return i;
}
*/
static void add_proc_loaded(DE_Handle *dh, Process *proc) 
{
    DE_ProcEntry *p;
    assert_drv_list_rwlocked();
    p = erts_alloc(ERTS_ALC_T_DDLL_PROCESS, sizeof(DE_ProcEntry));
    p->proc = proc;
    p->flags = 0;
    p->awaiting_status = ERL_DE_PROC_LOADED;
    p->next = dh->procs;
    dh->procs = p;
}

static void add_proc_loaded_deref(DE_Handle *dh, Process *proc) 
{
    DE_ProcEntry *p;
    assert_drv_list_rwlocked();
    p = erts_alloc(ERTS_ALC_T_DDLL_PROCESS, sizeof(DE_ProcEntry));
    p->proc = proc;
    p->awaiting_status = ERL_DE_PROC_LOADED;
    p->flags = ERL_DE_FL_DEREFERENCED;
    p->next = dh->procs;
    dh->procs = p;
}

static Eterm copy_ref(Eterm ref, Eterm *hp)
{
    RefThing *ptr = ref_thing_ptr(ref);
    memcpy(hp, ptr, sizeof(RefThing));
    return (make_internal_ref(hp));
}

static void add_proc_waiting(DE_Handle *dh, Process *proc, 
			     Uint status, Eterm ref) 
{
    DE_ProcEntry *p;
    assert_drv_list_rwlocked();
    p = erts_alloc(ERTS_ALC_T_DDLL_PROCESS, sizeof(DE_ProcEntry));
    p->proc = proc;
    p->flags = 0;
    p->awaiting_status = status;
    copy_ref(ref, p->heap);
    p->next = dh->procs;
    dh->procs = p;
}

static Eterm add_monitor(Process *p, DE_Handle *dh, Uint status)
{
    Eterm r;

    assert_drv_list_rwlocked();
    r = erts_make_ref(p);
    add_proc_waiting(dh, p, status, r);
    return r;
}
    

static void set_driver_reloading(DE_Handle *dh, Process *proc, char *path, char *name, Uint flags)
{
    DE_ProcEntry *p;

    assert_drv_list_rwlocked();
    p = erts_alloc(ERTS_ALC_T_DDLL_PROCESS, sizeof(DE_ProcEntry));
    p->proc = proc;
    p->awaiting_status = ERL_DE_OK;
    p->next = dh->procs;
    p->flags = ERL_DE_FL_DEREFERENCED;
    dh->procs = p;
    dh->status = ERL_DE_RELOAD;
    dh->reload_full_path = erts_alloc(ERTS_ALC_T_DDLL_HANDLE, sys_strlen(path) + 1);
    strcpy(dh->reload_full_path,path);
    dh->reload_driver_name = erts_alloc(ERTS_ALC_T_DDLL_HANDLE, sys_strlen(name) + 1);
    strcpy(dh->reload_driver_name,name);
    dh->reload_flags = flags;
}

static int do_load_driver_entry(DE_Handle *dh, char *path, char *name)
{
    void *init_handle;
    int res;
    ErlDrvEntry *dp;

    assert_drv_list_rwlocked();

    if ((res =  erts_sys_ddll_open(path, &(dh->handle), NULL)) != ERL_DE_NO_ERROR) {
	return res;
    }
    
    if ((res = erts_sys_ddll_load_driver_init(dh->handle, 
					      &init_handle)) != ERL_DE_NO_ERROR) {
	res = ERL_DE_LOAD_ERROR_NO_INIT;
	goto error;
    }
    
    dp = erts_sys_ddll_call_init(init_handle);
    if (dp == NULL) {
	res = ERL_DE_LOAD_ERROR_FAILED_INIT;
	goto error;
    }

    switch (dp->extended_marker) {
    case ERL_DRV_EXTENDED_MARKER:
	if (dp->major_version < ERL_DRV_MIN_REQUIRED_MAJOR_VERSION_ON_LOAD
	    || (ERL_DRV_EXTENDED_MAJOR_VERSION < dp->major_version
		|| (ERL_DRV_EXTENDED_MAJOR_VERSION == dp->major_version
		    && ERL_DRV_EXTENDED_MINOR_VERSION < dp->minor_version))) {
	    /* Incompatible driver version */
	    res = ERL_DE_LOAD_ERROR_INCORRECT_VERSION;
	    goto error;
	}
	break;
    default:
	/* Old driver; needs to be recompiled... */
	res = ERL_DE_LOAD_ERROR_INCORRECT_VERSION;
	goto error;
    }

    if (strcmp(name, dp->driver_name) != 0) {
	res = ERL_DE_LOAD_ERROR_BAD_NAME;
	goto error;
    }
    erts_smp_atomic_init_nob(&(dh->refc), (erts_aint_t) 0);
    erts_smp_atomic32_init_nob(&dh->port_count, 0);
    dh->full_path = erts_alloc(ERTS_ALC_T_DDLL_HANDLE, sys_strlen(path) + 1);
    sys_strcpy(dh->full_path, path);
    dh->flags = 0;
    dh->status = ERL_DE_OK;

    if (erts_add_driver_entry(dp, dh, 1) != 0 /* io.c */) { 
	/*
	 * The init in the driver struct did not return 0 
	 */
	erts_free(ERTS_ALC_T_DDLL_HANDLE, dh->full_path);
	dh->full_path = NULL;
	res = ERL_DE_LOAD_ERROR_FAILED_INIT;
	goto error;
    }
    return ERL_DE_NO_ERROR;

error:
    erts_sys_ddll_close(dh->handle);
    return res;
}

static int do_unload_driver_entry(DE_Handle *dh, Eterm *save_name)
{
    erts_driver_t *q, *p = driver_list;

    assert_drv_list_rwlocked();

    while (p != NULL) {
	if (p->handle == dh) {
		
	    q = p;
	    if (p->prev == NULL) {
		driver_list = p->next;
	    } else {
		p->prev->next = p->next;
	    }
	    if (p->next != NULL) {
		p->next->prev = p->prev;
	    }

	    if (save_name != NULL) {
		*save_name = mkatom(q->name);
	    } 
	    /* Future locking problems? Don't dare to let go of the
	       diver_list lock here!*/
	    if (q->finish) {
		int fpe_was_unmasked = erts_block_fpe();
		DTRACE1(driver_finish, q->name);
                LTTNG1(driver_finish, q->name);
		(*(q->finish))();
		erts_unblock_fpe(fpe_was_unmasked);
	    }
	    erts_sys_ddll_close(dh->handle);
	    erts_destroy_driver(q);
	    return 1;
	}
	p = p->next;
    }
    return 0;
}

static int load_driver_entry(DE_Handle **dhp, char *path, char *name)
{
    int res;
    DE_Handle *dh = erts_alloc(ERTS_ALC_T_DDLL_HANDLE, sizeof(DE_Handle));

    assert_drv_list_rwlocked();

    dh->handle = NULL;
    dh->procs = NULL;
    erts_smp_atomic32_init_nob(&dh->port_count, 0);
    erts_refc_init(&(dh->refc), (erts_aint_t) 0);
    dh->status = -1;
    dh->reload_full_path = NULL;
    dh->reload_driver_name = NULL;
    dh->reload_flags = 0;
    dh->full_path = NULL;
    dh->flags = 0;

    if ((res = do_load_driver_entry(dh, path, name)) != ERL_DE_NO_ERROR) {
	erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) dh);
	dh = NULL;
    }
    *dhp = dh;
    return res;
}

#if 0
static void unload_driver_entry(DE_Handle *dh)
{
    do_unload_driver_entry(dh, NULL);
    if (dh->full_path != NULL) {
	erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) dh->full_path);
    }
    erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) dh);
}
#endif
static int reload_driver_entry(DE_Handle *dh)
{
    char *path = dh->reload_full_path;
    char *name = dh->reload_driver_name;
    int loadres;
    Uint flags = dh->reload_flags;

    assert_drv_list_rwlocked();

    dh->reload_full_path = NULL;
    dh->reload_driver_name = NULL;

    ASSERT(erts_refc_read(&(dh->refc),0) == 0);
    ASSERT(dh->full_path != NULL);
    erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) dh->full_path);
    dh->full_path = NULL;

    loadres = do_load_driver_entry(dh, path, name);
    erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) path);
    erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) name);
    if (loadres == ERL_DE_NO_ERROR) {
	dh->status = ERL_DE_OK;
	dh->flags = flags;
    }
    restore_process_references(dh);
    return loadres;
}
 
/*
 * Notification {tag = atom(), ref = ref(), driver_name = atom()} or
 *              {'$DDLL_load_failure', ref = ref(), driver_name = atom(), 
 *               error_term = atom() | {system_error, int()}}
 */
   
static void notify_proc(Process *proc, Eterm ref, Eterm driver_name, Eterm type, 
			Eterm tag, int errcode)
{
    Eterm mess;
    Eterm r;
    Eterm *hp;
    ErtsMessage *mp;
    ErtsProcLocks rp_locks = 0;
    ErlOffHeap *ohp;
    ERTS_SMP_CHK_NO_PROC_LOCKS;

    assert_drv_list_rwlocked();
    if (errcode != 0) {
	int need = load_error_need(errcode);
	Eterm e;
	mp = erts_alloc_message_heap(proc, &rp_locks,
				     (6 /* tuple */ + 3 /* Error tuple */ + 
				      REF_THING_SIZE + need),
				     &hp, &ohp);
	r = copy_ref(ref,hp);
	hp += REF_THING_SIZE;
	e = build_load_error_hp(hp, errcode);
	hp += need;
	mess = TUPLE2(hp,tag,e);
	hp += 3;
	mess = TUPLE5(hp,type,r,am_driver,driver_name,mess);
    } else {	
	mp = erts_alloc_message_heap(proc, &rp_locks,
				     6 /* tuple */ + REF_THING_SIZE,
				     &hp, &ohp);
	r = copy_ref(ref,hp);
	hp += REF_THING_SIZE;
	mess = TUPLE5(hp,type,r,am_driver,driver_name,tag);
    }
    erts_queue_message(proc, rp_locks, mp, mess, am_system);
    erts_smp_proc_unlock(proc, rp_locks);
    ERTS_SMP_CHK_NO_PROC_LOCKS;
}

static void notify_all(DE_Handle *dh, char *name, Uint awaiting, Eterm type, Eterm tag)
{
    DE_ProcEntry **p;

    assert_drv_list_rwlocked();

    p = &(dh->procs);
    while (*p != NULL) {
	if ((*p)->awaiting_status == awaiting) {
	    DE_ProcEntry *pe;
	    pe = *p;
	    *p = pe->next;
	    notify_proc(pe->proc, make_internal_ref(&(pe->heap)), mkatom(name), type, tag, 0);
	    erts_free(ERTS_ALC_T_DDLL_PROCESS, (void *) pe);
	} else {
	    p = &((*p)->next);
	}
    }
}



typedef struct errcode_entry {
    char *atm;
    int code;
} ErrcodeEntry;

static ErrcodeEntry errcode_tab[] = {
    {"no_error", ERL_DE_NO_ERROR},
    {"no_driver_init", ERL_DE_LOAD_ERROR_NO_INIT},
    {"driver_init_failed", ERL_DE_LOAD_ERROR_FAILED_INIT},
    {"bad_driver_name", ERL_DE_LOAD_ERROR_BAD_NAME},
    {"driver_name_to_long", ERL_DE_LOAD_ERROR_NAME_TO_LONG},
    {"driver_incorrect_version", ERL_DE_LOAD_ERROR_INCORRECT_VERSION},
    {"no_ddll_available",  ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY},
    {"unspecified_error", ERL_DE_ERROR_UNSPECIFIED},
    {"symbol_not_found", ERL_DE_LOOKUP_ERROR_NOT_FOUND},
    {NULL,0}
};

static int errdesc_to_code(Eterm errdesc, int *code /* out */)
{
    int i;
    if (is_atom(errdesc)) {
	Atom *ap = atom_tab(atom_val(errdesc)); 
	for (i = 0; errcode_tab[i].atm != NULL; ++i) {
	    int len = sys_strlen(errcode_tab[i].atm);
	    if (len == ap->len && 
		!sys_strncmp(errcode_tab[i].atm,(char *) ap->name,len)) {
		*code = errcode_tab[i].code;
		return 0;
	    }
	}
	return -1;
    } else if (is_tuple(errdesc)) {
	Eterm *tp = tuple_val(errdesc);
	if (*tp != make_arityval(2) || tp[1] != am_open_error || is_not_small(tp[2])) {
	    return -1;
	}
	*code = signed_val(tp[2]);
	return 0;
    }
    return -1;
}

static Eterm build_load_error(Process *p, int code)
{
    int need = load_error_need(code);
    Eterm *hp = NULL;
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p));
    if (need) {
	hp = HAlloc(p,need);
    }
    return build_load_error_hp(hp,code);
}
    
static int load_error_need(int code)
{
    ErrcodeEntry *ee = errcode_tab;
    while (ee->atm != NULL) {
	if (ee->code == code) {
	    return 0;
	}
	++ee;
    }
    return 3;
}

static Eterm build_load_error_hp(Eterm *hp, int code)
{
    ErrcodeEntry *ee = errcode_tab;
    while (ee->atm != NULL) {
	if (ee->code == code) {
	    return mkatom(ee->atm);
	}
	++ee;
    }
    return TUPLE2(hp,am_open_error, make_small(code));
}
    


static Eterm mkatom(char *str)
{
    return erts_atom_put((byte *) str,
			 sys_strlen(str),
			 ERTS_ATOM_ENC_LATIN1,
			 1);
}

static char *pick_list_or_atom(Eterm name_term)
{ 
    char *name = NULL;
    ErlDrvSizeT name_len;
    if (is_atom(name_term)) {
	Atom *ap = atom_tab(atom_val(name_term));
	if (ap->len == 0) {
	    /* If io_lists with zero length is not allowed, 
	       then the empty atom shouldn't */
	    goto error;
	}
	name = erts_alloc(ERTS_ALC_T_DDLL_TMP_BUF, ap->len + 1);
	memcpy(name,ap->name,ap->len);
	name[ap->len] = '\0';
    } else {
	if (erts_iolist_size(name_term, &name_len)) {
	    goto error;
	}
	name = erts_alloc(ERTS_ALC_T_DDLL_TMP_BUF, name_len + 1);
	if (erts_iolist_to_buf(name_term, name, name_len) != 0) {
	    goto error;
	}
	name[name_len] = '\0';
    }
    return name;
 error:
    if (name != NULL) {
	erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    }
    return NULL;
}

static int build_proc_info(DE_Handle *dh, ProcEntryInfo **out_pei, Uint filter)
{
    ProcEntryInfo *pei = NULL;
    int num_pei = 0;
    int num_pei_allocated = 0;
    int i;
    DE_ProcEntry *pe;

    assert_drv_list_rwlocked();

    for (pe = dh->procs; pe != NULL; pe = pe->next) {
	Eterm id = pe->proc->common.id;
	Uint stat = pe->awaiting_status;
	if (stat == ERL_DE_PROC_AWAIT_UNLOAD_ONLY) {
	    stat = ERL_DE_PROC_AWAIT_UNLOAD;
	}
	if (stat != filter) {
	    continue;
	}
	for (i = 0; i < num_pei; ++i) {
	    if (pei[i].pid == id && pei[i].status == stat) {
		break;
	    }
	}
	if (i < num_pei) {
	    pei[i].count++;
	} else {
	    if (num_pei >= num_pei_allocated) {
		pei = (pei == NULL) 
		    ? erts_alloc(ERTS_ALC_T_DDLL_TMP_BUF, 
				 sizeof(ProcEntryInfo) * (num_pei_allocated = 10))
		    : erts_realloc(ERTS_ALC_T_DDLL_TMP_BUF, pei,
				   sizeof(ProcEntryInfo) * (num_pei_allocated += 10));
	    }
	    pei[num_pei].pid = id;
	    pei[num_pei].proc = pe->proc;
	    pei[num_pei].status = stat;
	    pei[num_pei].count = 1;
	    ++num_pei;
	}
    }
    *out_pei = pei;
    return num_pei;
}
	    
    

static erts_driver_t *lookup_driver(char *name)
{
    erts_driver_t *drv;
    assert_drv_list_locked();
    for (drv = driver_list; drv != NULL && strcmp(drv->name, name); drv = drv->next)
	;
    return drv;
}
