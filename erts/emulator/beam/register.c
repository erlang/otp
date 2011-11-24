/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2010. All Rights Reserved.
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
 * Manage registered processes.
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "hash.h"
#include "atom.h"
#include "register.h"

static Hash process_reg;

#define PREG_HASH_SIZE 10

#define REG_HASH(term) ((HashValue) atom_val(term))

static erts_smp_rwmtx_t regtab_rwmtx;

#define reg_try_read_lock()		erts_smp_rwmtx_tryrlock(&regtab_rwmtx)
#define reg_try_write_lock()		erts_smp_rwmtx_tryrwlock(&regtab_rwmtx)
#define reg_read_lock()			erts_smp_rwmtx_rlock(&regtab_rwmtx)
#define reg_write_lock()		erts_smp_rwmtx_rwlock(&regtab_rwmtx)
#define reg_read_unlock()		erts_smp_rwmtx_runlock(&regtab_rwmtx)
#define reg_write_unlock()		erts_smp_rwmtx_rwunlock(&regtab_rwmtx)

#ifdef ERTS_SMP
static ERTS_INLINE void
reg_safe_read_lock(Process *c_p, ErtsProcLocks *c_p_locks)
{
    if (*c_p_locks) {
	ASSERT(c_p);
	ASSERT(c_p_locks);
	ASSERT(*c_p_locks);

	if (reg_try_read_lock() != EBUSY) {
#ifdef ERTS_ENABLE_LOCK_CHECK
	    erts_proc_lc_might_unlock(c_p, *c_p_locks);
#endif
	    return;
	}

	/* Release process locks in order to avoid deadlock */
	erts_smp_proc_unlock(c_p, *c_p_locks);
	*c_p_locks = 0;
    }

    reg_read_lock();
}

static ERTS_INLINE void
reg_safe_write_lock(Process *c_p, ErtsProcLocks *c_p_locks)
{
    if (*c_p_locks) {
	ASSERT(c_p);
	ASSERT(c_p_locks);
	ASSERT(*c_p_locks);

	if (reg_try_write_lock() != EBUSY) {
#ifdef ERTS_ENABLE_LOCK_CHECK
	    erts_proc_lc_might_unlock(c_p, *c_p_locks);
#endif
	    return;
	}

	/* Release process locks in order to avoid deadlock */
	erts_smp_proc_unlock(c_p, *c_p_locks);
	*c_p_locks = 0;
    }

    reg_write_lock();
}

static ERTS_INLINE int
is_proc_alive(Process *p)
{
    int res;
    erts_pix_lock_t *pixlck = ERTS_PID2PIXLOCK(p->id);
    erts_pix_lock(pixlck);
    res = !p->is_exiting;
    erts_pix_unlock(pixlck);
    return res;
}

#endif

void register_info(int to, void *to_arg)
{
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	reg_read_lock();
    hash_info(to, to_arg, &process_reg);
    if (lock)
	reg_read_unlock();
}

static HashValue reg_hash(RegProc* obj)
{
    return REG_HASH(obj->name);
}

static int reg_cmp(RegProc *tmpl, RegProc *obj) {
    return tmpl->name != obj->name;
}

static RegProc* reg_alloc(RegProc *tmpl)
{
    RegProc* obj = (RegProc*) erts_alloc(ERTS_ALC_T_REG_PROC, sizeof(RegProc));
    if (!obj) {
	erl_exit(1, "Can't allocate %d bytes of memory\n", sizeof(RegProc));
    }
    obj->name = tmpl->name;
    obj->p = tmpl->p;
    obj->pt = tmpl->pt;
    return obj;
}

static void reg_free(RegProc *obj)
{
    erts_free(ERTS_ALC_T_REG_PROC, (void*) obj);
}

void init_register_table(void)
{
    HashFunctions f;
    erts_smp_rwmtx_opt_t rwmtx_opt = ERTS_SMP_RWMTX_OPT_DEFAULT_INITER;
    rwmtx_opt.type = ERTS_SMP_RWMTX_TYPE_FREQUENT_READ;
    rwmtx_opt.lived = ERTS_SMP_RWMTX_LONG_LIVED;

    erts_smp_rwmtx_init_opt(&regtab_rwmtx, &rwmtx_opt, "reg_tab");

    f.hash = (H_FUN) reg_hash;
    f.cmp  = (HCMP_FUN) reg_cmp;
    f.alloc = (HALLOC_FUN) reg_alloc;
    f.free = (HFREE_FUN) reg_free;

    hash_init(ERTS_ALC_T_REG_TABLE, &process_reg, "process_reg",
	      PREG_HASH_SIZE, f);
}

/*
 * Register a process or port (can't be registered twice).
 * Returns 0 if name, process or port is already registered.
 *
 * When smp support is enabled:
 *   * Assumes that main lock is locked (and only main lock)
 *     on c_p.
 *
 */
int erts_register_name(Process *c_p, Eterm name, Eterm id)
{
    int res = 0;
    Process *proc = NULL;
    Port *port = NULL;
    RegProc r, *rp;
    ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(c_p);

    if (is_not_atom(name) || name == am_undefined)
	return res;

    if (c_p->id == id) /* A very common case I think... */
	proc = c_p;
    else {
	if (is_not_internal_pid(id) && is_not_internal_port(id))
	    return res;
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
	if (is_internal_port(id)) {
	    port = erts_id2port(id, NULL, 0);
	    if (!port)
		goto done;
	}
    }

#ifdef ERTS_SMP
    {
	ErtsProcLocks proc_locks = proc ? ERTS_PROC_LOCK_MAIN : 0;
	reg_safe_write_lock(proc, &proc_locks);

	if (proc && !proc_locks)
	    erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
    }
#endif

    if (is_internal_pid(id)) {
	if (!proc)
	    proc = erts_pid2proc(NULL, 0, id, ERTS_PROC_LOCK_MAIN);
	r.p = proc;
	if (!proc)
	    goto done;
	if (proc->reg)
	    goto done;
	r.pt = NULL;
    }
    else {
	ASSERT(!INVALID_PORT(port, id));
	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(port));
	r.pt = port;
	if (r.pt->reg)
	    goto done;
	r.p = NULL;
    }

    r.name = name;
    
    rp = (RegProc*) hash_put(&process_reg, (void*) &r);
    if (proc && rp->p == proc) {
	if (IS_TRACED_FL(proc, F_TRACE_PROCS)) {
	    trace_proc(c_p, proc, am_register, name);
	}
	proc->reg = rp;
    }
    else if (port && rp->pt == port) {
    	if (IS_TRACED_FL(port, F_TRACE_PORTS)) {
		trace_port(port, am_register, name);
	}
	port->reg = rp;
    }

    if ((rp->p && rp->p->id == id) || (rp->pt && rp->pt->id == id)) {
	res = 1;
    }

 done:
    reg_write_unlock();
    if (port)
	erts_smp_port_unlock(port);
    if (c_p != proc) {
	if (proc)
	    erts_smp_proc_unlock(proc, ERTS_PROC_LOCK_MAIN);
	erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
    }
    return res;
}

/*
 *
 * When smp support is enabled:
 *   * Assumes that main lock is locked (and only main lock)
 *     on c_p.
 *
 *   * am_undefined is returned if c_p became exiting.
 */

Eterm
erts_whereis_name_to_id(Process *c_p, Eterm name)
{
    Eterm res = am_undefined;
    HashValue hval;
    int ix;
    HashBucket* b;
#ifdef ERTS_SMP
    ErtsProcLocks c_p_locks = c_p ? ERTS_PROC_LOCK_MAIN : 0;

    ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(c_p);
    reg_safe_read_lock(c_p, &c_p_locks);
    if (c_p && !c_p_locks)
        erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
#endif

    hval = REG_HASH(name);
    ix = hval % process_reg.size;
    b = process_reg.bucket[ix];

    /*
     * Note: We have inlined the code from hash.c for speed.
     */
	
    while (b) {
	RegProc* rp = (RegProc *) b;
	if (rp->name == name) {
	    /*
	     * SMP NOTE: No need to lock registered entity since it cannot
	     * be removed without acquiring write reg lock and id on entity
	     * is read only.
	     */
	    if (rp->p)
		res = rp->p->id;
	    else if (rp->pt)
		res = rp->pt->id;
	    break;
	}
	b = b->next;
    }

    reg_read_unlock();

    ASSERT(is_internal_pid(res) || is_internal_port(res) || res==am_undefined);

    return res;
}


void
erts_whereis_name(Process *c_p,
		  ErtsProcLocks c_p_locks,
		  Eterm name,
		  Process** proc,
		  ErtsProcLocks need_locks,
		  int flags,
		  Port** port)
{
    RegProc* rp = NULL;
    HashValue hval;
    int ix;
    HashBucket* b;
#ifdef ERTS_SMP
    ErtsProcLocks current_c_p_locks;
    Port *pending_port = NULL;

    if (!c_p)
	c_p_locks = 0;
    current_c_p_locks = c_p_locks;

 restart:

    reg_safe_read_lock(c_p, &current_c_p_locks);

    /* Locked locks:
     * - port lock on pending_port if pending_port != NULL
     * - read reg lock
     * - current_c_p_locks (either c_p_locks or 0) on c_p
     */
#endif

    hval = REG_HASH(name);
    ix = hval % process_reg.size;
    b = process_reg.bucket[ix];

    /*
     * Note: We have inlined the code from hash.c for speed.
     */

    while (b) {
	if (((RegProc *) b)->name == name) {
	    rp = (RegProc *) b;
	    break;
	}
	b = b->next;
    }

    if (proc) {
	if (!rp)
	    *proc = NULL;
	else {
#ifdef ERTS_SMP
	    if (!rp->p)
		*proc = NULL;
	    else {
		if (need_locks) {
		    erts_proc_safelock(c_p,
				       current_c_p_locks,
				       c_p_locks,
				       rp->p,
				       0,
				       need_locks);
		    current_c_p_locks = c_p_locks;
		}
		if ((flags & ERTS_P2P_FLG_ALLOW_OTHER_X) || is_proc_alive(rp->p))
		    *proc = rp->p;
		else {
		    if (need_locks)
			erts_smp_proc_unlock(rp->p, need_locks);
		    *proc = NULL;
		}
		if (*proc && (flags & ERTS_P2P_FLG_SMP_INC_REFC))
		    erts_smp_proc_inc_refc(rp->p);
	    }
#else
	    if (rp->p
		&& ((flags & ERTS_P2P_FLG_ALLOW_OTHER_X)
		    || rp->p->status != P_EXITING))
		*proc = rp->p;
	    else
		*proc = NULL;
#endif
	}
    }

    if (port) {
	if (!rp || !rp->pt)
	    *port = NULL;
	else {
#ifdef ERTS_SMP
	    if (pending_port == rp->pt)
		pending_port = NULL;
	    else {
		if (pending_port) {
		    /* Ahh! Registered port changed while reg lock
		       was unlocked... */
		    erts_smp_port_unlock(pending_port);
		    pending_port = NULL;
		}
		    
		if (erts_smp_port_trylock(rp->pt) == EBUSY) {
		    Eterm id = rp->pt->id; /* id read only... */
		    /* Unlock all locks, acquire port lock, and restart... */
		    if (current_c_p_locks) {
			erts_smp_proc_unlock(c_p, current_c_p_locks);
			current_c_p_locks = 0;
		    }
		    reg_read_unlock();
		    pending_port = erts_id2port(id, NULL, 0);
		    goto restart;
		}
	    }
#endif
	    *port = rp->pt;
	    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(*port));
	}
    }

#ifdef ERTS_SMP
    if (c_p && !current_c_p_locks)
	erts_smp_proc_lock(c_p, c_p_locks);
    if (pending_port)
	erts_smp_port_unlock(pending_port);
#endif

    reg_read_unlock();
}

Process *
erts_whereis_process(Process *c_p,
		     ErtsProcLocks c_p_locks,
		     Eterm name,
		     ErtsProcLocks need_locks,
		     int flags)
{
    Process *proc;
    erts_whereis_name(c_p, c_p_locks, name, &proc, need_locks, flags, NULL);
    return proc;
}


/*
 * Unregister a name
 * Return 0 if not registered
 * Otherwise returns 1
 *
 */
int erts_unregister_name(Process *c_p,
			 ErtsProcLocks c_p_locks,
			 Port *c_prt,
			 Eterm name)
{
    int res = 0;
    RegProc r, *rp;
    Port *port = c_prt;
#ifdef ERTS_SMP
    ErtsProcLocks current_c_p_locks;

    /*
     * SMP note: If 'c_prt != NULL' and 'c_prt->reg->name == name',
     *           we are *not* allowed to temporarily release the lock
     *           on c_prt.
     */

    if (!c_p) {
	c_p_locks = 0;
    }
    current_c_p_locks = c_p_locks;

 restart:

    reg_safe_write_lock(c_p, &current_c_p_locks);
#endif

    r.name = name;
    if (is_non_value(name)) {
	/* Unregister current process name */
	ASSERT(c_p);
#ifdef ERTS_SMP
	if (current_c_p_locks != c_p_locks) {
	    erts_smp_proc_lock(c_p, c_p_locks);
	    current_c_p_locks = c_p_locks;
	}
#endif
	if (c_p->reg) {
	    r.name = c_p->reg->name;
	} else {
	    /* Name got unregistered while main lock was released */
	    res = 0;
	    goto done;
	}
    }

    if ((rp = (RegProc*) hash_get(&process_reg, (void*) &r)) != NULL) {
	if (rp->pt) {
	    if (port != rp->pt) {
#ifdef ERTS_SMP
		if (port) {
		    ERTS_SMP_LC_ASSERT(port != c_prt);
		    erts_smp_port_unlock(port);
		    port = NULL;
		}

		if (erts_smp_port_trylock(rp->pt) == EBUSY) {
		    Eterm id = rp->pt->id; /* id read only... */
		    /* Unlock all locks, acquire port lock, and restart... */
		    if (current_c_p_locks) {
			erts_smp_proc_unlock(c_p, current_c_p_locks);
			current_c_p_locks = 0;
		    }
		    reg_write_unlock();
		    port = erts_id2port(id, NULL, 0);
		    goto restart;
		}
#endif
		port = rp->pt;
	    }

	    ASSERT(rp->pt == port);
	    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(port));

	    rp->pt->reg = NULL;
	    
	    if (IS_TRACED_FL(port, F_TRACE_PORTS)) {
		trace_port(port, am_unregister, r.name);
	    }

	} else if (rp->p) {

#ifdef ERTS_SMP
	    erts_proc_safelock(c_p,
			       current_c_p_locks,
			       c_p_locks,
			       rp->p,
			       (c_p == rp->p) ?  current_c_p_locks : 0,
			       ERTS_PROC_LOCK_MAIN);
	    current_c_p_locks = c_p_locks;
#endif
	    rp->p->reg = NULL;
	    if (IS_TRACED_FL(rp->p, F_TRACE_PROCS)) {
		trace_proc(c_p, rp->p, am_unregister, r.name);
	    }
#ifdef ERTS_SMP
	    if (rp->p != c_p) {
		erts_smp_proc_unlock(rp->p, ERTS_PROC_LOCK_MAIN);
	    }
#endif
	}
	hash_erase(&process_reg, (void*) &r);
	res = 1;
    }

 done:

    reg_write_unlock();
    if (c_prt != port) {
	if (port) {
	    erts_smp_port_unlock(port);
	}
	if (c_prt) {
	    erts_smp_port_lock(c_prt);
	}
    }
#ifdef ERTS_SMP
    if (c_p && !current_c_p_locks) {
	erts_smp_proc_lock(c_p, c_p_locks);
    }
#endif
    return res;
}

int process_reg_size(void)
{
    int size;
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	reg_read_lock();
    size = process_reg.size;
    if (lock)
	reg_read_unlock();
    return size;
}

int process_reg_sz(void)
{
    int sz;
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	reg_read_lock();
    sz = hash_table_sz(&process_reg);
    if (lock)
	reg_read_unlock();
    return sz;
}

/**********************************************************************/

#include "bif.h"

/* return a list of the registered processes */

BIF_RETTYPE registered_0(BIF_ALIST_0)
{
    int i;
    Eterm res;
    Uint need;
    Eterm* hp;
    HashBucket **bucket;
#ifdef ERTS_SMP
    ErtsProcLocks proc_locks = ERTS_PROC_LOCK_MAIN;

    ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(BIF_P);
    reg_safe_read_lock(BIF_P, &proc_locks);
    if (!proc_locks)
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
#endif

    bucket = process_reg.bucket;

    /* work out how much heap we need & maybe garb, by scanning through
       the registered process table */
    need = 0;
    for (i = 0; i < process_reg.size; i++) {
	HashBucket *b = bucket[i];
	while (b != NULL) {
	    need += 2;
	    b = b->next;
	}
    }

    if (need == 0) {
	reg_read_unlock();
	BIF_RET(NIL);
    }

    hp = HAlloc(BIF_P, need);
     
     /* scan through again and make the list */ 
    res = NIL;

    for (i = 0; i < process_reg.size; i++) {
	HashBucket *b = bucket[i];
	while (b != NULL) {
	    RegProc *reg = (RegProc *) b;

	    res = CONS(hp, reg->name, res);
	    hp += 2;
	    b = b->next;
	}
    }

    reg_read_unlock();

    BIF_RET(res);
}
