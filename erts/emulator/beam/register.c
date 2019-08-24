/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2018. All Rights Reserved.
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

static erts_rwmtx_t regtab_rwmtx;

#define reg_try_read_lock()		erts_rwmtx_tryrlock(&regtab_rwmtx)
#define reg_try_write_lock()		erts_rwmtx_tryrwlock(&regtab_rwmtx)
#define reg_read_lock()			erts_rwmtx_rlock(&regtab_rwmtx)
#define reg_write_lock()		erts_rwmtx_rwlock(&regtab_rwmtx)
#define reg_read_unlock()		erts_rwmtx_runlock(&regtab_rwmtx)
#define reg_write_unlock()		erts_rwmtx_rwunlock(&regtab_rwmtx)

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
	erts_proc_unlock(c_p, *c_p_locks);
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
	erts_proc_unlock(c_p, *c_p_locks);
	*c_p_locks = 0;
    }

    reg_write_lock();
}


static ERTS_INLINE int
is_proc_alive(Process *p)
{
    return !ERTS_PROC_IS_EXITING(p);
}

void register_info(fmtfn_t to, void *to_arg)
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
	erts_exit(ERTS_ERROR_EXIT, "Can't allocate %d bytes of memory\n", sizeof(RegProc));
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
    erts_rwmtx_opt_t rwmtx_opt = ERTS_RWMTX_OPT_DEFAULT_INITER;
    rwmtx_opt.type = ERTS_RWMTX_TYPE_FREQUENT_READ;
    rwmtx_opt.lived = ERTS_RWMTX_LONG_LIVED;

    erts_rwmtx_init_opt(&regtab_rwmtx, &rwmtx_opt, "reg_tab", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);

    f.hash = (H_FUN) reg_hash;
    f.cmp  = (HCMP_FUN) reg_cmp;
    f.alloc = (HALLOC_FUN) reg_alloc;
    f.free = (HFREE_FUN) reg_free;
    f.meta_alloc = (HMALLOC_FUN) erts_alloc;
    f.meta_free = (HMFREE_FUN) erts_free;
    f.meta_print = (HMPRINT_FUN) erts_print;

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
    ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(c_p);

    if (is_not_atom(name) || name == am_undefined)
	return res;

    if (c_p->common.id == id) /* A very common case I think... */
	proc = c_p;
    else {
	if (is_not_internal_pid(id) && is_not_internal_port(id))
	    return res;
	erts_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
	if (is_internal_port(id)) {
	    port = erts_id2port(id);
	    if (!port)
		goto done;
	}
    }

    {
	ErtsProcLocks proc_locks = proc ? ERTS_PROC_LOCK_MAIN : 0;
	reg_safe_write_lock(proc, &proc_locks);

	if (proc && !proc_locks)
	    erts_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
    }

    if (is_internal_pid(id)) {
	if (!proc)
	    proc = erts_pid2proc(NULL, 0, id, ERTS_PROC_LOCK_MAIN);
	r.p = proc;
	if (!proc)
	    goto done;
	if (proc->common.u.alive.reg)
	    goto done;
	r.pt = NULL;
    }
    else {
	ASSERT(!INVALID_PORT(port, id));
	ERTS_LC_ASSERT(erts_lc_is_port_locked(port));
	r.pt = port;
	if (r.pt->common.u.alive.reg)
	    goto done;
	r.p = NULL;
    }

    r.name = name;
    
    rp = (RegProc*) hash_put(&process_reg, (void*) &r);
    if (proc && rp->p == proc) {
	if (IS_TRACED_FL(proc, F_TRACE_PROCS)) {
	    trace_proc(proc, ERTS_PROC_LOCK_MAIN,
                       proc, am_register, name);
	}
	proc->common.u.alive.reg = rp;
    }
    else if (port && rp->pt == port) {
    	if (IS_TRACED_FL(port, F_TRACE_PORTS)) {
		trace_port(port, am_register, name);
	}
	port->common.u.alive.reg = rp;
    }

    if ((rp->p && rp->p->common.id == id)
	|| (rp->pt && rp->pt->common.id == id)) {
	res = 1;
    }

 done:
    reg_write_unlock();
    if (port)
	erts_port_release(port);
    if (c_p != proc) {
	if (proc)
	    erts_proc_unlock(proc, ERTS_PROC_LOCK_MAIN);
	erts_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
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
    ErtsProcLocks c_p_locks = 0;
    if (c_p) {
        c_p_locks = ERTS_PROC_LOCK_MAIN;
        ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(c_p);
    }
    reg_safe_read_lock(c_p, &c_p_locks);

    if (c_p && !c_p_locks)
        erts_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);

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
		res = rp->p->common.id;
	    else if (rp->pt)
		res = rp->pt->common.id;
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
		  Port** port,
                  int lock_port)
{
    RegProc* rp = NULL;
    HashValue hval;
    int ix;
    HashBucket* b;
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
			erts_proc_unlock(rp->p, need_locks);
		    *proc = NULL;
		}
	    }
	    if (*proc && (flags & ERTS_P2P_FLG_INC_REFC))
		erts_proc_inc_refc(*proc);
	}
    }

    if (port) {
	if (!rp || !rp->pt)
	    *port = NULL;
	else {
            if (lock_port) {
                if (pending_port == rp->pt)
                    pending_port = NULL;
                else {
                    if (pending_port) {
                        /* Ahh! Registered port changed while reg lock
                           was unlocked... */
                        erts_port_release(pending_port);
                        pending_port = NULL;
                    }
		    
                    if (erts_port_trylock(rp->pt) == EBUSY) {
                        Eterm id = rp->pt->common.id; /* id read only... */
                        /* Unlock all locks, acquire port lock, and restart... */
                        if (current_c_p_locks) {
                            erts_proc_unlock(c_p, current_c_p_locks);
                            current_c_p_locks = 0;
                        }
                        reg_read_unlock();
                        pending_port = erts_id2port(id);
                        goto restart;
                    }
                }
                ERTS_LC_ASSERT(erts_lc_is_port_locked(rp->pt));
            }
	    *port = rp->pt;
	}
    }

    if (c_p && !current_c_p_locks)
	erts_proc_lock(c_p, c_p_locks);
    if (pending_port)
	erts_port_release(pending_port);

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
    erts_whereis_name(c_p, c_p_locks, name, &proc, need_locks, flags, NULL, 0);
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
    ErtsProcLocks current_c_p_locks = 0;

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

    r.name = name;
    if (is_non_value(name)) {
	/* Unregister current process name */
	ASSERT(c_p);
	if (current_c_p_locks != c_p_locks) {
	    erts_proc_lock(c_p, c_p_locks);
	    current_c_p_locks = c_p_locks;
	}
	if (c_p->common.u.alive.reg) {
	    r.name = c_p->common.u.alive.reg->name;
	} else {
	    /* Name got unregistered while main lock was released */
	    res = 0;
	    goto done;
	}
    }

    if ((rp = (RegProc*) hash_get(&process_reg, (void*) &r)) != NULL) {
	if (rp->pt) {
	    if (port != rp->pt) {
		if (port) {
		    ASSERT(port != c_prt);
		    erts_port_release(port);
		    port = NULL;
		}

		if (erts_port_trylock(rp->pt) == EBUSY) {
		    Eterm id = rp->pt->common.id; /* id read only... */
		    /* Unlock all locks, acquire port lock, and restart... */
		    if (current_c_p_locks) {
			erts_proc_unlock(c_p, current_c_p_locks);
			current_c_p_locks = 0;
		    }
		    reg_write_unlock();
		    port = erts_id2port(id);
		    goto restart;
		}
		port = rp->pt;
	    }

	    ASSERT(rp->pt == port);
	    ERTS_LC_ASSERT(erts_lc_is_port_locked(port));

	    rp->pt->common.u.alive.reg = NULL;

	    if (IS_TRACED_FL(port, F_TRACE_PORTS)) {
                if (current_c_p_locks) {
                    erts_proc_unlock(c_p, current_c_p_locks);
                    current_c_p_locks = 0;
                }
		trace_port(port, am_unregister, r.name);
	    }

	} else if (rp->p) {

	    erts_proc_safelock(c_p,
			       current_c_p_locks,
			       c_p_locks,
			       rp->p,
			       (c_p == rp->p) ?  current_c_p_locks : 0,
			       ERTS_PROC_LOCK_MAIN);
	    current_c_p_locks = c_p_locks;
	    rp->p->common.u.alive.reg = NULL;
	    if (IS_TRACED_FL(rp->p, F_TRACE_PROCS)) {
                trace_proc(rp->p, (c_p == rp->p) ? c_p_locks : ERTS_PROC_LOCK_MAIN,
                           rp->p, am_unregister, r.name);
	    }
	    if (rp->p != c_p) {
		erts_proc_unlock(rp->p, ERTS_PROC_LOCK_MAIN);
	    }
	}
	hash_erase(&process_reg, (void*) &r);
	res = 1;
    }

 done:

    reg_write_unlock();
    if (c_prt != port) {
	if (port) {
	    erts_port_release(port);
	}
	if (c_prt) {
	    erts_port_lock(c_prt);
	}
    }
    if (c_p && !current_c_p_locks) {
	erts_proc_lock(c_p, c_p_locks);
    }
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
    ErtsProcLocks proc_locks = ERTS_PROC_LOCK_MAIN;

    ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(BIF_P);
    reg_safe_read_lock(BIF_P, &proc_locks);
    if (!proc_locks)
	erts_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

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
