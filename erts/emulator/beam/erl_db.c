/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
 * This file contains the 'ets' bif interface functions.
 */

/*
#ifdef DEBUG
#define HARDDEBUG 1
#endif
*/

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#define ERTS_WANT_DB_INTERNAL__
#include "erl_db.h"
#include "bif.h"
#include "big.h"
#include "erl_binary.h"


erts_atomic_t erts_ets_misc_mem_size;

/*
** Utility macros
*/

/* Get a key from any table structure and a tagged object */
#define TERM_GETKEY(tb, obj) db_getkey((tb)->common.keypos, (obj)) 

 
/* How safe are we from double-hits or missed objects
** when iterating without fixation? */ 
enum DbIterSafety {
    ITER_UNSAFE,      /* Must fixate to be safe */
    ITER_SAFE_LOCKED, /* Safe while table is locked, not between trap calls */
    ITER_SAFE         /* No need to fixate at all */
};
#  define ITERATION_SAFETY(Proc,Tab) \
    ((IS_TREE_TABLE((Tab)->common.status) || ONLY_WRITER(Proc,Tab)) ? ITER_SAFE \
     : (((Tab)->common.status & DB_FINE_LOCKED) ? ITER_UNSAFE : ITER_SAFE_LOCKED))

#define DID_TRAP(P,Ret) (!is_value(Ret) && ((P)->freason == TRAP))

/*
 * "fixed_tabs": list of all fixed tables for a process
 */
#ifdef DEBUG
static int fixed_tabs_find(DbFixation* first, DbFixation* fix);
#endif

static void fixed_tabs_insert(Process* p, DbFixation* fix)
{
    DbFixation* first = erts_psd_get(p, ERTS_PSD_ETS_FIXED_TABLES);

    if (!first) {
        fix->tabs.next = fix->tabs.prev = fix;
        erts_psd_set(p, ERTS_PSD_ETS_FIXED_TABLES, fix);
    }
    else {
        ASSERT(!fixed_tabs_find(first, fix));
        fix->tabs.prev = first->tabs.prev;
        fix->tabs.next = first;
        fix->tabs.prev->tabs.next = fix;
        first->tabs.prev = fix;
    }
}

static void fixed_tabs_delete(Process *p, DbFixation* fix)
{
    if (fix->tabs.next == fix) {
        DbFixation* old;
        ASSERT(fix->tabs.prev == fix);
        old = erts_psd_set(p, ERTS_PSD_ETS_FIXED_TABLES, NULL);
        ASSERT(old == fix); (void)old;
    }
    else {
        DbFixation *first = (DbFixation*) erts_psd_get(p, ERTS_PSD_ETS_FIXED_TABLES);

        ASSERT(fixed_tabs_find(first, fix));
        fix->tabs.prev->tabs.next = fix->tabs.next;
        fix->tabs.next->tabs.prev = fix->tabs.prev;

        if (fix == first)
            erts_psd_set(p, ERTS_PSD_ETS_FIXED_TABLES, fix->tabs.next);
    }
}

#ifdef DEBUG
static int fixed_tabs_find(DbFixation* first, DbFixation* fix)
{
    DbFixation* p;

    if (!first) {
        first = (DbFixation*) erts_psd_get(fix->procs.p, ERTS_PSD_ETS_FIXED_TABLES);
    }
    p = first;
    do {
        if (p == fix)
            return 1;
        ASSERT(p->procs.p == fix->procs.p);
        ASSERT(p->tabs.next->tabs.prev == p);
        p = p->tabs.next;
    } while (p != first);
    return 0;
}
#endif


/*
 * fixing_procs: tree of all processes fixating a table
 */
#define ERTS_RBT_PREFIX fixing_procs
#define ERTS_RBT_T DbFixation
#define ERTS_RBT_KEY_T Process*
#define ERTS_RBT_FLAGS_T int
#define ERTS_RBT_INIT_EMPTY_TNODE(T)                    \
    do {						\
	(T)->procs.parent = NULL;			\
	(T)->procs.right = NULL;				\
	(T)->procs.left = NULL;				\
    } while (0)
#define ERTS_RBT_IS_RED(T)        ((T)->procs.is_red)
#define ERTS_RBT_SET_RED(T)       ((T)->procs.is_red = 1)
#define ERTS_RBT_IS_BLACK(T)      (!(T)->procs.is_red)
#define ERTS_RBT_SET_BLACK(T)     ((T)->procs.is_red = 0)
#define ERTS_RBT_GET_FLAGS(T)     ((T)->procs.is_red)
#define ERTS_RBT_SET_FLAGS(T, F)  ((T)->procs.is_red = (F))
#define ERTS_RBT_GET_PARENT(T)    ((T)->procs.parent)
#define ERTS_RBT_SET_PARENT(T, P) ((T)->procs.parent = (P))
#define ERTS_RBT_GET_RIGHT(T)     ((T)->procs.right)
#define ERTS_RBT_SET_RIGHT(T, R)  ((T)->procs.right = (R))
#define ERTS_RBT_GET_LEFT(T)      ((T)->procs.left)
#define ERTS_RBT_SET_LEFT(T, L)   ((T)->procs.left = (L))
#define ERTS_RBT_GET_KEY(T)       ((T)->procs.p)
#define ERTS_RBT_IS_LT(KX, KY)    ((KX) < (KY))
#define ERTS_RBT_IS_EQ(KX, KY)    ((KX) == (KY))

#define ERTS_RBT_WANT_INSERT
#define ERTS_RBT_WANT_LOOKUP
#define ERTS_RBT_WANT_DELETE
#define ERTS_RBT_WANT_FOREACH
#define ERTS_RBT_WANT_FOREACH_DESTROY
#ifdef DEBUG
# define ERTS_RBT_WANT_LOOKUP
#endif
#define ERTS_RBT_UNDEF

#include "erl_rbtree.h"

#ifdef HARDDEBUG
# error Do something useful with CHECK_TABLES maybe
#else
# define CHECK_TABLES()
#endif


static void
send_ets_transfer_message(Process *c_p, Process *proc,
                          ErtsProcLocks *locks,
                          DbTable *tb, Eterm heir_data);
static void schedule_free_dbtable(DbTable* tb);
static void delete_sched_table(Process *c_p, DbTable *tb);

static void table_dec_refc(DbTable *tb, erts_aint_t min_val)
{
    if (erts_refc_dectest(&tb->common.refc, min_val) == 0)
	schedule_free_dbtable(tb);
}

static int
db_table_tid_destructor(Binary *unused)
{
    return 1;
}

static ERTS_INLINE void
make_btid(DbTable *tb)
{
    Binary *btid = erts_create_magic_indirection(db_table_tid_destructor);
    erts_atomic_t *tbref = erts_binary_to_magic_indirection(btid);
    erts_atomic_init_nob(tbref, (erts_aint_t) tb);
    tb->common.btid = btid;
    /*
     * Table and magic indirection refer eachother,
     * and table is refered once by being alive...
     */
    erts_refc_init(&tb->common.refc, 2);
    erts_refc_inc(&btid->intern.refc, 1);
}

static ERTS_INLINE DbTable* btid2tab(Binary* btid)
{
    erts_atomic_t *tbref = erts_binary_to_magic_indirection(btid);
    return (DbTable *) erts_atomic_read_nob(tbref);
}

static DbTable *
tid2tab(Eterm tid)
{
    DbTable *tb;
    Binary *btid;
    erts_atomic_t *tbref;
    if (!is_internal_magic_ref(tid))
        return NULL;

    btid = erts_magic_ref2bin(tid);
    if (ERTS_MAGIC_BIN_DESTRUCTOR(btid) != db_table_tid_destructor)
        return NULL;

    tbref = erts_binary_to_magic_indirection(btid);
    tb = (DbTable *) erts_atomic_read_nob(tbref);

    ASSERT(!tb || tb->common.btid == btid);

    return tb;
}

static ERTS_INLINE int
is_table_alive(DbTable *tb)
{
    erts_atomic_t *tbref;
    DbTable *rtb;

    tbref = erts_binary_to_magic_indirection(tb->common.btid);
    rtb = (DbTable *) erts_atomic_read_nob(tbref);

    ASSERT(!rtb || rtb == tb);

    return !!rtb;
}

static ERTS_INLINE int
is_table_named(DbTable *tb)
{
    return tb->common.type & DB_NAMED_TABLE;
}


static ERTS_INLINE void
tid_clear(Process *c_p, DbTable *tb)
{
    DbTable *rtb;
    Binary *btid = tb->common.btid;
    erts_atomic_t *tbref = erts_binary_to_magic_indirection(btid);
    rtb = (DbTable *) erts_atomic_xchg_nob(tbref, (erts_aint_t) NULL);
    ASSERT(!rtb || tb == rtb);
    if (rtb) {
        table_dec_refc(tb, 1);
        delete_sched_table(c_p, tb);
    }
}

static ERTS_INLINE Eterm
make_tid(Process *c_p, DbTable *tb)
{
    Eterm *hp = HAlloc(c_p, ERTS_MAGIC_REF_THING_SIZE);
    return erts_mk_magic_ref(&hp, &c_p->off_heap, tb->common.btid);
}


/* 
** The meta hash table of all NAMED ets tables
*/
#  define META_NAME_TAB_LOCK_CNT 16
union {
    erts_rwmtx_t lck;
    byte _cache_line_alignment[64];
}meta_name_tab_rwlocks[META_NAME_TAB_LOCK_CNT];
static struct meta_name_tab_entry {
    union {
	Eterm name_atom;
	Eterm mcnt; /* Length of mvec in multiple tab entry */
    }u;
    union {
	DbTable *tb;
	struct meta_name_tab_entry* mvec;
    }pu;
} *meta_name_tab;

static unsigned meta_name_tab_mask;

static ERTS_INLINE
struct meta_name_tab_entry* meta_name_tab_bucket(Eterm name, 
						 erts_rwmtx_t** lockp)
{
    unsigned bix = atom_val(name) & meta_name_tab_mask;
    struct meta_name_tab_entry* bucket = &meta_name_tab[bix];
    *lockp = &meta_name_tab_rwlocks[bix % META_NAME_TAB_LOCK_CNT].lck;
    return bucket;
}    


typedef enum {
    LCK_READ=1,     /* read only access */
    LCK_WRITE=2,    /* exclusive table write access */
    LCK_WRITE_REC=3, /* record write access */
    LCK_NONE=4
} db_lock_kind_t;

extern DbTableMethod db_hash;
extern DbTableMethod db_tree;

int user_requested_db_max_tabs;
int erts_ets_realloc_always_moves;
int erts_ets_always_compress;
static int db_max_tabs;
static Eterm ms_delete_all;
static Eterm ms_delete_all_buff[8]; /* To compare with for deletion 
				       of all objects */

/* 
** Forward decls, static functions 
*/

static void fix_table_locked(Process* p, DbTable* tb);
static void unfix_table_locked(Process* p,  DbTable* tb, db_lock_kind_t* kind);
static void set_heir(Process* me, DbTable* tb, Eterm heir, UWord heir_data);
static void free_heir_data(DbTable*);
static SWord free_fixations_locked(Process* p, DbTable *tb);

static SWord free_table_continue(Process *p, DbTable *tb, SWord reds);
static void print_table(fmtfn_t to, void *to_arg, int show,  DbTable* tb);
static BIF_RETTYPE ets_select_delete_1(BIF_ALIST_1);
static BIF_RETTYPE ets_select_count_1(BIF_ALIST_1);
static BIF_RETTYPE ets_select_replace_1(BIF_ALIST_1);
static BIF_RETTYPE ets_select_trap_1(BIF_ALIST_1);
static BIF_RETTYPE ets_delete_trap(BIF_ALIST_1);
static Eterm table_info(Process* p, DbTable* tb, Eterm What);

static BIF_RETTYPE ets_select1(Process* p, Eterm arg1);
static BIF_RETTYPE ets_select2(Process* p, Eterm arg1, Eterm arg2);
static BIF_RETTYPE ets_select3(Process* p, Eterm arg1, Eterm arg2, Eterm arg3);


/* 
 * Exported global
 */
Export ets_select_delete_continue_exp;
Export ets_select_count_continue_exp;
Export ets_select_replace_continue_exp;
Export ets_select_continue_exp;

/*
 * Static traps
 */
static Export ets_delete_continue_exp;
	
static void
free_dbtable(void *vtb)
{
    DbTable *tb = (DbTable *) vtb;
#ifdef HARDDEBUG
	if (erts_atomic_read_nob(&tb->common.memory_size) != sizeof(DbTable)) {
	    erts_fprintf(stderr, "ets: free_dbtable memory remain=%ld fix=%x\n",
			 erts_atomic_read_nob(&tb->common.memory_size)-sizeof(DbTable),
			 tb->common.fixations);
	}
#endif
	erts_rwmtx_destroy(&tb->common.rwlock);
	erts_mtx_destroy(&tb->common.fixlock);
	ASSERT(is_immed(tb->common.heir_data));

        if (tb->common.btid)
            erts_bin_release(tb->common.btid);

	erts_db_free(ERTS_ALC_T_DB_TABLE, tb, (void *) tb, sizeof(DbTable));
}

static void schedule_free_dbtable(DbTable* tb)
{
    /*
     * NON-SMP case: Caller is *not* allowed to access the *tb
     *               structure after this function has returned!          
     * SMP case:     Caller is allowed to access the *common* part of the *tb
     *  	     structure until the bif has returned (we typically need to
     *  	     unlock the table lock after this function has returned).
     *  	     Caller is *not* allowed to access the specialized part
     *  	     (hash or tree) of *tb after this function has returned.
     */
    ASSERT(erts_refc_read(&tb->common.refc, 0) == 0);
    ASSERT(erts_refc_read(&tb->common.fix_count, 0) == 0);
    erts_schedule_thr_prgr_later_cleanup_op(free_dbtable,
					    (void *) tb,
					    &tb->release.data,
					    sizeof(DbTable));
}

static ERTS_INLINE void
save_sched_table(Process *c_p, DbTable *tb)
{
    ErtsSchedulerData *esdp = erts_proc_sched_data(c_p);
    DbTable *first;

    ASSERT(esdp);
    esdp->ets_tables.count++;
    erts_refc_inc(&tb->common.refc, 1);

    first = esdp->ets_tables.clist;
    if (!first) {
        tb->common.all.next = tb->common.all.prev = tb;
        esdp->ets_tables.clist = tb;
    }
    else {
        tb->common.all.prev = first->common.all.prev;
        tb->common.all.next = first;
        tb->common.all.prev->common.all.next = tb;
        first->common.all.prev = tb;
    }
}

static ERTS_INLINE void
remove_sched_table(ErtsSchedulerData *esdp, DbTable *tb)
{
    ErtsEtsAllYieldData *eaydp;
    ASSERT(esdp);
    ASSERT(erts_get_ref_numbers_thr_id(ERTS_MAGIC_BIN_REFN(tb->common.btid))
           == (Uint32) esdp->no);

    ASSERT(esdp->ets_tables.count > 0);
    esdp->ets_tables.count--;

    eaydp = ERTS_SCHED_AUX_YIELD_DATA(esdp, ets_all);
    if (eaydp->ongoing) {
        /* ets:all() op process list from last to first... */
        if (eaydp->tab == tb) {
            if (eaydp->tab == esdp->ets_tables.clist)
                eaydp->tab = NULL;
            else
                eaydp->tab = tb->common.all.prev;
        }
    }

    if (tb->common.all.next == tb) {
        ASSERT(tb->common.all.prev == tb);
        ASSERT(esdp->ets_tables.clist == tb);
        esdp->ets_tables.clist = NULL;
    }
    else {
#ifdef DEBUG
        DbTable *tmp = esdp->ets_tables.clist;
        do {
            if (tmp == tb) break;
            tmp = tmp->common.all.next;
        } while (tmp != esdp->ets_tables.clist);
        ASSERT(tmp == tb);
#endif
        tb->common.all.prev->common.all.next = tb->common.all.next;
        tb->common.all.next->common.all.prev = tb->common.all.prev;

        if (esdp->ets_tables.clist == tb)
            esdp->ets_tables.clist = tb->common.all.next;

    }

    table_dec_refc(tb, 0);
}

static void
scheduled_remove_sched_table(void *vtb)
{
    remove_sched_table(erts_get_scheduler_data(), (DbTable *) vtb);
}

static void
delete_sched_table(Process *c_p, DbTable *tb)
{
    ErtsSchedulerData *esdp = erts_proc_sched_data(c_p);
    Uint32 sid;

    ASSERT(esdp);

    ASSERT(tb->common.btid);
    sid = erts_get_ref_numbers_thr_id(ERTS_MAGIC_BIN_REFN(tb->common.btid));
    ASSERT(1 <= sid && sid <= erts_no_schedulers);
    if (sid == (Uint32) esdp->no)
        remove_sched_table(esdp, tb);
    else
        erts_schedule_misc_aux_work((int) sid, scheduled_remove_sched_table, tb);
}

static ERTS_INLINE void
save_owned_table(Process *c_p, DbTable *tb)
{
    DbTable *first;

    erts_proc_lock(c_p, ERTS_PROC_LOCK_STATUS);

    first = (DbTable*) erts_psd_get(c_p, ERTS_PSD_ETS_OWNED_TABLES);

    erts_refc_inc(&tb->common.refc, 1);

    if (!first) {
        tb->common.owned.next = tb->common.owned.prev = tb;
        erts_psd_set(c_p, ERTS_PSD_ETS_OWNED_TABLES, tb);
    }
    else {
        tb->common.owned.prev = first->common.owned.prev;
        tb->common.owned.next = first;
        tb->common.owned.prev->common.owned.next = tb;
        first->common.owned.prev = tb;
    }
    erts_proc_unlock(c_p, ERTS_PROC_LOCK_STATUS);
}

static ERTS_INLINE void
delete_owned_table(Process *p, DbTable *tb)
{
    erts_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    if (tb->common.owned.next == tb) {
        DbTable* old;
        ASSERT(tb->common.owned.prev == tb);
        old = erts_psd_set(p, ERTS_PSD_ETS_OWNED_TABLES, NULL);
        ASSERT(old == tb); (void)old;
    }
    else {
        DbTable *first = (DbTable*) erts_psd_get(p, ERTS_PSD_ETS_OWNED_TABLES);
#ifdef DEBUG
        DbTable *tmp = first;
        do {
            if (tmp == tb) break;
            tmp = tmp->common.owned.next;
        } while (tmp != first);
        ASSERT(tmp == tb);
#endif
        tb->common.owned.prev->common.owned.next = tb->common.owned.next;
        tb->common.owned.next->common.owned.prev = tb->common.owned.prev;

        if (tb == first)
            erts_psd_set(p, ERTS_PSD_ETS_OWNED_TABLES, tb->common.owned.next);
    }
    erts_proc_unlock(p, ERTS_PROC_LOCK_STATUS);

    table_dec_refc(tb, 1);
}

static ERTS_INLINE void db_init_lock(DbTable* tb, int use_frequent_read_lock)
{
    erts_rwmtx_opt_t rwmtx_opt = ERTS_RWMTX_OPT_DEFAULT_INITER;
    if (use_frequent_read_lock)
	rwmtx_opt.type = ERTS_RWMTX_TYPE_FREQUENT_READ;
    if (erts_ets_rwmtx_spin_count >= 0)
	rwmtx_opt.main_spincount = erts_ets_rwmtx_spin_count;
    erts_rwmtx_init_opt(&tb->common.rwlock, &rwmtx_opt, "db_tab",
        tb->common.the_name, ERTS_LOCK_FLAGS_CATEGORY_DB);
    erts_mtx_init(&tb->common.fixlock, "db_tab_fix",
        tb->common.the_name, ERTS_LOCK_FLAGS_CATEGORY_DB);
    tb->common.is_thread_safe = !(tb->common.status & DB_FINE_LOCKED);
}

static ERTS_INLINE void db_lock(DbTable* tb, db_lock_kind_t kind)
{
    if (tb->common.type & DB_FINE_LOCKED) {
	if (kind == LCK_WRITE) {	   
	    erts_rwmtx_rwlock(&tb->common.rwlock);
	    tb->common.is_thread_safe = 1;
	} else {	
	    erts_rwmtx_rlock(&tb->common.rwlock);
	    ASSERT(!tb->common.is_thread_safe);
	}
    }
    else
    { 
	switch (kind) {
	case LCK_WRITE:
	case LCK_WRITE_REC:
	    erts_rwmtx_rwlock(&tb->common.rwlock);
	    break;
	default:
	    erts_rwmtx_rlock(&tb->common.rwlock);
	}
	ASSERT(tb->common.is_thread_safe);
    }
}

static ERTS_INLINE void db_unlock(DbTable* tb, db_lock_kind_t kind)
{
    /*
     * In NON-SMP case tb may refer to an already deallocated
     * DbTable structure. That is, ONLY the SMP case is allowed
     * to follow the tb pointer!
     */
    if (tb->common.type & DB_FINE_LOCKED) {
	if (kind == LCK_WRITE) {
	    ASSERT(tb->common.is_thread_safe);
	    tb->common.is_thread_safe = 0;
	    erts_rwmtx_rwunlock(&tb->common.rwlock);
	}
	else {
	    ASSERT(!tb->common.is_thread_safe);
	    erts_rwmtx_runlock(&tb->common.rwlock);
	}
    }
    else {
	ASSERT(tb->common.is_thread_safe);
	switch (kind) {
	case LCK_WRITE:
	case LCK_WRITE_REC:
	    erts_rwmtx_rwunlock(&tb->common.rwlock);
	    break;
	default:
	    erts_rwmtx_runlock(&tb->common.rwlock);
	}
    }
}

static ERTS_INLINE
DbTable* db_get_table_aux(Process *p,
			  Eterm id,
			  int what,
			  db_lock_kind_t kind,
			  int meta_already_locked)
{
    DbTable *tb;
    erts_rwmtx_t *mtl = NULL;

    /*
     * IMPORTANT: Only scheduler threads are allowed
     *            to access tables. Memory management
     *            depend on it.
     */
    ASSERT(erts_get_scheduler_data());

    if (is_atom(id)) {
	struct meta_name_tab_entry* bucket = meta_name_tab_bucket(id,&mtl);
	if (!meta_already_locked)
	    erts_rwmtx_rlock(mtl);
	else{
	    ERTS_LC_ASSERT(erts_lc_rwmtx_is_rlocked(mtl)
			       || erts_lc_rwmtx_is_rwlocked(mtl));
	    mtl = NULL;
	}
        tb = NULL;
	if (bucket->pu.tb != NULL) {
	    if (is_atom(bucket->u.name_atom)) { /* single */
		if (bucket->u.name_atom == id)
		    tb = bucket->pu.tb;
	    }
	    else { /* multi */
		Uint cnt = unsigned_val(bucket->u.mcnt);
		Uint i;
		for (i=0; i<cnt; i++) {
		    if (bucket->pu.mvec[i].u.name_atom == id) {
			tb = bucket->pu.mvec[i].pu.tb;
			break;
		    }
		}
	    }
	}
    }
    else
        tb = tid2tab(id);

    if (tb) {
	db_lock(tb, kind);
	if ((tb->common.status & what) == 0
            && p->common.id != tb->common.owner) {
	    db_unlock(tb, kind);
	    tb = NULL;
	}
    }
    if (mtl)
	erts_rwmtx_runlock(mtl);
    return tb;
}

static ERTS_INLINE
DbTable* db_get_table(Process *p,
		      Eterm id,
		      int what,
		      db_lock_kind_t kind)
{
    return db_get_table_aux(p, id, what, kind, 0);
}

static int insert_named_tab(Eterm name_atom, DbTable* tb, int have_lock)
{
    int ret = 0;
    erts_rwmtx_t* rwlock;
    struct meta_name_tab_entry* new_entry;
    struct meta_name_tab_entry* bucket = meta_name_tab_bucket(name_atom,
							      &rwlock);
    if (!have_lock)
	erts_rwmtx_rwlock(rwlock);

    if (bucket->pu.tb == NULL) { /* empty */
	new_entry = bucket;
    }
    else {
	struct meta_name_tab_entry* entries;
	Uint cnt;
	if (is_atom(bucket->u.name_atom)) { /* single */
	    size_t size;
	    if (bucket->u.name_atom == name_atom) {
		goto done;
	    }
	    cnt = 2;
	    size = sizeof(struct meta_name_tab_entry)*cnt;
	    entries = erts_db_alloc_nt(ERTS_ALC_T_DB_NTAB_ENT, size);
	    ERTS_ETS_MISC_MEM_ADD(size);
	    new_entry = &entries[0];
	    entries[1] = *bucket;
	}
	else { /* multi */
	    size_t size, old_size;
	    Uint i;
	    cnt = unsigned_val(bucket->u.mcnt);
	    for (i=0; i<cnt; i++) {
		if (bucket->pu.mvec[i].u.name_atom == name_atom) {
		    goto done;
		}
	    }
	    old_size = sizeof(struct meta_name_tab_entry)*cnt;
	    size = sizeof(struct meta_name_tab_entry)*(cnt+1);
	    entries = erts_db_realloc_nt(ERTS_ALC_T_DB_NTAB_ENT,
					 bucket->pu.mvec,
					 old_size,
					 size);
	    ERTS_ETS_MISC_MEM_ADD(size-old_size);
	    new_entry = &entries[cnt];
	    cnt++;
	}
	bucket->pu.mvec = entries;
	bucket->u.mcnt = make_small(cnt);
    }
    new_entry->pu.tb = tb;
    new_entry->u.name_atom = name_atom;
    ret = 1; /* Ok */

done:
    if (!have_lock)
	erts_rwmtx_rwunlock(rwlock);
    return ret;
}

static int remove_named_tab(DbTable *tb, int have_lock)
{
    int ret = 0;
    erts_rwmtx_t* rwlock;
    Eterm name_atom = tb->common.the_name;
    struct meta_name_tab_entry* bucket = meta_name_tab_bucket(name_atom,
							      &rwlock);
    ASSERT(is_table_named(tb));
    if (!have_lock && erts_rwmtx_tryrwlock(rwlock) == EBUSY) {
	db_unlock(tb, LCK_WRITE);
	erts_rwmtx_rwlock(rwlock);
	db_lock(tb, LCK_WRITE);
    }

    ERTS_LC_ASSERT(erts_lc_rwmtx_is_rwlocked(rwlock));

    if (bucket->pu.tb == NULL) {
	goto done;
    }
    else if (is_atom(bucket->u.name_atom)) { /* single */
	if (bucket->u.name_atom != name_atom) {
	    goto done;
	}
	bucket->pu.tb = NULL;
    }
    else { /* multi */
	Uint cnt = unsigned_val(bucket->u.mcnt);
	Uint i = 0;
	for (;;) {
	    if (bucket->pu.mvec[i].u.name_atom == name_atom) {
		break;
	    }
	    if (++i >= cnt) {
		goto done;
	    }
	}
	if (cnt == 2) { /* multi -> single */
	    size_t size;
	    struct meta_name_tab_entry* entries = bucket->pu.mvec;
	    *bucket = entries[1-i];
	    size = sizeof(struct meta_name_tab_entry)*cnt;
	    erts_db_free_nt(ERTS_ALC_T_DB_NTAB_ENT, entries, size);
	    ERTS_ETS_MISC_MEM_ADD(-size);
	    ASSERT(is_atom(bucket->u.name_atom));
	}
	else {
	    size_t size, old_size;
	    ASSERT(cnt > 2);
	    bucket->u.mcnt = make_small(--cnt);
	    if (i != cnt) {
		/* reposition last one before realloc destroys it */
		bucket->pu.mvec[i] = bucket->pu.mvec[cnt];
	    }
	    old_size = sizeof(struct meta_name_tab_entry)*(cnt+1);
	    size = sizeof(struct meta_name_tab_entry)*cnt;
	    bucket->pu.mvec = erts_db_realloc_nt(ERTS_ALC_T_DB_NTAB_ENT,
						 bucket->pu.mvec,
						 old_size,
						 size);
	    ERTS_ETS_MISC_MEM_ADD(size - old_size);
    
	}
    }
    ret = 1; /* Ok */

done:
    if (!have_lock)
	erts_rwmtx_rwunlock(rwlock);
    return ret;
}

/* Do a fast fixation of a hash table.
** Must be matched by a local unfix before releasing table lock.
*/
static ERTS_INLINE void local_fix_table(DbTable* tb)
{
    erts_refc_inc(&tb->common.fix_count, 1);
}	    
static ERTS_INLINE void local_unfix_table(DbTable* tb)
{	
    if (erts_refc_dectest(&tb->common.fix_count, 0) == 0) {
	ASSERT(IS_HASH_TABLE(tb->common.status));
	db_unfix_table_hash(&(tb->hash));
    }
}


/*
 * BIFs.
 */

BIF_RETTYPE ets_safe_fixtable_2(BIF_ALIST_2)
{
    DbTable *tb;
    db_lock_kind_t kind;
#ifdef HARDDEBUG
    erts_fprintf(stderr,
		"ets:safe_fixtable(%T,%T); Process: %T, initial: %T:%T/%bpu\n",
		BIF_ARG_1, BIF_ARG_2, BIF_P->common.id,
		BIF_P->u.initial[0], BIF_P->u.initial[1], BIF_P->u.initial[2]);
#endif
    kind = (BIF_ARG_2 == am_true) ? LCK_READ : LCK_WRITE_REC; 

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, kind)) == NULL) {
	    BIF_ERROR(BIF_P, BADARG);
	}

    if (BIF_ARG_2 == am_true) {
	fix_table_locked(BIF_P, tb);
    }
    else if (BIF_ARG_2 == am_false) {
	if (IS_FIXED(tb)) {
	    unfix_table_locked(BIF_P, tb, &kind);
	}
    }
    else {
	db_unlock(tb, kind);
	BIF_ERROR(BIF_P, BADARG);
    }
    db_unlock(tb, kind);
    BIF_RET(am_true);
}


/* 
** Returns the first Key in a table 
*/
BIF_RETTYPE ets_first_1(BIF_ALIST_1)
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();

    tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_READ);

    if (!tb) {
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_first(BIF_P, tb, &ret);

    db_unlock(tb, LCK_READ);

    if (cret != DB_ERROR_NONE) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(ret);
}

/* 
** The next BIF, given a key, return the "next" key 
*/
BIF_RETTYPE ets_next_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();

    tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_READ);

    if (!tb) {
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_next(BIF_P, tb, BIF_ARG_2, &ret);

    db_unlock(tb, LCK_READ);

    if (cret != DB_ERROR_NONE) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(ret);
}

/* 
** Returns the last Key in a table 
*/
BIF_RETTYPE ets_last_1(BIF_ALIST_1)
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();

    tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_READ);

    if (!tb) {
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_last(BIF_P, tb, &ret);

    db_unlock(tb, LCK_READ);

    if (cret != DB_ERROR_NONE) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(ret);
}

/* 
** The prev BIF, given a key, return the "previous" key 
*/
BIF_RETTYPE ets_prev_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();

    tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_READ);

    if (!tb) {
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_prev(BIF_P,tb,BIF_ARG_2,&ret);

    db_unlock(tb, LCK_READ);

    if (cret != DB_ERROR_NONE) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(ret);
}

/*
** take(Tab, Key)
*/
BIF_RETTYPE ets_take_2(BIF_ALIST_2)
{
    DbTable* tb;
#ifdef DEBUG
    int cret;
#endif
    Eterm ret;
    CHECK_TABLES();

    tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE_REC);
    if (!tb) {
        BIF_ERROR(BIF_P, BADARG);
    }
#ifdef DEBUG
    cret =
#endif
        tb->common.meth->db_take(BIF_P, tb, BIF_ARG_2, &ret);
    ASSERT(cret == DB_ERROR_NONE);
    db_unlock(tb, LCK_WRITE_REC);
    BIF_RET(ret);
}

/* 
** update_element(Tab, Key, {Pos, Value})
** update_element(Tab, Key, [{Pos, Value}])
*/
BIF_RETTYPE ets_update_element_3(BIF_ALIST_3)
{
    DbTable* tb;
    int cret = DB_ERROR_BADITEM;
    Eterm list;
    Eterm iter;
    DeclareTmpHeap(cell,2,BIF_P);
    DbUpdateHandle handle;

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE_REC)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    UseTmpHeap(2,BIF_P);
    if (!(tb->common.status & (DB_SET | DB_ORDERED_SET))) {
	goto bail_out;
    }
    if (is_tuple(BIF_ARG_3)) {
	list = CONS(cell, BIF_ARG_3, NIL);
    }
    else {
	list = BIF_ARG_3;
    }

    if (!tb->common.meth->db_lookup_dbterm(BIF_P, tb, BIF_ARG_2, THE_NON_VALUE, &handle)) {
	cret = DB_ERROR_BADKEY;
	goto bail_out;
    }

    /* First verify that list is ok to avoid nasty rollback scenarios
    */
    for (iter=list ; is_not_nil(iter); iter = CDR(list_val(iter))) {
	Eterm pv;
	Eterm* pvp;
	Sint position;

	if (is_not_list(iter)) {
	    goto finalize;
	}
	pv = CAR(list_val(iter));    /* {Pos,Value} */
	if (is_not_tuple(pv)) {
	    goto finalize;
	}
	pvp = tuple_val(pv);
	if (arityval(*pvp) != 2 || !is_small(pvp[1])) {
	    goto finalize;
	}
	position = signed_val(pvp[1]);
	if (position < 1 || position == tb->common.keypos || 
	    position > arityval(handle.dbterm->tpl[0])) {
	    goto finalize;
	}	
    }
    /* The point of no return, no failures from here on.
    */
    cret = DB_ERROR_NONE;

    for (iter=list ; is_not_nil(iter); iter = CDR(list_val(iter))) {
	Eterm* pvp = tuple_val(CAR(list_val(iter)));    /* {Pos,Value} */
	db_do_update_element(&handle, signed_val(pvp[1]), pvp[2]);
    }

finalize:
    tb->common.meth->db_finalize_dbterm(cret, &handle);

bail_out:
    UnUseTmpHeap(2,BIF_P);
    db_unlock(tb, LCK_WRITE_REC);

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(am_true);
    case DB_ERROR_BADKEY:
	BIF_RET(am_false);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
	break;
    }
}

static BIF_RETTYPE
do_update_counter(Process *p, Eterm arg1, Eterm arg2, Eterm arg3, Eterm arg4)
{
    DbTable* tb;
    int cret = DB_ERROR_BADITEM;
    Eterm upop_list;
    int list_size;
    Eterm ret;  /* int or [int] */
    Eterm* ret_list_currp = NULL;
    Eterm* ret_list_prevp = NULL;
    Eterm iter;
    DeclareTmpHeap(cell, 5, p);
    Eterm *tuple = cell+2;
    DbUpdateHandle handle;
    Uint halloc_size = 0; /* overestimated heap usage */
    Eterm* htop;          /* actual heap usage */
    Eterm* hstart;
    Eterm* hend;

    if ((tb = db_get_table(p, arg1, DB_WRITE, LCK_WRITE_REC)) == NULL) {
        BIF_ERROR(p, BADARG);
    }

    UseTmpHeap(5, p);

    if (!(tb->common.status & (DB_SET | DB_ORDERED_SET))) {
	goto bail_out;
    }
    if (is_integer(arg3)) { /* Incr */
        upop_list = CONS(cell,
                         TUPLE2(tuple, make_small(tb->common.keypos+1), arg3),
                         NIL);
    }
    else if (is_tuple(arg3)) { /* {Upop} */
        upop_list = CONS(cell, arg3, NIL);
    }
    else { /* [{Upop}] (probably) */
        upop_list = arg3;
	ret_list_prevp = &ret;
    }

    if (!tb->common.meth->db_lookup_dbterm(p, tb, arg2, arg4, &handle)) {
	goto bail_out; /* key not found */
    }

    /* First verify that list is ok to avoid nasty rollback scenarios
    */
    list_size = 0;
    for (iter=upop_list ; is_not_nil(iter); iter = CDR(list_val(iter)),
	                                    list_size += 2) {
	Eterm upop;
	Eterm* tpl;
	Sint position;
	Eterm incr, warp;
	Wterm oldcnt;

	if (is_not_list(iter)) {
	    goto finalize;
	}
	upop = CAR(list_val(iter));
	if (is_not_tuple(upop)) {
	    goto finalize;
	}
	tpl = tuple_val(upop);
	switch (arityval(*tpl)) {
	case 4: /* threshold specified */
	    if (is_not_integer(tpl[3])) {
		goto finalize;
	    }
	    warp = tpl[4];
	    if (is_big(warp)) {
		halloc_size += BIG_NEED_SIZE(big_arity(warp));
	    }
	    else if (is_not_small(warp)) {
		goto finalize;
	    }
	    /* Fall through */
	case 2:
	    if (!is_small(tpl[1])) {
		goto finalize;
	    }
	    incr = tpl[2];
	    if (is_big(incr)) {
		halloc_size += BIG_NEED_SIZE(big_arity(incr));
	    }
	    else if (is_not_small(incr)) {
		goto finalize;
	    }
	    position = signed_val(tpl[1]);
	    if (position < 1 || position == tb->common.keypos ||
		position > arityval(handle.dbterm->tpl[0])) {
		goto finalize;
	    }
	    oldcnt = db_do_read_element(&handle, position);
	    if (is_big(oldcnt)) {
		halloc_size += BIG_NEED_SIZE(big_arity(oldcnt));
	    }
	    else if (is_not_small(oldcnt)) {
		goto finalize;
	    }
	    break;
	default:
	    goto finalize;
	}
	halloc_size += 2;  /* worst growth case: small(0)+small(0)=big(2) */
    }

    /* The point of no return, no failures from here on.
    */
    cret = DB_ERROR_NONE;

    if (ret_list_prevp) { /* Prepare to return a list */
	ret = NIL;
	halloc_size += list_size;
	hstart = HAlloc(p, halloc_size);
	ret_list_currp = hstart;
	htop = hstart + list_size;
	hend = hstart + halloc_size;
    }
    else {
	hstart = htop = HAlloc(p, halloc_size);
    }
    hend = hstart + halloc_size;

    for (iter=upop_list ; is_not_nil(iter); iter = CDR(list_val(iter))) {

	Eterm* tpl = tuple_val(CAR(list_val(iter)));
	Sint position = signed_val(tpl[1]);
	Eterm incr = tpl[2];
	Wterm oldcnt = db_do_read_element(&handle,position);
	Eterm newcnt = db_add_counter(&htop, oldcnt, incr);

	if (newcnt == NIL) {
	    cret = DB_ERROR_SYSRES; /* Can only happen if BIG_ARITY_MAX */
	    ret = NIL;              /* is reached, ie should not happen */
	    htop = hstart;
	    break;
	}
	ASSERT(is_integer(newcnt));

	if (arityval(*tpl) == 4) { /* Maybe warp it */
	    Eterm threshold = tpl[3];
	    if ((CMP(incr,make_small(0)) < 0) ? /* negative increment? */
		(CMP(newcnt,threshold) < 0) :  /* if negative, check if below */
		(CMP(newcnt,threshold) > 0)) { /* else check if above threshold */

		newcnt = tpl[4];
	    }
	}

	db_do_update_element(&handle,position,newcnt);

	if (ret_list_prevp) {
	    *ret_list_prevp = CONS(ret_list_currp,newcnt,NIL);
	    ret_list_prevp = &CDR(ret_list_currp);
	    ret_list_currp += 2;
	}
	else {
	    ret = newcnt;
	    break;	    
	}
    }

    ASSERT(is_integer(ret) || is_nil(ret) || 
	   (is_list(ret) && (list_val(ret)+list_size)==ret_list_currp));
    ASSERT(htop <= hend);

    HRelease(p, hend, htop);

finalize:
    tb->common.meth->db_finalize_dbterm(cret, &handle);

bail_out:
    UnUseTmpHeap(5, p);
    db_unlock(tb, LCK_WRITE_REC);

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
        BIF_ERROR(p, SYSTEM_LIMIT);
    default:
        BIF_ERROR(p, BADARG);
	break;
    }
}

/*
** update_counter(Tab, Key, Incr)
** update_counter(Tab, Key, Upop)
** update_counter(Tab, Key, [{Upop}])
** Upop = {Pos,Incr} | {Pos,Incr,Threshold,WarpTo}
** Returns new value(s) (integer or [integer])
*/
BIF_RETTYPE ets_update_counter_3(BIF_ALIST_3)
{
    return do_update_counter(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, THE_NON_VALUE);
}

/*
** update_counter(Tab, Key, Incr, Default)
** update_counter(Tab, Key, Upop, Default)
** update_counter(Tab, Key, [{Upop}], Default)
** Upop = {Pos,Incr} | {Pos,Incr,Threshold,WarpTo}
** Returns new value(s) (integer or [integer])
*/
BIF_RETTYPE ets_update_counter_4(BIF_ALIST_4)
{
    if (is_not_tuple(BIF_ARG_4)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    return do_update_counter(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, BIF_ARG_4);
}


/* 
** The put BIF 
*/
BIF_RETTYPE ets_insert_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret = DB_ERROR_NONE;
    Eterm lst;
    DbTableMethod* meth;
    db_lock_kind_t kind;

    CHECK_TABLES();

    /* Write lock table if more than one object to keep atomicity */
    kind = ((is_list(BIF_ARG_2) && CDR(list_val(BIF_ARG_2)) != NIL)
	    ? LCK_WRITE : LCK_WRITE_REC);

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, kind)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (BIF_ARG_2 == NIL) {
	db_unlock(tb, kind);
	BIF_RET(am_true);
    }
    meth = tb->common.meth;
    if (is_list(BIF_ARG_2)) {
	for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
	    if (is_not_tuple(CAR(list_val(lst))) || 
		(arityval(*tuple_val(CAR(list_val(lst)))) < tb->common.keypos)) {
		goto badarg;
	    }
	}
	if (lst != NIL) {
	    goto badarg;
	}
	for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
	    cret = meth->db_put(tb, CAR(list_val(lst)), 0);
	    if (cret != DB_ERROR_NONE)
		break;
	}
    } else {
	if (is_not_tuple(BIF_ARG_2) || 
	    (arityval(*tuple_val(BIF_ARG_2)) < tb->common.keypos)) {
	    goto badarg;
	}
	cret = meth->db_put(tb, BIF_ARG_2, 0);
    }

    db_unlock(tb, kind);
    
    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(am_true);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
 badarg:
    db_unlock(tb, kind);
    BIF_ERROR(BIF_P, BADARG);    
}


/* 
** The put-if-not-already-there BIF... 
*/
BIF_RETTYPE ets_insert_new_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret = DB_ERROR_NONE;
    Eterm ret = am_true;
    Eterm obj;
    db_lock_kind_t kind;

    CHECK_TABLES();

    if (is_list(BIF_ARG_2)) {
	if (CDR(list_val(BIF_ARG_2)) != NIL) {
	    Eterm lst;
	    Eterm lookup_ret;
	    DbTableMethod* meth;

	    /* More than one object, use LCK_WRITE to keep atomicity */
	    kind = LCK_WRITE;
	    tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, kind);
	    if (tb == NULL) {
		BIF_ERROR(BIF_P, BADARG);
	    }
	    meth = tb->common.meth;
	    for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
		if (is_not_tuple(CAR(list_val(lst)))
		    || (arityval(*tuple_val(CAR(list_val(lst))))
			< tb->common.keypos)) {
		    goto badarg;
		}
	    }
	    if (lst != NIL) {
		goto badarg;
	    }    
	    for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
		cret = meth->db_member(tb, TERM_GETKEY(tb,CAR(list_val(lst))),
				       &lookup_ret);
		if ((cret != DB_ERROR_NONE) || (lookup_ret != am_false)) {
		    ret = am_false;
		    goto done;
		}
	    }
    
	    for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
		cret = meth->db_put(tb,CAR(list_val(lst)), 0);
		if (cret != DB_ERROR_NONE)
		    break;
	    }
	    goto done;
	}
	obj = CAR(list_val(BIF_ARG_2));
    }
    else {
	obj = BIF_ARG_2;
    }
    /* Only one object (or NIL) 
    */
    kind = LCK_WRITE_REC;
    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, kind)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (BIF_ARG_2 == NIL) {
	db_unlock(tb, kind);
	BIF_RET(am_true);
    }
    if (is_not_tuple(obj)
	|| (arityval(*tuple_val(obj)) < tb->common.keypos)) {
	goto badarg;
    }
    cret = tb->common.meth->db_put(tb, obj,
				   1); /* key_clash_fail */

done:
    db_unlock(tb, kind);
    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_BADKEY:
	BIF_RET(am_false);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
 badarg:
    db_unlock(tb, kind);
    BIF_ERROR(BIF_P, BADARG);    
}

/*
** Rename a (possibly) named table
*/

BIF_RETTYPE ets_rename_2(BIF_ALIST_2)
{
    DbTable* tb;
    Eterm ret;
    Eterm old_name;
    erts_rwmtx_t *lck1, *lck2;

#ifdef HARDDEBUG
    erts_fprintf(stderr,
		"ets:rename(%T,%T); Process: %T, initial: %T:%T/%bpu\n",
		BIF_ARG_1, BIF_ARG_2, BIF_P->common.id,
		BIF_P->u.initial[0], BIF_P->u.initial[1], BIF_P->u.initial[2]);
#endif


    if (is_not_atom(BIF_ARG_2)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    (void) meta_name_tab_bucket(BIF_ARG_2, &lck1);

    if (is_atom(BIF_ARG_1)) {
        old_name = BIF_ARG_1;
    named_tab:
	(void) meta_name_tab_bucket(old_name, &lck2);
	if (lck1 == lck2)
	    lck2 = NULL;
	else if (lck1 > lck2) {
	    erts_rwmtx_t *tmp = lck1;
	    lck1 = lck2;
	    lck2 = tmp;
	}
    }
    else {
        tb = tid2tab(BIF_ARG_1);
        if (!tb)
            BIF_ERROR(BIF_P, BADARG);
        else {
            if (is_table_named(tb)) {
                old_name = tb->common.the_name;
                goto named_tab;
            }
            lck2 = NULL;
        }
    }

    erts_rwmtx_rwlock(lck1);
    if (lck2)
	erts_rwmtx_rwlock(lck2);

    tb = db_get_table_aux(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE, 1);
    if (!tb)
	goto badarg;

    if (is_table_named(tb)) {
        if (!insert_named_tab(BIF_ARG_2, tb, 1))
            goto badarg;

        if (!remove_named_tab(tb, 1))
            erts_exit(ERTS_ERROR_EXIT,"Could not find named tab %s", tb->common.the_name);
        ret = BIF_ARG_2;
    }
    else { /* Not a named table */
        ret = BIF_ARG_1;
    }
    tb->common.the_name = BIF_ARG_2;

    db_unlock(tb, LCK_WRITE);
    erts_rwmtx_rwunlock(lck1);
    if (lck2)
	erts_rwmtx_rwunlock(lck2);
    BIF_RET(ret);
 badarg:
    if (tb)
	db_unlock(tb, LCK_WRITE);
    erts_rwmtx_rwunlock(lck1);
    if (lck2)
	erts_rwmtx_rwunlock(lck2);
    BIF_ERROR(BIF_P, BADARG);    
}


/* 
** The create table BIF     
** Args: (Name, Properties) 
*/

BIF_RETTYPE ets_new_2(BIF_ALIST_2)
{
    DbTable* tb = NULL;
    Eterm list;
    Eterm val;
    Eterm ret;
    Eterm heir;
    UWord heir_data;
    Uint32 status;
    Sint keypos;
    int is_named, is_compressed;
    int is_fine_locked, frequent_read;
#ifdef DEBUG
    int cret;
#endif
    DbTableMethod* meth;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (is_not_nil(BIF_ARG_2) && is_not_list(BIF_ARG_2)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    status = DB_SET | DB_PROTECTED;
    keypos = 1;
    is_named = 0;
    is_fine_locked = 0;
    frequent_read = 0;
    heir = am_none;
    heir_data = (UWord) am_undefined;
    is_compressed = erts_ets_always_compress;

    list = BIF_ARG_2;
    while(is_list(list)) {
	val = CAR(list_val(list));
	if (val == am_bag) {
	    status |= DB_BAG;
	    status &= ~(DB_SET | DB_DUPLICATE_BAG | DB_ORDERED_SET);
	}
	else if (val == am_duplicate_bag) {
	    status |= DB_DUPLICATE_BAG;
	    status &= ~(DB_SET | DB_BAG | DB_ORDERED_SET);
	}
	else if (val == am_ordered_set) {
	    status |= DB_ORDERED_SET;
	    status &= ~(DB_SET | DB_BAG | DB_DUPLICATE_BAG);
	}
	else if (is_tuple(val)) {
	    Eterm *tp = tuple_val(val);
	    if (arityval(tp[0]) == 2) {
		if (tp[1] == am_keypos
		    && is_small(tp[2]) && (signed_val(tp[2]) > 0)) {
		    keypos = signed_val(tp[2]);
		}		
		else if (tp[1] == am_write_concurrency) {
		    if (tp[2] == am_true) {
			is_fine_locked = 1;
		    } else if (tp[2] == am_false) {
			is_fine_locked = 0;
		    } else break;
		}
		else if (tp[1] == am_read_concurrency) {
		    if (tp[2] == am_true) {
			frequent_read = 1;
		    } else if (tp[2] == am_false) {
			frequent_read = 0;
		    } else break;
		    
		}
		else if (tp[1] == am_heir && tp[2] == am_none) {
		    heir = am_none;
		    heir_data = am_undefined;
		}
		else break;
	    }
	    else if (arityval(tp[0]) == 3 && tp[1] == am_heir
		     && is_internal_pid(tp[2])) {
		heir = tp[2];
		heir_data = tp[3];
	    }
	    else break;
	}
	else if (val == am_public) {
	    status |= DB_PUBLIC;
	    status &= ~(DB_PROTECTED|DB_PRIVATE);
	}
	else if (val == am_private) {
	    status |= DB_PRIVATE;
	    status &= ~(DB_PROTECTED|DB_PUBLIC);
	}
	else if (val == am_named_table) {
	    is_named = 1;
            status |= DB_NAMED_TABLE;
	}
	else if (val == am_compressed) {
	    is_compressed = 1;
	}
	else if (val == am_set || val == am_protected)
	    ;
	else break;

	list = CDR(list_val(list));
    }
    if (is_not_nil(list)) { /* bad opt or not a well formed list */
	BIF_ERROR(BIF_P, BADARG);
    }
    if (IS_HASH_TABLE(status)) {
	meth = &db_hash;
	if (is_fine_locked && !(status & DB_PRIVATE)) {
	    status |= DB_FINE_LOCKED;
	}
    }
    else if (IS_TREE_TABLE(status)) {
	meth = &db_tree;
    }
    else {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (frequent_read && !(status & DB_PRIVATE))
	status |= DB_FREQ_READ;

    /* we create table outside any table lock
     * and take the unusal cost of destroy table if it
     * fails to find a slot 
     */
    {
        DbTable init_tb;

	erts_atomic_init_nob(&init_tb.common.memory_size, 0);
	tb = (DbTable*) erts_db_alloc(ERTS_ALC_T_DB_TABLE,
				      &init_tb, sizeof(DbTable));
	erts_atomic_init_nob(&tb->common.memory_size,
				 erts_atomic_read_nob(&init_tb.common.memory_size));
    }

    tb->common.meth = meth;
    tb->common.the_name = BIF_ARG_1;
    tb->common.status = status;    
    tb->common.type = status & ERTS_ETS_TABLE_TYPES;
    /* Note, 'type' is *read only* from now on... */
    erts_refc_init(&tb->common.fix_count, 0);
    db_init_lock(tb, status & (DB_FINE_LOCKED|DB_FREQ_READ));
    tb->common.keypos = keypos;
    tb->common.owner = BIF_P->common.id;
    set_heir(BIF_P, tb, heir, heir_data);

    erts_atomic_init_nob(&tb->common.nitems, 0);

    tb->common.fixing_procs = NULL;
    tb->common.compress = is_compressed;

#ifdef DEBUG
    cret = 
#endif
	meth->db_create(BIF_P, tb);
    ASSERT(cret == DB_ERROR_NONE);

    make_btid(tb);

    if (is_named)
        ret = BIF_ARG_1;
    else
        ret = make_tid(BIF_P, tb);

    save_sched_table(BIF_P, tb);

    if (is_named && !insert_named_tab(BIF_ARG_1, tb, 0)) {
        tid_clear(BIF_P, tb);

	db_lock(tb,LCK_WRITE);
	free_heir_data(tb);
	tb->common.meth->db_free_table(tb);
	db_unlock(tb,LCK_WRITE);
        table_dec_refc(tb, 0);
	BIF_ERROR(BIF_P, BADARG);
    }
    
    BIF_P->flags |= F_USING_DB; /* So we can remove tb if p dies */
    save_owned_table(BIF_P, tb);

#ifdef HARDDEBUG
    erts_fprintf(stderr,
		"ets:new(%T,%T)=%T; Process: %T, initial: %T:%T/%bpu\n",
		 BIF_ARG_1, BIF_ARG_2, ret, BIF_P->common.id,
		 BIF_P->u.initial[0], BIF_P->u.initial[1], BIF_P->u.initial[2]);
#endif

    BIF_RET(ret);
}

/*
** Retrieves the tid() of a named ets table.
*/
BIF_RETTYPE ets_whereis_1(BIF_ALIST_1)
{
    DbTable* tb;
    Eterm res;

    if (is_not_atom(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_INFO, LCK_READ)) == NULL) {
        BIF_RET(am_undefined);
    }

    res = make_tid(BIF_P, tb);
    db_unlock(tb, LCK_READ);

    BIF_RET(res);
}

/* 
** The lookup BIF 
*/
BIF_RETTYPE ets_lookup_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_get(BIF_P, tb, BIF_ARG_2, &ret);

    db_unlock(tb, LCK_READ);

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }

}

/* 
** The lookup BIF 
*/
BIF_RETTYPE ets_member_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_member(tb, BIF_ARG_2, &ret);

    db_unlock(tb, LCK_READ);

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }

}

/* 
** Get an element from a term
** get_element_3(Tab, Key, Index)
** return the element or a list of elements if bag
*/
BIF_RETTYPE ets_lookup_element_3(BIF_ALIST_3)
{
    DbTable* tb;
    Sint index;
    int cret;
    Eterm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_not_small(BIF_ARG_3) || ((index = signed_val(BIF_ARG_3)) < 1)) {
	db_unlock(tb, LCK_READ);
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_get_element(BIF_P, tb, 
					   BIF_ARG_2, index, &ret);
    db_unlock(tb, LCK_READ);
    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}

/* 
 * BIF to erase a whole table and release all memory it holds 
 */
BIF_RETTYPE ets_delete_1(BIF_ALIST_1)
{
    SWord initial_reds = ERTS_BIF_REDS_LEFT(BIF_P);
    SWord reds = initial_reds;
    DbTable* tb;

#ifdef HARDDEBUG
    erts_fprintf(stderr,
		"ets:delete(%T); Process: %T, initial: %T:%T/%bpu\n",
		BIF_ARG_1, BIF_P->common.id,
		BIF_P->u.initial[0], BIF_P->u.initial[1], BIF_P->u.initial[2]);
#endif

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    
    /*
     * Clear all access bits to prevent any ets operation to access the
     * table while it is being deleted.
     */
    tb->common.status &= ~(DB_PROTECTED|DB_PUBLIC|DB_PRIVATE);
    tb->common.status |= DB_DELETE;

    if (tb->common.owner != BIF_P->common.id) {

	/*
	 * The table is being deleted by a process other than its owner.
	 * To make sure that the table will be completely deleted if the
	 * current process will be killed (e.g. by an EXIT signal), we will
	 * now transfer the ownership to the current process.
	 */

        Process *rp = erts_proc_lookup_raw(tb->common.owner);
        /*
         * Process 'rp' might be exiting, but our table lock prevents it
         * from terminating as it cannot complete erts_db_process_exiting().
         */
        ASSERT(!(ERTS_PSFLG_FREE & erts_atomic32_read_nob(&rp->state)));

        delete_owned_table(rp, tb);
        BIF_P->flags |= F_USING_DB;
        tb->common.owner = BIF_P->common.id;
        save_owned_table(BIF_P, tb);
    }

    tid_clear(BIF_P, tb);

    if (is_table_named(tb))
	remove_named_tab(tb, 0);
    
    /* disable inheritance */
    free_heir_data(tb);
    tb->common.heir = am_none;

    reds -= free_fixations_locked(BIF_P, tb);
    db_unlock(tb, LCK_WRITE);

    if (free_table_continue(BIF_P, tb, reds) < 0) {
	/*
	 * Package the DbTable* pointer into a bignum so that it can be safely
	 * passed through a trap. We used to pass the DbTable* pointer directly
	 * (it looks like an continuation pointer), but that is will crash the
	 * emulator if this BIF is call traced.
	 */
	Eterm *hp = HAlloc(BIF_P, 2);
	hp[0] = make_pos_bignum_header(1);
	hp[1] = (Eterm) tb;
        BUMP_ALL_REDS(BIF_P);
	BIF_TRAP1(&ets_delete_continue_exp, BIF_P, make_big(hp));
    }
    else {
        BUMP_REDS(BIF_P, (initial_reds - reds));
	BIF_RET(am_true);
    }
}

/* 
** BIF ets:give_away(Tab, Pid, GiftData)
*/
BIF_RETTYPE ets_give_away_3(BIF_ALIST_3)
{
    Process* to_proc = NULL;
    ErtsProcLocks to_locks = ERTS_PROC_LOCK_MAIN;
    Eterm to_pid = BIF_ARG_2;
    Eterm from_pid;
    DbTable* tb = NULL;

    if (!is_internal_pid(to_pid)) {
	goto badarg;
    }
    to_proc = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN, to_pid, to_locks);
    if (to_proc == NULL) {
	goto badarg;
    }

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE)) == NULL
	|| tb->common.owner != BIF_P->common.id) {
	goto badarg;
    }
    from_pid = tb->common.owner;
    if (to_pid == from_pid) {
	goto badarg;  /* or should we be idempotent? return false maybe */
    }

    delete_owned_table(BIF_P, tb);
    to_proc->flags |= F_USING_DB;
    tb->common.owner = to_pid;
    save_owned_table(to_proc, tb);

    db_unlock(tb,LCK_WRITE);
    send_ets_transfer_message(BIF_P, to_proc, &to_locks,
                              tb, BIF_ARG_3);
    erts_proc_unlock(to_proc, to_locks);
    UnUseTmpHeap(5,BIF_P);
    BIF_RET(am_true);

badarg:
    if (to_proc != NULL && to_proc != BIF_P) erts_proc_unlock(to_proc, to_locks);
    if (tb != NULL) db_unlock(tb, LCK_WRITE);
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE ets_setopts_2(BIF_ALIST_2)
{
    DbTable* tb = NULL;
    Eterm* tp;
    Eterm opt;
    Eterm heir = THE_NON_VALUE;
    UWord heir_data = (UWord) THE_NON_VALUE;
    Uint32 protection = 0;
    DeclareTmpHeap(fakelist,2,BIF_P);
    Eterm tail;

    UseTmpHeap(2,BIF_P);
    for (tail = is_tuple(BIF_ARG_2) ? CONS(fakelist, BIF_ARG_2, NIL) : BIF_ARG_2;	
	  is_list(tail);
	  tail = CDR(list_val(tail))) {

	opt = CAR(list_val(tail));
	if (!is_tuple(opt) || (tp = tuple_val(opt), arityval(tp[0]) < 2)) { 
	    goto badarg;
	}

	switch (tp[1]) {
	case am_heir:
	    if (heir != THE_NON_VALUE) goto badarg;
	    heir = tp[2];
	    if (arityval(tp[0]) == 2 && heir == am_none) {
		heir_data = am_undefined;
	    } 
	    else if (arityval(tp[0]) == 3 && is_internal_pid(heir)) {
		heir_data = tp[3];
	    }
	    else goto badarg;
	    break;

	case am_protection:
	    if (arityval(tp[0]) != 2 || protection != 0) goto badarg; 
	    switch (tp[2]) {
	    case am_private: protection = DB_PRIVATE; break;
	    case am_protected: protection = DB_PROTECTED; break;
	    case am_public: protection = DB_PUBLIC; break;
	    default: goto badarg;
	    }
	    break;

	default: goto badarg;
	}
    }

    if (tail != NIL
	|| (tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE)) == NULL
	|| tb->common.owner != BIF_P->common.id) {
	goto badarg;
    }

    if (heir_data != THE_NON_VALUE) {
	free_heir_data(tb);
	set_heir(BIF_P, tb, heir, heir_data);
    }
    if (protection) {
	tb->common.status &= ~(DB_PRIVATE|DB_PROTECTED|DB_PUBLIC);
	tb->common.status |= protection;
    }

    db_unlock (tb,LCK_WRITE);
    UnUseTmpHeap(2,BIF_P);
    BIF_RET(am_true);

badarg:
    UnUseTmpHeap(2,BIF_P);
    if (tb != NULL) {
	db_unlock(tb,LCK_WRITE);
    }
    BIF_ERROR(BIF_P, BADARG);
}

/* 
** BIF to erase a whole table and release all memory it holds 
*/
BIF_RETTYPE ets_delete_all_objects_1(BIF_ALIST_1)
{
    DbTable* tb;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    tb->common.meth->db_delete_all_objects(BIF_P, tb);

    db_unlock(tb, LCK_WRITE);

    BIF_RET(am_true);
}

/* 
** Erase an object with given key, or maybe several objects if we have a bag  
** Called as db_erase(Tab, Key), where Key is element 1 of the
** object(s) we want to erase                                  
*/
BIF_RETTYPE ets_delete_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE_REC)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_erase(tb,BIF_ARG_2,&ret);

    db_unlock(tb, LCK_WRITE_REC);

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}

/* 
** Erase a specific object, or maybe several objects if we have a bag  
*/
BIF_RETTYPE ets_delete_object_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE_REC)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (is_not_tuple(BIF_ARG_2) || 
	(arityval(*tuple_val(BIF_ARG_2)) < tb->common.keypos)) {
	db_unlock(tb, LCK_WRITE_REC);
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_erase_object(tb, BIF_ARG_2, &ret);
    db_unlock(tb, LCK_WRITE_REC);

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}

/*
** This is for trapping, cannot be called directly.
*/
static BIF_RETTYPE ets_select_delete_1(BIF_ALIST_1)
{
    Process *p = BIF_P;
    Eterm a1 = BIF_ARG_1;
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    Eterm ret;
    Eterm *tptr;
    db_lock_kind_t kind = LCK_WRITE_REC;
    
    CHECK_TABLES();
    ASSERT(is_tuple(a1));
    tptr = tuple_val(a1);
    ASSERT(arityval(*tptr) >= 1);
    
    if ((tb = db_get_table(p, tptr[1], DB_WRITE, kind)) == NULL) {
	BIF_ERROR(p,BADARG);
    }

    cret = tb->common.meth->db_select_delete_continue(p,tb,a1,&ret);

    if(!DID_TRAP(p,ret) && ITERATION_SAFETY(p,tb) != ITER_SAFE) {  
	unfix_table_locked(p, tb, &kind);
    }

    db_unlock(tb, kind);

    switch (cret) {
    case DB_ERROR_NONE:      
	ERTS_BIF_PREP_RET(result, ret);
	break;
    default:
	ERTS_BIF_PREP_ERROR(result, p, BADARG);
	break;
    }
    erts_match_set_release_result(p);

    return result;
}
    

BIF_RETTYPE ets_select_delete_2(BIF_ALIST_2)
{
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    Eterm ret;
    enum DbIterSafety safety;

    CHECK_TABLES();

    if(eq(BIF_ARG_2, ms_delete_all)) {
	int nitems;
	if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE)) == NULL) {
	    BIF_ERROR(BIF_P, BADARG);
	}
	nitems = erts_atomic_read_nob(&tb->common.nitems);
	tb->common.meth->db_delete_all_objects(BIF_P, tb);
	db_unlock(tb, LCK_WRITE);
	BIF_RET(erts_make_integer(nitems,BIF_P));
    }

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE_REC)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    safety = ITERATION_SAFETY(BIF_P,tb);
    if (safety == ITER_UNSAFE) {
	local_fix_table(tb);
    }
    cret = tb->common.meth->db_select_delete(BIF_P, tb, BIF_ARG_1, BIF_ARG_2, &ret);

    if (DID_TRAP(BIF_P,ret) && safety != ITER_SAFE) {
	fix_table_locked(BIF_P,tb);
    }
    if (safety == ITER_UNSAFE) {
	local_unfix_table(tb);
    }
    db_unlock(tb, LCK_WRITE_REC);

    switch (cret) {
    case DB_ERROR_NONE:	
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	ERTS_BIF_PREP_ERROR(result, BIF_P, SYSTEM_LIMIT);
	break;
    default:
	ERTS_BIF_PREP_ERROR(result, BIF_P, BADARG);
	break;
    }

    erts_match_set_release_result(BIF_P);

    return result;
}

/*
 * ets:all/0
 *
 * ets:all() calls ets:internal_request_all/0 which
 * requests information about all tables from
 * each scheduler thread. Each scheduler replies
 * to the calling process with information about
 * existing tables created on that specific scheduler.
 */

struct ErtsEtsAllReq_ {
    erts_atomic32_t refc;
    Process *proc;
    ErtsOIRefStorage ref;
    ErtsEtsAllReqList list[1]; /* one per scheduler */
};

#define ERTS_ETS_ALL_REQ_SIZE           \
    (sizeof(ErtsEtsAllReq)              \
     + (sizeof(ErtsEtsAllReqList)       \
        * (erts_no_schedulers - 1)))

typedef struct {
    ErtsEtsAllReq *ongoing;
    ErlHeapFragment *hfrag;
    DbTable *tab;
    ErtsEtsAllReq *queue;
} ErtsEtsAllData;

/* Tables handled before yielding */
#define ERTS_ETS_ALL_TB_YCNT 200
/*
 * Min yield count required before starting
 * an operation that will require yield.
 */
#define ERTS_ETS_ALL_TB_YCNT_START 10

#ifdef DEBUG
/* Test yielding... */
#undef ERTS_ETS_ALL_TB_YCNT
#undef ERTS_ETS_ALL_TB_YCNT_START
#define ERTS_ETS_ALL_TB_YCNT 10
#define ERTS_ETS_ALL_TB_YCNT_START 1
#endif

static int
ets_all_reply(ErtsSchedulerData *esdp, ErtsEtsAllReq **reqpp,
              ErlHeapFragment **hfragpp, DbTable **tablepp,
              int *yield_count_p)
{
    ErtsEtsAllReq *reqp = *reqpp;
    ErlHeapFragment *hfragp = *hfragpp;
    int ycount = *yield_count_p;
    DbTable *tb, *first;
    Uint sz;
    Eterm list, msg, ref, *hp;
    ErlOffHeap *ohp;
    ErtsMessage *mp;

    /*
     * - save_sched_table() inserts at end of circular list.
     *
     * - This function scans from the end so we know that
     *   the amount of tables to scan wont grow even if we
     *   yield.
     *
     * - remove_sched_table() updates the table we yielded
     *   on if it removes it.
     */

    if (hfragp) {
        /* Restart of a yielded operation... */
        ASSERT(hfragp->used_size < hfragp->alloc_size);
        ohp = &hfragp->off_heap;
        hp = &hfragp->mem[hfragp->used_size];
        list = *hp;
        hfragp->used_size = hfragp->alloc_size;
        first = esdp->ets_tables.clist;
        tb = *tablepp;
    }
    else {
        /* A new operation... */
        ASSERT(!*tablepp);

        /* Max heap size needed... */
        sz = esdp->ets_tables.count;
        sz *= ERTS_MAGIC_REF_THING_SIZE + 2;
        sz += 3 + ERTS_REF_THING_SIZE;
        hfragp = new_message_buffer(sz);

        hp = &hfragp->mem[0];
        ohp = &hfragp->off_heap;
        list = NIL;
        first = esdp->ets_tables.clist;
        tb = first ? first->common.all.prev : NULL;
    }

    if (tb) {
        while (1) {
            if (is_table_alive(tb)) {
                Eterm tid;
                if (is_table_named(tb))
                    tid = tb->common.the_name;
                else
                    tid = erts_mk_magic_ref(&hp, ohp, tb->common.btid);
                list = CONS(hp, tid, list);
                hp += 2;
            }

            if (tb == first)
                break;

            tb = tb->common.all.prev;

            if (--ycount <= 0) {
                sz = hp - &hfragp->mem[0];
                ASSERT(hfragp->alloc_size > sz + 1);
                *hp = list;
                hfragp->used_size = sz;
                *hfragpp = hfragp;
                *reqpp = reqp;
                *tablepp = tb;
                *yield_count_p = 0;
                return 1; /* Yield! */
            }
        }
    }

    ref = erts_oiref_storage_make_ref(&reqp->ref, &hp);
    msg = TUPLE2(hp, ref, list);
    hp += 3;

    sz = hp - &hfragp->mem[0];
    ASSERT(sz <= hfragp->alloc_size);

    hfragp = erts_resize_message_buffer(hfragp, sz, &msg, 1);

    mp = erts_alloc_message(0, NULL);
    mp->data.heap_frag = hfragp;

    erts_queue_message(reqp->proc, 0, mp, msg, am_system);

    erts_proc_dec_refc(reqp->proc);

    if (erts_atomic32_dec_read_nob(&reqp->refc) == 0)
        erts_free(ERTS_ALC_T_ETS_ALL_REQ, reqp);

    *reqpp = NULL;
    *hfragpp = NULL;
    *tablepp = NULL;
    *yield_count_p = ycount;

    return 0;
}

int
erts_handle_yielded_ets_all_request(ErtsSchedulerData *esdp,
                                    ErtsEtsAllYieldData *eaydp)
{
    int ix = (int) esdp->no - 1;
    int yc = ERTS_ETS_ALL_TB_YCNT;

    while (1) {
        if (!eaydp->ongoing) {
            ErtsEtsAllReq *ongoing;

            if (!eaydp->queue)
                return 0; /* All work completed! */

            if (yc < ERTS_ETS_ALL_TB_YCNT_START && yc > esdp->ets_tables.count)
                return 1; /* Yield! */

            eaydp->ongoing = ongoing = eaydp->queue;
            if (ongoing->list[ix].next == ongoing)
                eaydp->queue = NULL;
            else {
                ongoing->list[ix].next->list[ix].prev = ongoing->list[ix].prev;
                ongoing->list[ix].prev->list[ix].next = ongoing->list[ix].next;
                eaydp->queue = ongoing->list[ix].next;
            }
            ASSERT(!eaydp->hfrag);
            ASSERT(!eaydp->tab);
        }

        if (ets_all_reply(esdp, &eaydp->ongoing, &eaydp->hfrag, &eaydp->tab, &yc))
            return 1; /* Yield! */
    }
}

static void
handle_ets_all_request(void *vreq)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    ErtsEtsAllYieldData *eayp = ERTS_SCHED_AUX_YIELD_DATA(esdp, ets_all);
    ErtsEtsAllReq *req = (ErtsEtsAllReq *) vreq;

    if (!eayp->ongoing && !eayp->queue) {
        /* No ets:all() operations ongoing... */
        ErlHeapFragment *hf = NULL;
        DbTable *tb = NULL;
        int yc = ERTS_ETS_ALL_TB_YCNT;
        if (ets_all_reply(esdp, &req, &hf, &tb, &yc)) {
            /* Yielded... */
            ASSERT(hf);
            eayp->ongoing = req;
            eayp->hfrag = hf;
            eayp->tab = tb;
            erts_notify_new_aux_yield_work(esdp);
        }
    }
    else {
        /* Ongoing ets:all() operations; queue up this request... */
        int ix = (int) esdp->no - 1;
        if (!eayp->queue) {
            req->list[ix].next = req;
            req->list[ix].prev = req;
            eayp->queue = req;
        }
        else {
            req->list[ix].next = eayp->queue;
            req->list[ix].prev = eayp->queue->list[ix].prev;
            eayp->queue->list[ix].prev = req;
            req->list[ix].prev->list[ix].next = req;
        }
    }
}

BIF_RETTYPE ets_internal_request_all_0(BIF_ALIST_0)
{
    Eterm ref = erts_make_ref(BIF_P);
    ErtsEtsAllReq *req = erts_alloc(ERTS_ALC_T_ETS_ALL_REQ,
                                    ERTS_ETS_ALL_REQ_SIZE);
    erts_atomic32_init_nob(&req->refc,
			       (erts_aint32_t) erts_no_schedulers);
    erts_oiref_storage_save(&req->ref, ref);
    req->proc = BIF_P;
    erts_proc_add_refc(BIF_P, (Sint) erts_no_schedulers);

    if (erts_no_schedulers > 1)
	erts_schedule_multi_misc_aux_work(1,
					  erts_no_schedulers,
                                          handle_ets_all_request,
					  (void *) req);

    handle_ets_all_request((void *) req);
    BIF_RET(ref);
}


/*
** db_slot(Db, Slot) -> [Items].
*/
BIF_RETTYPE ets_slot_2(BIF_ALIST_2) 
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    /* The slot number is checked in table specific code. */
    cret = tb->common.meth->db_slot(BIF_P, tb, BIF_ARG_2, &ret);
    db_unlock(tb, LCK_READ);
    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}

/* 
** The match BIF,  called as ets:match(Table, Pattern), ets:match(Continuation) or ets:match(Table,Pattern,ChunkSize).
*/

BIF_RETTYPE ets_match_1(BIF_ALIST_1)
{
    return ets_select1(BIF_P, BIF_ARG_1);
}

BIF_RETTYPE ets_match_2(BIF_ALIST_2)
{
    Eterm ms;
    DeclareTmpHeap(buff,8,BIF_P);
    Eterm *hp = buff;
    Eterm res;

    UseTmpHeap(8,BIF_P);
    ms = CONS(hp, am_DollarDollar, NIL);
    hp += 2;
    ms = TUPLE3(hp, BIF_ARG_2, NIL, ms); 
    hp += 4;
    ms = CONS(hp, ms, NIL);
    res = ets_select2(BIF_P, BIF_ARG_1, ms);
    UnUseTmpHeap(8,BIF_P);
    return res;
}

BIF_RETTYPE ets_match_3(BIF_ALIST_3)
{
    Eterm ms;
    DeclareTmpHeap(buff,8,BIF_P);
    Eterm *hp = buff;
    Eterm res;

    UseTmpHeap(8,BIF_P);
    ms = CONS(hp, am_DollarDollar, NIL);
    hp += 2;
    ms = TUPLE3(hp, BIF_ARG_2, NIL, ms); 
    hp += 4;
    ms = CONS(hp, ms, NIL);
    res = ets_select3(BIF_P, BIF_ARG_1, ms, BIF_ARG_3);
    UnUseTmpHeap(8,BIF_P);
    return res;
}


BIF_RETTYPE ets_select_3(BIF_ALIST_3)
{
    return ets_select3(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
}

static BIF_RETTYPE
ets_select3(Process* p, Eterm arg1, Eterm arg2, Eterm arg3)
{
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    Eterm ret;
    Sint chunk_size;
    enum DbIterSafety safety;

    CHECK_TABLES();

    /* Chunk size strictly greater than 0 */
    if (is_not_small(arg3) || (chunk_size = signed_val(arg3)) <= 0) {
	BIF_ERROR(p, BADARG);
    }
    if ((tb = db_get_table(p, arg1, DB_READ, LCK_READ)) == NULL) {
	BIF_ERROR(p, BADARG);
    }
    safety = ITERATION_SAFETY(p,tb);
    if (safety == ITER_UNSAFE) {
	local_fix_table(tb);
    }
    cret = tb->common.meth->db_select_chunk(p, tb, arg1,
					    arg2, chunk_size,
					    0 /* not reversed */,
					    &ret);
    if (DID_TRAP(p,ret) && safety != ITER_SAFE) {
	fix_table_locked(p, tb);
    }
    if (safety == ITER_UNSAFE) {
	local_unfix_table(tb);
    }
    db_unlock(tb, LCK_READ);

    switch (cret) {
    case DB_ERROR_NONE:
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	ERTS_BIF_PREP_ERROR(result, p, SYSTEM_LIMIT);
	break;
    default:
	ERTS_BIF_PREP_ERROR(result, p, BADARG);
	break;
    }

    erts_match_set_release_result(p);

    return result;
}


/* We get here instead of in the real BIF when trapping */
static BIF_RETTYPE ets_select_trap_1(BIF_ALIST_1)
{
    Process *p = BIF_P;
    Eterm a1 = BIF_ARG_1;
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    Eterm ret;
    Eterm *tptr;
    db_lock_kind_t kind = LCK_READ;

    CHECK_TABLES();

    tptr = tuple_val(a1);
    ASSERT(arityval(*tptr) >= 1);

    if ((tb = db_get_table(p, tptr[1], DB_READ, kind)) == NULL) {
	BIF_ERROR(p, BADARG);
    }

    cret = tb->common.meth->db_select_continue(p, tb, a1,
					       &ret);

    if (!DID_TRAP(p,ret) && ITERATION_SAFETY(p,tb) != ITER_SAFE) {
	unfix_table_locked(p, tb, &kind);
    }
    db_unlock(tb, kind);

    switch (cret) {
    case DB_ERROR_NONE:
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	ERTS_BIF_PREP_ERROR(result, p, SYSTEM_LIMIT);
	break;
    default:
	ERTS_BIF_PREP_ERROR(result, p, BADARG);
	break;
    }

    erts_match_set_release_result(p);

    return result;
}


BIF_RETTYPE ets_select_1(BIF_ALIST_1)
{
    return ets_select1(BIF_P, BIF_ARG_1);
}

static BIF_RETTYPE ets_select1(Process *p, Eterm arg1)
{
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    Eterm ret;
    Eterm *tptr;
    enum DbIterSafety safety;

    CHECK_TABLES();

    /*
     * Make sure that the table exists.
     */

    if (!is_tuple(arg1)) {
	if (arg1 == am_EOT) {
	    BIF_RET(am_EOT);
	}
	BIF_ERROR(p, BADARG);
    }
    tptr = tuple_val(arg1);
    if (arityval(*tptr) < 1 ||
	(tb = db_get_table(p, tptr[1], DB_READ, LCK_READ)) == NULL) {
	BIF_ERROR(p, BADARG);
    }

    safety = ITERATION_SAFETY(p,tb);
    if (safety == ITER_UNSAFE) {
	local_fix_table(tb);
    }

    cret = tb->common.meth->db_select_continue(p,tb, arg1, &ret);

    if (DID_TRAP(p,ret) && safety != ITER_SAFE) {
	fix_table_locked(p, tb);
    }
    if (safety == ITER_UNSAFE) {
	local_unfix_table(tb);
    }
    db_unlock(tb, LCK_READ);

    switch (cret) {
    case DB_ERROR_NONE:
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	ERTS_BIF_PREP_ERROR(result, p, SYSTEM_LIMIT);
	break;
    default:
	ERTS_BIF_PREP_ERROR(result, p, BADARG);
	break;
    }

    erts_match_set_release_result(p);

    return result;
}

BIF_RETTYPE ets_select_2(BIF_ALIST_2)
{
    return ets_select2(BIF_P, BIF_ARG_1, BIF_ARG_2);
}

static BIF_RETTYPE
ets_select2(Process* p, Eterm arg1, Eterm arg2)
{
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    enum DbIterSafety safety;
    Eterm ret;

    CHECK_TABLES();

    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(p, arg1, DB_READ, LCK_READ)) == NULL) {
	BIF_ERROR(p, BADARG);
    }
    safety = ITERATION_SAFETY(p,tb);
    if (safety == ITER_UNSAFE) {
	local_fix_table(tb);
    }

    cret = tb->common.meth->db_select(p, tb, arg1, arg2, 0, &ret);

    if (DID_TRAP(p,ret) && safety != ITER_SAFE) {
	fix_table_locked(p, tb);
    }    
    if (safety == ITER_UNSAFE) {
	local_unfix_table(tb);
    }
    db_unlock(tb, LCK_READ);

    switch (cret) {
    case DB_ERROR_NONE:
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	ERTS_BIF_PREP_ERROR(result, p, SYSTEM_LIMIT);
	break;
    default:
	ERTS_BIF_PREP_ERROR(result, p, BADARG);
	break;
    }

    erts_match_set_release_result(p);

    return result;
}

/* We get here instead of in the real BIF when trapping */
static BIF_RETTYPE ets_select_count_1(BIF_ALIST_1)
{
    Process *p = BIF_P;
    Eterm a1 = BIF_ARG_1;
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    Eterm ret;
    Eterm *tptr;
    db_lock_kind_t kind = LCK_READ;

    CHECK_TABLES();

    tptr = tuple_val(a1);
    ASSERT(arityval(*tptr) >= 1);
    if ((tb = db_get_table(p, tptr[1], DB_READ, kind)) == NULL) {
	BIF_ERROR(p, BADARG);
    }

    cret = tb->common.meth->db_select_count_continue(p, tb, a1, &ret);

    if (!DID_TRAP(p,ret) && ITERATION_SAFETY(p,tb) != ITER_SAFE) {
	unfix_table_locked(p, tb, &kind);
    }
    db_unlock(tb, kind);

    switch (cret) {
    case DB_ERROR_NONE:
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	ERTS_BIF_PREP_ERROR(result, p, SYSTEM_LIMIT);
	break;
    default:
	ERTS_BIF_PREP_ERROR(result, p, BADARG);
	break;
    }

    erts_match_set_release_result(p);

    return result;
}

BIF_RETTYPE ets_select_count_2(BIF_ALIST_2)
{
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    enum DbIterSafety safety;
    Eterm ret;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    safety = ITERATION_SAFETY(BIF_P,tb);
    if (safety == ITER_UNSAFE) {
	local_fix_table(tb);
    }
    cret = tb->common.meth->db_select_count(BIF_P,tb, BIF_ARG_1, BIF_ARG_2, &ret);

    if (DID_TRAP(BIF_P,ret) && safety != ITER_SAFE) {
	fix_table_locked(BIF_P, tb);
    }
    if (safety == ITER_UNSAFE) {
	local_unfix_table(tb);
    }
    db_unlock(tb, LCK_READ);
    switch (cret) {
    case DB_ERROR_NONE:
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	ERTS_BIF_PREP_ERROR(result, BIF_P, SYSTEM_LIMIT);
	break;
    default:
	ERTS_BIF_PREP_ERROR(result, BIF_P, BADARG);
	break;
    }

    erts_match_set_release_result(BIF_P);

    return result;
}

/*
 ** This is for trapping, cannot be called directly.
 */
static BIF_RETTYPE ets_select_replace_1(BIF_ALIST_1)
{
    Process *p = BIF_P;
    Eterm a1 = BIF_ARG_1;
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    Eterm ret;
    Eterm *tptr;
    db_lock_kind_t kind = LCK_WRITE_REC;

    CHECK_TABLES();
    ASSERT(is_tuple(a1));
    tptr = tuple_val(a1);
    ASSERT(arityval(*tptr) >= 1);

    if ((tb = db_get_table(p, tptr[1], DB_WRITE, kind)) == NULL) {
        BIF_ERROR(p,BADARG);
    }

    cret = tb->common.meth->db_select_replace_continue(p,tb,a1,&ret);

    if(!DID_TRAP(p,ret) && ITERATION_SAFETY(p,tb) != ITER_SAFE) {
        unfix_table_locked(p, tb, &kind);
    }

    db_unlock(tb, kind);

    switch (cret) {
    case DB_ERROR_NONE:
        ERTS_BIF_PREP_RET(result, ret);
        break;
    default:
        ERTS_BIF_PREP_ERROR(result, p, BADARG);
        break;
    }
    erts_match_set_release_result(p);

    return result;
}


BIF_RETTYPE ets_select_replace_2(BIF_ALIST_2)
{
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    Eterm ret;
    enum DbIterSafety safety;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE_REC)) == NULL) {
        BIF_ERROR(BIF_P, BADARG);
    }

    if (tb->common.status & DB_BAG) {
        /* Bag implementation presented both semantic consistency
           and performance issues */
        db_unlock(tb, LCK_WRITE_REC);
        BIF_ERROR(BIF_P, BADARG);
    }

    safety = ITERATION_SAFETY(BIF_P,tb);
    if (safety == ITER_UNSAFE) {
        local_fix_table(tb);
    }
    cret = tb->common.meth->db_select_replace(BIF_P, tb, BIF_ARG_1, BIF_ARG_2, &ret);

    if (DID_TRAP(BIF_P,ret) && safety != ITER_SAFE) {
        fix_table_locked(BIF_P,tb);
    }
    if (safety == ITER_UNSAFE) {
        local_unfix_table(tb);
    }
    db_unlock(tb, LCK_WRITE_REC);

    switch (cret) {
    case DB_ERROR_NONE:
        ERTS_BIF_PREP_RET(result, ret);
        break;
    case DB_ERROR_SYSRES:
        ERTS_BIF_PREP_ERROR(result, BIF_P, SYSTEM_LIMIT);
        break;
    default:
        ERTS_BIF_PREP_ERROR(result, BIF_P, BADARG);
        break;
    }

    erts_match_set_release_result(BIF_P);

    return result;
}


BIF_RETTYPE ets_select_reverse_3(BIF_ALIST_3)
{
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    enum DbIterSafety safety;
    Eterm ret;
    Sint chunk_size;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    
    /* Chunk size strictly greater than 0 */
    if (is_not_small(BIF_ARG_3) || (chunk_size = signed_val(BIF_ARG_3)) <= 0) {
	db_unlock(tb, LCK_READ);
	BIF_ERROR(BIF_P, BADARG);
    }
    safety = ITERATION_SAFETY(BIF_P,tb);
    if (safety == ITER_UNSAFE) {
	local_fix_table(tb);
    }
    cret = tb->common.meth->db_select_chunk(BIF_P,tb, BIF_ARG_1,
					    BIF_ARG_2, chunk_size, 
					    1 /* reversed */, &ret);
    if (DID_TRAP(BIF_P,ret) && safety != ITER_SAFE) {
	fix_table_locked(BIF_P, tb);
    }
    if (safety == ITER_UNSAFE) {
	local_unfix_table(tb);
    }
    db_unlock(tb, LCK_READ);
    switch (cret) {
    case DB_ERROR_NONE:
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	ERTS_BIF_PREP_ERROR(result, BIF_P, SYSTEM_LIMIT);
	break;
    default:
	ERTS_BIF_PREP_ERROR(result, BIF_P, BADARG);
	break;
    }
    erts_match_set_release_result(BIF_P);
    return result;
}

BIF_RETTYPE ets_select_reverse_1(BIF_ALIST_1)
{
    return ets_select1(BIF_P, BIF_ARG_1);
}

BIF_RETTYPE ets_select_reverse_2(BIF_ALIST_2)
{
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    enum DbIterSafety safety;
    Eterm ret;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    safety = ITERATION_SAFETY(BIF_P,tb);
    if (safety == ITER_UNSAFE) {
	local_fix_table(tb);
    }
    cret = tb->common.meth->db_select(BIF_P,tb, BIF_ARG_1, BIF_ARG_2,
				      1 /*reversed*/, &ret);

    if (DID_TRAP(BIF_P,ret) && safety != ITER_SAFE) {
	fix_table_locked(BIF_P, tb);
    }    
    if (safety == ITER_UNSAFE) {
	local_unfix_table(tb);
    }
    db_unlock(tb, LCK_READ);
    switch (cret) {
    case DB_ERROR_NONE:
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	ERTS_BIF_PREP_ERROR(result, BIF_P, SYSTEM_LIMIT);
	break;
    default:
	ERTS_BIF_PREP_ERROR(result, BIF_P, BADARG);
	break;
    }
    erts_match_set_release_result(BIF_P);
    return result;
}


/* 
** ets:match_object(Continuation), ets:match_object(Table, Pattern), ets:match_object(Table,Pattern,ChunkSize) 
*/
BIF_RETTYPE ets_match_object_1(BIF_ALIST_1)
{
    return ets_select1(BIF_P, BIF_ARG_1);
}

BIF_RETTYPE ets_match_object_2(BIF_ALIST_2)
{
    Eterm ms;
    DeclareTmpHeap(buff,8,BIF_P);
    Eterm *hp = buff;
    Eterm res;

    UseTmpHeap(8,BIF_P);
    ms = CONS(hp, am_DollarUnderscore, NIL);
    hp += 2;
    ms = TUPLE3(hp, BIF_ARG_2, NIL, ms); 
    hp += 4;
    ms = CONS(hp, ms, NIL);
    res = ets_select2(BIF_P, BIF_ARG_1, ms);
    UnUseTmpHeap(8,BIF_P);
    return res;
}

BIF_RETTYPE ets_match_object_3(BIF_ALIST_3)
{
    Eterm ms;
    DeclareTmpHeap(buff,8,BIF_P);
    Eterm *hp = buff;
    Eterm res;

    UseTmpHeap(8,BIF_P);
    ms = CONS(hp, am_DollarUnderscore, NIL);
    hp += 2;
    ms = TUPLE3(hp, BIF_ARG_2, NIL, ms); 
    hp += 4;
    ms = CONS(hp, ms, NIL);
    res = ets_select3(BIF_P, BIF_ARG_1, ms, BIF_ARG_3);
    UnUseTmpHeap(8,BIF_P);
    return res;
}

/* 
 * BIF to extract information about a particular table.
 */ 

BIF_RETTYPE ets_info_1(BIF_ALIST_1)
{
    static Eterm fields[] = {am_protection, am_keypos, am_type, am_named_table,
                             am_node, am_size, am_name, am_heir, am_owner, am_memory, am_compressed,
                             am_write_concurrency,
                             am_read_concurrency,
                             am_id};
    Eterm results[sizeof(fields)/sizeof(Eterm)];
    DbTable* tb;
    Eterm res;
    int i;
    Eterm* hp;
    /*Process* rp = NULL;*/
    /* If/when we implement lockless private tables:
    Eterm owner;
    */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_INFO, LCK_READ)) == NULL) {
	if (is_atom(BIF_ARG_1) || is_ref(BIF_ARG_1)) {
	    BIF_RET(am_undefined);
	}
	BIF_ERROR(BIF_P, BADARG);
    }

    /* If/when we implement lockless private tables:
    owner = tb->common.owner;
    */

    /* If/when we implement lockless private tables:
    if ((tb->common.status & DB_PRIVATE) && owner != BIF_P->common.id) {
	db_unlock(tb, LCK_READ);
	rp = erts_pid2proc_not_running(BIF_P, ERTS_PROC_LOCK_MAIN,
				       owner, ERTS_PROC_LOCK_MAIN);
	if (rp == NULL) {
	    BIF_RET(am_undefined);
	}
	if (rp == ERTS_PROC_LOCK_BUSY) {
	    ERTS_BIF_YIELD1(bif_export[BIF_ets_info_1], BIF_P, BIF_ARG_1);
	}
	if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_INFO, LCK_READ)) == NULL
	    || tb->common.owner != owner) {
	    if (BIF_P != rp)
		erts_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);
	    if (is_atom(BIF_ARG_1) || is_small(BIF_ARG_1)) {
		BIF_RET(am_undefined);
	    }
	    BIF_ERROR(BIF_P, BADARG);
	}
    }*/
    for (i = 0; i < sizeof(fields)/sizeof(Eterm); i++) {
	results[i] = table_info(BIF_P, tb, fields[i]);
	ASSERT(is_value(results[i]));
    }
    db_unlock(tb, LCK_READ);

    /*if (rp != NULL && rp != BIF_P)
	erts_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);*/

    hp = HAlloc(BIF_P, 5*sizeof(fields)/sizeof(Eterm));
    res = NIL;
    for (i = 0; i < sizeof(fields)/sizeof(Eterm); i++) {
	Eterm tuple;
	tuple = TUPLE2(hp, fields[i], results[i]);
	hp += 3;
	res = CONS(hp, tuple, res);
	hp += 2;
    }
    BIF_RET(res);
}

/* 
 * BIF to extract information about a particular table.
 */ 

BIF_RETTYPE ets_info_2(BIF_ALIST_2)
{
    DbTable* tb;
    Eterm ret = THE_NON_VALUE;

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_INFO, LCK_READ)) == NULL) {
	if (is_atom(BIF_ARG_1) || is_ref(BIF_ARG_1)) {
	    BIF_RET(am_undefined);
	}
	BIF_ERROR(BIF_P, BADARG);
    }
    ret = table_info(BIF_P, tb, BIF_ARG_2);
    db_unlock(tb, LCK_READ);
    if (is_non_value(ret)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(ret);
}


BIF_RETTYPE ets_is_compiled_ms_1(BIF_ALIST_1)
{
    if (erts_db_get_match_prog_binary(BIF_ARG_1)) {
	BIF_RET(am_true);
    } else {
	BIF_RET(am_false);
    }
}

BIF_RETTYPE ets_match_spec_compile_1(BIF_ALIST_1)
{
    Binary *mp = db_match_set_compile(BIF_P, BIF_ARG_1, DCOMP_TABLE);
    Eterm *hp;
    if (mp == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    hp = HAlloc(BIF_P, ERTS_MAGIC_REF_THING_SIZE);

    BIF_RET(erts_db_make_match_prog_ref(BIF_P, mp, &hp));
}

BIF_RETTYPE ets_match_spec_run_r_3(BIF_ALIST_3)
{
    Eterm ret = BIF_ARG_3;
    int i = 0;
    Eterm *hp;
    Eterm lst;
    Binary *mp;
    Eterm res;
    Uint32 dummy;

    if (!(is_list(BIF_ARG_1) || BIF_ARG_1 == NIL)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    
    mp = erts_db_get_match_prog_binary(BIF_ARG_2);
    if (!mp)
	goto error;

    if (BIF_ARG_1 == NIL) {
	BIF_RET(BIF_ARG_3);
    }
    for (lst = BIF_ARG_1; is_list(lst); lst = CDR(list_val(lst))) {
	if (++i > CONTEXT_REDS) {
	    BUMP_ALL_REDS(BIF_P);
	    BIF_TRAP3(bif_export[BIF_ets_match_spec_run_r_3],
		      BIF_P,lst,BIF_ARG_2,ret);
	}
	res = db_prog_match(BIF_P, BIF_P,
                            mp, CAR(list_val(lst)), NULL, 0,
			    ERTS_PAM_COPY_RESULT, &dummy);
	if (is_value(res)) {
	    hp = HAlloc(BIF_P, 2);
	    ret = CONS(hp,res,ret);
	    /*hp += 2;*/
	} 
    }
    if (lst != NIL) {
	goto error;
    }
    BIF_RET2(ret,i);
}


/*
** External interface (NOT BIF's)
*/

int erts_ets_rwmtx_spin_count = -1;

/* Init the db */

void init_db(ErtsDbSpinCount db_spin_count)
{
    int i;
    Eterm *hp;
    unsigned bits;
    size_t size;

    int max_spin_count = (1 << 15) - 1; /* internal limit */
    erts_rwmtx_opt_t rwmtx_opt = ERTS_RWMTX_OPT_DEFAULT_INITER;
    rwmtx_opt.type = ERTS_RWMTX_TYPE_FREQUENT_READ;
    rwmtx_opt.lived = ERTS_RWMTX_LONG_LIVED;

    switch (db_spin_count) {
    case ERTS_DB_SPNCNT_NONE:
	erts_ets_rwmtx_spin_count = 0;
	break;
    case ERTS_DB_SPNCNT_VERY_LOW:
	erts_ets_rwmtx_spin_count = 100;
	break;
    case ERTS_DB_SPNCNT_LOW:
	erts_ets_rwmtx_spin_count = 200;
	erts_ets_rwmtx_spin_count += erts_no_schedulers * 50;
	if (erts_ets_rwmtx_spin_count > 1000)
	    erts_ets_rwmtx_spin_count = 1000;
	break;
    case ERTS_DB_SPNCNT_HIGH:
	erts_ets_rwmtx_spin_count = 2000;
	erts_ets_rwmtx_spin_count += erts_no_schedulers * 100;
	if (erts_ets_rwmtx_spin_count > 15000)
	    erts_ets_rwmtx_spin_count = 15000;
	break;
    case ERTS_DB_SPNCNT_VERY_HIGH:
	erts_ets_rwmtx_spin_count = 15000;
	erts_ets_rwmtx_spin_count += erts_no_schedulers * 500;
	if (erts_ets_rwmtx_spin_count > max_spin_count)
	    erts_ets_rwmtx_spin_count = max_spin_count;
	break;
    case ERTS_DB_SPNCNT_EXTREMELY_HIGH:
	erts_ets_rwmtx_spin_count = max_spin_count;
	break;
    case ERTS_DB_SPNCNT_NORMAL:
    default:
	erts_ets_rwmtx_spin_count = -1;
	break;
    }

    if (erts_ets_rwmtx_spin_count >= 0)
	rwmtx_opt.main_spincount = erts_ets_rwmtx_spin_count;

    for (i=0; i<META_NAME_TAB_LOCK_CNT; i++) {
        erts_rwmtx_init_opt(&meta_name_tab_rwlocks[i].lck, &rwmtx_opt,
            "meta_name_tab", make_small(i),
            ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_DB);
    }

    erts_atomic_init_nob(&erts_ets_misc_mem_size, 0);
    db_initialize_util();

    if (user_requested_db_max_tabs < DB_DEF_MAX_TABS)
	db_max_tabs = DB_DEF_MAX_TABS;
    else
	db_max_tabs = user_requested_db_max_tabs;

    bits = erts_fit_in_bits_int32(db_max_tabs-1);
    if (bits > SMALL_BITS) {
	erts_exit(ERTS_ERROR_EXIT,"Max limit for ets tabled too high %u (max %u).",
		 db_max_tabs, ((Uint)1)<<SMALL_BITS);
    }

    meta_name_tab_mask = (((Uint) 1)<<(bits-1)) - 1; /* At least half the size of main tab */
    size = sizeof(struct meta_name_tab_entry)*(meta_name_tab_mask+1);
    meta_name_tab = erts_db_alloc_nt(ERTS_ALC_T_DB_TABLES, size);
    ERTS_ETS_MISC_MEM_ADD(size);

    for (i=0; i<=meta_name_tab_mask; i++) {
	meta_name_tab[i].pu.tb = NULL;
	meta_name_tab[i].u.name_atom = NIL;
    }

    db_initialize_hash();
    db_initialize_tree();

    /* Non visual BIF to trap to. */
    erts_init_trap_export(&ets_select_delete_continue_exp,
			  am_ets, am_atom_put("delete_trap",11), 1,
			  &ets_select_delete_1);

    /* Non visual BIF to trap to. */
    erts_init_trap_export(&ets_select_count_continue_exp,
			  am_ets, am_atom_put("count_trap",11), 1,
			  &ets_select_count_1);

    /* Non visual BIF to trap to. */
    erts_init_trap_export(&ets_select_replace_continue_exp,
                          am_ets, am_atom_put("replace_trap",11), 1,
                          &ets_select_replace_1);

    /* Non visual BIF to trap to. */
    erts_init_trap_export(&ets_select_continue_exp,
			  am_ets, am_atom_put("select_trap",11), 1,
			  &ets_select_trap_1);

    /* Non visual BIF to trap to. */
    erts_init_trap_export(&ets_delete_continue_exp,
			  am_ets, am_atom_put("delete_trap",11), 1,
			  &ets_delete_trap);

    hp = ms_delete_all_buff;
    ms_delete_all = CONS(hp, am_true, NIL);
    hp += 2;
    ms_delete_all = TUPLE3(hp,am_Underscore,NIL,ms_delete_all);
    hp +=4;
    ms_delete_all = CONS(hp, ms_delete_all,NIL);
}

void
erts_ets_sched_spec_data_init(ErtsSchedulerData *esdp)
{
    ErtsEtsAllYieldData *eaydp = ERTS_SCHED_AUX_YIELD_DATA(esdp, ets_all);
    eaydp->ongoing = NULL;
    eaydp->hfrag = NULL;
    eaydp->tab = NULL;
    eaydp->queue = NULL;
    esdp->ets_tables.clist = NULL;
    esdp->ets_tables.count = 0;
}


/* In: Table LCK_WRITE
** Return TRUE : ok, table not mine and NOT locked anymore.
** Return FALSE: failed, table still mine (LCK_WRITE)
*/
static int give_away_to_heir(Process* p, DbTable* tb)
{
    Process* to_proc;
    ErtsProcLocks to_locks = ERTS_PROC_LOCK_MAIN;
    Eterm to_pid;
    UWord heir_data;

    ASSERT(tb->common.owner == p->common.id);
    ASSERT(is_internal_pid(tb->common.heir));
    ASSERT(tb->common.heir != p->common.id);
retry:
    to_pid = tb->common.heir;
    to_proc = erts_pid2proc_opt(p, ERTS_PROC_LOCK_MAIN,
				to_pid, to_locks,
				ERTS_P2P_FLG_TRY_LOCK);
    if (to_proc == ERTS_PROC_LOCK_BUSY) {
	db_unlock(tb,LCK_WRITE);    
	to_proc = erts_pid2proc(p, ERTS_PROC_LOCK_MAIN,
				to_pid, to_locks);    
	db_lock(tb,LCK_WRITE);
	ASSERT(tb != NULL);
    
	if (tb->common.owner != p->common.id) {
	    if (to_proc != NULL ) {
		erts_proc_unlock(to_proc, to_locks);
	    }
	    db_unlock(tb,LCK_WRITE);
	    return !0; /* ok, someone already gave my table away */
	}
	if (tb->common.heir != to_pid) {  /* someone changed the heir */ 
	    if (to_proc != NULL ) {
		erts_proc_unlock(to_proc, to_locks);
	    }
	    if (to_pid == p->common.id || to_pid == am_none) {
		return 0; /* no real heir, table still mine */
	    }
	    goto retry;
	}
    }
    if (to_proc == NULL) {
	return 0; /* heir not alive, table still mine */
    }
    if (to_proc->common.u.alive.started_interval
	!= tb->common.heir_started_interval) {
	erts_proc_unlock(to_proc, to_locks);
	return 0; /* heir dead and pid reused, table still mine */
    }
    
    delete_owned_table(p, tb);
    to_proc->flags |= F_USING_DB;
    tb->common.owner = to_pid;
    save_owned_table(to_proc, tb);

    db_unlock(tb,LCK_WRITE);
    heir_data = tb->common.heir_data;
    if (!is_immed(heir_data)) {
	Eterm* tpv = ((DbTerm*)heir_data)->tpl; /* tuple_val */
	ASSERT(arityval(*tpv) == 1);
	heir_data = tpv[1];
    }
    send_ets_transfer_message(p, to_proc, &to_locks, tb, heir_data);
    erts_proc_unlock(to_proc, to_locks);
    return !0;
}

static void
send_ets_transfer_message(Process *c_p, Process *proc,
                          ErtsProcLocks *locks,
                          DbTable *tb, Eterm heir_data)
{
    Uint hsz, hd_sz;
    ErtsMessage *mp;
    Eterm *hp;
    ErlOffHeap *ohp;
    Eterm tid, hd_copy, msg, sender;

    hsz = 5;
    if (!is_table_named(tb))
        hsz += ERTS_MAGIC_REF_THING_SIZE;
    if (is_immed(heir_data))
        hd_sz = 0;
    else {
        hd_sz = size_object(heir_data);
        hsz += hd_sz;
    }

    mp = erts_alloc_message_heap(proc, locks, hsz, &hp, &ohp);
    if (is_table_named(tb))
        tid = tb->common.the_name;
    else
        tid = erts_mk_magic_ref(&hp, ohp, tb->common.btid);
    if (!hd_sz)
        hd_copy = heir_data;
    else
        hd_copy = copy_struct(heir_data, hd_sz, &hp, ohp);
    sender = c_p->common.id;
    msg = TUPLE4(hp, am_ETS_TRANSFER, tid, sender, hd_copy);
    erts_queue_message(proc, *locks, mp, msg, sender);
}


/* Auto-release fixation from exiting process */
static SWord proc_cleanup_fixed_table(Process* p, DbFixation* fix)
{
    DbTable* tb = btid2tab(fix->tabs.btid);
    SWord work = 0;

    ASSERT(fix->procs.p == p); (void)p;
    if (tb) {
	db_lock(tb, LCK_WRITE_REC);
	if (!(tb->common.status & DB_DELETE)) {
	    erts_aint_t diff;
	    erts_mtx_lock(&tb->common.fixlock);

	    ASSERT(fixing_procs_rbt_lookup(tb->common.fixing_procs, p));

	    diff = -((erts_aint_t) fix->counter);
	    erts_refc_add(&tb->common.fix_count,diff,0);
	    fix->counter = 0;

	    fixing_procs_rbt_delete(&tb->common.fixing_procs, fix);

	    erts_mtx_unlock(&tb->common.fixlock);
	    if (!IS_FIXED(tb) && IS_HASH_TABLE(tb->common.status)) {
		work += db_unfix_table_hash(&(tb->hash));
	    }

	    ASSERT(sizeof(DbFixation) == ERTS_ALC_DBG_BLK_SZ(fix));
	    ERTS_DB_ALC_MEM_UPDATE_(tb, sizeof(DbFixation), 0);
	}
	db_unlock(tb, LCK_WRITE_REC);
    }

    erts_bin_release(fix->tabs.btid);
    erts_free(ERTS_ALC_T_DB_FIXATION, fix);
    ERTS_ETS_MISC_MEM_ADD(-sizeof(DbFixation));
    ++work;

    return work;
}


/*
 * erts_db_process_exiting() is called when a process terminates.
 * It returns 0 when completely done, and !0 when it wants to
 * yield. c_p->u.terminate can hold a pointer to a state while
 * yielding.
 */
#define ERTS_DB_INTERNAL_ERROR(LSTR) \
  erts_exit(ERTS_ABORT_EXIT, "%s:%d:erts_db_process_exiting(): " LSTR "\n", \
	   __FILE__, __LINE__)

int
erts_db_process_exiting(Process *c_p, ErtsProcLocks c_p_locks)
{
    typedef struct {
        enum {
            GET_OWNED_TABLE,
            FREE_OWNED_TABLE,
            UNFIX_TABLES,
        }op;
        DbTable *tb;
    } CleanupState;
    CleanupState *state = (CleanupState *) c_p->u.terminate;
    Eterm pid = c_p->common.id;
    CleanupState default_state;
    SWord initial_reds = ERTS_BIF_REDS_LEFT(c_p);
    SWord reds = initial_reds;

    if (!state) {
	state = &default_state;
	state->op = GET_OWNED_TABLE;
        state->tb = NULL;
    }

    do {
	switch (state->op) {
        case GET_OWNED_TABLE: {
            DbTable* tb;
            erts_proc_lock(c_p, ERTS_PROC_LOCK_STATUS);
            tb = (DbTable*) erts_psd_get(c_p, ERTS_PSD_ETS_OWNED_TABLES);
            erts_proc_unlock(c_p, ERTS_PROC_LOCK_STATUS);

            if (!tb) {
                /* Done with owned tables; now fixations */
                state->op = UNFIX_TABLES;
                break;
            }

            ASSERT(tb != state->tb);
            state->tb = tb;
            db_lock(tb, LCK_WRITE);
            /*
             *  Ownership may have changed since we looked up the table.
             */
            if (tb->common.owner != pid) {
                db_unlock(tb, LCK_WRITE);
                break;
            }
            if (tb->common.heir != am_none
                && tb->common.heir != pid
                && give_away_to_heir(c_p, tb)) {
                break;
            }
            tid_clear(c_p, tb);
            /* Clear all access bits. */
            tb->common.status &= ~(DB_PROTECTED | DB_PUBLIC | DB_PRIVATE);
            tb->common.status |= DB_DELETE;

            if (is_table_named(tb))
                remove_named_tab(tb, 0);

            free_heir_data(tb);
            reds -= free_fixations_locked(c_p, tb);
            db_unlock(tb, LCK_WRITE);
            state->op = FREE_OWNED_TABLE;
            break;
        }
        case FREE_OWNED_TABLE:
            reds = free_table_continue(c_p, state->tb, reds);
            if (reds < 0)
                goto yield;

            state->op = GET_OWNED_TABLE;
            break;

	case UNFIX_TABLES: {
	    DbFixation* fix;

            fix = (DbFixation*) erts_psd_get(c_p, ERTS_PSD_ETS_FIXED_TABLES);

            if (!fix) {
                /* Done */

                if (state != &default_state)
                    erts_free(ERTS_ALC_T_DB_PROC_CLEANUP, state);
                c_p->u.terminate = NULL;

                BUMP_REDS(c_p, (initial_reds - reds));
                return 0;
            }

            fixed_tabs_delete(c_p, fix);
            reds -= proc_cleanup_fixed_table(c_p, fix);

            break;
        }
	default:
	    ERTS_DB_INTERNAL_ERROR("Bad internal state");
        }

    } while (reds > 0);

 yield:

    if (state == &default_state) {
	c_p->u.terminate = erts_alloc(ERTS_ALC_T_DB_PROC_CLEANUP,
				      sizeof(CleanupState));
	sys_memcpy(c_p->u.terminate, (void*) state, sizeof(CleanupState));
    }
    else
        ASSERT(state == c_p->u.terminate);

    return !0;
}


/*  SMP note: table only need to be LCK_READ locked */
static void fix_table_locked(Process* p, DbTable* tb)
{
    DbFixation *fix;

    erts_mtx_lock(&tb->common.fixlock);
    erts_refc_inc(&tb->common.fix_count,1);
    fix = tb->common.fixing_procs;
    if (fix == NULL) {
	tb->common.time.monotonic
	    = erts_get_monotonic_time(erts_proc_sched_data(p));
	tb->common.time.offset = erts_get_time_offset();
    }
    else {
	fix = fixing_procs_rbt_lookup(fix, p);
	if (fix) {
	    ASSERT(fixed_tabs_find(NULL, fix));
	    ++(fix->counter);

	    erts_mtx_unlock(&tb->common.fixlock);
	    return;
	}
    }
    fix = (DbFixation *) erts_db_alloc(ERTS_ALC_T_DB_FIXATION,
				       tb, sizeof(DbFixation));
    ERTS_ETS_MISC_MEM_ADD(sizeof(DbFixation));
    fix->tabs.btid = tb->common.btid;
    erts_refc_inc(&fix->tabs.btid->intern.refc, 2);
    fix->procs.p = p;
    fix->counter = 1;
    fixing_procs_rbt_insert(&tb->common.fixing_procs, fix);

    erts_mtx_unlock(&tb->common.fixlock);
    p->flags |= F_USING_DB;

    fixed_tabs_insert(p, fix);
}

/* SMP note: May re-lock table 
*/
static void unfix_table_locked(Process* p,  DbTable* tb,
			       db_lock_kind_t* kind_p)
{
    DbFixation* fix;

    erts_mtx_lock(&tb->common.fixlock);
    fix = fixing_procs_rbt_lookup(tb->common.fixing_procs, p);

    if (fix) {
	erts_refc_dec(&tb->common.fix_count,0);
	--(fix->counter);
	ASSERT(fix->counter >= 0);
	if (fix->counter == 0) {
	    fixing_procs_rbt_delete(&tb->common.fixing_procs, fix);
	    erts_mtx_unlock(&tb->common.fixlock);
	    fixed_tabs_delete(p, fix);

	    erts_refc_dec(&fix->tabs.btid->intern.refc, 1);

	    erts_db_free(ERTS_ALC_T_DB_FIXATION,
			 tb, (void *) fix, sizeof(DbFixation));
	    ERTS_ETS_MISC_MEM_ADD(-sizeof(DbFixation));
	    goto unlocked;
	}
    }
    erts_mtx_unlock(&tb->common.fixlock);
unlocked:

    if (!IS_FIXED(tb) && IS_HASH_TABLE(tb->common.status)
	&& erts_atomic_read_nob(&tb->hash.fixdel) != (erts_aint_t)NULL) {
	if (*kind_p == LCK_READ && tb->common.is_thread_safe) {
	    /* Must have write lock while purging pseudo-deleted (OTP-8166) */
	    erts_rwmtx_runlock(&tb->common.rwlock);
	    erts_rwmtx_rwlock(&tb->common.rwlock);
	    *kind_p = LCK_WRITE;
	    if (tb->common.status & DB_DELETE) return;
	}
	db_unfix_table_hash(&(tb->hash));
    }
}

struct free_fixations_ctx
{
    Process* p;
    DbTable* tb;
    SWord cnt;
};

static void free_fixations_op(DbFixation* fix, void* vctx)
{
    struct free_fixations_ctx* ctx = (struct free_fixations_ctx*) vctx;
    erts_aint_t diff;

    ASSERT(!btid2tab(fix->tabs.btid));
    ASSERT(fix->counter > 0);
    ASSERT(ctx->tb->common.status & DB_DELETE);

    diff = -((erts_aint_t) fix->counter);
    erts_refc_add(&ctx->tb->common.fix_count, diff, 0);

    if (fix->procs.p != ctx->p) { /* Fixated by other process */
        fix->counter = 0;

        /* Fake memory stats for table */
        ASSERT(sizeof(DbFixation) == ERTS_ALC_DBG_BLK_SZ(fix));
        ERTS_DB_ALC_MEM_UPDATE_(ctx->tb, sizeof(DbFixation), 0);

        erts_schedule_ets_free_fixation(fix->procs.p->common.id, fix);
        /*
         * Either sys task is scheduled and erts_db_execute_free_fixation()
         * will remove 'fix' or process will exit, drop sys task and
         * proc_cleanup_fixed_table() will remove 'fix'.
         */
    }
    else
    {
        fixed_tabs_delete(fix->procs.p, fix);

        erts_bin_release(fix->tabs.btid);

        erts_db_free(ERTS_ALC_T_DB_FIXATION,
		     ctx->tb, (void *) fix, sizeof(DbFixation));
        ERTS_ETS_MISC_MEM_ADD(-sizeof(DbFixation));
    }
    ctx->cnt++;
}

int erts_db_execute_free_fixation(Process* p, DbFixation* fix)
{
    ASSERT(fix->counter == 0);
    fixed_tabs_delete(p, fix);

    erts_bin_release(fix->tabs.btid);

    erts_free(ERTS_ALC_T_DB_FIXATION, fix);
    ERTS_ETS_MISC_MEM_ADD(-sizeof(DbFixation));
    return 1;
}

static SWord free_fixations_locked(Process* p, DbTable *tb)
{
    struct free_fixations_ctx ctx;

    ERTS_LC_ASSERT(erts_lc_rwmtx_is_rwlocked(&tb->common.rwlock));

    ctx.p = p;
    ctx.tb = tb;
    ctx.cnt = 0;
    fixing_procs_rbt_foreach_destroy(&tb->common.fixing_procs,
                                     free_fixations_op, &ctx);
    tb->common.fixing_procs = NULL;
    return ctx.cnt;
}

static void set_heir(Process* me, DbTable* tb, Eterm heir, UWord heir_data)
{	
    tb->common.heir = heir;
    if (heir == am_none) {
	return;
    }
    if (heir == me->common.id) {
	erts_ensure_later_proc_interval(me->common.u.alive.started_interval);
	tb->common.heir_started_interval = me->common.u.alive.started_interval;
    }
    else {
	Process* heir_proc= erts_proc_lookup(heir);
	if (heir_proc != NULL) {
	    erts_ensure_later_proc_interval(heir_proc->common.u.alive.started_interval);
	    tb->common.heir_started_interval = heir_proc->common.u.alive.started_interval;
	} else {
	    tb->common.heir = am_none;
	}
    }

    if (!is_immed(heir_data)) {
	DeclareTmpHeap(tmp,2,me);
	Eterm wrap_tpl;
	int size;
	DbTerm* dbterm;
	Eterm* top;
	ErlOffHeap tmp_offheap;

	UseTmpHeap(2,me);
	/* Make a dummy 1-tuple around data to use DbTerm */
	wrap_tpl = TUPLE1(tmp,heir_data);
	size = size_object(wrap_tpl);
	dbterm = erts_db_alloc(ERTS_ALC_T_DB_HEIR_DATA, (DbTable *)tb,
			       (sizeof(DbTerm) + sizeof(Eterm)*(size-1)));
	dbterm->size = size;
	top = dbterm->tpl;
	tmp_offheap.first  = NULL;
	copy_struct(wrap_tpl, size, &top, &tmp_offheap);
	dbterm->first_oh = tmp_offheap.first;
	heir_data = (UWord)dbterm;
	UnUseTmpHeap(2,me);
	ASSERT(!is_immed(heir_data));
    }
    tb->common.heir_data = heir_data;
}

static void free_heir_data(DbTable* tb)
{
    if (tb->common.heir != am_none && !is_immed(tb->common.heir_data)) {
	DbTerm* p = (DbTerm*) tb->common.heir_data;
	db_cleanup_offheap_comp(p);
	erts_db_free(ERTS_ALC_T_DB_HEIR_DATA, tb, (void *)p,
		     sizeof(DbTerm) + (p->size-1)*sizeof(Eterm));
    }
    #ifdef DEBUG
    tb->common.heir_data = am_undefined;
    #endif
}

static BIF_RETTYPE ets_delete_trap(BIF_ALIST_1)
{
    SWord initial_reds = ERTS_BIF_REDS_LEFT(BIF_P);
    SWord reds = initial_reds;
    Eterm cont = BIF_ARG_1;
    Eterm* ptr = big_val(cont);
    DbTable *tb = *((DbTable **) (UWord) (ptr + 1));

    ASSERT(*ptr == make_pos_bignum_header(1));

    if (free_table_continue(BIF_P, tb, reds) < 0) {
        BUMP_ALL_REDS(BIF_P);
        BIF_TRAP1(&ets_delete_continue_exp, BIF_P, cont);
    }
    else {
        BUMP_REDS(BIF_P, (initial_reds - reds));
	BIF_RET(am_true);
    }
}


/*
 * free_table_continue() returns reductions left
 * done if >= 0
 * yield if < 0
 */
static SWord free_table_continue(Process *p, DbTable *tb, SWord reds)
{
    reds = tb->common.meth->db_free_table_continue(tb, reds);

    if (reds < 0) {
#ifdef HARDDEBUG
	erts_fprintf(stderr,"ets: free_table_cont %T (continue begin)\r\n",
		     tb->common.id);
#endif
	/* More work to be done. Let other processes work and call us again. */
    }
    else {
#ifdef HARDDEBUG
	erts_fprintf(stderr,"ets: free_table_cont %T (continue end)\r\n",
		     tb->common.id);
#endif
	/* Completely done - we will not get called again. */
        delete_owned_table(p, tb);
        table_dec_refc(tb, 0);
    }
    return reds;
}

struct fixing_procs_info_ctx
{
    Process* p;
    Eterm list;
};

static void fixing_procs_info_op(DbFixation* fix, void* vctx)
{
    struct fixing_procs_info_ctx* ctx = (struct fixing_procs_info_ctx*) vctx;
    Eterm* hp;
    Eterm tpl;

    hp = HAllocX(ctx->p, 5, 100);
    tpl = TUPLE2(hp, fix->procs.p->common.id, make_small(fix->counter));
    hp += 3;
    ctx->list = CONS(hp, tpl, ctx->list);
}

static Eterm table_info(Process* p, DbTable* tb, Eterm What)
{
    Eterm ret = THE_NON_VALUE;
    int use_monotonic;

    if (What == am_size) {
	ret = make_small(erts_atomic_read_nob(&tb->common.nitems));
    } else if (What == am_type) {
	if (tb->common.status & DB_SET)  {
	    ret = am_set;
	} else if (tb->common.status & DB_DUPLICATE_BAG) {
	    ret = am_duplicate_bag;
	} else if (tb->common.status & DB_ORDERED_SET) {
	    ret = am_ordered_set;
	} else { /*TT*/
	    ASSERT(tb->common.status & DB_BAG);
	    ret = am_bag;
	}
    } else if (What == am_memory) {
	Uint words = (Uint) ((erts_atomic_read_nob(&tb->common.memory_size)
			      + sizeof(Uint)
			      - 1)
			     / sizeof(Uint));
	ret = erts_make_integer(words, p);
    } else if (What == am_owner) {
	ret = tb->common.owner;
    } else if (What == am_heir) {
	ret = tb->common.heir;
    } else if (What == am_protection) {
	if (tb->common.status & DB_PRIVATE) 
	    ret = am_private;
	else if (tb->common.status & DB_PROTECTED)
	    ret = am_protected;
	else if (tb->common.status & DB_PUBLIC)
	    ret = am_public;
    } else if (What == am_write_concurrency) {
        ret = tb->common.status & DB_FINE_LOCKED ? am_true : am_false;
    } else if (What == am_read_concurrency) {
        ret = tb->common.status & DB_FREQ_READ ? am_true : am_false;
    } else if (What == am_name) {
	ret = tb->common.the_name;
    } else if (What == am_keypos) {
	ret = make_small(tb->common.keypos);
    } else if (What == am_node) {
	ret = erts_this_dist_entry->sysname;
    } else if (What == am_named_table) {
	ret = is_table_named(tb) ? am_true : am_false;
    } else if (What == am_compressed) {
	ret = tb->common.compress ? am_true : am_false;
    } else if (What == am_id) {
        ret = make_tid(p, tb);
    }

    /*
     * For debugging purposes
     */
    else if (What == am_data) {
	print_table(ERTS_PRINT_STDOUT, NULL, 1, tb);
	ret = am_true;
    } else if (What == am_atom_put("fixed",5)) { 
	if (IS_FIXED(tb))
	    ret = am_true;
	else
	    ret = am_false;
    } else if ((use_monotonic
		= ERTS_IS_ATOM_STR("safe_fixed_monotonic_time",
				   What))
	       || ERTS_IS_ATOM_STR("safe_fixed", What)) {
	erts_mtx_lock(&tb->common.fixlock);
	if (IS_FIXED(tb)) {
	    Uint need;
	    Eterm *hp;
	    Eterm time;
	    Sint64 mtime;
	    struct fixing_procs_info_ctx ctx;
	    
	    need = 3;
	    if (use_monotonic) {
		mtime = (Sint64) tb->common.time.monotonic;
		mtime += ERTS_MONOTONIC_OFFSET_NATIVE;
		if (!IS_SSMALL(mtime))
		    need += ERTS_SINT64_HEAP_SIZE(mtime);
	    }
	    else {
		mtime = 0;
		need += 4;
	    }
	    ctx.p = p;
	    ctx.list = NIL;
	    fixing_procs_rbt_foreach(tb->common.fixing_procs,
				     fixing_procs_info_op,
				     &ctx);

	    hp = HAlloc(p, need);
	    if (use_monotonic)
		time = (IS_SSMALL(mtime)
		       ? make_small(mtime)
		       : erts_sint64_to_big(mtime, &hp));
	    else {
		Uint ms, s, us;
		erts_make_timestamp_value(&ms, &s, &us,
					  tb->common.time.monotonic,
					  tb->common.time.offset);
		time = TUPLE3(hp, make_small(ms), make_small(s), make_small(us));
		hp += 4;
	    }
	    ret = TUPLE2(hp, time, ctx.list);
	} else {
	    ret = am_false;
	}
	erts_mtx_unlock(&tb->common.fixlock);
    } else if (What == am_atom_put("stats",5)) {
	if (IS_HASH_TABLE(tb->common.status)) {
	    FloatDef f;
	    DbHashStats stats;
	    Eterm avg, std_dev_real, std_dev_exp;
	    Eterm* hp;

	    db_calc_stats_hash(&tb->hash, &stats);
	    hp = HAlloc(p, 1 + 7 + FLOAT_SIZE_OBJECT*3);
	    f.fd = stats.avg_chain_len;
	    avg = make_float(hp);
	    PUT_DOUBLE(f, hp);
	    hp += FLOAT_SIZE_OBJECT;

	    f.fd = stats.std_dev_chain_len;
	    std_dev_real = make_float(hp);
	    PUT_DOUBLE(f, hp);
	    hp += FLOAT_SIZE_OBJECT;
	    
	    f.fd = stats.std_dev_expected;
	    std_dev_exp = make_float(hp);
	    PUT_DOUBLE(f, hp);
	    hp += FLOAT_SIZE_OBJECT;
	    ret = TUPLE7(hp, make_small(erts_atomic_read_nob(&tb->hash.nactive)),
			 avg, std_dev_real, std_dev_exp,
			 make_small(stats.min_chain_len),
			 make_small(stats.max_chain_len),
			 make_small(stats.kept_items));
	}
	else {
	    ret = am_false;
	}
    }
    return ret;
}

static void print_table(fmtfn_t to, void *to_arg, int show,  DbTable* tb)
{
    Eterm tid;
    Eterm heap[ERTS_MAGIC_REF_THING_SIZE];

    if (is_table_named(tb)) {
        tid = tb->common.the_name;
    } else {
        ErlOffHeap oh;
        ERTS_INIT_OFF_HEAP(&oh);
        write_magic_ref_thing(heap, &oh, (ErtsMagicBinary *) tb->common.btid);
        tid = make_internal_ref(heap);
    }

    erts_print(to, to_arg, "Table: %T\n", tid);
    erts_print(to, to_arg, "Name: %T\n", tb->common.the_name);

    tb->common.meth->db_print(to, to_arg, show, tb);

    erts_print(to, to_arg, "Objects: %d\n", (int)erts_atomic_read_nob(&tb->common.nitems));
    erts_print(to, to_arg, "Words: %bpu\n",
	       (Uint) ((erts_atomic_read_nob(&tb->common.memory_size)
			+ sizeof(Uint)
			- 1)
		       / sizeof(Uint)));
    erts_print(to, to_arg, "Type: %T\n", table_info(NULL, tb, am_type));
    erts_print(to, to_arg, "Protection: %T\n", table_info(NULL, tb, am_protection));
    erts_print(to, to_arg, "Compressed: %T\n", table_info(NULL, tb, am_compressed));
    erts_print(to, to_arg, "Write Concurrency: %T\n", table_info(NULL, tb, am_write_concurrency));
    erts_print(to, to_arg, "Read Concurrency: %T\n", table_info(NULL, tb, am_read_concurrency));
}

typedef struct {
    fmtfn_t to;
    void *to_arg;
    int show;
} ErtsPrintDbInfo;

static void
db_info_print(DbTable *tb, void *vpdbip)
{
    ErtsPrintDbInfo *pdbip = (ErtsPrintDbInfo *) vpdbip;
    erts_print(pdbip->to, pdbip->to_arg, "=ets:%T\n", tb->common.owner);
    erts_print(pdbip->to, pdbip->to_arg, "Slot: %bpu\n", (Uint) tb);
    print_table(pdbip->to, pdbip->to_arg, pdbip->show, tb);
}

void db_info(fmtfn_t to, void *to_arg, int show)    /* Called by break handler */
{
    ErtsPrintDbInfo pdbi;

    pdbi.to = to;
    pdbi.to_arg = to_arg;
    pdbi.show = show;

    erts_db_foreach_table(db_info_print, &pdbi);
}

Uint
erts_get_ets_misc_mem_size(void)
{
    ERTS_THR_MEMORY_BARRIER;
    /* Memory not allocated in ets_alloc */
    return (Uint) erts_atomic_read_nob(&erts_ets_misc_mem_size);
}

/* SMP Note: May only be used when system is locked */
void
erts_db_foreach_table(void (*func)(DbTable *, void *), void *arg)
{
    int ix;

    ASSERT(erts_thr_progress_is_blocking());

    for (ix = 0; ix < erts_no_schedulers; ix++) {
        ErtsSchedulerData *esdp = ERTS_SCHEDULER_IX(ix);
        DbTable *first = esdp->ets_tables.clist;
        if (first) {
            DbTable *tb = first;
            do {
                if (is_table_alive(tb))
                    (*func)(tb, arg);
                tb = tb->common.all.next;
            } while (tb != first);
        }
    }
}

/* SMP Note: May only be used when system is locked */
void
erts_db_foreach_offheap(DbTable *tb,
			void (*func)(ErlOffHeap *, void *),
			void *arg)
{
    tb->common.meth->db_foreach_offheap(tb, func, arg);
}

/* retrieve max number of ets tables */
Uint
erts_db_get_max_tabs()
{
    return db_max_tabs;
}

/*
 * For testing of meta tables only.
 *
 * Given a name atom (as returned from ets:new/2), return a list of 'cnt'
 * number of other names that will hash to the same bucket in meta_name_tab.
 *
 * WARNING: Will bloat the atom table!
 */
Eterm
erts_ets_colliding_names(Process* p, Eterm name, Uint cnt)
{
    Eterm list = NIL;
    Eterm* hp = HAlloc(p,cnt*2);
    Uint index = atom_val(name) & meta_name_tab_mask;

    while (cnt) {
        if (index != atom_val(name)) {
            while (index >= atom_table_size()) {
                char tmp[20];
                erts_snprintf(tmp, sizeof(tmp), "am%x", atom_table_size());
                erts_atom_put((byte *) tmp, sys_strlen(tmp), ERTS_ATOM_ENC_LATIN1, 1);
            }
            list = CONS(hp, make_atom(index), list);
            hp += 2;
            --cnt;
        }
        index += meta_name_tab_mask + 1;
    }
    return list;
}

#ifdef ERTS_ENABLE_LOCK_COUNT

void erts_lcnt_enable_db_lock_count(DbTable *tb, int enable) {
    if(enable) {
        erts_lcnt_install_new_lock_info(&tb->common.rwlock.lcnt, "db_tab",
            tb->common.the_name, ERTS_LOCK_TYPE_RWMUTEX | ERTS_LOCK_FLAGS_CATEGORY_DB);
        erts_lcnt_install_new_lock_info(&tb->common.fixlock.lcnt, "db_tab_fix",
            tb->common.the_name, ERTS_LOCK_TYPE_MUTEX | ERTS_LOCK_FLAGS_CATEGORY_DB);
    } else {
        erts_lcnt_uninstall(&tb->common.rwlock.lcnt);
        erts_lcnt_uninstall(&tb->common.fixlock.lcnt);
    }

    if(IS_HASH_TABLE(tb->common.status)) {
        erts_lcnt_enable_db_hash_lock_count(&tb->hash, enable);
    }
}

static void lcnt_update_db_locks_per_sched(void *enable) {
    ErtsSchedulerData *esdp;
    DbTable *head;

    esdp = erts_get_scheduler_data();
    head = esdp->ets_tables.clist;

    if(head) {
        DbTable *iterator = head;

        do {
            if(is_table_alive(iterator)) {
                erts_lcnt_enable_db_lock_count(iterator, !!enable);
            }

            iterator = iterator->common.all.next;
        } while (iterator != head);
    }
}

void erts_lcnt_update_db_locks(int enable) {
    erts_schedule_multi_misc_aux_work(0, erts_no_schedulers,
        &lcnt_update_db_locks_per_sched, (void*)(UWord)enable);
}

#endif /* ERTS_ENABLE_LOCK_COUNT */
