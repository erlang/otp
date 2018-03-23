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

#include "global.h"
#include "erl_node_tables.h"
#include "dist.h"
#include "big.h"
#include "error.h"
#include "erl_thr_progress.h"
#include "dtrace-wrapper.h"
#include "erl_binary.h"
#include "erl_bif_unique.h"
#include "erl_proc_sig_queue.h"

Hash erts_dist_table;
Hash erts_node_table;
erts_rwmtx_t erts_dist_table_rwmtx;
erts_rwmtx_t erts_node_table_rwmtx;

DistEntry *erts_hidden_dist_entries;
DistEntry *erts_visible_dist_entries;
DistEntry *erts_pending_dist_entries;
DistEntry *erts_not_connected_dist_entries; /* including erts_this_dist_entry */
Sint erts_no_of_hidden_dist_entries;
Sint erts_no_of_visible_dist_entries;
Sint erts_no_of_pending_dist_entries;
Sint erts_no_of_not_connected_dist_entries; /* including erts_this_dist_entry */

DistEntry *erts_this_dist_entry;
ErlNode *erts_this_node;
char erts_this_node_sysname_BUFFER[256],
    *erts_this_node_sysname = "uninitialized yet";

static Uint node_entries = 0;
static Uint dist_entries = 0;

static int references_atoms_need_init = 1;

static ErtsMonotonicTime orig_node_tab_delete_delay;
static ErtsMonotonicTime node_tab_delete_delay;

/* -- The distribution table ---------------------------------------------- */

#define ErtsBin2DistEntry(B) \
    ((DistEntry *) ERTS_MAGIC_BIN_DATA((B)))
#define ErtsDistEntry2Bin(DEP) \
    ((Binary *) ERTS_MAGIC_BIN_FROM_DATA((DEP)))

static ERTS_INLINE erts_aint_t
de_refc_read(DistEntry *dep, erts_aint_t min)
{
    return erts_refc_read(&ErtsDistEntry2Bin(dep)->intern.refc, min);
}

static ERTS_INLINE erts_aint_t
de_refc_inc_read(DistEntry *dep, erts_aint_t min)
{
    return erts_refc_inctest(&ErtsDistEntry2Bin(dep)->intern.refc, min);
}

static ERTS_INLINE void
de_refc_inc(DistEntry *dep, erts_aint_t min)
{
    erts_refc_inc(&ErtsDistEntry2Bin(dep)->intern.refc, min);
}

static ERTS_INLINE void
de_refc_dec(DistEntry *dep, erts_aint_t min)
{
#ifdef DEBUG
    (void) erts_refc_read(&ErtsDistEntry2Bin(dep)->intern.refc, min+1);
#endif
    erts_bin_release(ErtsDistEntry2Bin(dep));
}

static ERTS_INLINE erts_aint_t
de_refc_dec_read(DistEntry *dep, erts_aint_t min)
{
    return erts_refc_dectest(&ErtsDistEntry2Bin(dep)->intern.refc, min);
}

void
erts_ref_dist_entry(DistEntry *dep)
{
    ASSERT(dep);
    if (de_refc_inc_read(dep, 1) == 1) {
        de_refc_inc(dep, 2);  /* Pending delete */
    }
}

void
erts_deref_dist_entry(DistEntry *dep)
{
    ASSERT(dep);
    de_refc_dec(dep, 0);
}

#ifdef DEBUG
static int
is_in_de_list(DistEntry *dep, DistEntry *dep_list)
{
    DistEntry *tdep;
    for(tdep = dep_list; tdep; tdep = tdep->next)
	if(tdep == dep)
	    return 1;
    return 0;
}
#endif

static HashValue
dist_table_hash(void *dep)
{
    return atom_tab(atom_val(((DistEntry *) dep)->sysname))->slot.bucket.hvalue;
}

static int
dist_table_cmp(void *dep1, void *dep2)
{
    return (((DistEntry *) dep1)->sysname == ((DistEntry *) dep2)->sysname
	    ? 0 : 1);
}

static void*
dist_table_alloc(void *dep_tmpl)
{
    erts_aint_t refc;
    Eterm sysname;
    Binary *bin;
    DistEntry *dep;
    erts_rwmtx_opt_t rwmtx_opt = ERTS_RWMTX_OPT_DEFAULT_INITER;
    rwmtx_opt.type = ERTS_RWMTX_TYPE_FREQUENT_READ;

    sysname = ((DistEntry *) dep_tmpl)->sysname;

    bin = erts_create_magic_binary_x(sizeof(DistEntry),
                                     erts_dist_entry_destructor,
                                     ERTS_ALC_T_DIST_ENTRY,
                                     0);
    dep = ErtsBin2DistEntry(bin);

    dist_entries++;

    refc = de_refc_dec_read(dep, -1);
    ASSERT(refc == -1); (void)refc;

    dep->prev				= NULL;
    erts_rwmtx_init_opt(&dep->rwmtx, &rwmtx_opt, "dist_entry", sysname,
        ERTS_LOCK_FLAGS_CATEGORY_DISTRIBUTION);
    dep->sysname			= sysname;
    dep->cid				= NIL;
    erts_atomic_init_nob(&dep->input_handler, (erts_aint_t) NIL);
    dep->connection_id			= 0;
    dep->state				= ERTS_DE_STATE_IDLE;
    dep->flags				= 0;
    dep->version			= 0;

    dep->mld                            = NULL;

    erts_mtx_init(&dep->qlock, "dist_entry_out_queue", sysname,
        ERTS_LOCK_FLAGS_CATEGORY_DISTRIBUTION);
    erts_atomic32_init_nob(&dep->qflgs, 0);
    erts_atomic_init_nob(&dep->qsize, 0);
    erts_atomic64_init_nob(&dep->in, 0);
    erts_atomic64_init_nob(&dep->out, 0);
    dep->out_queue.first		= NULL;
    dep->out_queue.last			= NULL;
    dep->suspended			= NULL;

    dep->tmp_out_queue.first    	= NULL;
    dep->tmp_out_queue.last     	= NULL;
    dep->finalized_out_queue.first	= NULL;
    dep->finalized_out_queue.last	= NULL;

    erts_atomic_init_nob(&dep->dist_cmd_scheduled, 0);
    erts_port_task_handle_init(&dep->dist_cmd);
    dep->send				= NULL;
    dep->cache				= NULL;
    dep->transcode_ctx                  = NULL;

    /* Link in */

    /* All new dist entries are "not connected".
     * erts_this_dist_entry is also always included among "not connected"
     */
    dep->next = erts_not_connected_dist_entries;
    if(erts_not_connected_dist_entries) {
	ASSERT(erts_not_connected_dist_entries->prev == NULL);
	erts_not_connected_dist_entries->prev = dep;
    }
    erts_not_connected_dist_entries = dep;
    erts_no_of_not_connected_dist_entries++;

    return (void *) dep;
}

static void
dist_table_free(void *vdep)
{
    DistEntry *dep = (DistEntry *) vdep;

    ASSERT(de_refc_read(dep, -1) == -1);
    ASSERT(dep->state == ERTS_DE_STATE_IDLE);
    ASSERT(is_nil(dep->cid));
    ASSERT(dep->mld == NULL);

    /* Link out */

    /* All dist entries about to be removed are "not connected" */

    if(dep->prev) {
	ASSERT(is_in_de_list(dep, erts_not_connected_dist_entries));
	dep->prev->next = dep->next;
    }
    else {
	ASSERT(erts_not_connected_dist_entries == dep);
	erts_not_connected_dist_entries = dep->next;
    }

    if(dep->next)
	dep->next->prev = dep->prev;

    ASSERT(erts_no_of_not_connected_dist_entries > 0);
    erts_no_of_not_connected_dist_entries--;

    ASSERT(!dep->cache);
    erts_rwmtx_destroy(&dep->rwmtx);
    erts_mtx_destroy(&dep->qlock);

#ifdef DEBUG
    sys_memset(vdep, 0x77, sizeof(DistEntry));
#endif
    erts_bin_free(ErtsDistEntry2Bin(dep));

    ASSERT(dist_entries > 0);
    dist_entries--;
}


void
erts_dist_table_info(fmtfn_t to, void *to_arg)
{
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	erts_rwmtx_rlock(&erts_dist_table_rwmtx);
    hash_info(to, to_arg, &erts_dist_table);
    if (lock)
	erts_rwmtx_runlock(&erts_dist_table_rwmtx);
}

static ERTS_INLINE DistEntry *find_dist_entry(Eterm sysname,
                                              int inc_refc,
                                              int connected_only)
{
    DistEntry *res;
    DistEntry de;
    de.sysname = sysname;
    erts_rwmtx_rlock(&erts_dist_table_rwmtx);
    res = hash_get(&erts_dist_table, (void *) &de);
    if (res) {
        if (connected_only && is_nil(res->cid))
            res = NULL;
        else {
            int pend_delete;
            erts_aint_t refc;
            if (inc_refc) {
                refc = de_refc_inc_read(res, 1);
                pend_delete = refc < 2;
            }
            else {
                refc = de_refc_read(res, 0);
                pend_delete = refc < 1;
            }
            if (pend_delete) /* Pending delete */
                de_refc_inc(res, 1);
        }
    }
    erts_rwmtx_runlock(&erts_dist_table_rwmtx);
    return res;
}

DistEntry *
erts_channel_no_to_dist_entry(Uint cno)
{
    /*
     * Does NOT increase reference count!
     */

/*
 * For this node (and previous incarnations of this node),
 * ERST_INTERNAL_CHANNEL_NO (will always be 0 I guess) is used as
 * channel no. For other nodes, the atom index of the atom corresponding
 * to the node name is used as channel no.
 */
    if (cno == ERST_INTERNAL_CHANNEL_NO)
	return erts_this_dist_entry;

    if((cno > MAX_ATOM_INDEX)
       || (cno >= atom_table_size())
       ||  (atom_tab(cno) == NULL))
	return NULL;
        
    /* cno is a valid atom index; find corresponding dist entry (if there
       is one) */
    return find_dist_entry(make_atom(cno), 0, 0);
}

DistEntry *
erts_sysname_to_connected_dist_entry(Eterm sysname)
{
    /*
     * Does NOT increase reference count!
     */
    if(erts_this_dist_entry->sysname == sysname)
	return erts_this_dist_entry;
    return find_dist_entry(sysname, 0, 1);
}

DistEntry *erts_find_or_insert_dist_entry(Eterm sysname)
{
    /*
     * This function DOES increase reference count!
     */
    DistEntry *res;
    DistEntry de;
    erts_aint_t refc;
    res = find_dist_entry(sysname, 1, 0);
    if (res)
	return res;
    de.sysname = sysname;
    erts_rwmtx_rwlock(&erts_dist_table_rwmtx);
    res = hash_put(&erts_dist_table, (void *) &de);
    refc = de_refc_inc_read(res, 0);
    if (refc < 2) /* New or pending delete */
        de_refc_inc(res, 1);
    erts_rwmtx_rwunlock(&erts_dist_table_rwmtx);
    return res;
}

DistEntry *erts_find_dist_entry(Eterm sysname)
{
    /*
     * Does NOT increase reference count!
     */
    return find_dist_entry(sysname, 0, 0);
}

DistEntry *
erts_dhandle_to_dist_entry(Eterm dhandle)
{
    Binary *bin;
    if (!is_internal_magic_ref(dhandle))
        return NULL;
    bin = erts_magic_ref2bin(dhandle);
    if (ERTS_MAGIC_BIN_DESTRUCTOR(bin) != erts_dist_entry_destructor)
        return NULL;
    return ErtsBin2DistEntry(bin);
}

Eterm
erts_build_dhandle(Eterm **hpp, ErlOffHeap* ohp, DistEntry *dep)
{
    Binary *bin = ErtsDistEntry2Bin(dep);
    ASSERT(bin);
    ASSERT(ERTS_MAGIC_BIN_DESTRUCTOR(bin) == erts_dist_entry_destructor);
    return erts_mk_magic_ref(hpp, ohp, bin);
}

Eterm
erts_make_dhandle(Process *c_p, DistEntry *dep)
{
    Eterm *hp = HAlloc(c_p, ERTS_MAGIC_REF_THING_SIZE);
    return erts_build_dhandle(&hp, &c_p->off_heap, dep);
}

static void start_timer_delete_dist_entry(void *vdep);
static void prepare_try_delete_dist_entry(void *vdep);
static void try_delete_dist_entry(DistEntry*);

static void schedule_delete_dist_entry(DistEntry* dep)
{
    /*
     * Here we need thread progress to wait for other threads, that may have
     * done lookup without refc++, to do either refc++ or drop their refs.
     *
     * Note that timeouts do not guarantee thread progress.
     */
    erts_schedule_thr_prgr_later_op(start_timer_delete_dist_entry,
                                    dep, &dep->later_op);
}

static void
start_timer_delete_dist_entry(void *vdep)
{
    if (node_tab_delete_delay == 0) {
        prepare_try_delete_dist_entry(vdep);
    }
    else {
        ASSERT(node_tab_delete_delay > 0);
        erts_start_timer_callback(node_tab_delete_delay,
                                  prepare_try_delete_dist_entry,
                                  vdep);
    }
}

static void
prepare_try_delete_dist_entry(void *vdep)
{
    DistEntry *dep = vdep;
    erts_aint_t refc;

    /*
     * Time has passed since we decremented refc to zero and DistEntry may
     * have been revived. Do a fast check without table lock first.
     */
    refc = de_refc_read(dep, 0);
    if (refc == 0) {
        try_delete_dist_entry(dep);
    }
    else {
        /*
         * Someone has done lookup and done refc++ for us.
         */
        refc = de_refc_dec_read(dep, 0);
        if (refc == 0)
            schedule_delete_dist_entry(dep);
    }
}

static void try_delete_dist_entry(DistEntry* dep)
{
    erts_aint_t refc;

    erts_rwmtx_rwlock(&erts_dist_table_rwmtx);
    /*
     * Another thread might have looked up this dist entry after
     * we decided to delete it (refc became zero). If so, the other
     * thread incremented refc one extra step for this thread.
     *
     * If refc reach -1, no one has done lookup and no one can do lookup
     * as we have table lock. Delete the entry.
     *
     * If refc reach 0, someone raced us and either
     *  (1) did lookup with own refc++ and already released it again
     *  (2) did lookup without own refc++
     * Schedule new delete operation.
     *
     * If refc > 0, the entry is in use. Keep the entry.
     */
    refc = de_refc_dec_read(dep, -1);
    if (refc == -1)
	(void) hash_erase(&erts_dist_table, (void *) dep);
    erts_rwmtx_rwunlock(&erts_dist_table_rwmtx);

    if (refc == 0) {
        schedule_delete_dist_entry(dep);
    }
}

int erts_dist_entry_destructor(Binary *bin)
{
    DistEntry *dep = ErtsBin2DistEntry(bin);
    erts_aint_t refc;

    refc = de_refc_read(dep, -1);

    if (refc == -1)
        return 1; /* Allow deallocation of structure... */

    schedule_delete_dist_entry(dep);

    return 0;
}

Uint
erts_dist_table_size(void)
{
    Uint res;
#ifdef DEBUG
    HashInfo hi;
    DistEntry *dep;
    int i;
#endif
    int lock = !ERTS_IS_CRASH_DUMPING;

    if (lock)
	erts_rwmtx_rlock(&erts_dist_table_rwmtx);
#ifdef DEBUG
    hash_get_info(&hi, &erts_dist_table);
    ASSERT(dist_entries == hi.objs);

    i = 0;
    for(dep = erts_visible_dist_entries; dep; dep = dep->next)
	i++;
    ASSERT(i == erts_no_of_visible_dist_entries);
    i = 0;
    for(dep = erts_hidden_dist_entries; dep; dep = dep->next)
	i++;
    ASSERT(i == erts_no_of_hidden_dist_entries);
    i = 0;
    for(dep = erts_pending_dist_entries; dep; dep = dep->next)
        i++;
    ASSERT(i == erts_no_of_pending_dist_entries);
    i = 0;
    for(dep = erts_not_connected_dist_entries; dep; dep = dep->next)
	i++;
    ASSERT(i == erts_no_of_not_connected_dist_entries);

    ASSERT(dist_entries == (erts_no_of_visible_dist_entries
			    + erts_no_of_hidden_dist_entries
                            + erts_no_of_pending_dist_entries
			    + erts_no_of_not_connected_dist_entries));
#endif

    res = (hash_table_sz(&erts_dist_table)
	   + dist_entries*sizeof(DistEntry)
	   + erts_dist_cache_size());
    if (lock)
	erts_rwmtx_runlock(&erts_dist_table_rwmtx);
    return res;
}

void
erts_set_dist_entry_not_connected(DistEntry *dep)
{
    DistEntry** head;

    ERTS_LC_ASSERT(erts_lc_is_de_rwlocked(dep));
    erts_rwmtx_rwlock(&erts_dist_table_rwmtx);

    ASSERT(dep != erts_this_dist_entry);

    if (dep->state == ERTS_DE_STATE_PENDING) {
        ASSERT(is_nil(dep->cid));
        ASSERT(erts_no_of_pending_dist_entries > 0);
        erts_no_of_pending_dist_entries--;
        head = &erts_pending_dist_entries;
    }
    else {
        ASSERT(dep->state != ERTS_DE_STATE_IDLE);
        ASSERT(is_internal_port(dep->cid) || is_internal_pid(dep->cid));
        if (dep->flags & DFLAG_PUBLISHED) {
            ASSERT(erts_no_of_visible_dist_entries > 0);
            erts_no_of_visible_dist_entries--;
            head = &erts_visible_dist_entries;
        }
        else {
            ASSERT(erts_no_of_hidden_dist_entries > 0);
            erts_no_of_hidden_dist_entries--;
            head = &erts_hidden_dist_entries;
        }
    }

    if(dep->prev) {
        ASSERT(is_in_de_list(dep, *head));
        dep->prev->next = dep->next;
    }
    else {
        ASSERT(*head == dep);
        *head = dep->next;
    }
    if(dep->next)
	dep->next->prev = dep->prev;

    dep->state = ERTS_DE_STATE_EXITING;
    dep->flags = 0;
    dep->prev = NULL;
    dep->cid = NIL;

    dep->next = erts_not_connected_dist_entries;
    if(erts_not_connected_dist_entries) {
	ASSERT(erts_not_connected_dist_entries->prev == NULL);
	erts_not_connected_dist_entries->prev = dep;
    }
    erts_not_connected_dist_entries = dep;
    erts_no_of_not_connected_dist_entries++;
    erts_rwmtx_rwunlock(&erts_dist_table_rwmtx);
}

void
erts_set_dist_entry_pending(DistEntry *dep)
{
    ErtsMonLnkDist *mld = erts_mon_link_dist_create(dep->sysname);
    ERTS_LC_ASSERT(erts_lc_is_de_rwlocked(dep));
    
    erts_rwmtx_rwlock(&erts_dist_table_rwmtx);

    ASSERT(dep != erts_this_dist_entry);
    ASSERT(dep->state == ERTS_DE_STATE_IDLE);
    ASSERT(is_nil(dep->cid));

    if(dep->prev) {
        ASSERT(is_in_de_list(dep, erts_not_connected_dist_entries));
        dep->prev->next = dep->next;
    }
    else {
        ASSERT(dep == erts_not_connected_dist_entries);
        erts_not_connected_dist_entries = dep->next;
    }

    if(dep->next)
	dep->next->prev = dep->prev;

    erts_no_of_not_connected_dist_entries--;

    dep->state = ERTS_DE_STATE_PENDING;
    dep->flags = (DFLAG_DIST_MANDATORY | DFLAG_DIST_HOPEFULLY | DFLAG_NO_MAGIC);
    dep->connection_id = (dep->connection_id + 1) & ERTS_DIST_CON_ID_MASK;

    ASSERT(!dep->mld);
    mld->connection_id = dep->connection_id;
    dep->mld = mld;

    dep->prev = NULL;
    dep->next = erts_pending_dist_entries;
    if(erts_pending_dist_entries) {
	ASSERT(erts_pending_dist_entries->prev == NULL);
	erts_pending_dist_entries->prev = dep;
    }
    erts_pending_dist_entries = dep;
    erts_no_of_pending_dist_entries++;
    erts_rwmtx_rwunlock(&erts_dist_table_rwmtx);
}

void
erts_set_dist_entry_connected(DistEntry *dep, Eterm cid, Uint flags)
{
    erts_aint32_t set_qflgs;

    ASSERT(dep->mld);

    ERTS_LC_ASSERT(erts_lc_is_de_rwlocked(dep));
    erts_rwmtx_rwlock(&erts_dist_table_rwmtx);

    ASSERT(dep != erts_this_dist_entry);
    ASSERT(is_nil(dep->cid));
    ASSERT(dep->state == ERTS_DE_STATE_PENDING);
    ASSERT(is_internal_port(cid) || is_internal_pid(cid));

    if(dep->prev) {
	ASSERT(is_in_de_list(dep, erts_pending_dist_entries));
	dep->prev->next = dep->next;
    }
    else {
	ASSERT(erts_pending_dist_entries == dep);
	erts_pending_dist_entries = dep->next;
    }

    if(dep->next)
	dep->next->prev = dep->prev;

    ASSERT(erts_no_of_pending_dist_entries > 0);
    erts_no_of_pending_dist_entries--;

    dep->state = ERTS_DE_STATE_CONNECTED;
    dep->flags = flags & ~DFLAG_NO_MAGIC;
    dep->cid = cid;
    erts_atomic_set_nob(&dep->input_handler,
                            (erts_aint_t) cid);

    dep->prev = NULL;

    erts_atomic64_set_nob(&dep->in, 0);
    erts_atomic64_set_nob(&dep->out, 0);
    set_qflgs = (is_internal_port(cid) ?
                 ERTS_DE_QFLG_PORT_CTRL : ERTS_DE_QFLG_PROC_CTRL);
    erts_atomic32_read_bor_nob(&dep->qflgs, set_qflgs);

    if(flags & DFLAG_PUBLISHED) {
	dep->next = erts_visible_dist_entries;
	if(erts_visible_dist_entries) {
	    ASSERT(erts_visible_dist_entries->prev == NULL);
	    erts_visible_dist_entries->prev = dep;
	}
	erts_visible_dist_entries = dep;
	erts_no_of_visible_dist_entries++;
    }
    else {
	dep->next = erts_hidden_dist_entries;
	if(erts_hidden_dist_entries) {
	    ASSERT(erts_hidden_dist_entries->prev == NULL);
	    erts_hidden_dist_entries->prev = dep;
	}
	erts_hidden_dist_entries = dep;
	erts_no_of_hidden_dist_entries++;
    }
    erts_rwmtx_rwunlock(&erts_dist_table_rwmtx);
}

/* -- Node table --------------------------------------------------------- */

/* Some large primes */
#define PRIME0 ((HashValue) 268438039)
#define PRIME1 ((HashValue) 268440479)
#define PRIME2 ((HashValue) 268439161)
#define PRIME3 ((HashValue) 268437017)

static HashValue
node_table_hash(void *venp)
{
    Uint32 cre = ((ErlNode *) venp)->creation;
    HashValue h = atom_tab(atom_val(((ErlNode *) venp)->sysname))->slot.bucket.hvalue;

    return (h + cre) * PRIME0;
}

static int
node_table_cmp(void *venp1, void *venp2)
{
    return ((((ErlNode *) venp1)->sysname == ((ErlNode *) venp2)->sysname
	     && ((ErlNode *) venp1)->creation == ((ErlNode *) venp2)->creation)
	    ? 0
	    : 1);
}

static void*
node_table_alloc(void *venp_tmpl)
{
    ErlNode *enp;

    enp = (ErlNode *) erts_alloc(ERTS_ALC_T_NODE_ENTRY, sizeof(ErlNode));

    node_entries++;

    erts_refc_init(&enp->refc, -1);
    enp->creation = ((ErlNode *) venp_tmpl)->creation;
    enp->sysname = ((ErlNode *) venp_tmpl)->sysname;
    enp->dist_entry = erts_find_or_insert_dist_entry(((ErlNode *) venp_tmpl)->sysname);

    return (void *) enp;
}

static void
node_table_free(void *venp)
{
    ErlNode *enp = (ErlNode *) venp;

    ERTS_LC_ASSERT(enp != erts_this_node || erts_thr_progress_is_blocking());

    erts_deref_dist_entry(enp->dist_entry);
#ifdef DEBUG
    sys_memset(venp, 0x55, sizeof(ErlNode));
#endif
    erts_free(ERTS_ALC_T_NODE_ENTRY, venp);

    ASSERT(node_entries > 0);
    node_entries--;
}

Uint
erts_node_table_size(void)
{
    Uint res;
#ifdef DEBUG
    HashInfo hi;
#endif
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	erts_rwmtx_rlock(&erts_node_table_rwmtx);
#ifdef DEBUG
    hash_get_info(&hi, &erts_node_table);
    ASSERT(node_entries == hi.objs);
#endif
    res = hash_table_sz(&erts_node_table) + node_entries*sizeof(ErlNode);
    if (lock)
	erts_rwmtx_runlock(&erts_node_table_rwmtx);
    return res;
}

void
erts_node_table_info(fmtfn_t to, void *to_arg)
{
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	erts_rwmtx_rlock(&erts_node_table_rwmtx);
    hash_info(to, to_arg, &erts_node_table);
    if (lock)
	erts_rwmtx_runlock(&erts_node_table_rwmtx);
}


ErlNode *erts_find_or_insert_node(Eterm sysname, Uint32 creation)
{
    ErlNode *res;
    ErlNode ne;
    ne.sysname = sysname;
    ne.creation = creation;

    erts_rwmtx_rlock(&erts_node_table_rwmtx);
    res = hash_get(&erts_node_table, (void *) &ne);
    if (res && res != erts_this_node) {
	erts_aint_t refc = erts_refc_inctest(&res->refc, 0);
	if (refc < 2) /* New or pending delete */
	    erts_refc_inc(&res->refc, 1);
    }
    erts_rwmtx_runlock(&erts_node_table_rwmtx);
    if (res)
	return res;

    erts_rwmtx_rwlock(&erts_node_table_rwmtx);
    res = hash_put(&erts_node_table, (void *) &ne);
    ASSERT(res);
    if (res != erts_this_node) {
	erts_aint_t refc = erts_refc_inctest(&res->refc, 0);
	if (refc < 2) /* New or pending delete */
	    erts_refc_inc(&res->refc, 1);
    }
    erts_rwmtx_rwunlock(&erts_node_table_rwmtx);
    return res;
}

static void try_delete_node(void *venp)
{
    ErlNode *enp = (ErlNode *) venp;
    erts_aint_t refc;

    erts_rwmtx_rwlock(&erts_node_table_rwmtx);
    /*
     * Another thread might have looked up this node after we
     * decided to delete it (refc became zero). If so, the other
     * thread incremented refc twice. Once for the new reference
     * and once for this thread.
     *
     * If refc reach -1, no one has used the entry since we
     * set up the timer. Delete the entry.
     *
     * If refc reach 0, the entry is currently not in use
     * but has been used since we set up the timer. Set up a
     * new timer.
     *
     * If refc > 0, the entry is in use. Keep the entry.
     */
    refc = erts_refc_dectest(&enp->refc, -1);
    if (refc == -1)
	(void) hash_erase(&erts_node_table, (void *) enp);
    erts_rwmtx_rwunlock(&erts_node_table_rwmtx);

    if (refc == 0)
	erts_schedule_delete_node(enp);
}

void erts_schedule_delete_node(ErlNode *enp)
{
    ASSERT(enp != erts_this_node);
    if (enp != erts_this_node) {
	if (node_tab_delete_delay == 0)
	    try_delete_node((void *) enp);
	else if (node_tab_delete_delay > 0)
	    erts_start_timer_callback(node_tab_delete_delay,
				      try_delete_node,
				      (void *) enp);
    }
}

struct pn_data {
    fmtfn_t to;
    void *to_arg;
    Eterm sysname;
    int no_sysname;
    int no_total;
};

static void print_node(void *venp, void *vpndp)
{
    struct pn_data *pndp = ((struct pn_data *) vpndp);
    ErlNode *enp = ((ErlNode *) venp);

    if(pndp->sysname == NIL
       || enp->sysname == pndp->sysname) {
	if (pndp->no_sysname == 0) {
	    erts_print(pndp->to, pndp->to_arg, "Creation:");
	}
	if(pndp->sysname == NIL) {
	    erts_print(pndp->to, pndp->to_arg, "Name: %T ", enp->sysname);
	}
	erts_print(pndp->to, pndp->to_arg, " %d", enp->creation);
#ifdef DEBUG
	erts_print(pndp->to, pndp->to_arg, " (refc=%ld)",
		   erts_refc_read(&enp->refc, 0));
#endif
	pndp->no_sysname++;
    }
    pndp->no_total++;
}

void erts_print_node_info(fmtfn_t to,
			  void *to_arg,
			  Eterm sysname,
			  int *no_sysname,
			  int *no_total)
{
    int lock = !ERTS_IS_CRASH_DUMPING;
    struct pn_data pnd;

    pnd.to = to;
    pnd.to_arg = to_arg;
    pnd.sysname = sysname;
    pnd.no_sysname = 0;
    pnd.no_total = 0;

    if (lock)
	erts_rwmtx_rlock(&erts_node_table_rwmtx);
    hash_foreach(&erts_node_table, print_node, (void *) &pnd);
    if (pnd.no_sysname != 0) {
	erts_print(to, to_arg, "\n");
    }
    if (lock)
	erts_rwmtx_runlock(&erts_node_table_rwmtx);

    if(no_sysname)
	*no_sysname = pnd.no_sysname;
    if(no_total)
	*no_total = pnd.no_total;
}

/* ----------------------------------------------------------------------- */

void
erts_set_this_node(Eterm sysname, Uint creation)
{
    ERTS_LC_ASSERT(erts_thr_progress_is_blocking());
    ASSERT(2 <= de_refc_read(erts_this_dist_entry, 2));

    if (erts_refc_dectest(&erts_this_node->refc, 0) == 0)
        try_delete_node(erts_this_node);

    erts_deref_dist_entry(erts_this_dist_entry);

    erts_this_node = NULL; /* to make sure refc is bumped for this node */
    erts_this_node = erts_find_or_insert_node(sysname, creation);
    erts_this_dist_entry = erts_this_node->dist_entry;

    erts_ref_dist_entry(erts_this_dist_entry);

    erts_this_node_sysname = erts_this_node_sysname_BUFFER;
    erts_snprintf(erts_this_node_sysname, sizeof(erts_this_node_sysname_BUFFER),
		  "%T", sysname);
}

Uint
erts_delayed_node_table_gc(void)
{
    if (node_tab_delete_delay < 0)
	return (Uint) ERTS_NODE_TAB_DELAY_GC_INFINITY;
    if (node_tab_delete_delay == 0)
	return (Uint) 0;
    return (Uint) ((node_tab_delete_delay-1)/1000 + 1);
}

void erts_init_node_tables(int dd_sec)
{
    erts_rwmtx_opt_t rwmtx_opt = ERTS_RWMTX_OPT_DEFAULT_INITER;
    HashFunctions f;
    ErlNode node_tmpl;

    if (dd_sec == ERTS_NODE_TAB_DELAY_GC_INFINITY)
	node_tab_delete_delay = (ErtsMonotonicTime) -1;
    else
	node_tab_delete_delay = ((ErtsMonotonicTime) dd_sec)*1000;

    orig_node_tab_delete_delay = node_tab_delete_delay;

    rwmtx_opt.type = ERTS_RWMTX_TYPE_FREQUENT_READ;
    rwmtx_opt.lived = ERTS_RWMTX_LONG_LIVED;

    erts_rwmtx_init_opt(&erts_node_table_rwmtx, &rwmtx_opt, "node_table", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_DISTRIBUTION);
    erts_rwmtx_init_opt(&erts_dist_table_rwmtx, &rwmtx_opt, "dist_table", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_DISTRIBUTION);

    f.hash       = (H_FUN)		dist_table_hash;
    f.cmp        = (HCMP_FUN)		dist_table_cmp;
    f.alloc      = (HALLOC_FUN)		dist_table_alloc;
    f.free       = (HFREE_FUN)		dist_table_free;
    f.meta_alloc = (HMALLOC_FUN) 	erts_alloc;
    f.meta_free  = (HMFREE_FUN) 	erts_free;
    f.meta_print = (HMPRINT_FUN) 	erts_print;
    hash_init(ERTS_ALC_T_DIST_TABLE, &erts_dist_table, "dist_table", 11, f);

    f.hash  = (H_FUN)      			node_table_hash;
    f.cmp   = (HCMP_FUN)   			node_table_cmp;
    f.alloc = (HALLOC_FUN) 			node_table_alloc;
    f.free  = (HFREE_FUN)  			node_table_free;
    hash_init(ERTS_ALC_T_NODE_TABLE, &erts_node_table, "node_table", 11, f);

    erts_hidden_dist_entries				= NULL;
    erts_visible_dist_entries				= NULL;
    erts_pending_dist_entries			        = NULL;
    erts_not_connected_dist_entries			= NULL;
    erts_no_of_hidden_dist_entries			= 0;
    erts_no_of_visible_dist_entries			= 0;
    erts_no_of_pending_dist_entries			= 0;
    erts_no_of_not_connected_dist_entries		= 0;

    node_tmpl.sysname = am_Noname;
    node_tmpl.creation = 0;
    erts_this_node = hash_put(&erts_node_table, &node_tmpl);
     /* +1 for erts_this_node */
    erts_refc_init(&erts_this_node->refc, 1);

    ASSERT(erts_this_node->dist_entry != NULL);
    erts_this_dist_entry = erts_this_node->dist_entry;
    /* +1 for erts_this_dist_entry */
    erts_ref_dist_entry(erts_this_dist_entry);

    ASSERT(2 == de_refc_read(erts_this_dist_entry, 2));

    erts_this_node_sysname = erts_this_node_sysname_BUFFER;
    erts_snprintf(erts_this_node_sysname, sizeof(erts_this_node_sysname_BUFFER),
                  "%T", erts_this_node->sysname);

    references_atoms_need_init = 1;
}

#ifdef ERTS_ENABLE_LOCK_CHECK
int erts_lc_is_de_rwlocked(DistEntry *dep)
{
    return erts_lc_rwmtx_is_rwlocked(&dep->rwmtx);
}
int erts_lc_is_de_rlocked(DistEntry *dep)
{
    return erts_lc_rwmtx_is_rlocked(&dep->rwmtx);
}
#endif

#ifdef ERTS_ENABLE_LOCK_COUNT

static void erts_lcnt_enable_dist_lock_count(void *dep_raw, void *enable) {
    DistEntry *dep = (DistEntry*)dep_raw;

    if(enable) {
        erts_lcnt_install_new_lock_info(&dep->rwmtx.lcnt, "dist_entry", dep->sysname, 
            ERTS_LOCK_TYPE_RWMUTEX | ERTS_LOCK_FLAGS_CATEGORY_DISTRIBUTION);
        erts_lcnt_install_new_lock_info(&dep->qlock.lcnt, "dist_entry_out_queue", dep->sysname,
            ERTS_LOCK_TYPE_MUTEX | ERTS_LOCK_FLAGS_CATEGORY_DISTRIBUTION);
        if (dep->mld)
            erts_lcnt_install_new_lock_info(&dep->mld->mtx.lcnt, "dist_entry_links", dep->sysname,
                                            ERTS_LOCK_TYPE_MUTEX | ERTS_LOCK_FLAGS_CATEGORY_DISTRIBUTION);
    } else {
        erts_lcnt_uninstall(&dep->rwmtx.lcnt);
        erts_lcnt_uninstall(&dep->qlock.lcnt);
        if (dep->mld)
            erts_lcnt_uninstall(&dep->mld->mtx.lcnt);
    }
}

void erts_lcnt_update_distribution_locks(int enable) {
    erts_rwmtx_rlock(&erts_dist_table_rwmtx);
    hash_foreach(&erts_dist_table, erts_lcnt_enable_dist_lock_count,
        (void*)(UWord)enable);
    erts_rwmtx_runlock(&erts_dist_table_rwmtx);
}
#endif

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * The following is only supposed to be used for testing, and debugging.     *
 *                                                                           *
 * erts_get_node_and_dist_references() returns a table of all references to  *
 * all entries in the node and dist tables. The hole system will be searched *
 * at once. This will give a consistent view over the references, but can    *
 * can damage the real-time properties of the system.                        *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "erl_db.h"

#undef  INIT_AM
#define INIT_AM(S) AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)

static Eterm AM_heap;
static Eterm AM_link;
static Eterm AM_monitor;
static Eterm AM_process;
static Eterm AM_port;
static Eterm AM_ets;
static Eterm AM_binary;
static Eterm AM_match_spec;
static Eterm AM_control;
static Eterm AM_dist;
static Eterm AM_node;
static Eterm AM_dist_references;
static Eterm AM_node_references;
static Eterm AM_system;
static Eterm AM_timer;
static Eterm AM_delayed_delete_timer;
static Eterm AM_thread_progress_delete_timer;
static Eterm AM_signal;

static void setup_reference_table(void);
static Eterm reference_table_term(Uint **hpp, ErlOffHeap *ohp, Uint *szp);
static void delete_reference_table(void);

#undef ERTS_MAX__
#define ERTS_MAX__(A, B) ((A) > (B) ? (A) : (B))

#define ID_HEAP_SIZE \
    ERTS_MAX__(ERTS_MAGIC_REF_THING_SIZE, \
               ERTS_MAX__(BIG_UINT_HEAP_SIZE, 3))

typedef struct node_referrer_ {
    struct node_referrer_ *next;
    int heap_ref;
    int link_ref;
    int monitor_ref;
    int ets_ref;
    int bin_ref;
    int timer_ref;
    int system_ref;
    int signal_ref;
    Eterm id;
    Uint id_heap[ID_HEAP_SIZE];
    ErlOffHeap off_heap;
} NodeReferrer;

typedef struct {
    ErlNode *node;
    NodeReferrer *referrers;
} ReferredNode;

typedef struct dist_referrer_ {
    struct dist_referrer_ *next;
    int heap_ref;
    int ets_ref;
    int node_ref;
    int ctrl_ref;
    int system_ref;
    int signal_ref;
    Eterm id;
    Uint creation;
    Uint id_heap[ID_HEAP_SIZE];
} DistReferrer;

typedef struct {
    DistEntry *dist;
    DistReferrer *referrers;
} ReferredDist;

typedef struct inserted_bin_ {
    struct inserted_bin_ *next;
    Binary *bin_val;
} InsertedBin;

static ReferredNode *referred_nodes;
static int no_referred_nodes;
static ReferredDist *referred_dists;
static int no_referred_dists;
static InsertedBin *inserted_bins;

Eterm
erts_get_node_and_dist_references(struct process *proc)
{
    Uint *hp;
    Uint size;
    Eterm res;
#ifdef DEBUG
    Uint *endp;
#endif

    erts_proc_unlock(proc, ERTS_PROC_LOCK_MAIN);
    erts_thr_progress_block();
    /* No need to lock any thing since we are alone... */

    if (references_atoms_need_init) {
	INIT_AM(heap);
	INIT_AM(link);
	INIT_AM(monitor);
	INIT_AM(process);
	INIT_AM(port);
	INIT_AM(ets);
	INIT_AM(binary);
	INIT_AM(match_spec);
	INIT_AM(control);
	INIT_AM(dist);
	INIT_AM(node);
	INIT_AM(dist_references);
	INIT_AM(node_references);
	INIT_AM(timer);
	INIT_AM(system);
	INIT_AM(delayed_delete_timer);
        INIT_AM(thread_progress_delete_timer);
	INIT_AM(signal);
	references_atoms_need_init = 0;
    }

    setup_reference_table();

    /* Get term size */
    size = 0;
    (void) reference_table_term(NULL, NULL, &size);

    hp = HAlloc(proc, size);
#ifdef DEBUG
    ASSERT(size > 0);
    endp = hp + size;
#endif

    /* Write term */
    res = reference_table_term(&hp, &proc->off_heap, NULL);

    ASSERT(endp == hp);

    delete_reference_table();

    erts_thr_progress_unblock();
    erts_proc_lock(proc, ERTS_PROC_LOCK_MAIN);
    return res;
}

#define HEAP_REF 1
#define LINK_REF 2
#define ETS_REF  3 
#define BIN_REF  4
#define NODE_REF 5
#define CTRL_REF 6
#define MONITOR_REF 7
#define TIMER_REF 8
#define SYSTEM_REF 9
#define SIGNAL_REF 10

#define INC_TAB_SZ 10

static void
insert_dist_referrer(ReferredDist *referred_dist,
		    int type,
		    Eterm id,
		    Uint creation)
{
    DistReferrer *drp;

    for(drp = referred_dist->referrers; drp; drp = drp->next)
	if(id == drp->id && (type == CTRL_REF
			     || creation == drp->creation))
	    break;

    if(!drp) {
	drp = (DistReferrer *) erts_alloc(ERTS_ALC_T_NC_TMP,
					  sizeof(DistReferrer));
	drp->next = referred_dist->referrers;
	referred_dist->referrers = drp;
	if(IS_CONST(id))
	    drp->id = id;
	else {
	    Uint *hp = &drp->id_heap[0];
	    ASSERT(is_tuple(id));
            drp->id = copy_struct(id, size_object(id), &hp, NULL);
	}
	drp->creation = creation;
	drp->heap_ref = 0;
        drp->ets_ref = 0;
	drp->node_ref = 0;
	drp->ctrl_ref = 0;
	drp->system_ref = 0;
        drp->signal_ref = 0;
    }

    switch (type) {
    case NODE_REF:	drp->node_ref++;	break;
    case CTRL_REF:	drp->ctrl_ref++;	break;
    case HEAP_REF:	drp->heap_ref++;	break;
    case ETS_REF:	drp->ets_ref++;	        break;
    case SYSTEM_REF:	drp->system_ref++;	break;
    case SIGNAL_REF:	drp->signal_ref++;	break;
    default:		ASSERT(0);
    }
}

static void
insert_dist_entry(DistEntry *dist, int type, Eterm id, Uint creation)
{
    ReferredDist *rdp = NULL;
    int i;

    for(i = 0; i < no_referred_dists; i++) {
	if(dist == referred_dists[i].dist) {
	    rdp = &referred_dists[i];
	    break;
	}
    }

    if(!rdp)
	erts_exit(ERTS_ERROR_EXIT,
		 "Reference to non-existing distribution table entry found!\n");

    insert_dist_referrer(rdp, type, id, creation);
}

static void
insert_node_referrer(ReferredNode *referred_node, int type, Eterm id)
{
    NodeReferrer *nrp;

    for(nrp = referred_node->referrers; nrp; nrp = nrp->next)
	if(EQ(id, nrp->id))
	    break;

    if(!nrp) {
	nrp = (NodeReferrer *) erts_alloc(ERTS_ALC_T_NC_TMP,
					  sizeof(NodeReferrer));
	nrp->next = referred_node->referrers;
        ERTS_INIT_OFF_HEAP(&nrp->off_heap);
	referred_node->referrers = nrp;
	if(IS_CONST(id))
	    nrp->id = id;
	else {
	    Uint *hp = &nrp->id_heap[0];
	    ASSERT(is_big(id) || is_tuple(id) || is_internal_magic_ref(id));
	    nrp->id = copy_struct(id, size_object(id), &hp, &nrp->off_heap);
	}
	nrp->heap_ref = 0;
	nrp->link_ref = 0;
	nrp->monitor_ref = 0;
	nrp->ets_ref = 0;
	nrp->bin_ref = 0;
	nrp->timer_ref = 0;
	nrp->system_ref = 0;
	nrp->signal_ref = 0;
    }

    switch (type) {
    case HEAP_REF:	nrp->heap_ref++;	break;
    case LINK_REF:	nrp->link_ref++;	break;
    case ETS_REF:	nrp->ets_ref++;		break;
    case BIN_REF:	nrp->bin_ref++;		break;
    case MONITOR_REF:   nrp->monitor_ref++;     break;
    case TIMER_REF:	nrp->timer_ref++;	break;
    case SYSTEM_REF:	nrp->system_ref++;	break;
    case SIGNAL_REF:	nrp->signal_ref++;	break;
    default:		ASSERT(0);
    }
}

static void
insert_node(ErlNode *node, int type, Eterm id)
{
    int i;
    ReferredNode *rnp = NULL;
    for(i = 0; i < no_referred_nodes; i++) {
	if(node == referred_nodes[i].node) {
	    rnp = &referred_nodes[i];
	    break;
	}
    }

    if (!rnp)
	erts_exit(ERTS_ERROR_EXIT, "Reference to non-existing node table entry found!\n");

    insert_node_referrer(rnp, type, id);
}

static void
insert_erl_node(void *venp, void *unused)
{
    ErlNode *enp = (ErlNode *) venp;

    insert_dist_entry(enp->dist_entry, NODE_REF, enp->sysname, enp->creation);
}

struct insert_offheap2_arg {
    int type;
    Eterm id;
};

static void insert_offheap(ErlOffHeap *, int, Eterm);

static void
insert_offheap2(ErlOffHeap *oh, void *arg) 
{
    struct insert_offheap2_arg *a = (struct insert_offheap2_arg *) arg;
    insert_offheap(oh, a->type, a->id);
}

#define ErtsIsDistEntryBinary(Bin)                                      \
    (((Bin)->intern.flags & BIN_FLAG_MAGIC)                             \
     && ERTS_MAGIC_BIN_DESTRUCTOR((Bin)) == erts_dist_entry_destructor)

#define IsSendCtxBinary(Bin)                                            \
    (((Bin)->intern.flags & BIN_FLAG_MAGIC)                             \
     && ERTS_MAGIC_BIN_DESTRUCTOR((Bin)) == erts_dsend_context_dtor)


static void
insert_offheap(ErlOffHeap *oh, int type, Eterm id)
{
    union erl_off_heap_ptr u;
    struct insert_offheap2_arg a;
    a.type = BIN_REF;

    for (u.hdr = oh->first; u.hdr; u.hdr = u.hdr->next) {
	switch (thing_subtag(u.hdr->thing_word)) {
	case REF_SUBTAG:
            if (ErtsIsDistEntryBinary(u.mref->mb))
                insert_dist_entry(ErtsBin2DistEntry(u.mref->mb),
                                  type, id, 0);
	    else if(IsMatchProgBinary(u.mref->mb)) {
		InsertedBin *ib;
		int insert_bin = 1;
		for (ib = inserted_bins; ib; ib = ib->next)
		    if(ib->bin_val == (Binary *) u.mref->mb) {
			insert_bin = 0;
			break;
		    }
		if (insert_bin) {
		    DeclareTmpHeapNoproc(id_heap,BIG_UINT_HEAP_SIZE);
		    Uint *hp = &id_heap[0];
		    InsertedBin *nib;
		    UseTmpHeapNoproc(BIG_UINT_HEAP_SIZE);
		    a.id = erts_bld_uint(&hp, NULL, (Uint) u.mref->mb);
		    erts_match_prog_foreach_offheap((Binary *) u.mref->mb,
						    insert_offheap2,
						    (void *) &a);
		    nib = erts_alloc(ERTS_ALC_T_NC_TMP, sizeof(InsertedBin));
		    nib->bin_val = (Binary *) u.mref->mb;
		    nib->next = inserted_bins;
		    inserted_bins = nib;
		    UnUseTmpHeapNoproc(BIG_UINT_HEAP_SIZE);
		}
	    }
            else if (IsSendCtxBinary(u.mref->mb)) {
                ErtsSendContext* ctx = ERTS_MAGIC_BIN_DATA(u.mref->mb);
                if (ctx->deref_dep)
                    insert_dist_entry(ctx->dep, type, id, 0);
            }
	    break;
	case REFC_BINARY_SUBTAG:
	case FUN_SUBTAG:
	    break; /* No need to */
	default:
	    ASSERT(is_external_header(u.hdr->thing_word));
	    insert_node(u.ext->node, type, id);
	    break;
	}
    }
}

static void insert_monitor_data(ErtsMonitor *mon, int type, Eterm id)
{
    ErtsMonitorData *mdp = erts_monitor_to_data(mon);
    if ((mdp->origin.flags & (ERTS_ML_FLG_DBG_VISITED
                              | ERTS_ML_FLG_EXTENDED)) == ERTS_ML_FLG_EXTENDED) {
        if (mon->type != ERTS_MON_TYPE_NODE) {
            ErtsMonitorDataExtended *mdep = (ErtsMonitorDataExtended *) mdp;
            ASSERT(mon->flags & ERTS_ML_FLG_EXTENDED);
            if (mdep->uptr.ohhp) {
                ErlOffHeap oh;
                ERTS_INIT_OFF_HEAP(&oh);
                oh.first = mdep->uptr.ohhp;
                insert_offheap(&oh, type, id);
            }
        }
    }
    mdp->origin.flags |= ERTS_ML_FLG_DBG_VISITED;
}

static void insert_monitor(ErtsMonitor *mon, void *idp)
{
    Eterm id = *((Eterm *) idp);
    insert_monitor_data(mon, MONITOR_REF, id);
}

static void clear_visited_monitor(ErtsMonitor *mon, void *p)
{
    ErtsMonitorData *mdp = erts_monitor_to_data(mon);
    mdp->origin.flags &= ~ERTS_ML_FLG_DBG_VISITED;
}

static void
insert_p_monitors(ErtsPTabElementCommon *p)
{
    Eterm id = p->id;
    erts_monitor_tree_foreach(p->u.alive.monitors,
                              insert_monitor,
                              (void *) &id);
    erts_monitor_list_foreach(p->u.alive.lt_monitors,
                              insert_monitor,
                              (void *) &id);
}

static void
insert_dist_monitors(DistEntry *dep)
{
    if (dep->mld) {
        erts_monitor_list_foreach(dep->mld->monitors,
                                  insert_monitor,
                                  (void *) &dep->sysname);
        erts_monitor_tree_foreach(dep->mld->orig_name_monitors,
                                  insert_monitor,
                                  (void *) &dep->sysname);
    }
}

static void
clear_visited_p_monitors(ErtsPTabElementCommon *p)
{
    erts_monitor_tree_foreach(p->u.alive.monitors,
                              clear_visited_monitor,
                              NULL);
    erts_monitor_list_foreach(p->u.alive.lt_monitors,
                              clear_visited_monitor,
                              NULL);
}

static void
clear_visited_dist_monitors(DistEntry *dep)
{
    if (dep->mld) {
        erts_monitor_list_foreach(dep->mld->monitors,
                                  clear_visited_monitor,
                                  NULL);
        erts_monitor_tree_foreach(dep->mld->orig_name_monitors,
                                  clear_visited_monitor,
                                  NULL);
    }
}

static void insert_link_data(ErtsLink *lnk, int type, Eterm id)
{
    ErtsLinkData *ldp = erts_link_to_data(lnk);
    if ((ldp->a.flags & (ERTS_ML_FLG_DBG_VISITED
                         | ERTS_ML_FLG_EXTENDED)) == ERTS_ML_FLG_EXTENDED) {
        ErtsLinkDataExtended *ldep = (ErtsLinkDataExtended *) ldp;
        if (ldep->ohhp) {
            ErlOffHeap oh;
            ERTS_INIT_OFF_HEAP(&oh);
            oh.first = ldep->ohhp;
            insert_offheap(&oh, type, id);
        }
    }
    ldp->a.flags |= ERTS_ML_FLG_DBG_VISITED;
}

static void insert_link(ErtsLink *lnk, void *idp)
{
    Eterm id = *((Eterm *) idp);
    insert_link_data(lnk, LINK_REF, id);
}

static void clear_visited_link(ErtsLink *lnk, void *p)
{
    ErtsLinkData *ldp = erts_link_to_data(lnk);
    ldp->a.flags &= ~ERTS_ML_FLG_DBG_VISITED;
}

static void
insert_p_links(ErtsPTabElementCommon *p)
{
    Eterm id = p->id;
    erts_link_tree_foreach(p->u.alive.links, insert_link, (void *) &id);
}

static void
insert_dist_links(DistEntry *dep)
{
    if (dep->mld)
        erts_link_list_foreach(dep->mld->links,
                               insert_link,
                               (void *) &dep->sysname);
}

static void
clear_visited_p_links(ErtsPTabElementCommon *p)
{
    erts_link_tree_foreach(p->u.alive.links,
                           clear_visited_link,
                           NULL);
}

static void
clear_visited_dist_links(DistEntry *dep)
{
    if (dep->mld)
        erts_link_list_foreach(dep->mld->links,
                               clear_visited_link,
                               NULL);
}

static void
insert_ets_table(DbTable *tab, void *unused)
{
    ErlOffHeap off_heap;
    Eterm heap[ERTS_MAGIC_REF_THING_SIZE];
    struct insert_offheap2_arg a;
    a.type = ETS_REF;
    if (tab->common.status & DB_NAMED_TABLE)
        a.id = tab->common.the_name;
    else {
        Eterm *hp = &heap[0];
        ERTS_INIT_OFF_HEAP(&off_heap);
        a.id = erts_mk_magic_ref(&hp, &off_heap, tab->common.btid);
    }
    erts_db_foreach_offheap(tab, insert_offheap2, (void *) &a);
    if (is_not_atom(a.id))
        erts_cleanup_offheap(&off_heap);
}

static void
insert_bif_timer(Eterm receiver, Eterm msg, ErlHeapFragment *bp, void *arg)
{
    if (bp) {
	DeclareTmpHeapNoproc(heap,3);

	UseTmpHeapNoproc(3);
	insert_offheap(&bp->off_heap,
		       TIMER_REF,
		       (is_internal_pid(receiver)
			? receiver
			: TUPLE2(&heap[0], AM_process, receiver)));
	UnUseTmpHeapNoproc(3);
    }
}

static void
init_referred_node(void *node, void *unused)
{
    referred_nodes[no_referred_nodes].node = (ErlNode *) node;
    referred_nodes[no_referred_nodes].referrers = NULL;
    no_referred_nodes++;
}

static void
init_referred_dist(void *dist, void *unused)
{
    referred_dists[no_referred_dists].dist = (DistEntry *) dist;
    referred_dists[no_referred_dists].referrers = NULL;
    no_referred_dists++;
}

static void
insert_sys_msg(Eterm from, Eterm to, Eterm msg, ErlHeapFragment *bp)
{
    insert_offheap(&bp->off_heap, HEAP_REF, to);
}

static void
insert_delayed_delete_node(void *state,
			   ErtsMonotonicTime timeout_pos,
			   void *vnp)
{
    Eterm heap[3];
    insert_node((ErlNode *) vnp,
		SYSTEM_REF,
		TUPLE2(&heap[0], AM_system, AM_delayed_delete_timer));
}

static void
insert_thr_prgr_delete_dist_entry(void *arg, ErtsThrPrgrVal thr_prgr, void *vdep)
{
    DistEntry *dep = vdep;
    Eterm heap[3];
    insert_dist_entry(dep,
		      SYSTEM_REF,
		      TUPLE2(&heap[0], AM_system, AM_thread_progress_delete_timer),
		      0);
}

static void
insert_delayed_delete_dist_entry(void *state,
				 ErtsMonotonicTime timeout_pos,
				 void *vdep)
{
    DistEntry *dep = vdep;
    Eterm heap[3];
    insert_dist_entry(dep,
		      SYSTEM_REF,
		      TUPLE2(&heap[0], AM_system, AM_delayed_delete_timer),
		      0);
}

static void
insert_message(ErtsMessage *msg, int type, Process *proc)
{
    ErlHeapFragment *heap_frag = NULL;

    ASSERT(ERTS_SIG_IS_MSG(msg));
    if (msg->data.attached) {
        if (msg->data.attached == ERTS_MSG_COMBINED_HFRAG)
            heap_frag = &msg->hfrag;
        else if (ERTS_SIG_IS_INTERNAL_MSG(msg))
            heap_frag = msg->data.heap_frag;
        else {
            if (msg->data.dist_ext->dep)
                insert_dist_entry(msg->data.dist_ext->dep,
                                  type, proc->common.id, 0);
            if (is_not_nil(ERL_MESSAGE_TOKEN(msg)))
                heap_frag = erts_dist_ext_trailer(msg->data.dist_ext);
        }
    }
    while (heap_frag) {
        insert_offheap(&(heap_frag->off_heap),
                       type,
                       proc->common.id);
        heap_frag = heap_frag->next;
    }
}

static void
insert_sig_msg(ErtsMessage *msg, void *arg)
{
    insert_message(msg, SIGNAL_REF, (Process *) arg);
}

static void
insert_sig_offheap(ErlOffHeap *ohp, void *arg)
{
    Process *proc = arg;
    insert_offheap(ohp, SIGNAL_REF, proc->common.id);
}

static void
insert_sig_monitor(ErtsMonitor *mon, void *arg)
{
    Process *proc = arg;
    insert_monitor_data(mon, SIGNAL_REF, proc->common.id);
}

static void
insert_sig_link(ErtsLink *lnk, void *arg)
{
    Process *proc = arg;
    insert_link_data(lnk, SIGNAL_REF, proc->common.id);
}

static void
setup_reference_table(void)
{
    ErlHeapFragment *hfp;
    DistEntry *dep;
    HashInfo hi;
    int i, max;
    DeclareTmpHeapNoproc(heap,3);

    inserted_bins = NULL;

    hash_get_info(&hi, &erts_node_table);
    referred_nodes = erts_alloc(ERTS_ALC_T_NC_TMP,
				hi.objs*sizeof(ReferredNode));
    no_referred_nodes = 0;
    hash_foreach(&erts_node_table, init_referred_node, NULL);
    ASSERT(no_referred_nodes == hi.objs);

    hash_get_info(&hi, &erts_dist_table);
    referred_dists = erts_alloc(ERTS_ALC_T_NC_TMP,
				hi.objs*sizeof(ReferredDist));
    no_referred_dists = 0;
    hash_foreach(&erts_dist_table, init_referred_dist, NULL);
    ASSERT(no_referred_dists == hi.objs);

    /* Go through the hole system, and build a table of all references
       to ErlNode and DistEntry structures */

    erts_debug_callback_timer_foreach(try_delete_node,
				      insert_delayed_delete_node,
				      NULL);
    erts_debug_callback_timer_foreach(prepare_try_delete_dist_entry,
				      insert_delayed_delete_dist_entry,
				      NULL);
    erts_debug_later_op_foreach(start_timer_delete_dist_entry,
                                insert_thr_prgr_delete_dist_entry,
                                NULL);

    UseTmpHeapNoproc(3);
    insert_node(erts_this_node,
		SYSTEM_REF,
		TUPLE2(&heap[0], AM_system, am_undefined));

    insert_dist_entry(erts_this_dist_entry,
                      SYSTEM_REF,
                      TUPLE2(&heap[0], AM_system, am_undefined),
                      erts_this_node->creation);
    UnUseTmpHeapNoproc(3);

    max = erts_ptab_max(&erts_proc);
    /* Insert all processes */
    for (i = 0; i < max; i++) {
	Process *proc = erts_pix2proc(i);
	if (proc) {
	    int mli;
	    ErtsMessage *msg_list[] = {proc->msg_frag};

	    /* Insert Heap */
	    insert_offheap(&(proc->off_heap),
			   HEAP_REF,
			   proc->common.id);
	    /* Insert heap fragments buffers */
	    for(hfp = proc->mbuf; hfp; hfp = hfp->next)
		insert_offheap(&(hfp->off_heap),
			       HEAP_REF,
			       proc->common.id);

	    /* Insert msg buffers */
	    for (mli = 0; mli < sizeof(msg_list)/sizeof(msg_list[0]); mli++) {
		ErtsMessage *msg;
		for (msg = msg_list[mli]; msg; msg = msg->next)
                    insert_message(msg, HEAP_REF, proc);
	    }

            /* Insert signal queue */
            erts_proc_sig_debug_foreach_sig(proc,
                                            insert_sig_msg,
                                            insert_sig_offheap,
                                            insert_sig_monitor,
                                            insert_sig_link,
                                            (void *) proc);

	    /* Insert links */
            insert_p_links(&proc->common);

            /* Insert monitors */
            insert_p_monitors(&proc->common);

            {
                DistEntry *dep = ERTS_PROC_GET_DIST_ENTRY(proc);
                if (dep)
                    insert_dist_entry(dep,
                                      CTRL_REF,
                                      proc->common.id,
                                      0);
            }
	}
    }
    
    erts_foreach_sys_msg_in_q(insert_sys_msg);

    /* Insert all ports */
    max = erts_ptab_max(&erts_port);
    for (i = 0; i < max; i++) {
	ErlOffHeap *ohp;
	erts_aint32_t state;
	Port *prt;

	prt = erts_pix2port(i);
	if (!prt)
	    continue;

	state = erts_atomic32_read_nob(&prt->state);
	if (state & ERTS_PORT_SFLGS_DEAD)
	    continue;

	/* Insert links */
        insert_p_links(&prt->common);
	/* Insert monitors */
        insert_p_monitors(&prt->common);
	/* Insert port data */
	ohp = erts_port_data_offheap(prt);
	if (ohp)
	    insert_offheap(ohp, HEAP_REF, prt->common.id);
	/* Insert controller */
	if (prt->dist_entry)
	    insert_dist_entry(prt->dist_entry,
			      CTRL_REF,
			      prt->common.id,
			      0);
    }

    { /* Add binaries stored elsewhere ... */
	ErlOffHeap oh;
	ProcBin pb[2];
	int i = 0;
	Binary *default_match_spec;
	Binary *default_meta_match_spec;

	oh.first = NULL;
	/* Only the ProcBin members thing_word, val and next will be inspected
	   (by insert_offheap()) */
#undef  ADD_BINARY
#define ADD_BINARY(Bin)				 	     \
	if ((Bin)) {					     \
	    pb[i].thing_word = REFC_BINARY_SUBTAG;           \
	    pb[i].val = (Bin);				     \
	    pb[i].next = oh.first;		             \
	    oh.first = (struct erl_off_heap_header*) &pb[i]; \
	    i++;				             \
	}

	erts_get_default_trace_pattern(NULL,
				       &default_match_spec,
				       &default_meta_match_spec,
				       NULL,
				       NULL);

	ADD_BINARY(default_match_spec);
	ADD_BINARY(default_meta_match_spec);

	insert_offheap(&oh, BIN_REF, AM_match_spec);
#undef  ADD_BINARY
    }

    /* Insert all dist links */

    for(dep = erts_visible_dist_entries; dep; dep = dep->next) {
        insert_dist_links(dep);
        insert_dist_monitors(dep);
    }

    for(dep = erts_hidden_dist_entries; dep; dep = dep->next) {
        insert_dist_links(dep);
        insert_dist_monitors(dep);
    }

    for(dep = erts_pending_dist_entries; dep; dep = dep->next) {
        insert_dist_links(dep);
        insert_dist_monitors(dep);
    }

    /* Not connected dist entries should not have any links,
       but inspect them anyway */
    for(dep = erts_not_connected_dist_entries; dep; dep = dep->next) {
        insert_dist_links(dep);
        insert_dist_monitors(dep);
    }

    /* Insert all ets tables */
    erts_db_foreach_table(insert_ets_table, NULL);

    /* Insert all bif timers */
    erts_debug_bif_timer_foreach(insert_bif_timer, NULL);

    /* Insert node table (references to dist) */
    hash_foreach(&erts_node_table, insert_erl_node, NULL);
}

/*
  Returns an erlang term on this format:

 	 {{node_references,
 	   [{{Node, Creation}, Refc,
	     [{{ReferrerType, ID},
	       [{ReferenceType,References},
		'...']},
	      '...']},
	     '...']},
 	  {dist_references,
 	   [{Node, Refc,
	     [{{ReferrerType, ID},
	       [{ReferenceType,References},
		'...']},
	      '...']},
 	    '...']}}
 */

static Eterm
reference_table_term(Uint **hpp, ErlOffHeap *ohp, Uint *szp)
{
#undef  MK_2TUP
#undef  MK_3TUP
#undef  MK_CONS
#undef  MK_UINT
#define MK_2TUP(E1, E2)		erts_bld_tuple(hpp, szp, 2, (E1), (E2))
#define MK_3TUP(E1, E2, E3)	erts_bld_tuple(hpp, szp, 3, (E1), (E2), (E3))
#define MK_CONS(CAR, CDR)	erts_bld_cons(hpp, szp, (CAR), (CDR))
#define MK_UINT(UI)		erts_bld_uint(hpp, szp, (UI))
    int i;
    Eterm tup;
    Eterm tup2;
    Eterm nl = NIL;
    Eterm dl = NIL;
    Eterm nrid;

    for(i = 0; i < no_referred_nodes; i++) {
	NodeReferrer *nrp;
	Eterm nril = NIL;

	for(nrp = referred_nodes[i].referrers; nrp; nrp = nrp->next) {
	    Eterm nrl = NIL;
	    /* NodeReferenceList = [{ReferenceType,References}] */
	    if(nrp->heap_ref) {
		tup = MK_2TUP(AM_heap, MK_UINT(nrp->heap_ref));
		nrl = MK_CONS(tup, nrl);
	    }
	    if(nrp->link_ref) {
		tup = MK_2TUP(AM_link, MK_UINT(nrp->link_ref));
		nrl = MK_CONS(tup, nrl);
	    }
	    if(nrp->monitor_ref) {
		tup = MK_2TUP(AM_monitor, MK_UINT(nrp->monitor_ref));
		nrl = MK_CONS(tup, nrl);
	    }
	    if(nrp->ets_ref) {
		tup = MK_2TUP(AM_ets, MK_UINT(nrp->ets_ref));
		nrl = MK_CONS(tup, nrl);
	    }
	    if(nrp->bin_ref) {
		tup = MK_2TUP(AM_binary, MK_UINT(nrp->bin_ref));
		nrl = MK_CONS(tup, nrl);
	    }
	    if(nrp->timer_ref) {
		tup = MK_2TUP(AM_timer, MK_UINT(nrp->timer_ref));
		nrl = MK_CONS(tup, nrl);
	    }
	    if(nrp->system_ref) {
		tup = MK_2TUP(AM_system, MK_UINT(nrp->system_ref));
		nrl = MK_CONS(tup, nrl);
	    }
	    if(nrp->signal_ref) {
		tup = MK_2TUP(AM_signal, MK_UINT(nrp->signal_ref));
		nrl = MK_CONS(tup, nrl);
	    }

	    nrid = nrp->id;
	    if (!IS_CONST(nrp->id)) {
		Uint nrid_sz = size_object(nrp->id);
		if (szp)
		    *szp += nrid_sz;
		if (hpp)
		    nrid = copy_struct(nrp->id, nrid_sz, hpp, ohp);
	    }

	    if (is_internal_pid(nrid) || nrid == am_error_logger) {
		ASSERT(!nrp->ets_ref && !nrp->bin_ref && !nrp->system_ref);
		tup = MK_2TUP(AM_process, nrid);
	    }
	    else if (is_tuple(nrid)) {
		Eterm *t;
		ASSERT(!nrp->ets_ref && !nrp->bin_ref);
		t = tuple_val(nrid);
		ASSERT(2 == arityval(t[0]));
		tup = MK_2TUP(t[1], t[2]);
	    }
	    else if(is_internal_port(nrid)) {
		ASSERT(!nrp->heap_ref && !nrp->ets_ref && !nrp->bin_ref
		       && !nrp->timer_ref && !nrp->system_ref && !nrp->signal_ref);
		tup = MK_2TUP(AM_port, nrid);
	    }
	    else if(nrp->ets_ref) {
		ASSERT(!nrp->heap_ref && !nrp->link_ref && 
		       !nrp->monitor_ref && !nrp->bin_ref
		       && !nrp->timer_ref && !nrp->system_ref && !nrp->signal_ref);
		tup = MK_2TUP(AM_ets, nrid);
	    }
	    else if(nrp->bin_ref) {
		ASSERT(is_small(nrid) || is_big(nrid));
		ASSERT(!nrp->heap_ref && !nrp->ets_ref && !nrp->link_ref && 
		       !nrp->monitor_ref && !nrp->timer_ref
		       && !nrp->system_ref && !nrp->signal_ref);
		tup = MK_2TUP(AM_match_spec, nrid);
	    }
	    else  {
		ASSERT(!nrp->heap_ref && !nrp->ets_ref && !nrp->bin_ref
                       && !nrp->signal_ref);
		ASSERT(is_atom(nrid));
		tup = MK_2TUP(AM_dist, nrid);
	    }
	    tup = MK_2TUP(tup, nrl);
	    /* NodeReferenceIdList = [{{ReferrerType, ID}, NodeReferenceList}] */
	    nril = MK_CONS(tup, nril);
	}

	/* NodeList = [{{Node, Creation}, Refc, NodeReferenceIdList}] */

	tup = MK_2TUP(referred_nodes[i].node->sysname,
		      MK_UINT(referred_nodes[i].node->creation));
	tup = MK_3TUP(tup, MK_UINT(erts_refc_read(&referred_nodes[i].node->refc, 0)), nril);
	nl = MK_CONS(tup, nl);
    }

    for(i = 0; i < no_referred_dists; i++) {
	DistReferrer *drp;
	Eterm dril = NIL;
	for(drp = referred_dists[i].referrers; drp; drp = drp->next) {
	    Eterm drl = NIL;

	    /* DistReferenceList = [{ReferenceType,References}] */
	    if(drp->node_ref) {
		tup = MK_2TUP(AM_node, MK_UINT(drp->node_ref));
		drl = MK_CONS(tup, drl);
	    }
	    if(drp->ctrl_ref) {
		tup = MK_2TUP(AM_control, MK_UINT(drp->ctrl_ref));
		drl = MK_CONS(tup, drl);
	    }
	    if(drp->heap_ref) {
		tup = MK_2TUP(AM_heap, MK_UINT(drp->heap_ref));
		drl = MK_CONS(tup, drl);
	    }
            if(drp->ets_ref) {
                tup = MK_2TUP(AM_ets, MK_UINT(drp->ets_ref));
                drl = MK_CONS(tup, drl);
            }
	    if(drp->system_ref) {
		tup = MK_2TUP(AM_system, MK_UINT(drp->system_ref));
		drl = MK_CONS(tup, drl);
	    }
	    if(drp->signal_ref) {
		tup = MK_2TUP(AM_signal, MK_UINT(drp->signal_ref));
		drl = MK_CONS(tup, drl);
	    }

	    if (is_internal_pid(drp->id)) {
		ASSERT(!drp->node_ref);
		tup = MK_2TUP(AM_process, drp->id);
	    }
	    else if(is_internal_port(drp->id)) {
		ASSERT(drp->ctrl_ref && !drp->node_ref && !drp->signal_ref);
		tup = MK_2TUP(AM_port, drp->id);
	    }
	    else if (is_tuple(drp->id)) {
		Eterm *t;
		ASSERT(drp->system_ref && !drp->node_ref
		       && !drp->ctrl_ref && !drp->heap_ref && !drp->ets_ref
                       && !drp->signal_ref);
		t = tuple_val(drp->id);
		ASSERT(2 == arityval(t[0]));
		tup = MK_2TUP(t[1], t[2]);
	    }
	    else if (drp->ets_ref) {
                ASSERT(!drp->heap_ref && !drp->node_ref &&
                       !drp->ctrl_ref && !drp->system_ref
                       && !drp->signal_ref);
                tup = MK_2TUP(AM_ets, drp->id);
            }
	    else {
		ASSERT(!drp->ctrl_ref && drp->node_ref && !drp->signal_ref);
		ASSERT(is_atom(drp->id));
		tup = MK_2TUP(drp->id, MK_UINT(drp->creation));
		tup = MK_2TUP(AM_node, tup);
	    }

	    tup = MK_2TUP(tup, drl);

	    /* DistReferenceIdList =
	       [{{ReferrerType, ID}, DistReferenceList}] */
	    dril = MK_CONS(tup, dril);

	}

	/* DistList = [{Dist, Refc, ReferenceIdList}] */
	tup = MK_3TUP(referred_dists[i].dist->sysname,
		      MK_UINT(de_refc_read(referred_dists[i].dist, 0)),
		      dril);
	dl = MK_CONS(tup, dl);
    }

    /* {{node_references, NodeList}, {dist_references, DistList}} */

    tup = MK_2TUP(AM_node_references, nl);
    tup2 = MK_2TUP(AM_dist_references, dl);
    tup = MK_2TUP(tup, tup2);

    return tup;
#undef  MK_2TUP
#undef  MK_3TUP
#undef  MK_CONS
#undef  MK_UINT

}

static void noop_sig_msg(ErtsMessage *msg, void *arg)
{

}

static void noop_sig_offheap(ErlOffHeap *oh, void *arg)
{

}

static void
delete_reference_table(void)
{
    DistEntry *dep;
    int i, max;
    for(i = 0; i < no_referred_nodes; i++) {
	NodeReferrer *nrp;
	NodeReferrer *tnrp;
	nrp = referred_nodes[i].referrers;
	while(nrp) {
            erts_cleanup_offheap(&nrp->off_heap);
	    tnrp = nrp;
	    nrp = nrp->next;
	    erts_free(ERTS_ALC_T_NC_TMP, (void *) tnrp);
	}
    }
    if (referred_nodes)
	erts_free(ERTS_ALC_T_NC_TMP, (void *) referred_nodes);

    for(i = 0; i < no_referred_dists; i++) {
	DistReferrer *drp;
	DistReferrer *tdrp;
	drp = referred_dists[i].referrers;
	while(drp) {
	    tdrp = drp;
	    drp = drp->next;
	    erts_free(ERTS_ALC_T_NC_TMP, (void *) tdrp);
	}
    }
    if (referred_dists)
	erts_free(ERTS_ALC_T_NC_TMP, (void *) referred_dists);
    while(inserted_bins) {
	InsertedBin *ib = inserted_bins;
	inserted_bins = inserted_bins->next;
	erts_free(ERTS_ALC_T_NC_TMP, (void *)ib);
    }

    /* Cleanup... */

    max = erts_ptab_max(&erts_proc);
    for (i = 0; i < max; i++) {
	Process *proc = erts_pix2proc(i);
	if (proc) {
            clear_visited_p_links(&proc->common);
            clear_visited_p_monitors(&proc->common);
            erts_proc_sig_debug_foreach_sig(proc,
                                            noop_sig_msg,
                                            noop_sig_offheap,
                                            clear_visited_monitor,
                                            clear_visited_link,
                                            (void *) proc);
        }
    }

    max = erts_ptab_max(&erts_port);
    for (i = 0; i < max; i++) {
	erts_aint32_t state;
	Port *prt;

	prt = erts_pix2port(i);
	if (!prt)
	    continue;

	state = erts_atomic32_read_nob(&prt->state);
	if (state & ERTS_PORT_SFLGS_DEAD)
	    continue;

        clear_visited_p_links(&prt->common);
        clear_visited_p_monitors(&prt->common);
    }

    for(dep = erts_visible_dist_entries; dep; dep = dep->next) {
        clear_visited_dist_links(dep);
        clear_visited_dist_monitors(dep);
    }

    for(dep = erts_hidden_dist_entries; dep; dep = dep->next) {
        clear_visited_dist_links(dep);
        clear_visited_dist_monitors(dep);
    }

    for(dep = erts_pending_dist_entries; dep; dep = dep->next) {
        clear_visited_dist_links(dep);
        clear_visited_dist_monitors(dep);
    }

    for(dep = erts_not_connected_dist_entries; dep; dep = dep->next) {
        clear_visited_dist_links(dep);
        clear_visited_dist_monitors(dep);
    }
}

void
erts_debug_test_node_tab_delayed_delete(Sint64 millisecs)
{
    erts_thr_progress_block();

    if (millisecs < 0)
	node_tab_delete_delay = orig_node_tab_delete_delay;
    else
	node_tab_delete_delay = millisecs;

    erts_thr_progress_unblock();
}
