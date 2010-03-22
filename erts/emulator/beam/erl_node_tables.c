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

#include "global.h"
#include "erl_node_tables.h"
#include "dist.h"
#include "big.h"
#include "error.h"

Hash erts_dist_table;
Hash erts_node_table;
erts_smp_rwmtx_t erts_dist_table_rwmtx;
erts_smp_rwmtx_t erts_node_table_rwmtx;

DistEntry *erts_hidden_dist_entries;
DistEntry *erts_visible_dist_entries;
DistEntry *erts_not_connected_dist_entries;
Sint erts_no_of_hidden_dist_entries;
Sint erts_no_of_visible_dist_entries;
Sint erts_no_of_not_connected_dist_entries;

DistEntry *erts_this_dist_entry;
ErlNode *erts_this_node;

static Uint node_entries;
static Uint dist_entries;

static int references_atoms_need_init = 1;

/* -- The distribution table ---------------------------------------------- */

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
    Eterm chnl_nr;
    Eterm sysname;
    DistEntry *dep;

    if(((DistEntry *) dep_tmpl) == erts_this_dist_entry)
	return dep_tmpl;

    sysname = ((DistEntry *) dep_tmpl)->sysname;
    chnl_nr = make_small((Uint) atom_val(sysname));
    dep = (DistEntry *) erts_alloc(ERTS_ALC_T_DIST_ENTRY, sizeof(DistEntry));

    dist_entries++;

    dep->prev				= NULL;
    erts_refc_init(&dep->refc, -1);
    erts_smp_rwmtx_init_x(&dep->rwmtx, "dist_entry", chnl_nr);
    dep->sysname			= sysname;
    dep->cid				= NIL;
    dep->connection_id			= 0;
    dep->status				= 0;
    dep->flags				= 0;
    dep->version			= 0;

    erts_smp_mtx_init_x(&dep->lnk_mtx, "dist_entry_links", chnl_nr);
    dep->node_links			= NULL;
    dep->nlinks				= NULL;
    dep->monitors			= NULL;

    erts_smp_spinlock_init_x(&dep->qlock, "dist_entry_out_queue", chnl_nr);
    dep->qflgs				= 0;
    dep->qsize				= 0;
    dep->out_queue.first		= NULL;
    dep->out_queue.last			= NULL;
    dep->suspended.first		= NULL;
    dep->suspended.last			= NULL;

    dep->finalized_out_queue.first	= NULL;
    dep->finalized_out_queue.last	= NULL;

    erts_smp_atomic_init(&dep->dist_cmd_scheduled, 0);
    erts_port_task_handle_init(&dep->dist_cmd);
    dep->send				= NULL;
    dep->cache				= NULL;

    /* Link in */

    /* All new dist entries are "not connected" */
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

    if(dep == erts_this_dist_entry)
	return;

    ASSERT(is_nil(dep->cid));
    ASSERT(dep->nlinks == NULL);
    ASSERT(dep->node_links == NULL);
    ASSERT(dep->monitors == NULL);

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
    erts_smp_rwmtx_destroy(&dep->rwmtx);
    erts_smp_mtx_destroy(&dep->lnk_mtx);
    erts_smp_spinlock_destroy(&dep->qlock);

#ifdef DEBUG
    sys_memset(vdep, 0x77, sizeof(DistEntry));
#endif
    erts_free(ERTS_ALC_T_DIST_ENTRY, (void *) dep);

    ASSERT(dist_entries > 1);
    dist_entries--;
}


void
erts_dist_table_info(int to, void *to_arg)
{
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	erts_smp_rwmtx_rlock(&erts_dist_table_rwmtx);
    hash_info(to, to_arg, &erts_dist_table);
    if (lock)
	erts_smp_rwmtx_runlock(&erts_dist_table_rwmtx);
}

DistEntry *
erts_channel_no_to_dist_entry(Uint cno)
{
/*
 * For this node (and previous incarnations of this node),
 * ERST_INTERNAL_CHANNEL_NO (will always be 0 I guess) is used as
 * channel no. For other nodes, the atom index of the atom corresponding
 * to the node name is used as channel no.
 */
    if(cno == ERST_INTERNAL_CHANNEL_NO) {
	erts_refc_inc(&erts_this_dist_entry->refc, 2);
	return erts_this_dist_entry;
    }

    if((cno > MAX_ATOM_INDEX)
       || (cno >= atom_table_size())
       ||  (atom_tab(cno) == NULL))
	return NULL;
        
    /* cno is a valid atom index; find corresponding dist entry (if there
       is one) */
    return erts_find_dist_entry(make_atom(cno));
}


DistEntry *
erts_sysname_to_connected_dist_entry(Eterm sysname)
{
    DistEntry de;
    DistEntry *res_dep;
    de.sysname = sysname;
  
    if(erts_this_dist_entry->sysname == sysname) {
	erts_refc_inc(&erts_this_dist_entry->refc, 2);
	return erts_this_dist_entry;
    }

    erts_smp_rwmtx_rlock(&erts_dist_table_rwmtx);
    res_dep = (DistEntry *) hash_get(&erts_dist_table, (void *) &de);
    if (res_dep) {
	long refc = erts_refc_inctest(&res_dep->refc, 1);
	if (refc < 2) /* Pending delete */
	    erts_refc_inc(&res_dep->refc, 1);
    }
    erts_smp_rwmtx_runlock(&erts_dist_table_rwmtx);
    if (res_dep) {
	int deref;
	erts_smp_rwmtx_rlock(&res_dep->rwmtx);
	deref = is_nil(res_dep->cid);
	erts_smp_rwmtx_runlock(&res_dep->rwmtx);
	if (deref) {
	    erts_deref_dist_entry(res_dep);
	    res_dep = NULL;
	}
    }
    return res_dep;
}

DistEntry *erts_find_or_insert_dist_entry(Eterm sysname)
{
    DistEntry *res;
    DistEntry de;
    long refc;
    res = erts_find_dist_entry(sysname);
    if (res)
	return res;
    de.sysname = sysname;
    erts_smp_rwmtx_rwlock(&erts_dist_table_rwmtx);
    res = hash_put(&erts_dist_table, (void *) &de);
    refc = erts_refc_inctest(&res->refc, 0);
    if (refc < 2) /* New or pending delete */
	erts_refc_inc(&res->refc, 1);
    erts_smp_rwmtx_rwunlock(&erts_dist_table_rwmtx);
    return res;
}

DistEntry *erts_find_dist_entry(Eterm sysname)
{
    DistEntry *res;
    DistEntry de;
    de.sysname = sysname;
    erts_smp_rwmtx_rlock(&erts_dist_table_rwmtx);
    res = hash_get(&erts_dist_table, (void *) &de);
    if (res) {
	long refc = erts_refc_inctest(&res->refc, 1);
	if (refc < 2) /* Pending delete */
	    erts_refc_inc(&res->refc, 1);
    }
    erts_smp_rwmtx_runlock(&erts_dist_table_rwmtx);
    return res;
}

void erts_delete_dist_entry(DistEntry *dep)
{
    ASSERT(dep != erts_this_dist_entry);
    if(dep != erts_this_dist_entry) {
	erts_smp_rwmtx_rwlock(&erts_dist_table_rwmtx);
	/*
	 * Another thread might have looked up this dist entry after
	 * we decided to delete it (refc became zero). If so, the other
	 * thread incremented refc twice. Once for the new reference
	 * and once for this thread. Therefore, delete dist entry if
	 * refc is 0 or -1 after a decrement.
	 */
	if (erts_refc_dectest(&dep->refc, -1) <= 0)
	    (void) hash_erase(&erts_dist_table, (void *) dep);
	erts_smp_rwmtx_rwunlock(&erts_dist_table_rwmtx);
    }
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
	erts_smp_rwmtx_rlock(&erts_dist_table_rwmtx);
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
    for(dep = erts_not_connected_dist_entries; dep; dep = dep->next)
	i++;
    ASSERT(i == erts_no_of_not_connected_dist_entries);

    ASSERT(dist_entries == (erts_no_of_visible_dist_entries
			    + erts_no_of_hidden_dist_entries
			    + erts_no_of_not_connected_dist_entries
			    + 1 /* erts_this_dist_entry */));
#endif

    res = (hash_table_sz(&erts_dist_table)
	   + dist_entries*sizeof(DistEntry)
	   + erts_dist_cache_size());
    if (lock)
	erts_smp_rwmtx_runlock(&erts_dist_table_rwmtx);
    return res;
}

void
erts_set_dist_entry_not_connected(DistEntry *dep)
{
    ERTS_SMP_LC_ASSERT(erts_lc_is_de_rwlocked(dep));
    erts_smp_rwmtx_rwlock(&erts_dist_table_rwmtx);

    ASSERT(dep != erts_this_dist_entry);
    ASSERT(is_internal_port(dep->cid));

    if(dep->flags & DFLAG_PUBLISHED) {
	if(dep->prev) {
	    ASSERT(is_in_de_list(dep, erts_visible_dist_entries));
	    dep->prev->next = dep->next;
	}
	else {
	    ASSERT(erts_visible_dist_entries == dep);
	    erts_visible_dist_entries = dep->next;
	}

	ASSERT(erts_no_of_visible_dist_entries > 0);
	erts_no_of_visible_dist_entries--;
    }
    else {
	if(dep->prev) {
	    ASSERT(is_in_de_list(dep, erts_hidden_dist_entries));
	    dep->prev->next = dep->next;
	}
	else {
	    ASSERT(erts_hidden_dist_entries == dep);
	    erts_hidden_dist_entries = dep->next;
	}

	ASSERT(erts_no_of_hidden_dist_entries > 0);
	erts_no_of_hidden_dist_entries--;
    }

    if(dep->next)
	dep->next->prev = dep->prev;

    dep->status &= ~ERTS_DE_SFLG_CONNECTED;
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
    erts_smp_rwmtx_rwunlock(&erts_dist_table_rwmtx);
}

void
erts_set_dist_entry_connected(DistEntry *dep, Eterm cid, Uint flags)
{
    ERTS_SMP_LC_ASSERT(erts_lc_is_de_rwlocked(dep));
    erts_smp_rwmtx_rwlock(&erts_dist_table_rwmtx);

    ASSERT(dep != erts_this_dist_entry);
    ASSERT(is_nil(dep->cid));
    ASSERT(is_internal_port(cid));

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

    dep->status |= ERTS_DE_SFLG_CONNECTED;
    dep->flags = flags;
    dep->cid = cid;
    dep->connection_id++;
    dep->connection_id &= ERTS_DIST_EXT_CON_ID_MASK;
    dep->prev = NULL;

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
    erts_smp_rwmtx_rwunlock(&erts_dist_table_rwmtx);
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

    h *= PRIME0;
    h += cre & 0xff;

#if MAX_CREATION >= (1 << 8)
    h *= PRIME1;
    h += (cre >> 8) & 0xff;
#endif

#if MAX_CREATION >= (1 << 16)
    h *= PRIME2;
    h += (cre >> 16) & 0xff;
#endif

#if MAX_CREATION >= (1 << 24)
    h *= PRIME3;
    h += (cre >> 24) & 0xff;
#endif

#if 0
/* XXX Problems in older versions of GCC */
 #if MAX_CREATION >= (1UL << 32)
 #error "MAX_CREATION larger than size of expected creation storage (Uint32)"
 #endif
#endif
    return h;
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

    if(((ErlNode *) venp_tmpl) == erts_this_node)
	return venp_tmpl;

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

    if(enp == erts_this_node)
	return;

    erts_deref_dist_entry(enp->dist_entry);
#ifdef DEBUG
    sys_memset(venp, 0x55, sizeof(ErlNode));
#endif
    erts_free(ERTS_ALC_T_NODE_ENTRY, venp);

    ASSERT(node_entries > 1);
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
	erts_smp_rwmtx_rwlock(&erts_node_table_rwmtx);
#ifdef DEBUG
    hash_get_info(&hi, &erts_node_table);
    ASSERT(node_entries == hi.objs);
#endif
    res = hash_table_sz(&erts_node_table) + node_entries*sizeof(ErlNode);
    if (lock)
	erts_smp_rwmtx_rwunlock(&erts_node_table_rwmtx);
    return res;
}

void
erts_node_table_info(int to, void *to_arg)
{
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	erts_smp_rwmtx_rwlock(&erts_node_table_rwmtx);
    hash_info(to, to_arg, &erts_node_table);
    if (lock)
	erts_smp_rwmtx_rwunlock(&erts_node_table_rwmtx);
}


ErlNode *erts_find_or_insert_node(Eterm sysname, Uint creation)
{
    ErlNode *res;
    ErlNode ne;
    ne.sysname = sysname;
    ne.creation = creation;
    erts_smp_rwmtx_rwlock(&erts_node_table_rwmtx);
    res = hash_put(&erts_node_table, (void *) &ne);
    ASSERT(res);
    if (res != erts_this_node) {
	long refc = erts_refc_inctest(&res->refc, 0);
	if (refc < 2) /* New or pending delete */
	    erts_refc_inc(&res->refc, 1);
    }
    erts_smp_rwmtx_rwunlock(&erts_node_table_rwmtx);
    return res;
}

void erts_delete_node(ErlNode *enp)
{
    ASSERT(enp != erts_this_node);
    if(enp != erts_this_node) {
	erts_smp_rwmtx_rwlock(&erts_node_table_rwmtx);
	/*
	 * Another thread might have looked up this node after we
	 * decided to delete it (refc became zero). If so, the other
	 * thread incremented refc twice. Once for the new reference
	 * and once for this thread. Therefore, delete node if refc
	 * is 0 or -1 after a decrement.
	 */
	if (erts_refc_dectest(&enp->refc, -1) <= 0)
	    (void) hash_erase(&erts_node_table, (void *) enp);
	erts_smp_rwmtx_rwunlock(&erts_node_table_rwmtx);
    }
}

struct pn_data {
    int to;
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
		   erts_refc_read(&enp->refc, 1));
#endif
	pndp->no_sysname++;
    }
    pndp->no_total++;
}

void erts_print_node_info(int to,
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
	erts_smp_rwmtx_rwlock(&erts_node_table_rwmtx);
    hash_foreach(&erts_node_table, print_node, (void *) &pnd);
    if (pnd.no_sysname != 0) {
	erts_print(to, to_arg, "\n");
    }
    if (lock)
	erts_smp_rwmtx_rwunlock(&erts_node_table_rwmtx);

    if(no_sysname)
	*no_sysname = pnd.no_sysname;
    if(no_total)
	*no_total = pnd.no_total;
}

/* ----------------------------------------------------------------------- */

void
erts_set_this_node(Eterm sysname, Uint creation)
{
    erts_smp_rwmtx_rwlock(&erts_node_table_rwmtx);
    erts_smp_rwmtx_rwlock(&erts_dist_table_rwmtx);

    (void) hash_erase(&erts_dist_table, (void *) erts_this_dist_entry);
    erts_this_dist_entry->sysname = sysname;
    erts_this_dist_entry->creation = creation;
    (void) hash_put(&erts_dist_table, (void *) erts_this_dist_entry);

    (void) hash_erase(&erts_node_table, (void *) erts_this_node);
    erts_this_node->sysname = sysname;
    erts_this_node->creation = creation;
    (void) hash_put(&erts_node_table, (void *) erts_this_node);

    erts_smp_rwmtx_rwunlock(&erts_dist_table_rwmtx);
    erts_smp_rwmtx_rwunlock(&erts_node_table_rwmtx);

}

void erts_init_node_tables(void)
{
    HashFunctions f;

    f.hash  = (H_FUN)			dist_table_hash;
    f.cmp   = (HCMP_FUN)		dist_table_cmp;
    f.alloc = (HALLOC_FUN)		dist_table_alloc;
    f.free  = (HFREE_FUN)		dist_table_free;

    erts_this_dist_entry = erts_alloc(ERTS_ALC_T_DIST_ENTRY, sizeof(DistEntry));
    dist_entries = 1;

    hash_init(ERTS_ALC_T_DIST_TABLE, &erts_dist_table, "dist_table", 11, f);

    erts_hidden_dist_entries				= NULL;
    erts_visible_dist_entries				= NULL;
    erts_not_connected_dist_entries			= NULL;
    erts_no_of_hidden_dist_entries			= 0;
    erts_no_of_visible_dist_entries			= 0;
    erts_no_of_not_connected_dist_entries		= 0;

    erts_this_dist_entry->next				= NULL;
    erts_this_dist_entry->prev				= NULL;
    erts_refc_init(&erts_this_dist_entry->refc, 1); /* erts_this_node */

    erts_smp_rwmtx_init_x(&erts_this_dist_entry->rwmtx,
			  "dist_entry",
			  make_small(ERST_INTERNAL_CHANNEL_NO));
    erts_this_dist_entry->sysname			= am_Noname;
    erts_this_dist_entry->cid				= NIL;
    erts_this_dist_entry->connection_id			= 0;
    erts_this_dist_entry->status			= 0;
    erts_this_dist_entry->flags				= 0;
    erts_this_dist_entry->version			= 0;

    erts_smp_mtx_init_x(&erts_this_dist_entry->lnk_mtx,
			"dist_entry_links",
			make_small(ERST_INTERNAL_CHANNEL_NO));
    erts_this_dist_entry->node_links			= NULL;
    erts_this_dist_entry->nlinks			= NULL;
    erts_this_dist_entry->monitors			= NULL;

    erts_smp_spinlock_init_x(&erts_this_dist_entry->qlock,
			     "dist_entry_out_queue",
			     make_small(ERST_INTERNAL_CHANNEL_NO));
    erts_this_dist_entry->qflgs				= 0;
    erts_this_dist_entry->qsize				= 0;
    erts_this_dist_entry->out_queue.first		= NULL;
    erts_this_dist_entry->out_queue.last		= NULL;
    erts_this_dist_entry->suspended.first		= NULL;
    erts_this_dist_entry->suspended.last		= NULL;

    erts_this_dist_entry->finalized_out_queue.first	= NULL;
    erts_this_dist_entry->finalized_out_queue.last	= NULL;
    erts_smp_atomic_init(&erts_this_dist_entry->dist_cmd_scheduled, 0);
    erts_port_task_handle_init(&erts_this_dist_entry->dist_cmd);
    erts_this_dist_entry->send				= NULL;
    erts_this_dist_entry->cache				= NULL;

    (void) hash_put(&erts_dist_table, (void *) erts_this_dist_entry);

    f.hash  = (H_FUN)      			node_table_hash;
    f.cmp   = (HCMP_FUN)   			node_table_cmp;
    f.alloc = (HALLOC_FUN) 			node_table_alloc;
    f.free  = (HFREE_FUN)  			node_table_free;

    hash_init(ERTS_ALC_T_NODE_TABLE, &erts_node_table, "node_table", 11, f);

    erts_this_node = erts_alloc(ERTS_ALC_T_NODE_ENTRY, sizeof(ErlNode));
    node_entries = 1;

    erts_refc_init(&erts_this_node->refc, 1); /* The system itself */
    erts_this_node->sysname			= am_Noname;
    erts_this_node->creation			= 0;
    erts_this_node->dist_entry			= erts_this_dist_entry;

    (void) hash_put(&erts_node_table, (void *) erts_this_node);

    erts_smp_rwmtx_init(&erts_node_table_rwmtx, "node_table");
    erts_smp_rwmtx_init(&erts_dist_table_rwmtx, "dist_table");

    references_atoms_need_init = 1;
}

#ifdef ERTS_SMP
#ifdef ERTS_ENABLE_LOCK_CHECK
int erts_lc_is_de_rwlocked(DistEntry *dep)
{
    return erts_smp_lc_rwmtx_is_rwlocked(&dep->rwmtx);
}
int erts_lc_is_de_rlocked(DistEntry *dep)
{
    return erts_smp_lc_rwmtx_is_rlocked(&dep->rwmtx);
}
#endif
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
#ifdef HYBRID
static Eterm AM_processes;
#endif

static void setup_reference_table(void);
static Eterm reference_table_term(Uint **hpp, Uint *szp);
static void delete_reference_table(void);

#if BIG_UINT_HEAP_SIZE > 3 /* 2-tuple */
#define ID_HEAP_SIZE BIG_UINT_HEAP_SIZE
#else
#define ID_HEAP_SIZE 3 /* 2-tuple */
#endif

typedef struct node_referrer_ {
    struct node_referrer_ *next;
    int heap_ref;
    int link_ref;
    int monitor_ref;
    int ets_ref;
    int bin_ref;
    int timer_ref;
    int system_ref;
    Eterm id;
    Uint id_heap[ID_HEAP_SIZE];
} NodeReferrer;

typedef struct {
    ErlNode *node;
    NodeReferrer *referrers;
} ReferredNode;

typedef struct dist_referrer_ {
    struct dist_referrer_ *next;
    int heap_ref;
    int node_ref;
    int ctrl_ref;
    Eterm id;
    Uint creation;
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

    erts_smp_proc_unlock(proc, ERTS_PROC_LOCK_MAIN);
    erts_smp_block_system(0);
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
#ifdef HYBRID
	INIT_AM(processes);
#endif
	references_atoms_need_init = 0;
    }

    setup_reference_table();

    /* Get term size */
    size = 0;
    (void) reference_table_term(NULL, &size);

    hp = HAlloc(proc, size);
#ifdef DEBUG
    ASSERT(size > 0);
    endp = hp + size;
#endif

    /* Write term */
    res = reference_table_term(&hp, NULL);

    ASSERT(endp == hp);

    delete_reference_table();

    erts_smp_release_system();
    erts_smp_proc_lock(proc, ERTS_PROC_LOCK_MAIN);
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
	drp->id = id;
	drp->creation = creation;
	drp->heap_ref = 0;
	drp->node_ref = 0;
	drp->ctrl_ref = 0;
    }

    switch (type) {
    case NODE_REF:	drp->node_ref++;	break;
    case CTRL_REF:	drp->ctrl_ref++;	break;
    case HEAP_REF:	drp->heap_ref++;	break;
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
	erl_exit(1,
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
	referred_node->referrers = nrp;
	if(IS_CONST(id))
	    nrp->id = id;
	else {
	    Uint *hp = &nrp->id_heap[0];
	    ASSERT(is_big(id) || is_tuple(id));
	    nrp->id = copy_struct(id, size_object(id), &hp, NULL);
	}
	nrp->heap_ref = 0;
	nrp->link_ref = 0;
	nrp->monitor_ref = 0;
	nrp->ets_ref = 0;
	nrp->bin_ref = 0;
	nrp->timer_ref = 0;
	nrp->system_ref = 0;
    }

    switch (type) {
    case HEAP_REF:	nrp->heap_ref++;	break;
    case LINK_REF:	nrp->link_ref++;	break;
    case ETS_REF:	nrp->ets_ref++;		break;
    case BIN_REF:	nrp->bin_ref++;		break;
    case MONITOR_REF:   nrp->monitor_ref++;     break;
    case TIMER_REF:	nrp->timer_ref++;	break;
    case SYSTEM_REF:	nrp->system_ref++;	break;
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
	erl_exit(1, "Reference to non-existing node table entry found!\n");

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

static void
insert_offheap(ErlOffHeap *oh, int type, Eterm id)
{
    if(oh->externals) {
	ExternalThing *etp = oh->externals;
	while (etp) {
	    insert_node(etp->node, type, id);
	    etp = etp->next;
	}
    }

    if(oh->mso) {
	ProcBin *pb;
	struct insert_offheap2_arg a;
	a.type = BIN_REF;
	for(pb = oh->mso; pb; pb = pb->next) {
	    if(IsMatchProgBinary(pb->val)) {
		InsertedBin *ib;
		int insert_bin = 1;
		for (ib = inserted_bins; ib; ib = ib->next)
		    if(ib->bin_val == pb->val) {
			insert_bin = 0;
			break;
		    }
		if (insert_bin) {
#if HALFWORD_HEAP
		    UWord val = (UWord) pb->val;
		    DeclareTmpHeapNoproc(id_heap,BIG_UINT_HEAP_SIZE*2); /* extra place allocated */
#else
		    DeclareTmpHeapNoproc(id_heap,BIG_UINT_HEAP_SIZE);
#endif
		    Uint *hp = &id_heap[0];
		    InsertedBin *nib;
#if HALFWORD_HEAP
		    int actual_need = BIG_UWORD_HEAP_SIZE(val);
		    ASSERT(actual_need <= (BIG_UINT_HEAP_SIZE*2));
		    UseTmpHeapNoproc(actual_need);
		    a.id = erts_bld_uword(&hp, NULL, (UWord) val);
#else
		    UseTmpHeapNoproc(BIG_UINT_HEAP_SIZE);
		    a.id = erts_bld_uint(&hp, NULL, (Uint) pb->val);
#endif
		    erts_match_prog_foreach_offheap(pb->val,
						    insert_offheap2,
						    (void *) &a);
		    nib = erts_alloc(ERTS_ALC_T_NC_TMP, sizeof(InsertedBin));
		    nib->bin_val = pb->val;
		    nib->next = inserted_bins;
		    inserted_bins = nib;
#if HALFWORD_HEAP
		    UnUseTmpHeapNoproc(actual_need);
#else
		    UnUseTmpHeapNoproc(BIG_UINT_HEAP_SIZE);
#endif
		}
	    }
	}
    }

#if 0
    if(oh->funs) {
	/* No need to */
    }
#endif
}

static void doit_insert_monitor(ErtsMonitor *monitor, void *p)
{
    Eterm *idp = p;
    if(is_external(monitor->pid))
	insert_node(external_thing_ptr(monitor->pid)->node, MONITOR_REF, *idp);
    if(is_external(monitor->ref))
	insert_node(external_thing_ptr(monitor->ref)->node, MONITOR_REF, *idp);
}

static void doit_insert_link(ErtsLink *lnk, void *p)
{
    Eterm *idp = p;
    if(is_external(lnk->pid))
	insert_node(external_thing_ptr(lnk->pid)->node, LINK_REF, 
		    *idp);
}


static void
insert_monitors(ErtsMonitor *monitors, Eterm id)
{
    erts_doforall_monitors(monitors,&doit_insert_monitor,&id);
}

static void
insert_links(ErtsLink *lnk, Eterm id)
{
    erts_doforall_links(lnk,&doit_insert_link,&id);
}

static void doit_insert_link2(ErtsLink *lnk, void *p)
{
    Eterm *idp = p;
    if(is_external(lnk->pid))
	insert_node(external_thing_ptr(lnk->pid)->node, LINK_REF, 
		    *idp);
    insert_links(ERTS_LINK_ROOT(lnk), *idp);
}

static void
insert_links2(ErtsLink *lnk, Eterm id)
{
    erts_doforall_links(lnk,&doit_insert_link2,&id);
}

static void
insert_ets_table(DbTable *tab, void *unused)
{
    struct insert_offheap2_arg a;
    a.type = ETS_REF;
    a.id = tab->common.id;
    erts_db_foreach_offheap(tab, insert_offheap2, (void *) &a);
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

#ifdef ERTS_SMP
static void
insert_sys_msg(Eterm from, Eterm to, Eterm msg, ErlHeapFragment *bp)
{
    insert_offheap(&bp->off_heap, HEAP_REF, to);
}
#endif

static void
setup_reference_table(void)
{
    ErlHeapFragment *hfp;
    DistEntry *dep;
    HashInfo hi;
    int i;
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

    UseTmpHeapNoproc(3);
    insert_node(erts_this_node,
		SYSTEM_REF,
		TUPLE2(&heap[0], AM_system, am_undefined));

#ifdef HYBRID
    /* Insert Heap */
    insert_offheap(&erts_global_offheap,
		   HEAP_REF,
		   TUPLE2(&heap[0], AM_processes, am_undefined));
#endif
    UnUseTmpHeapNoproc(3);

    /* Insert all processes */
    for (i = 0; i < erts_max_processes; i++)
	if (process_tab[i]) {
	    ErlMessage *msg;
	    /* Insert Heap */
	    insert_offheap(&(process_tab[i]->off_heap),
			   HEAP_REF,
			   process_tab[i]->id);
	    /* Insert message buffers */
	    for(hfp = process_tab[i]->mbuf; hfp; hfp = hfp->next)
		insert_offheap(&(hfp->off_heap),
			       HEAP_REF,
			       process_tab[i]->id);
	    /* Insert msg msg buffers */
	    for (msg = process_tab[i]->msg.first; msg; msg = msg->next) {
		ErlHeapFragment *heap_frag = NULL;
		if (msg->data.attached) {
		    if (is_value(ERL_MESSAGE_TERM(msg)))
			heap_frag = msg->data.heap_frag;
		    else {
			if (msg->data.dist_ext->dep)
			    insert_dist_entry(msg->data.dist_ext->dep,
					      HEAP_REF, process_tab[i]->id, 0);
			if (is_not_nil(ERL_MESSAGE_TOKEN(msg)))
			    heap_frag = erts_dist_ext_trailer(msg->data.dist_ext);
		    }
		}
		if (heap_frag)
		    insert_offheap(&(heap_frag->off_heap),
				   HEAP_REF,
				   process_tab[i]->id);
	    }
#ifdef ERTS_SMP
	    for (msg = process_tab[i]->msg_inq.first; msg; msg = msg->next) {
		ErlHeapFragment *heap_frag = NULL;
		if (msg->data.attached) {
		    if (is_value(ERL_MESSAGE_TERM(msg)))
			heap_frag = msg->data.heap_frag;
		    else {
			if (msg->data.dist_ext->dep)
			    insert_dist_entry(msg->data.dist_ext->dep,
					      HEAP_REF, process_tab[i]->id, 0);
			if (is_not_nil(ERL_MESSAGE_TOKEN(msg)))
			    heap_frag = erts_dist_ext_trailer(msg->data.dist_ext);
		    }
		}
		if (heap_frag)
		    insert_offheap(&(heap_frag->off_heap),
				   HEAP_REF,
				   process_tab[i]->id);
	    }
#endif
	    /* Insert links */
	    if(process_tab[i]->nlinks)
		insert_links(process_tab[i]->nlinks, process_tab[i]->id);
	    if(process_tab[i]->monitors)
		insert_monitors(process_tab[i]->monitors, process_tab[i]->id);
	    /* Insert controller */
	    {
		DistEntry *dep = ERTS_PROC_GET_DIST_ENTRY(process_tab[i]);
		if (dep)
		    insert_dist_entry(dep, CTRL_REF, process_tab[i]->id, 0);
	    }
	}
    
#ifdef ERTS_SMP
    erts_foreach_sys_msg_in_q(insert_sys_msg);
#endif

    /* Insert all ports */
    for (i = 0; i < erts_max_ports; i++) {
	if (erts_port[i].status & ERTS_PORT_SFLGS_DEAD)
	    continue;

	/* Insert links */
	if(erts_port[i].nlinks)
	    insert_links(erts_port[i].nlinks, erts_port[i].id);
	/* Insert port data */
	for(hfp = erts_port[i].bp; hfp; hfp = hfp->next)
	    insert_offheap(&(hfp->off_heap), HEAP_REF, erts_port[i].id);
	/* Insert controller */
	if (erts_port[i].dist_entry)
	    insert_dist_entry(erts_port[i].dist_entry,
			      CTRL_REF,
			      erts_port[i].id,
			      0);
    }

    { /* Add binaries stored elsewhere ... */
	ErlOffHeap oh;
	ProcBin pb[2] = {{0},{0}};
	ProcBin *mso = NULL;
	int i = 0;
	Binary *default_match_spec;
	Binary *default_meta_match_spec;

	/* Only the ProcBin members val and next will be inspected
	   (by insert_offheap()) */
#undef  ADD_BINARY
#define ADD_BINARY(Bin)					\
	if ((Bin)) {					\
	    pb[i].val = (Bin);				\
	    pb[i].next = mso;				\
	    mso = &pb[i];				\
	    i++;					\
	}

	erts_get_default_trace_pattern(NULL,
				       &default_match_spec,
				       &default_meta_match_spec,
				       NULL,
				       NULL);

	ADD_BINARY(default_match_spec);
	ADD_BINARY(default_meta_match_spec);

	oh.mso = mso;
	oh.externals = NULL;
#ifndef HYBRID /* FIND ME! */
	oh.funs = NULL;
#endif
	insert_offheap(&oh, BIN_REF, AM_match_spec);
#undef  ADD_BINARY
    }

    /* Insert all dist links */

    for(dep = erts_visible_dist_entries; dep; dep = dep->next) {
	if(dep->nlinks)
	    insert_links2(dep->nlinks, dep->sysname);
	if(dep->node_links)
	    insert_links(dep->node_links, dep->sysname);
	if(dep->monitors)
	    insert_monitors(dep->monitors, dep->sysname);
    }

    for(dep = erts_hidden_dist_entries; dep; dep = dep->next) {
	if(dep->nlinks)
	    insert_links2(dep->nlinks, dep->sysname);
	if(dep->node_links)
	    insert_links(dep->node_links, dep->sysname);
	if(dep->monitors)
	    insert_monitors(dep->monitors, dep->sysname);
    }

    /* Not connected dist entries should not have any links,
       but inspect them anyway */
    for(dep = erts_not_connected_dist_entries; dep; dep = dep->next) {
	if(dep->nlinks)
	    insert_links2(dep->nlinks, dep->sysname);
	if(dep->node_links)
	    insert_links(dep->node_links, dep->sysname);
	if(dep->monitors)
	    insert_monitors(dep->monitors, dep->sysname);
    }

    /* Insert all ets tables */
    erts_db_foreach_table(insert_ets_table, NULL);

    /* Insert all bif timers */
    erts_bif_timer_foreach(insert_bif_timer, NULL);

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
reference_table_term(Uint **hpp, Uint *szp)
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

	    nrid = nrp->id;
	    if (!IS_CONST(nrp->id)) {

		Uint nrid_sz = size_object(nrp->id);
		if (szp)
		    *szp += nrid_sz;
		if (hpp)
		    nrid = copy_struct(nrp->id, nrid_sz, hpp, NULL);
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
		       && !nrp->timer_ref && !nrp->system_ref);
		tup = MK_2TUP(AM_port, nrid);
	    }
	    else if(nrp->ets_ref) {
		ASSERT(!nrp->heap_ref && !nrp->link_ref && 
		       !nrp->monitor_ref && !nrp->bin_ref
		       && !nrp->timer_ref && !nrp->system_ref);
		tup = MK_2TUP(AM_ets, nrid);
	    }
	    else if(nrp->bin_ref) {
		ASSERT(is_small(nrid) || is_big(nrid));
		ASSERT(!nrp->heap_ref && !nrp->ets_ref && !nrp->link_ref && 
		       !nrp->monitor_ref && !nrp->timer_ref
		       && !nrp->system_ref);
		tup = MK_2TUP(AM_match_spec, nrid);
	    }
	    else  {
		ASSERT(!nrp->heap_ref && !nrp->ets_ref && !nrp->bin_ref);
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
	tup = MK_3TUP(tup, MK_UINT(erts_refc_read(&referred_nodes[i].node->refc, 1)), nril);
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

	    if (is_internal_pid(drp->id)) {
		ASSERT(!drp->node_ref);
		tup = MK_2TUP(AM_process, drp->id);
	    }
	    else if(is_internal_port(drp->id)) {
		ASSERT(drp->ctrl_ref && !drp->node_ref);
		tup = MK_2TUP(AM_port, drp->id);
	    }
	    else {
		ASSERT(!drp->ctrl_ref && drp->node_ref);
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
		      MK_UINT(erts_refc_read(&referred_dists[i].dist->refc, 1)),
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

static void
delete_reference_table(void)
{
    Uint i;
    for(i = 0; i < no_referred_nodes; i++) {
	NodeReferrer *nrp;
	NodeReferrer *tnrp;
	nrp = referred_nodes[i].referrers;
	while(nrp) {
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
}

