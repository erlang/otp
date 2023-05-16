/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2018-2023. All Rights Reserved.
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
 * Description:	Monitor and link implementation.
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include <stddef.h>
#include "global.h"
#include "erl_node_tables.h"
#include "erl_monitor_link.h"
#include "erl_bif_unique.h"

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Red-black tree implementation used for monitors and links.                *
\*                                                                           */

static ERTS_INLINE Eterm
ml_get_key(ErtsMonLnkNode *mln)
{
    char *ptr = (char *) mln;
    ptr += mln->key_offset;

#ifdef ERTS_ML_DEBUG
    switch (mln->type) {
    case ERTS_MON_TYPE_ALIAS:
    case ERTS_MON_TYPE_PROC:
    case ERTS_MON_TYPE_PORT:
    case ERTS_MON_TYPE_DIST_PROC:
    case ERTS_MON_TYPE_DIST_PORT:
    case ERTS_MON_TYPE_TIME_OFFSET:
    case ERTS_MON_TYPE_RESOURCE: {
        ErtsMonitorData *mdp = erts_monitor_to_data(mln);
        ERTS_ML_ASSERT(&mdp->ref == (Eterm *) ptr);
        break;
    }
    case ERTS_LNK_TYPE_PROC:
    case ERTS_LNK_TYPE_DIST_PROC:
    case ERTS_LNK_TYPE_PORT:
    case ERTS_MON_TYPE_NODE:
    case ERTS_MON_TYPE_NODES:
    case ERTS_MON_TYPE_SUSPEND:
        ERTS_ML_ASSERT(&mln->other.item == (Eterm *) ptr);
        break;
    default:
        ERTS_INTERNAL_ERROR("Invalid type");
        break;
    }
#endif

    return *((Eterm *) ptr);
}

/*
 * Comparison functions for valid *and only valid* keys. That
 * is, if the set of valid keys is changed, this function needs
 * to be updated...
 *
 * Note that this function does *not* order terms according to
 * term order, and that the order may vary between emulator
 * restarts...
 */
static int
ml_cmp_keys(Eterm key1, Eterm key2)
{
    /*
     * A monitor key is either an internal pid, a (nodename) atom,
     * a small (monitor nodes bitmask), an ordinary internal ref,
     * or an external ref.
     *
     * Order between monitors with keys of different types:
     *  internal pid < atom < small < internal ref < external ref
     *
     * A link key is either a pid, an internal port
     *
     * Order between links with keys of different types:
     *  internal pid < internal port < external pid
     *
     */
    ERTS_ML_ASSERT(is_internal_pid(key1)
                   || is_internal_port(key1)
                   || is_atom(key1)
                   || is_small(key1)
                   || is_external_pid(key1)
                   || is_internal_ordinary_ref(key1)
                   || is_internal_pid_ref(key1)
                   || is_external_ref(key1));

    ERTS_ML_ASSERT(is_internal_pid(key2)
                   || is_internal_port(key2)
                   || is_atom(key2)
                   || is_small(key2)
                   || is_external_pid(key2)
                   || is_internal_ordinary_ref(key2)
                   || is_internal_pid_ref(key2)
                   || is_external_ref(key2));

    if (is_immed(key1)) {
        int key1_tag, key2_tag;

        ERTS_ML_ASSERT(is_internal_pid(key1)
                       || is_internal_port(key1)
                       || is_atom(key1)
                       || is_small(key1));

        if (key1 == key2)
            return 0;

        if (is_boxed(key2))
            return -1;

        ERTS_ML_ASSERT(is_internal_pid(key2)
                       || is_internal_port(key2)
                       || is_atom(key2)
                       || is_small(key2));

        key1_tag = (int) (key1 & _TAG_IMMED1_MASK);
        key2_tag = (int) (key2 & _TAG_IMMED1_MASK);

        if (key1_tag != key2_tag)
            return key1_tag - key2_tag;

        ASSERT((is_atom(key1) && is_atom(key2))
               || (is_small(key1) && is_small(key2))
               || (is_internal_pid(key1) && is_internal_pid(key2))
               || (is_internal_port(key1) && is_internal_port(key2)));

        return key1 < key2 ? -1 : 1;
    }
    else {
        Eterm *w1, hdr1;

        ERTS_ML_ASSERT(is_boxed(key1));

        w1 = boxed_val(key1);
        hdr1 = *w1;

        if (is_ref_thing_header(hdr1)) {
            Eterm *w2;

            ERTS_ML_ASSERT(is_ordinary_ref_thing(w1) || is_pid_ref_thing(w1));

            if (!is_internal_ref(key2))
                return is_immed(key2) ? 1 : -1;

            w2 = internal_ref_val(key2);

            ERTS_ML_ASSERT(is_ordinary_ref_thing(w2) || is_pid_ref_thing(w2));

            return sys_memcmp((void *) internal_non_magic_ref_thing_numbers(w1),
                              (void *) internal_non_magic_ref_thing_numbers(w2),
                              ERTS_REF_NUMBERS*sizeof(Uint32));
        }

        ERTS_ML_ASSERT(is_external(key1));

        if (is_not_external(key2))
            return 1;
        else {
            Uint ndw1, ndw2;
            ExternalThing *et1, *et2;
            ErlNode *n1, *n2;

            ERTS_ML_ASSERT((is_external_ref(key1) && is_external_ref(key2))
                           || (is_external_pid(key1) && is_external_pid(key2)));

            et1 = (ExternalThing *) w1;
            et2 = (ExternalThing *) external_val(key2);

            n1 = et1->node;
            n2 = et2->node;

            if (n1 != n2) {
                if (n1->sysname != n2->sysname)
                    return n1->sysname < n2->sysname ? -1 : 1;
                ASSERT(n1->creation != n2->creation);
                return n1->creation < n2->creation ? -1 : 1;
            }

            ndw1 = external_thing_data_words(et1);
            ndw2 = external_thing_data_words(et1);
            if (ndw1 != ndw2)
                return ndw1 < ndw2 ? -1 : 1;

            return sys_memcmp((void *) &et1->data.ui[0],
                              (void *) &et2->data.ui[0],
                              ndw1*sizeof(Eterm));
        }
    }
}

#define ERTS_ML_TPFLG_RED               (((UWord) 1) << 0)

#define ERTS_ML_TPFLGS_MASK             (ERTS_ML_TPFLG_RED)

#define ERTS_RBT_PREFIX mon_lnk
#define ERTS_RBT_T ErtsMonLnkNode
#define ERTS_RBT_KEY_T Eterm
#define ERTS_RBT_FLAGS_T UWord
#define ERTS_RBT_INIT_EMPTY_TNODE(T)					\
    do {								\
	(T)->node.tree.parent = (UWord) NULL;                           \
	(T)->node.tree.right = NULL;                                    \
	(T)->node.tree.left = NULL;                                     \
    } while (0)
#define ERTS_RBT_IS_RED(T)						\
    (!!((T)->node.tree.parent & ERTS_ML_TPFLG_RED))
#define ERTS_RBT_SET_RED(T)						\
    ((T)->node.tree.parent |= ERTS_ML_TPFLG_RED)
#define ERTS_RBT_IS_BLACK(T)						\
    (!ERTS_RBT_IS_RED((T)))
#define ERTS_RBT_SET_BLACK(T)						\
    ((T)->node.tree.parent &= ~ERTS_ML_TPFLG_RED)
#define ERTS_RBT_GET_FLAGS(T)						\
    ((T)->node.tree.parent & ERTS_ML_TPFLGS_MASK)
#define ERTS_RBT_SET_FLAGS(T, F)					\
    do {								\
	ERTS_ML_ASSERT((((UWord) (F)) & ~ERTS_ML_TPFLGS_MASK) == 0);	\
	(T)->node.tree.parent &= ~ERTS_ML_TPFLGS_MASK;                 \
	(T)->node.tree.parent |= (F);                                   \
    } while (0)
#define ERTS_RBT_GET_PARENT(T)						\
    ((ERTS_RBT_T *) ((T)->node.tree.parent & ~ERTS_ML_TPFLGS_MASK))
#define ERTS_RBT_SET_PARENT(T, P)					\
    do {								\
	ERTS_ML_ASSERT((((UWord) (P)) & ERTS_ML_TPFLGS_MASK) == 0);	\
	(T)->node.tree.parent &= ERTS_ML_TPFLGS_MASK;                  \
	(T)->node.tree.parent |= (UWord) (P);                           \
    } while (0)
#define ERTS_RBT_GET_RIGHT(T) ((T)->node.tree.right)
#define ERTS_RBT_SET_RIGHT(T, R) ((T)->node.tree.right = (R))
#define ERTS_RBT_GET_LEFT(T) ((T)->node.tree.left)
#define ERTS_RBT_SET_LEFT(T, L) ((T)->node.tree.left = (L))
#define ERTS_RBT_GET_KEY(T) (ml_get_key((T)))
#define ERTS_RBT_CMP_KEYS(KX, KY) (ml_cmp_keys((KX), (KY)))
#define ERTS_RBT_WANT_DELETE
#define ERTS_RBT_WANT_LOOKUP
#define ERTS_RBT_WANT_LOOKUP_INSERT
#define ERTS_RBT_WANT_LOOKUP_CREATE
#define ERTS_RBT_WANT_INSERT
#define ERTS_RBT_WANT_REPLACE
#define ERTS_RBT_WANT_FOREACH
#define ERTS_RBT_WANT_FOREACH_YIELDING
#define ERTS_RBT_WANT_FOREACH_DESTROY
#define ERTS_RBT_WANT_FOREACH_DESTROY_YIELDING
#define ERTS_RBT_UNDEF

#include "erl_rbtree.h"

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Tree Operations                                                           *
\*                                                                           */

static ErtsMonLnkNode *
ml_rbt_lookup(ErtsMonLnkNode *root, Eterm ref)
{
    ErtsMonLnkNode *ml = mon_lnk_rbt_lookup(root, ref);
    ASSERT(!ml || (ml->flags & ERTS_ML_FLG_IN_TABLE));
    return ml;
}

static ErtsMonLnkNode *
ml_rbt_lookup_insert(ErtsMonLnkNode **root, ErtsMonLnkNode *ml)
{
    ErtsMonLnkNode *res;
    ERTS_ML_ASSERT(!(ml->flags & ERTS_ML_FLG_IN_TABLE));
    res = mon_lnk_rbt_lookup_insert(root, ml);
    if (!res)
        ml->flags |= ERTS_ML_FLG_IN_TABLE;
    return res;
}

static ErtsMonLnkNode *
ml_rbt_lookup_create(ErtsMonLnkNode ** root, Eterm key,
                     ErtsMonLnkNode *(*create)(Eterm, void *),
                     void *carg, int *created)
{
    ErtsMonLnkNode *ml;
    ml = mon_lnk_rbt_lookup_create(root, key, create, carg, created);
    if (*created)
        ml->flags |= ERTS_ML_FLG_IN_TABLE;
    return ml;
}

static void
ml_rbt_insert(ErtsMonLnkNode **root, ErtsMonLnkNode *ml)
{
#ifdef ERTS_ML_DEBUG
    ErtsMonLnkNode *res;
    ERTS_ML_ASSERT(!(ml->flags & ERTS_ML_FLG_IN_TABLE));
    res = ml_rbt_lookup(*root, ml_get_key(ml));
    ERTS_ML_ASSERT(res == NULL);
#endif

    mon_lnk_rbt_insert(root, ml);
    ml->flags |= ERTS_ML_FLG_IN_TABLE;
}

static void
ml_rbt_replace(ErtsMonLnkNode **root, ErtsMonLnkNode *old, ErtsMonLnkNode *new)
{
    ERTS_ML_ASSERT(old->flags & ERTS_ML_FLG_IN_TABLE);
    ERTS_ML_ASSERT(!(new->flags & ERTS_ML_FLG_IN_TABLE));

    mon_lnk_rbt_replace(root, old, new);
    old->flags &= ~ERTS_ML_FLG_IN_TABLE;
    new->flags |= ERTS_ML_FLG_IN_TABLE;
}

static void
ml_rbt_delete(ErtsMonLnkNode **root, ErtsMonLnkNode *ml)
{
    ERTS_ML_ASSERT(ml->flags & ERTS_ML_FLG_IN_TABLE);
    mon_lnk_rbt_delete(root, ml);
    ml->flags &= ~ERTS_ML_FLG_IN_TABLE;
}


static void
ml_rbt_foreach(ErtsMonLnkNode *root,
               ErtsMonLnkNodeFunc func,
               void *arg)
{
    mon_lnk_rbt_foreach(root, func, arg);
}

typedef struct {
    ErtsMonLnkNode *root;
    mon_lnk_rbt_yield_state_t rbt_ystate;
} ErtsMonLnkYieldState;

static int
ml_rbt_foreach_yielding(ErtsMonLnkNode *root,
                        ErtsMonLnkNodeFunc func,
                        void *arg,
                        void **vyspp,
                        Sint limit)
{
    int res;
    ErtsMonLnkYieldState ys = {root, ERTS_RBT_YIELD_STAT_INITER};
    ErtsMonLnkYieldState *ysp;

    ysp = (ErtsMonLnkYieldState *) *vyspp;
    if (!ysp)
	ysp = &ys;
    res = mon_lnk_rbt_foreach_yielding(ysp->root, func, arg,
                                       &ysp->rbt_ystate, limit);
    if (res > 0) {
	if (ysp != &ys)
	    erts_free(ERTS_ALC_T_ML_YIELD_STATE, ysp);
	*vyspp = NULL;
    }
    else {

	if (ysp == &ys) {
	    ysp = erts_alloc(ERTS_ALC_T_ML_YIELD_STATE,
			     sizeof(ErtsMonLnkYieldState));
	    sys_memcpy((void *) ysp, (void *) &ys,
		       sizeof(ErtsMonLnkYieldState));
	}

	*vyspp = (void *) ysp;
    }

    return res;
}

typedef struct {
    ErtsMonLnkNodeFunc func;
    void *arg;
} ErtsMonLnkForeachDeleteContext;

static int
rbt_wrap_foreach_delete(ErtsMonLnkNode *ml, void *vctxt, Sint reds)
{
    ErtsMonLnkForeachDeleteContext *ctxt = vctxt;
    ERTS_ML_ASSERT(ml->flags & ERTS_ML_FLG_IN_TABLE);
    ml->flags &= ~ERTS_ML_FLG_IN_TABLE;
    return ctxt->func(ml, ctxt->arg, reds);
}

static void
ml_rbt_foreach_delete(ErtsMonLnkNode **root,
                      ErtsMonLnkNodeFunc func,
                      void *arg)
{
    ErtsMonLnkForeachDeleteContext ctxt;
    ctxt.func = func;
    ctxt.arg = arg;
    mon_lnk_rbt_foreach_destroy(root,
                                rbt_wrap_foreach_delete,
                                (void *) &ctxt);
}

static int
ml_rbt_foreach_delete_yielding(ErtsMonLnkNode **root,
                               ErtsMonLnkNodeFunc func,
                               void *arg,
                               void **vyspp,
                               Sint limit)
{
    int res;
    ErtsMonLnkYieldState ys = {*root, ERTS_RBT_YIELD_STAT_INITER};
    ErtsMonLnkYieldState *ysp;
    ErtsMonLnkForeachDeleteContext ctxt;
    ctxt.func = func;
    ctxt.arg = arg;

    ysp = (ErtsMonLnkYieldState *) *vyspp;
    if (!ysp) {
        *root = NULL;
	ysp = &ys;
    }
    res = mon_lnk_rbt_foreach_destroy_yielding(&ysp->root,
                                               rbt_wrap_foreach_delete,
                                               (void *) &ctxt,
                                               &ysp->rbt_ystate,
                                               limit);
    if (res > 0) {
	if (ysp != &ys)
	    erts_free(ERTS_ALC_T_ML_YIELD_STATE, ysp);
	*vyspp = NULL;
    }
    else {

	if (ysp == &ys) {
	    ysp = erts_alloc(ERTS_ALC_T_ML_YIELD_STATE,
			     sizeof(ErtsMonLnkYieldState));
	    sys_memcpy((void *) ysp, (void *) &ys,
		       sizeof(ErtsMonLnkYieldState));
	}

	*vyspp = (void *) ysp;
    }

    return res;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * List Operations                                                           *
\*                                                                           */

static int
ml_dl_list_foreach_yielding(ErtsMonLnkNode *list,
                            ErtsMonLnkNodeFunc func,
                            void *arg,
                            void **vyspp,
                            Sint reds)
{
    ErtsMonLnkNode *ml = (ErtsMonLnkNode *) *vyspp;

    ERTS_ML_ASSERT(!ml || list);

    if (!ml)
        ml = list;

    if (ml) {
        do {
            ERTS_ML_ASSERT(ml->flags & ERTS_ML_FLG_IN_TABLE);
            reds -= func(ml, arg, reds);
            ml = ml->node.list.next;
        } while (ml != list && reds > 0);
        if (ml != list) {
            *vyspp = (void *) ml;
            return 0; /* yield */
        }
    }

    *vyspp = NULL;
    return reds <= 0 ? 1 : reds; /* done */
}

static int
ml_dl_list_foreach_delete_yielding(ErtsMonLnkNode **list,
                                   ErtsMonLnkNodeFunc func,
                                   void *arg,
                                   void **vyspp,
                                   Sint reds)
{
    ErtsMonLnkNode *first = *list;
    ErtsMonLnkNode *ml = (ErtsMonLnkNode *) *vyspp;

    ERTS_ML_ASSERT(!ml || first);

    if (!ml)
        ml = first;

    if (ml) {
        do {
            ErtsMonLnkNode *next = ml->node.list.next;
            ERTS_ML_ASSERT(ml->flags & ERTS_ML_FLG_IN_TABLE);
            ml->flags &= ~ERTS_ML_FLG_IN_TABLE;
            reds -= func(ml, arg, reds);
            ml = next;
        } while (ml != first && reds > 0);
        if (ml != first) {
            *vyspp = (void *) ml;
            return 0; /* yield */
        }
    }

    *vyspp = NULL;
    *list = NULL;
    return reds <= 0 ? 1 : reds; /* done */
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Misc                                                                      *
\*                                                                           */

ErtsMonLnkDist *
erts_mon_link_dist_create(Eterm nodename)
{
    ErtsMonLnkDist *mld = erts_alloc(ERTS_ALC_T_ML_DIST,
                                     sizeof(ErtsMonLnkDist));
    mld->nodename = nodename;
    mld->connection_id = ~((Uint32) 0);
    erts_atomic_init_nob(&mld->refc, 1);
    erts_mtx_init(&mld->mtx, "dist_entry_links", nodename,
                  ERTS_LOCK_FLAGS_CATEGORY_DISTRIBUTION);
    mld->alive = !0;
    mld->links = NULL;
    mld->monitors = NULL;
    mld->orig_name_monitors = NULL;
    mld->dist_pend_spawn_exit = NULL;
    return mld;
}

static void
mon_link_dist_destroy(void* vmld)
{
    ErtsMonLnkDist *mld = (ErtsMonLnkDist*)vmld;
    ERTS_ML_ASSERT(erts_atomic_read_nob(&mld->refc) == 0);
    ERTS_ML_ASSERT(!mld->alive);
    ERTS_ML_ASSERT(!mld->links);
    ERTS_ML_ASSERT(!mld->monitors);
    ERTS_ML_ASSERT(!mld->orig_name_monitors);
    ERTS_ML_ASSERT(!mld->dist_pend_spawn_exit);

    erts_mtx_destroy(&mld->mtx);
    erts_free(ERTS_ALC_T_ML_DIST, mld);
}

void
erts_schedule_mon_link_dist_destruction__(ErtsMonLnkDist *mld)
{
    ERTS_ML_ASSERT(erts_atomic_read_nob(&mld->refc) == 0);
    ERTS_ML_ASSERT(!mld->alive);
    ERTS_ML_ASSERT(!mld->links);
    ERTS_ML_ASSERT(!mld->monitors);
    ERTS_ML_ASSERT(!mld->orig_name_monitors);

    erts_schedule_thr_prgr_later_cleanup_op(mon_link_dist_destroy,
                                            mld,
                                            &mld->cleanup_lop,
                                            sizeof(ErtsMonLnkDist));
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Monitor Operations                                                        *
\*                                                                           */

#ifdef ERTS_ML_DEBUG
size_t erts_monitor_origin_offset;
size_t erts_monitor_origin_key_offset;
size_t erts_monitor_target_offset;
size_t erts_monitor_target_key_offset;
size_t erts_monitor_node_key_offset;
#endif

static ERTS_INLINE void
monitor_init(void)
{
#ifdef ERTS_ML_DEBUG
    erts_monitor_origin_offset = offsetof(ErtsMonitorData, origin);
    erts_monitor_origin_key_offset = offsetof(ErtsMonitorData, ref);
    ASSERT(erts_monitor_origin_key_offset >= erts_monitor_origin_offset);
    erts_monitor_origin_key_offset -= erts_monitor_origin_offset;
    erts_monitor_target_offset = offsetof(ErtsMonitorData, u.target);
    erts_monitor_target_key_offset = offsetof(ErtsMonitorData, ref);
    ASSERT(erts_monitor_target_key_offset >= erts_monitor_target_offset);
    erts_monitor_target_key_offset -= erts_monitor_target_offset;
    erts_monitor_node_key_offset = offsetof(ErtsMonitor, other.item);
#endif
}

ErtsMonitor *
erts_monitor_tree_lookup(ErtsMonitor *root, Eterm key)
{
    ERTS_ML_ASSERT(is_internal_ordinary_ref(key)
                   || is_internal_pid_ref(key)
                   || is_external_ref(key)
                   || is_atom(key)
                   || is_small(key)
                   || is_internal_pid(key));
    return (ErtsMonitor *) ml_rbt_lookup((ErtsMonLnkNode *) root, key);
}

ErtsMonitor *
erts_monotor_tree_lookup_insert(ErtsMonitor **root, ErtsMonitor *mon)
{
    return (ErtsMonitor *) ml_rbt_lookup_insert((ErtsMonLnkNode **) root,
                                                (ErtsMonLnkNode *) mon);
}

typedef struct {
    Uint16 type;
    Eterm origin;
} ErtsMonitorCreateCtxt;

static ErtsMonLnkNode *
create_monitor(Eterm target, void *vcctxt)
{
    ErtsMonitorCreateCtxt *cctxt = vcctxt;
    ErtsMonitorData *mdp = erts_monitor_create(cctxt->type,
                                               NIL,
                                               cctxt->origin,
                                               target,
                                               NIL,
                                               THE_NON_VALUE);
    ERTS_ML_ASSERT(ml_cmp_keys(ml_get_key(&mdp->origin), target) == 0);
    return (ErtsMonLnkNode *) &mdp->origin;
}

ErtsMonitor *
erts_monitor_tree_lookup_create(ErtsMonitor **root, int *created, Uint16 type,
                                Eterm origin, Eterm target)
{
    ErtsMonitor *res;
    ErtsMonitorCreateCtxt cctxt = {type, origin};

    ERTS_ML_ASSERT(type == ERTS_MON_TYPE_NODE
                   || type == ERTS_MON_TYPE_NODES
                   || type == ERTS_MON_TYPE_SUSPEND);

    res = (ErtsMonitor *) ml_rbt_lookup_create((ErtsMonLnkNode **) root,
                                               target, create_monitor,
                                               (void *) &cctxt,
                                               created);

    ERTS_ML_ASSERT(res && erts_monitor_is_origin(res));

    return res;
}

void
erts_monitor_tree_insert(ErtsMonitor **root, ErtsMonitor *mon)
{
    ml_rbt_insert((ErtsMonLnkNode **) root, (ErtsMonLnkNode *) mon);
}

void
erts_monitor_tree_replace(ErtsMonitor **root, ErtsMonitor *old, ErtsMonitor *new)
{
    ml_rbt_replace((ErtsMonLnkNode **) root,
                   (ErtsMonLnkNode *) old,
                   (ErtsMonLnkNode *) new);
}

void
erts_monitor_tree_delete(ErtsMonitor **root, ErtsMonitor *mon)
{
    ml_rbt_delete((ErtsMonLnkNode **) root, (ErtsMonLnkNode *) mon);
}

void
erts_monitor_tree_foreach(ErtsMonitor *root,
                          ErtsMonitorFunc func,
                          void *arg)
{
    ml_rbt_foreach((ErtsMonLnkNode *) root,
                   (ErtsMonLnkNodeFunc) func,
                   arg);
}

void
erts_debug_monitor_tree_destroying_foreach(ErtsMonitor *root,
                                           ErtsMonitorFunc func,
                                           void *arg,
                                           void *vysp)
{
    void *tmp_vysp;
    Sint reds;
    if (!vysp)
        tmp_vysp = NULL;
    else {
        tmp_vysp = erts_alloc(ERTS_ALC_T_ML_YIELD_STATE,
                              sizeof(ErtsMonLnkYieldState));
        sys_memcpy(tmp_vysp, tmp_vysp, sizeof(ErtsMonLnkYieldState));
    }
    do {
        reds = ml_rbt_foreach_yielding((ErtsMonLnkNode *) root,
                                       (ErtsMonLnkNodeFunc) func,
                                       arg, &tmp_vysp, (Sint) INT_MAX);
    } while (reds <= 0);
    ERTS_ML_ASSERT(!tmp_vysp);
}

int
erts_monitor_tree_foreach_yielding(ErtsMonitor *root,
                                   ErtsMonitorFunc func,
                                   void *arg,
                                   void **vyspp,
                                   Sint limit)
{
    return ml_rbt_foreach_yielding((ErtsMonLnkNode *) root,
                                   (int (*)(ErtsMonLnkNode*, void*, Sint)) func,
                                   arg, vyspp, limit);
}

void
erts_monitor_tree_foreach_delete(ErtsMonitor **root,
                                 ErtsMonitorFunc func,
                                 void *arg)
{
    ml_rbt_foreach_delete((ErtsMonLnkNode **) root,
                          (int (*)(ErtsMonLnkNode*, void*, Sint)) func,
                          arg);
}

int
erts_monitor_tree_foreach_delete_yielding(ErtsMonitor **root,
                                          ErtsMonitorFunc func,
                                          void *arg,
                                          void **vyspp,
                                          Sint limit)
{
    return ml_rbt_foreach_delete_yielding((ErtsMonLnkNode **) root,
                                          (int (*)(ErtsMonLnkNode*, void*, Sint)) func,
                                          arg, vyspp, limit);
}

void
erts_monitor_list_foreach(ErtsMonitor *list,
                          ErtsMonitorFunc func,
                          void *arg)
{
    void *ystate = NULL;
    while (!ml_dl_list_foreach_yielding((ErtsMonLnkNode *) list,
                                        (int (*)(ErtsMonLnkNode *, void *, Sint)) func,
                                        arg, &ystate, (Sint) INT_MAX));
}

void
erts_debug_monitor_list_destroying_foreach(ErtsMonitor *list,
                                           ErtsMonitorFunc func,
                                           void *arg,
                                           void *vysp)
{
    void *tmp_vysp = vysp;
    while (!ml_dl_list_foreach_yielding((ErtsMonLnkNode *) list,
                                        (int (*)(ErtsMonLnkNode *, void *, Sint)) func,
                                        arg, &tmp_vysp, (Sint) INT_MAX));
    ERTS_ML_ASSERT(!tmp_vysp);
}

int
erts_monitor_list_foreach_yielding(ErtsMonitor *list,
                                   ErtsMonitorFunc func,
                                   void *arg,
                                   void **vyspp,
                                   Sint limit)
{
    return ml_dl_list_foreach_yielding((ErtsMonLnkNode *) list,
                                       (int (*)(ErtsMonLnkNode *, void *, Sint)) func,
                                       arg, vyspp, limit);
}

void
erts_monitor_list_foreach_delete(ErtsMonitor **list,
                                 ErtsMonitorFunc func,
                                 void *arg)
{
    void *ystate = NULL;
    while (!ml_dl_list_foreach_delete_yielding((ErtsMonLnkNode **) list,
                                               (int (*)(ErtsMonLnkNode*, void*, Sint)) func,
                                               arg, &ystate, (Sint) INT_MAX));
}

int
erts_monitor_list_foreach_delete_yielding(ErtsMonitor **list,
                                          ErtsMonitorFunc func,
                                          void *arg,
                                          void **vyspp,
                                          Sint limit)
{
    return ml_dl_list_foreach_delete_yielding((ErtsMonLnkNode **) list,
                                              (int (*)(ErtsMonLnkNode*, void*, Sint)) func,
                                              arg, vyspp, limit);

}

static Eterm
mk_heap_fragment_eterm(Eterm value)
{
    /*
     * Save non-immediate value in its own heap fragment. Return
     * pointer to heap fragment as a tagged continuation pointer
     * which can be stored as an Eterm. Tagged pointer to value
     * is stored immediately after heap data.
     */

    Uint vsz = size_object(value);
    ErlHeapFragment *bp = new_message_buffer(vsz+1);
    Eterm *hp = &bp->mem[0];
    bp->used_size = vsz;
    ASSERT(bp->alloc_size == vsz+1);
    bp->mem[vsz] = copy_struct(value, vsz, &hp, &bp->off_heap);
    return make_cp((void*)bp);
}

ErtsMonitorData *
erts_monitor_create(Uint16 type, Eterm ref, Eterm orgn, Eterm trgt, Eterm name, Eterm tag)
{
    ErtsMonitorData *mdp;
    Uint16 tag_flag;

    switch (type) {
    case ERTS_MON_TYPE_PROC:
    case ERTS_MON_TYPE_PORT:
        if (is_nil(name)) {
            Eterm *hp;
            Eterm *ref_thing;

        case ERTS_MON_TYPE_TIME_OFFSET:

            ERTS_ML_ASSERT(is_nil(name));
            ERTS_ML_ASSERT(is_immed(orgn) && is_immed(trgt));
            ERTS_ML_ASSERT(is_internal_ordinary_ref(ref)
                           || is_internal_pid_ref(ref));

            if (is_non_value(tag)) {
                ErtsMonitorDataHeap *mdhp;
                mdhp = erts_alloc(ERTS_ALC_T_MONITOR, sizeof(ErtsMonitorDataHeap));
                mdp = &mdhp->md;
                hp = &mdhp->ref_heap[0];
                ERTS_ML_ASSERT(((void *) mdp) == ((void *) mdhp));
                tag_flag = (Uint16) 0;
            }
            else {
                ErtsMonitorDataTagHeap *mdthp;
                mdthp = erts_alloc(ERTS_ALC_T_MONITOR_TAG, sizeof(ErtsMonitorDataTagHeap));
                mdp = &mdthp->md;
                hp = &mdthp->heap[0];
                ERTS_ML_ASSERT(((void *) mdp) == ((void *) mdthp));
                *hp = is_immed(tag) ? tag : mk_heap_fragment_eterm(tag);
                hp++;
                tag_flag = ERTS_ML_FLG_TAG;
            }

            ref_thing = internal_ref_val(ref);
            sys_memcpy((void *) hp,
                       (void *) ref_thing,
                       sizeof(Eterm)*(1 + thing_arityval(*ref_thing)));
            mdp->ref = make_internal_ref(hp);
            mdp->origin.other.item = trgt;
            mdp->origin.offset = (Uint16) offsetof(ErtsMonitorData, origin);
            mdp->origin.key_offset = (Uint16) offsetof(ErtsMonitorData, ref);
            ERTS_ML_ASSERT(mdp->origin.key_offset >= mdp->origin.offset);
            mdp->origin.key_offset -= mdp->origin.offset;
            mdp->origin.flags = tag_flag;
            mdp->origin.type = type;

            mdp->u.target.other.item = orgn;
            mdp->u.target.offset = (Uint16) offsetof(ErtsMonitorData, u.target);
            mdp->u.target.key_offset = (Uint16) offsetof(ErtsMonitorData, ref);
            ERTS_ML_ASSERT(mdp->u.target.key_offset >= mdp->u.target.offset);
            mdp->u.target.key_offset -= mdp->u.target.offset;
            mdp->u.target.flags = ERTS_ML_FLG_TARGET | tag_flag;
            mdp->u.target.type = type;
            erts_atomic32_init_nob(&mdp->refc, 2);
            break;
        }
    case ERTS_MON_TYPE_DIST_PROC:
    case ERTS_MON_TYPE_DIST_PORT:
    case ERTS_MON_TYPE_RESOURCE:
    case ERTS_MON_TYPE_NODE:
    case ERTS_MON_TYPE_NODES: {
        ErtsMonitorDataExtended *mdep;
        Uint size = sizeof(ErtsMonitorDataExtended) - sizeof(Eterm);
        Uint rsz, osz, tsz, thfsz;
        Eterm *hp;
        ErlOffHeap oh;
        Uint16 name_flag;
        Uint16 pending_flag;

        rsz = is_immed(ref) ? 0 : size_object(ref);
        if (trgt != am_pending) {
            if (is_not_immed(trgt))
                tsz = size_object(trgt);
            else
                tsz = 0;
            pending_flag = (Uint16) 0;
            name_flag = is_nil(name) ? ((Uint16) 0) : ERTS_ML_FLG_NAME;
        }
        else {
            /* Pending spawn_request() */
            pending_flag = ERTS_ML_FLG_SPAWN_PENDING;
            /* Prepare for storage of external pid */
            tsz = EXTERNAL_PID_HEAP_SIZE;
            /* name contains tag */
            
            /* Not by name */
            name_flag = (Uint16) 0;
            
        }
        if (type == ERTS_MON_TYPE_RESOURCE)
            osz = 0;
        else
            osz = is_immed(orgn) ? 0 : size_object(orgn);

        thfsz = is_non_value(tag) ? 0 : 1;
        
        size += (rsz + osz + tsz + thfsz) * sizeof(Eterm);

        mdep = erts_alloc(ERTS_ALC_T_MONITOR_EXT, size);

        ERTS_INIT_OFF_HEAP(&oh);

        hp = &mdep->heap[0];
        if (!thfsz)
            tag_flag = (Uint16) 0;
        else {
            *hp = is_immed(tag) ? tag : mk_heap_fragment_eterm(tag);
            hp++;
            tag_flag = ERTS_ML_FLG_TAG;
        }

        if (pending_flag) {
            /* Make room for the future pid... */
#ifdef DEBUG
            int i;
            for (i = 0; i < EXTERNAL_PID_HEAP_SIZE; i++)
                hp[i] = THE_NON_VALUE;
#endif
            hp += EXTERNAL_PID_HEAP_SIZE;
        }

        mdp = &mdep->md;
        ERTS_ML_ASSERT(((void *) mdp) == ((void *) mdep));

        mdp->ref = rsz ? copy_struct(ref, rsz, &hp, &oh) : ref;

        mdp->origin.other.item = tsz ? copy_struct(trgt, tsz, &hp, &oh) : trgt;
        mdp->origin.offset = (Uint16) offsetof(ErtsMonitorData, origin);
        mdp->origin.flags = ERTS_ML_FLG_EXTENDED|name_flag|pending_flag|tag_flag;
        mdp->origin.type = type;

        if (type == ERTS_MON_TYPE_RESOURCE)
            mdp->u.target.other.ptr = (void *) orgn;
        else
            mdp->u.target.other.item = osz ? copy_struct(orgn, osz, &hp, &oh) : orgn;
        mdp->u.target.offset = (Uint16) offsetof(ErtsMonitorData, u.target);
        mdp->u.target.flags = ERTS_ML_FLG_TARGET|ERTS_ML_FLG_EXTENDED|name_flag|tag_flag;
        mdp->u.target.type = type;

        if (type == ERTS_MON_TYPE_NODE || type == ERTS_MON_TYPE_NODES) {
            mdep->u.refc = 0;
            mdp->origin.key_offset = (Uint16) offsetof(ErtsMonitor, other.item);
            mdp->u.target.key_offset = (Uint16) offsetof(ErtsMonitor, other.item);
            ERTS_ML_ASSERT(!oh.first);
            mdep->uptr.node_monitors = NULL;
        }
        else {
            mdep->u.name = name;
            if (pending_flag) {
                /* spawn_request() tag is in 'name' */
                if (is_not_immed(name)) {
                    /*
                     * Save the tag in its own heap fragment with a
                     * little trick:
                     *
                     * bp->mem[0]             = Beginning of heap
                     * bp->mem[bp->used_size] = The tag
                     * mdep->u.name           = Countinuation
                     *                          pointer to heap
                     *                          fragment...
                     */
                    mdep->u.name = mk_heap_fragment_eterm(name);
                }
            }

            mdp->origin.key_offset = (Uint16) offsetof(ErtsMonitorData, ref);
            ERTS_ML_ASSERT(mdp->origin.key_offset >= mdp->origin.offset);
            mdp->origin.key_offset -= mdp->origin.offset;

            mdp->u.target.key_offset = (Uint16) offsetof(ErtsMonitorData, ref);
            ERTS_ML_ASSERT(mdp->u.target.key_offset >= mdp->u.target.offset);
            mdp->u.target.key_offset -= mdp->u.target.offset;

            mdep->uptr.ohhp = oh.first;
        }
        mdep->dist = NULL;
        erts_atomic32_init_nob(&mdp->refc, 2);
        break;
    }
    case ERTS_MON_TYPE_SUSPEND: {
        ErtsMonitorSuspend *msp;

        ERTS_ML_ASSERT(is_nil(name));
        ERTS_ML_ASSERT(is_nil(ref));
        ERTS_ML_ASSERT(is_internal_pid(orgn) && is_internal_pid(trgt));

        msp = erts_alloc(ERTS_ALC_T_MONITOR_SUSPEND,
                         sizeof(ErtsMonitorSuspend));
        mdp = &msp->md;
        ERTS_ML_ASSERT(((void *) mdp) == ((void *) msp));

        mdp->ref = NIL;

        mdp->origin.other.item = trgt;
        mdp->origin.offset = (Uint16) offsetof(ErtsMonitorData, origin);
        mdp->origin.key_offset = (Uint16) offsetof(ErtsMonitor, other.item);
        ERTS_ML_ASSERT(mdp->origin.key_offset >= mdp->origin.offset);
        mdp->origin.flags = (Uint16) ERTS_ML_FLG_EXTENDED;
        mdp->origin.type = type;

        mdp->u.target.other.item = orgn;
        mdp->u.target.offset = (Uint16) offsetof(ErtsMonitorData, u.target);
        mdp->u.target.key_offset = (Uint16) offsetof(ErtsMonitor, other.item);
        mdp->u.target.flags = ERTS_ML_FLG_TARGET|ERTS_ML_FLG_EXTENDED;
        mdp->u.target.type = type;

        msp->next = NULL;
        erts_atomic_init_relb(&msp->state, 0);
        erts_atomic32_init_nob(&mdp->refc, 2);
        break;
    }
    case ERTS_MON_TYPE_ALIAS: {
        Eterm *ref_thing;

        ERTS_ML_ASSERT(is_nil(name) && is_nil(trgt));
        ERTS_ML_ASSERT(is_internal_pid(orgn));
        ERTS_ML_ASSERT(is_internal_pid_ref(ref));
        ERTS_ML_ASSERT(orgn == erts_get_pid_of_ref(ref));
        
        mdp = erts_alloc(ERTS_ALC_T_ALIAS, sizeof(ErtsMonitorData));

        ref_thing = internal_ref_val(ref);
        sys_memcpy((void *) &mdp->u.ref_heap[0],
                   (void *) ref_thing,
                   sizeof(Eterm)*(1 + thing_arityval(*ref_thing)));
        mdp->ref = make_internal_ref(&mdp->u.ref_heap[0]);
        mdp->origin.other.item = orgn;
        mdp->origin.offset = (Uint16) offsetof(ErtsMonitorData, origin);
        mdp->origin.key_offset = (Uint16) offsetof(ErtsMonitorData, ref);
        ERTS_ML_ASSERT(mdp->origin.key_offset >= mdp->origin.offset);
        mdp->origin.key_offset -= mdp->origin.offset;
        mdp->origin.flags = (Uint16) 0;
        mdp->origin.type = type;

        erts_atomic32_init_nob(&mdp->refc, 1);
        break;
    }
    default:
        ERTS_INTERNAL_ERROR("Invalid monitor type");
        mdp = NULL;
        break;
    }

    return mdp;
}

/*
 * erts_monitor_destroy__() should only be called from
 * erts_monitor_release() or erts_monitor_release_both().
 */
void
erts_monitor_destroy__(ErtsMonitorData *mdp)
{
    ERTS_ML_ASSERT(erts_atomic32_read_nob(&mdp->refc) == 0);
    ERTS_ML_ASSERT(!(mdp->origin.flags & ERTS_ML_FLG_IN_TABLE));
    ERTS_ML_ASSERT(mdp->origin.type == ERTS_MON_TYPE_ALIAS
                   || !(mdp->u.target.flags & ERTS_ML_FLG_IN_TABLE));
    ERTS_ML_ASSERT(mdp->origin.type == ERTS_MON_TYPE_ALIAS
                   || ((mdp->origin.flags & ERTS_ML_FLGS_SAME)
                       == (mdp->u.target.flags & ERTS_ML_FLGS_SAME)));

    switch (mdp->origin.type) {
    case ERTS_MON_TYPE_ALIAS:
        ERTS_ML_ASSERT(!(mdp->origin.flags & ERTS_ML_FLG_TAG));
        erts_free(ERTS_ALC_T_ALIAS, mdp);
        break;
    case ERTS_MON_TYPE_SUSPEND:
        ERTS_ML_ASSERT(!(mdp->origin.flags & ERTS_ML_FLG_TAG));
        erts_free(ERTS_ALC_T_MONITOR_SUSPEND, mdp);
        break;
    default: {
        ErtsMonitorDataExtended *mdep;
        ErlOffHeap oh;
        ErtsAlcType_t atype;

        if (!(mdp->origin.flags & ERTS_ML_FLG_TAG)) {
            if (mdp->origin.flags & ERTS_ML_FLG_EXTENDED)
                atype = ERTS_ALC_T_MONITOR_EXT;
            else
                atype = ERTS_ALC_T_MONITOR;
        }
        else {
            Eterm tag_storage;
            if (mdp->origin.flags & ERTS_ML_FLG_EXTENDED) {
                atype = ERTS_ALC_T_MONITOR_EXT;
                tag_storage = ((ErtsMonitorDataExtended *) mdp)->heap[0];
            }
            else {
                atype = ERTS_ALC_T_MONITOR_TAG;
                tag_storage = ((ErtsMonitorDataTagHeap *) mdp)->heap[0];
            }
            if (is_CP(tag_storage))
                free_message_buffer((ErlHeapFragment *) cp_val(tag_storage));
        }
        if (atype != ERTS_ALC_T_MONITOR_EXT) {
            erts_free(atype, mdp);
            break;
        }
        mdep = (ErtsMonitorDataExtended *) mdp;
        if (mdp->origin.type == ERTS_MON_TYPE_NODE)
            ERTS_ML_ASSERT(!mdep->uptr.node_monitors);
        else if (mdep->uptr.ohhp) {
            ERTS_INIT_OFF_HEAP(&oh);
            oh.first = mdep->uptr.ohhp;
            erts_cleanup_offheap(&oh);
        }
        if (mdep->dist)
            erts_mon_link_dist_dec_refc(mdep->dist);
        if (mdp->origin.flags & ERTS_ML_FLG_SPAWN_PENDING) {
            /*
             * We have the spawn_request() tag stored in
             * mdep->u.name via a little trick
             * (see pending_flag in erts_monitor_create()).
             * If non-immediate value make sure to release
             * this heap fragment as well.
             */
            if (is_not_immed(mdep->u.name)) {
                ErlHeapFragment *bp;
                bp = (ErlHeapFragment *) cp_val(mdep->u.name);
                free_message_buffer(bp);
            }
        }
        erts_free(ERTS_ALC_T_MONITOR_EXT, mdp);
        break;
    }
    }
}

void
erts_monitor_set_dead_dist(ErtsMonitor *mon, Eterm nodename)
{
    ErtsMonitorDataExtended *mdep;
    mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(mon);

    ERTS_ML_ASSERT(mon->flags & ERTS_ML_FLG_EXTENDED);
    ERTS_ML_ASSERT(mon->type == ERTS_MON_TYPE_DIST_PROC);
    ERTS_ML_ASSERT(!mdep->dist);

    mdep->dist = erts_mon_link_dist_create(nodename);
    mdep->dist->alive = 0;
}

Uint
erts_monitor_size(ErtsMonitor *mon)
{
    Uint size, refc;
    ErtsMonitorData *mdp = erts_monitor_to_data(mon);

    switch (mdp->origin.type) {
    case ERTS_MON_TYPE_ALIAS:
        size = sizeof(ErtsMonitorData);
        break;
    case ERTS_MON_TYPE_SUSPEND:
        size = sizeof(ErtsMonitorSuspend);
        break;
    default: {
        ErtsMonitorDataExtended *mdep;
        Uint hsz;
        if (!(mon->flags & ERTS_ML_FLG_EXTENDED)) {
            size = sizeof(ErtsMonitorDataHeap);
            break;
        }

        mdep = (ErtsMonitorDataExtended *) mdp;
        hsz = 0;

        if (mon->type != ERTS_MON_TYPE_NODE
            && mon->type != ERTS_MON_TYPE_NODES) {
            if (!is_immed(mdep->md.ref))
                hsz += NC_HEAP_SIZE(mdep->md.ref);
            if (mon->type == ERTS_MON_TYPE_DIST_PROC
                || mon->type == ERTS_MON_TYPE_DIST_PORT) {
                if (!is_immed(mdep->md.origin.other.item))
                    hsz += NC_HEAP_SIZE(mdep->md.origin.other.item);
                if (!is_immed(mdep->md.u.target.other.item))
                    hsz += NC_HEAP_SIZE(mdep->md.u.target.other.item);
            }
        }
        size = sizeof(ErtsMonitorDataExtended) + (hsz - 1)*sizeof(Eterm);
        break;
    }
    }
    
    refc = (Uint) erts_atomic32_read_nob(&mdp->refc);
    ASSERT(refc > 0);

    return size / refc;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Link Operations                                                           *
 *                                                                           *
\*                                                                           */

#ifdef ERTS_ML_DEBUG
size_t erts_link_proc_offset;
size_t erts_link_dist_offset;
size_t erts_link_key_offset;
#endif

static ERTS_INLINE void
link_init(void)
{
#ifdef ERTS_ML_DEBUG
    erts_link_proc_offset = offsetof(ErtsLinkData, proc);
    erts_link_dist_offset = offsetof(ErtsLinkData, dist);
    erts_link_key_offset = offsetof(ErtsLink, other.item);
#endif
}

ErtsLink *
erts_link_tree_lookup(ErtsLink *root, Eterm key)
{
    ASSERT(is_pid(key) || is_internal_port(key));
    return (ErtsLink *) ml_rbt_lookup((ErtsMonLnkNode *) root, key);
}

ErtsLink *
erts_link_tree_lookup_insert(ErtsLink **root, ErtsLink *lnk)
{
    return (ErtsLink *) ml_rbt_lookup_insert((ErtsMonLnkNode **) root,
                                             (ErtsMonLnkNode *) lnk);
}

static ErtsMonLnkNode *
create_link_internal(Eterm id, void *vtypep)
{
    ErtsMonLnkNode *lnk = erts_link_internal_create(*((Uint16 *) vtypep), id);
    ERTS_ML_ASSERT(ml_cmp_keys(lnk->other.item, id) == 0);
    return (ErtsMonLnkNode *) lnk;
}

ErtsLink *erts_link_internal_tree_lookup_create(ErtsLink **root, int *created,
                                                Uint16 type, Eterm other)
{
    return (ErtsLink *) ml_rbt_lookup_create((ErtsMonLnkNode **) root,
                                             other, create_link_internal,
                                             (void *) &type,
                                             created);
}

typedef struct {
    Uint16 type;
    Eterm this_;
} ErtsLinkCreateCtxt;

static ErtsMonLnkNode *
create_link_external(Eterm other, void *vcctxt)
{
    ErtsLinkCreateCtxt *cctxt = vcctxt;
    ErtsLinkData *ldp = erts_link_external_create(cctxt->type, cctxt->this_, other);
    return (ErtsMonLnkNode *) &ldp->proc;
}

ErtsLink *erts_link_external_tree_lookup_create(ErtsLink **root, int *created,
                                                Uint16 type, Eterm this_,
                                                Eterm other)
{
    ErtsLinkCreateCtxt cctxt;
    cctxt.type = type;
    cctxt.this_ = this_;
    return (ErtsLink *) ml_rbt_lookup_create((ErtsMonLnkNode **) root,
                                             other, create_link_external,
                                             (void *) &cctxt,
                                             created);
}

void
erts_link_tree_insert(ErtsLink **root, ErtsLink *lnk)
{
    ml_rbt_insert((ErtsMonLnkNode **) root, (ErtsMonLnkNode *) lnk);
}

void
erts_link_tree_replace(ErtsLink **root, ErtsLink *old, ErtsLink *new)
{
    ml_rbt_replace((ErtsMonLnkNode **) root,
                   (ErtsMonLnkNode *) old,
                   (ErtsMonLnkNode *) new);
}

void
erts_link_tree_delete(ErtsLink **root, ErtsLink *lnk)
{
    ml_rbt_delete((ErtsMonLnkNode **) root, (ErtsMonLnkNode *) lnk);
}

void
erts_link_tree_foreach(ErtsLink *root,
                       ErtsLinkFunc func,
                       void *arg)
{
    ml_rbt_foreach((ErtsMonLnkNode *) root,
                   (ErtsMonLnkNodeFunc) func,
                   arg);

}

void
erts_debug_link_tree_destroying_foreach(ErtsLink *root,
                                        ErtsLinkFunc func,
                                        void *arg,
                                        void *vysp)
{
    void *tmp_vysp;
    Sint reds;
    if (!vysp)
        tmp_vysp = NULL;
    else {
        tmp_vysp = erts_alloc(ERTS_ALC_T_ML_YIELD_STATE,
                              sizeof(ErtsMonLnkYieldState));
        sys_memcpy(tmp_vysp, vysp, sizeof(ErtsMonLnkYieldState));
    }
    do {
        reds = ml_rbt_foreach_yielding((ErtsMonLnkNode *) root,
                                       (ErtsMonLnkNodeFunc) func,
                                       arg, &tmp_vysp, (Sint) INT_MAX);
    } while (reds <= 0);
    ERTS_ML_ASSERT(!tmp_vysp);
}

int
erts_link_tree_foreach_yielding(ErtsLink *root,
                                ErtsLinkFunc func,
                                void *arg,
                                void **vyspp,
                                Sint limit)
{
    return ml_rbt_foreach_yielding((ErtsMonLnkNode *) root,
                                   (ErtsMonLnkNodeFunc) func,
                                   arg, vyspp, limit);
}

void
erts_link_tree_foreach_delete(ErtsLink **root,
                              ErtsLinkFunc func,
                              void *arg)
{
    ml_rbt_foreach_delete((ErtsMonLnkNode **) root,
                          (ErtsMonLnkNodeFunc) func,
                          arg);
}

int
erts_link_tree_foreach_delete_yielding(ErtsLink **root,
                                       ErtsLinkFunc func,
                                       void *arg,
                                       void **vyspp,
                                       Sint limit)
{
    return ml_rbt_foreach_delete_yielding((ErtsMonLnkNode **) root,
                                          (ErtsMonLnkNodeFunc) func,
                                          arg, vyspp, limit);
}

void
erts_link_list_foreach(ErtsLink *list,
                       ErtsLinkFunc func,
                       void *arg)
{
    void *ystate = NULL;
    while (!ml_dl_list_foreach_yielding((ErtsMonLnkNode *) list,
                                        (ErtsMonLnkNodeFunc) func,
                                        arg, &ystate, (Sint) INT_MAX));
}

int
erts_link_list_foreach_yielding(ErtsLink *list,
                                ErtsLinkFunc func,
                                void *arg,
                                void **vyspp,
                                Sint limit)
{
    return ml_dl_list_foreach_yielding((ErtsMonLnkNode *) list,
                                       (ErtsMonLnkNodeFunc) func,
                                       arg, vyspp, limit);
}

void
erts_link_list_foreach_delete(ErtsLink **list,
                               ErtsLinkFunc func,
                               void *arg)
{
    void *ystate = NULL;
    while (!ml_dl_list_foreach_delete_yielding((ErtsMonLnkNode **) list,
                                               (ErtsMonLnkNodeFunc) func,
                                               arg, &ystate, (Sint) INT_MAX));
}

int
erts_link_list_foreach_delete_yielding(ErtsLink **list,
                                       int (*func)(ErtsLink *, void *, Sint),
                                       void *arg,
                                       void **vyspp,
                                       Sint limit)
{
    return ml_dl_list_foreach_delete_yielding((ErtsMonLnkNode **) list,
                                              (ErtsMonLnkNodeFunc) func,
                                              arg, vyspp, limit);
}

ErtsLink *
erts_link_internal_create(Uint16 type, Eterm id)
{
    ErtsILink *ilnk;
#ifdef ERTS_ML_DEBUG
    switch (type) {
    case ERTS_LNK_TYPE_PROC:
        ERTS_ML_ASSERT(is_internal_pid(id));
        break;
    case ERTS_LNK_TYPE_PORT:
        ERTS_ML_ASSERT(is_internal_pid(id) || is_internal_port(id));
        break;
    default:
        ERTS_INTERNAL_ERROR("Invalid link type");
        break;
    }
#endif

    ilnk = erts_alloc(ERTS_ALC_T_LINK, sizeof(ErtsILink));

    ilnk->link.other.item = id;
    ilnk->link.key_offset = (Uint16) offsetof(ErtsLink, other.item);
    ilnk->link.offset = (Uint16) 0;
    ilnk->link.flags = (Uint16) 0;
    ilnk->link.type = type;
    ilnk->unlinking = 0;

    return &ilnk->link;
}

ErtsLinkData *
erts_link_external_create(Uint16 type, Eterm this_, Eterm other)
{
    ErtsLinkData *ldp;
    ErtsELink *elnk;
    Uint size, hsz;
    Eterm *hp;
    ErlOffHeap oh;

    ERTS_ML_ASSERT(type == ERTS_LNK_TYPE_DIST_PROC);
    ERTS_ML_ASSERT(is_internal_pid(this_));
    ERTS_ML_ASSERT(is_external_pid(other));

    hsz = EXTERNAL_PID_HEAP_SIZE;

    size = sizeof(ErtsELink) - sizeof(Eterm);
    size += hsz*sizeof(Eterm);

    ldp = erts_alloc(ERTS_ALC_T_LINK_EXT, size);

    elnk = (ErtsELink *) ldp;
    hp = &elnk->heap[0];

    ERTS_INIT_OFF_HEAP(&oh);

    ldp->proc.other.item = STORE_NC(&hp, &oh, other);
    ldp->proc.flags = ERTS_ML_FLG_EXTENDED;
    ldp->proc.key_offset = (Uint16) offsetof(ErtsLink, other.item);
    ldp->proc.offset = (Uint16) offsetof(ErtsLinkData, proc);
    ldp->proc.type = type;

    ldp->dist.other.item = this_;
    ldp->dist.flags = ERTS_ML_FLG_EXTENDED;
    ldp->dist.key_offset = (Uint16) offsetof(ErtsLink, other.item);
    ldp->dist.offset = (Uint16) offsetof(ErtsLinkData, dist);
    ldp->dist.type = type;

    elnk->ohhp = oh.first;
    elnk->dist = NULL;
    elnk->unlinking = 0;

    erts_atomic32_init_nob(&ldp->refc, 2);

    return ldp;
}

/*
 * erts_link_destroy_elink__() should only be called from
 * erts_link_release() or erts_link_release_both().
 */
void
erts_link_destroy_elink__(ErtsELink *elnk)
{
    ErlOffHeap oh;
    ERTS_ML_ASSERT(erts_atomic32_read_nob(&elnk->ld.refc) == 0);
    ERTS_ML_ASSERT(!(elnk->ld.proc.flags & ERTS_ML_FLG_IN_TABLE));
    ERTS_ML_ASSERT(!(elnk->ld.dist.flags & ERTS_ML_FLG_IN_TABLE));
    ERTS_ML_ASSERT((elnk->ld.proc.flags & ERTS_ML_FLGS_SAME)
                   == (elnk->ld.dist.flags & ERTS_ML_FLGS_SAME));
    ERTS_ML_ASSERT(elnk->ld.proc.flags & ERTS_ML_FLG_EXTENDED);
    ERTS_ML_ASSERT(elnk->ld.dist.flags & ERTS_ML_FLG_EXTENDED);

    if (elnk->ohhp) {
        ERTS_INIT_OFF_HEAP(&oh);
        oh.first = elnk->ohhp;
        erts_cleanup_offheap(&oh);
    }
    if (elnk->dist)
        erts_mon_link_dist_dec_refc(elnk->dist);
    erts_free(ERTS_ALC_T_LINK_EXT, elnk);
}

void
erts_link_set_dead_dist(ErtsLink *lnk, Eterm nodename)
{
    ErtsELink *elnk;

    ERTS_ML_ASSERT(lnk->flags & ERTS_ML_FLG_EXTENDED);
    ERTS_ML_ASSERT(lnk->type == ERTS_LNK_TYPE_DIST_PROC);

    elnk = erts_link_to_elink(lnk);

    ERTS_ML_ASSERT(!elnk->dist);

    elnk->dist = erts_mon_link_dist_create(nodename);
    elnk->dist->alive = 0;
}

Uint
erts_link_size(ErtsLink *lnk)
{
    Uint size, refc;

    if (!(lnk->flags & ERTS_ML_FLG_EXTENDED)) {
        size = sizeof(ErtsLink);
        refc = 1;
    }
    else {
        ErtsELink *elnk = erts_link_to_elink(lnk);

        ASSERT(lnk->type == ERTS_LNK_TYPE_DIST_PROC);
        ASSERT(is_external_pid_header(elnk->heap[0]));

        size = sizeof(ErtsELink);
        size += thing_arityval(elnk->heap[0])*sizeof(Eterm);
        refc = (Uint) erts_atomic32_read_nob(&elnk->ld.refc);
        ASSERT(refc > 0);
    }
    

    return size / refc;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Misc                                                                      *
\*                                                                           */

void
erts_monitor_link_init(void)
{
    monitor_init();
    link_init();
}
