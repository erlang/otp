/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2018-2024. All Rights Reserved.
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
 *              === Monitors ==================================================
 *
 *              The monitor data structure contains:
 *              - an 'origin' part that should be inserted in a data structure
 *                of the origin entity, i.e., the entity monitoring another
 *                entity
 *              - a 'target' part that should be inserted in a data structure
 *                of the target entity, i.e., the entity being monitored by
 *                another entity
 *              - a shared part that contains information shared between both
 *                origin and target entities
 *
 *              That is, the two halves of the monitor as well as shared data
 *              are allocated in one single continuous memory block. The
 *              origin and target parts can separately each be inserted in
 *              either a (red-black) tree, a (circular double linked) list, or
 *              in a process signal queue.
 *
 *              Each process and port contains:
 *              - a monitor list for local target monitors that is accessed
 *                via the ERTS_P_LT_MONITORS() macro, and
 *              - a monitor tree for other monitors that is accessed via the
 *                ERTS_P_MONITORS() macro
 *
 *              These fields of processes/ports are protected by the main lock
 *              of the process/port. These are only intended to be accessed by
 *              the process/port itself. When setting up or tearing down a
 *              monitor one should *only* operate on the monitor tree/list of
 *              the currently executing process/port and send signals to the
 *              other involved process/port so it can modify its own monitor
 *              tree/list by itself (see erl_proc_sig_queue.h). One should
 *              absolutely *not* acquire the lock of the other involved
 *              process/port and operate on its monitor tree/list directly.
 *
 *              Each dist entry contains a monitor/link dist structure that
 *              contains:
 *              - a monitor tree for origin named monitors that is accessed via
 *                the field 'orig_name_monitors', and
 *              - a monitor list for other monitors that is accessed via the
 *                'monitors' field.
 *              Monitors in these fields contain information about all monitors
 *              over this specific connection.
 *
 *              The fields of the dist structure are protected by a mutex in
 *              the same dist structure. Operations on these fields are
 *              normally performed by the locally involved process only,
 *              except when a connection is taken down. However in the case
 *              of distributed named monitors that originates from another
 *              node this is not possible. That is this operation is also
 *              performed from another context that the locally involved
 *              process.
 *
 *              Access to monitor trees are performed using the
 *              erts_monitor_tree_* functions below. Access to monitor lists
 *              are performed using the erts_monitor_list_* functions below.
 *
 *
 *              The different monitor types:
 *
 *              --- ERTS_MON_TYPE_PROC ----------------------------------------
 *
 *              A local process (origin) monitors another local process
 *              (target).
 *
 *              Origin:
 *                      Other Item:     Target process identifier
 *              Target:
 *                      Other Item:     Origin process identifier
 *              Shared:
 *                      Key:            Reference
 *                      Name:           Name (atom) if by name
 *
 *              Valid keys are only ordinary internal references or internal
 *              pid-reference.
 *
 *              Origin part of the monitor is stored in the monitor tree of
 *              origin process and target part of the monitor is stored in
 *              monitor list for local targets on the target process.
 *
 *              --- ERTS_MON_TYPE_PORT ----------------------------------------
 *
 *              A local process (origin) monitors a local port (target), or a
 *              local port (origin) monitors a local process (target).
 *
 *              Origin:
 *                      Other Item:     Target process/port identifier
 *              Target:
 *                      Other Item:     Origin process/port identifier
 *              Shared:
 *                      Key:            Reference
 *                      Name:           Name (atom) if by name
 *
 *              Valid keys are only ordinary internal references or internal
 *              pid-reference.
 *
 *              Origin part of the monitor is stored in the monitor tree of
 *              origin process/port and target part of the monitor is stored
 *              in monitor list for local targets on the target process/port.
 *
 *
 *              --- ERTS_MON_TYPE_TIME_OFFSET ---------------------------------
 *
 *              A local process (origin) monitors time offset (target)
 *
 *              Origin:
 *                      Other Item:     clock_service
 *              Target:
 *                      Other Item:     Origin process identifier
 *              Shared:
 *                      Key:            Reference
 *
 *              Valid keys are only ordinary internal references or internal
 *              pid-reference.
 *
 *              Origin part of the monitor is stored in the monitor tree of
 *              origin process and target part of the monitor is stored in
 *              monitor list referred by the variable 'time_offset_monitors'
 *              (see erl_time_sup.c).
 *
 *
 *              --- ERTS_MON_TYPE_DIST_PROC -----------------------------------
 *
 *              A local process (origin) monitors a remote process (target).
 *              Origin node on local process and target node on dist entry.
 *
 *              Origin:
 *                      Other Item:     Remote process identifier/Node name
 *                                      if by name
 *              Target:
 *                      Other Item:     Local process identifier
 *              Shared:
 *                      Key:            Reference
 *                      Name:           Name (atom) if by name
 *                      Dist:           Pointer to dist structure
 *
 *              Valid keys are only ordinary internal references or internal
 *              pid-reference.
 *
 *              Origin part of the monitor is stored in the monitor tree of
 *              origin process and target part of the monitor is stored in
 *              monitor list referred by 'monitors' field of the dist
 *              structure.
 *
 *
 *              A remote process (origin) monitors a local process (target).
 *              Origin node on dist entry and target node on local process.
 *
 *              Origin:
 *                      Other Item:     Local process identifier
 *              Target:
 *                      Other Item:     Remote process identifier
 *              Shared:
 *                      Key:            Reference
 *                      Name:           Name (atom) if by name
 *
 *              Valid keys are only external references.
 *
 *              If monitor by name, the origin part of the monitor is stored
 *              in the monitor tree referred by 'orig_name_monitors' field in
 *              dist structure; otherwise in the monitor list referred by
 *              'monitors' field in dist structure. The target part of the
 *              monitor is stored in the monitor tree of the local target
 *              process.
 *
 *
 *              --- ERTS_MON_TYPE_DIST_PORT -----------------------------------
 *
 *              A local process (origin) monitors a port (target) on an old
 *              incarnation of the local node. Note that it is currently only
 *              for this since operations against remote ports is not
 *              supported.
 *
 *              Origin:
 *                      Other Item:     Monitored port identifier
 *              Target:
 *                      Other Item:     Local process identifier
 *              Shared:
 *                      Key:            Reference
 *                      Dist:           NULL
 *
 *              Valid keys are only ordinary internal references or internal
 *              pid-reference.
 *
 *              Origin part of the monitor is stored in the monitor tree of
 *              origin process and target part is currently only used when
 *              passing monitor down signal (monitors against old incarnations
 *              will always immediately trigger monitor down noproc since the
 *              process wont be alive).
 *
 *              --- ERTS_MON_TYPE_RESOURCE ------------------------------------
 *
 *              A NIF resource (origin) monitors a process (target).
 *
 *              Origin:
 *                      Other Item:     Target process identifier
 *              Target:
 *                      Other Ptr:      Pointer to resource
 *              Shared:
 *                      Key:            Reference
 *
 *              Valid keys are only ordinary internal references.
 *
 *              Origin part of the monitor is stored in the monitor tree of
 *              origin resource (see erl_nif.c) and target part of the
 *              monitor is stored in monitor list for local targets on the
 *              target process.
 *
 *              --- ERTS_MON_TYPE_NODE ----------------------------------------
 *
 *              A local process (origin) monitors a distribution connection
 *              (target) via erlang:monitor_node().
 *
 *              Origin:
 *                      Other Item:     Node name (atom)
 *                      Key:            Node name
 *              Target:
 *                      Other Item:     Origin process identifier
 *                      Key:            Origin process identifier
 *              Shared:
 *                      Refc:           Number of invocations
 *
 *              Valid keys are only node-name atoms and internal process
 *              identifiers.
 *
 *              Origin part of the monitor is stored in the monitor tree of
 *              origin process and target part of the monitor is stored in
 *              monitor list referred by 'monitors' field of the dist
 *              structure.
 *
 *              --- ERTS_MON_TYPE_NODES ---------------------------------------
 *
 *              A local process (origin) monitors all connections (target),
 *              via net_kernel:monitor_nodes().
 *
 *              Origin:
 *                      Other Item:     Bit mask (small)
 *                      Key:            Bit mask
 *              Target:
 *                      Other Item:     Origin process identifier
 *                      Key:            Origin process identifier
 *              Shared:
 *                      Refc:           Number of invocations
 *
 *              Valid keys are only small integers and internal process
 *              identifiers.
 *
 *              Origin part of the monitor is stored in the monitor tree of
 *              origin process and target part of the monitor is stored in
 *              monitor list referred by the variable 'nodes_monitors' (see
 *              dist.c).
 *
 *              --- ERTS_MON_TYPE_SUSPEND -------------------------------------
 *
 *              Suspend monitor. A local process (origin) suspends another
 *              local process (target).
 *
 *              Origin:
 *                      Other Item:     Process identifier of suspendee
 *                                      (target)
 *                      Key:            Process identifier of suspendee
 *                                      (target)
 *              Target:
 *                      Other Item:     Process identifier of suspender
 *                                      (origin)
 *                      Key:            Process identifier of suspender
 *                                      (origin)
 *              Shared:
 *                      Next:           Pointer to another suspend monitor
 *                      State:          Number of suspends and a flag
 *                                      indicating if the suspend is
 *                                      active or not.
 *
 *              Origin part of the monitor is stored in the monitor tree of
 *              origin process and target part of the monitor is stored in
 *              monitor list for local targets on the target process.
 *
 *
 *
 *              === Links =====================================================
 *
 *              The link data structure contains:
 *              - an 'a' part that should be inserted in a data structure of
 *                one entity and contains the identifier of the other involved
 *                entity (b)
 *              - a 'b' part that should be inserted in a data structure of
 *                the other involved entity and contains the identifier of the
 *                other involved entity (a)
 *              - shared part that contains information shared between both
 *                involved entities
 *
 *              That is, the two halves of the link as well as shared data
 *              are allocated in one single continuous memory block. The 'a'
 *              and the 'b' parts can separately each be inserted in either
 *              a (red-black) tree, a (circular double linked) list, or in a
 *              process signal queue.
 *
 *              Each process and port contains:
 *              - a link tree for links that is accessed via the
 *                ERTS_P_LINKS() macro
 *
 *              This field of processes/ports is protected by the main lock of
 *              the process/port. It is only intended to be accessed by the
 *              process/port itself. When setting up or tearing down a link
 *              one should *only* operate on the link tree of the currently
 *              executing process/port and send signals to the other involved
 *              process/port so it can modify its own monitor tree by itself
 *              (see erl_proc_sig_queue.h). One should absolutely *not*
 *              acquire the lock of the other involved process/port and
 *              operate on its link tree directly.
 *
 *              Each dist entry contains a monitor/link dist structure that
 *              contains:
 *              - a link list for links via the 'links' field.
 *              Links in this field contain information about all links over
 *              this specific connection.
 *
 *              The fields of the dist structure are protected by a mutex in
 *              the same dist structure. Operation on the 'links' fields are
 *              normally performed by the locally involved process only,
 *              except when a connection is taken down.
 *
 *              Access to link trees are performed using the erts_link_tree_*
 *              functions below. Access to link lists are performed using the
 *              erts_link_list_* functions below.
 *
 *              There can only be one link between the same pair of
 *              processes/ports. Since a link can be simultaneously initiated
 *              from both ends we always save the link data structure with the
 *              lowest address if multiple links should appear between the
 *              same pair of processes/ports.
 *
 *
 *              The different link types:
 *
 *              --- ERTS_LNK_TYPE_PROC -----------------------------------------
 *
 *              A link between a local process A and a local process B.
 *
 *              A:
 *                      Other Item:     B process identifier
 *                      Key:            B process identifier
 *              B:
 *                      Other Item:     A process identifier
 *                      Key:            A process identifier
 *
 *              Valid keys are only internal process identifiers.
 *
 *              'A' part of the link stored in the link tree of process A and
 *              'B' part of the link is stored in link tree of process B.
 *
 *              --- ERTS_LNK_TYPE_PORT -----------------------------------------
 *
 *              A link between a local process/port A and a local process/port
 *              B.
 *
 *              A:
 *                      Other Item:     B process/port identifier
 *                      Key:            B process/port identifier
 *              B:
 *                      Other Item:     A process/port identifier
 *                      Key:            A process/port identifier
 *
 *              Valid keys are internal process identifiers and internal port
 *              identifiers.
 *
 *              'A' part of the link stored in the link tree of process/port
 *              A and 'B' part of the link is stored in link tree of
 *              process/port B.
 *
 *              --- ERTS_LNK_TYPE_DIST_PROC ------------------------------------
 *
 *              A link between a local process and a remote process. Either of
 *              the processes can be used as A or B.
 *
 *              A:
 *                      Other Item:     B process identifier
 *                      Key:            B process identifier
 *              B:
 *                      Other Item:     A process identifier
 *                      Key:            A process identifier
 *              Shared:
 *                      Dist:           Pointer to dist structure
 *
 *              Valid keys are internal and external process identifiers.
 *
 *              The part of the link with a remote pid as "other item" is
 *              stored in the link tree of the local process. The part of
 *              the link with a local pid as "other item" is stored in the
 *              links list of the dist structure.
 *
 *              ===============================================================
 *
 * Author: 	Rickard Green
 *
 */

#ifndef ERL_MONITOR_LINK_H__
#define ERL_MONITOR_LINK_H__

#define ERTS_PROC_SIG_QUEUE_TYPE_ONLY
#include "erl_proc_sig_queue.h"
#undef ERTS_PROC_SIG_QUEUE_TYPE_ONLY

#define ERL_THR_PROGRESS_TSD_TYPE_ONLY
#include "erl_thr_progress.h"
#undef ERL_THR_PROGRESS_TSD_TYPE_ONLY

#include "erl_alloc.h"

#if defined(DEBUG) || 0
#  define ERTS_ML_DEBUG
#else
#  undef ERTS_ML_DEBUG
#endif

#ifdef ERTS_ML_DEBUG
#  define ERTS_ML_ASSERT ERTS_ASSERT
#else
#  define ERTS_ML_ASSERT(E) ((void) 1)
#endif

#define ERTS_ML_STATE_ALIAS_BITS        2
#define ERTS_ML_STATE_ALIAS_SHIFT       11
#define ERTS_ML_STATE_ALIAS_MASK        \
    ((((Uint16) 1 << ERTS_ML_STATE_ALIAS_BITS) - 1) \
     << ERTS_ML_STATE_ALIAS_SHIFT)

#define ERTS_ML_STATE_ALIAS_NONE        (((Uint16) 0) << ERTS_ML_STATE_ALIAS_SHIFT)
#define ERTS_ML_STATE_ALIAS_UNALIAS     (((Uint16) 1) << ERTS_ML_STATE_ALIAS_SHIFT)
#define ERTS_ML_STATE_ALIAS_DEMONITOR   (((Uint16) 2) << ERTS_ML_STATE_ALIAS_SHIFT)
#define ERTS_ML_STATE_ALIAS_ONCE        (((Uint16) 3) << ERTS_ML_STATE_ALIAS_SHIFT)

#define ERTS_MON_TYPE_MAX               ((Uint16) 9)

#define ERTS_MON_TYPE_PROC              ((Uint16) 0)
#define ERTS_MON_TYPE_PORT              ((Uint16) 1)
#define ERTS_MON_TYPE_TIME_OFFSET       ((Uint16) 2)
#define ERTS_MON_TYPE_DIST_PROC         ((Uint16) 3)
#define ERTS_MON_TYPE_DIST_PORT         ((Uint16) 4)
#define ERTS_MON_TYPE_RESOURCE          ((Uint16) 5)
#define ERTS_MON_TYPE_NODE              ((Uint16) 6)
#define ERTS_MON_TYPE_NODES             ((Uint16) 7)
#define ERTS_MON_TYPE_SUSPEND           ((Uint16) 8)
#define ERTS_MON_TYPE_ALIAS             ERTS_MON_TYPE_MAX

#define ERTS_MON_LNK_TYPE_MAX           (ERTS_MON_TYPE_MAX + ((Uint16) 3))
#define ERTS_LNK_TYPE_MAX               ERTS_MON_LNK_TYPE_MAX

#define ERTS_LNK_TYPE_PROC              (ERTS_MON_TYPE_MAX + ((Uint16) 1))
#define ERTS_LNK_TYPE_PORT              (ERTS_MON_TYPE_MAX + ((Uint16) 2))
#define ERTS_LNK_TYPE_DIST_PROC         ERTS_LNK_TYPE_MAX

#define ERTS_ML_FLG_TARGET              (((Uint16) 1) << 0)
#define ERTS_ML_FLG_IN_TABLE            (((Uint16) 1) << 1)
#define ERTS_ML_FLG_IN_SUBTABLE         (((Uint16) 1) << 2)
#define ERTS_ML_FLG_NAME                (((Uint16) 1) << 3)
#define ERTS_ML_FLG_EXTENDED            (((Uint16) 1) << 4)
#define ERTS_ML_FLG_SPAWN_PENDING       (((Uint16) 1) << 5)
#define ERTS_ML_FLG_SPAWN_MONITOR       (((Uint16) 1) << 6)
#define ERTS_ML_FLG_SPAWN_LINK          (((Uint16) 1) << 7)
#define ERTS_ML_FLG_SPAWN_ABANDONED     (((Uint16) 1) << 8)
#define ERTS_ML_FLG_SPAWN_NO_SMSG       (((Uint16) 1) << 9)
#define ERTS_ML_FLG_SPAWN_NO_EMSG       (((Uint16) 1) << 10)
#define ERTS_ML_FLG_ALIAS_BIT1          (((Uint16) 1) << 11)
#define ERTS_ML_FLG_ALIAS_BIT2          (((Uint16) 1) << 12)
#define ERTS_ML_FLG_TAG                 (((Uint16) 1) << 13)

#define ERTS_ML_FLG_DBG_VISITED         (((Uint16) 1) << 15)

#define ERTS_ML_FLGS_SPAWN              (ERTS_ML_FLG_SPAWN_PENDING      \
                                         | ERTS_ML_FLG_SPAWN_MONITOR    \
                                         | ERTS_ML_FLG_SPAWN_LINK       \
                                         | ERTS_ML_FLG_SPAWN_ABANDONED  \
                                         | ERTS_ML_FLG_SPAWN_NO_SMSG    \
                                         | ERTS_ML_FLG_SPAWN_NO_EMSG)

/* Flags that should be the same on both monitor/link halves */
#define ERTS_ML_FLGS_SAME \
    (ERTS_ML_FLG_EXTENDED|ERTS_ML_FLG_NAME)

typedef struct ErtsMonLnkNode__ ErtsMonLnkNode;
typedef int (*ErtsMonLnkNodeFunc)(ErtsMonLnkNode *, void *, Sint);

typedef struct {
    UWord parent; /* Parent ptr and flags... */
    ErtsMonLnkNode *right;
    ErtsMonLnkNode *left;
} ErtsMonLnkTreeNode;

typedef struct {
    ErtsMonLnkNode *next;
    ErtsMonLnkNode *prev;
} ErtsMonLnkListNode;

struct ErtsMonLnkNode__ {
    union {
        ErtsNonMsgSignal signal;
        ErtsMonLnkTreeNode tree;
        ErtsMonLnkListNode list;
    } node;
    union {
        Eterm item;
        void *ptr;
    } other;
    Uint16 offset; /* offset from monitor/link data to this structure (node) */
    Uint16 key_offset; /* offset from this structure (node) to key */
    Uint16 flags;
    Uint16 type;
};

typedef struct ErtsMonLnkDist__ {
    Eterm nodename;
    Uint32 connection_id;
    erts_atomic_t refc;
    erts_mtx_t mtx;
    int alive;
    ErtsMonLnkNode *links; /* Link double linked circular list */
    ErtsMonLnkNode *monitors; /* Monitor double linked circular list */
    ErtsMonLnkNode *orig_name_monitors; /* Origin named monitors
                                           read-black tree */
    ErtsMonLnkNode *dist_pend_spawn_exit;
    ErtsThrPrgrLaterOp cleanup_lop;
} ErtsMonLnkDist;

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Misc                                                                      *
\*                                                                           */

/**
 *
 * @brief Initialize monitor/link implementation
 *
 */
void erts_monitor_link_init(void);

/**
 *
 * @brief Create monitor/link dist structure to attach to dist entry
 *
 * Create dist structure containing monitor and link containers. This
 * structure is to be attached to a connected dist entry.
 *
 * @param[in]     nodename      Node name as an atom
 *
 * @returns                     Pointer to dist structure
 *
 */
ErtsMonLnkDist *erts_mon_link_dist_create(Eterm nodename);

/**
 *
 * @brief Increase reference count of monitor/link dist structure
 *
 * @param[in]     mld           Pointer to dist structure
 *
 */
ERTS_GLB_INLINE void erts_mon_link_dist_inc_refc(ErtsMonLnkDist *mld);

/**
 *
 * @brief Decrease reference count of monitor/link dist structure
 *
 * @param[in]     mld           Pointer to dist structure
 *
 */
ERTS_GLB_INLINE void erts_mon_link_dist_dec_refc(ErtsMonLnkDist *mld);

/* internal functions... */
ERTS_GLB_INLINE void erts_ml_dl_list_insert__(ErtsMonLnkNode **list,
                                              ErtsMonLnkNode *ml);
ERTS_GLB_INLINE void erts_ml_dl_list_delete__(ErtsMonLnkNode **list,
                                              ErtsMonLnkNode *ml);
ERTS_GLB_INLINE ErtsMonLnkNode *erts_ml_dl_list_first__(ErtsMonLnkNode *list);
ERTS_GLB_INLINE ErtsMonLnkNode *erts_ml_dl_list_last__(ErtsMonLnkNode *list);
void erts_schedule_mon_link_dist_destruction__(ErtsMonLnkDist *mld);
ERTS_GLB_INLINE void *erts_ml_node_to_main_struct__(ErtsMonLnkNode *mln);

/* implementations for globally inlined misc functions... */
#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_mon_link_dist_inc_refc(ErtsMonLnkDist *mld)
{
    ERTS_ML_ASSERT(erts_atomic_read_nob(&mld->refc) > 0);
    erts_atomic_inc_nob(&mld->refc);
}

ERTS_GLB_INLINE void
erts_mon_link_dist_dec_refc(ErtsMonLnkDist *mld)
{
    ERTS_ML_ASSERT(erts_atomic_read_nob(&mld->refc) > 0);
    if (erts_atomic_dec_read_nob(&mld->refc) == 0)
        erts_schedule_mon_link_dist_destruction__(mld);
}

ERTS_GLB_INLINE void *
erts_ml_node_to_main_struct__(ErtsMonLnkNode *mln)
{
    return (void *) (((char *) mln) - ((size_t) mln->offset));
}

ERTS_GLB_INLINE void
erts_ml_dl_list_insert__(ErtsMonLnkNode **list, ErtsMonLnkNode *ml)
{
    ErtsMonLnkNode *first = *list;
    ERTS_ML_ASSERT(!(ml->flags & ERTS_ML_FLG_IN_TABLE));
    if (!first) {
        ml->node.list.next = ml->node.list.prev = ml;
        *list = ml;
    }
    else {
        ERTS_ML_ASSERT(first->node.list.prev->node.list.next == first);
        ERTS_ML_ASSERT(first->node.list.next->node.list.prev == first);
        ml->node.list.next = first;
        ml->node.list.prev = first->node.list.prev;
        first->node.list.prev = ml;
        ml->node.list.prev->node.list.next = ml;
    }
    ml->flags |= ERTS_ML_FLG_IN_TABLE;
}

ERTS_GLB_INLINE void
erts_ml_dl_list_delete__(ErtsMonLnkNode **list, ErtsMonLnkNode *ml)
{
    ERTS_ML_ASSERT(ml->flags & ERTS_ML_FLG_IN_TABLE);
    if (ml->node.list.next == ml) {
        ERTS_ML_ASSERT(ml->node.list.prev == ml);
        ERTS_ML_ASSERT(*list == ml);

        *list = NULL;
    }
    else {
        ERTS_ML_ASSERT(ml->node.list.prev->node.list.next == ml);
        ERTS_ML_ASSERT(ml->node.list.prev != ml);
        ERTS_ML_ASSERT(ml->node.list.next->node.list.prev == ml);
        ERTS_ML_ASSERT(ml->node.list.next != ml);

        if (*list == ml)
            *list = ml->node.list.next;
        ml->node.list.prev->node.list.next = ml->node.list.next;
        ml->node.list.next->node.list.prev = ml->node.list.prev;
    }
    ml->flags &= ~ERTS_ML_FLG_IN_TABLE;
}

ERTS_GLB_INLINE ErtsMonLnkNode *
erts_ml_dl_list_first__(ErtsMonLnkNode *list)
{
    return list;
}

ERTS_GLB_INLINE ErtsMonLnkNode *
erts_ml_dl_list_last__(ErtsMonLnkNode *list)
{
    if (!list)
        return NULL;
    return list->node.list.prev;
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Monitor Operations                                                        *
\*                                                                           */


typedef struct ErtsMonLnkNode__ ErtsMonitor;
typedef int (*ErtsMonitorFunc)(ErtsMonitor *, void *, Sint);

typedef struct {
    ErtsMonitor origin;
    union {
        ErtsMonitor target;
        Eterm ref_heap[ERTS_MAX_INTERNAL_REF_SIZE];
    } u;
    Eterm ref;
    erts_atomic32_t refc;
} ErtsMonitorData;

typedef struct {
    ErtsMonitorData md;
    Eterm ref_heap[ERTS_MAX_INTERNAL_REF_SIZE];
} ErtsMonitorDataHeap;

typedef struct {
    ErtsMonitorData md;
    Eterm heap[1 + ERTS_MAX_INTERNAL_REF_SIZE];
} ErtsMonitorDataTagHeap;

typedef struct ErtsMonitorDataExtended__ ErtsMonitorDataExtended;

struct ErtsMonitorDataExtended__ {
    ErtsMonitorData md;
    union {
        Eterm name;
        Uint refc;
    } u;
    union {
        struct erl_off_heap_header *ohhp;
        ErtsMonitor *node_monitors;
    } uptr;
    ErtsMonLnkDist *dist;
    Eterm heap[1]; /* heap start... */
};

typedef struct ErtsMonitorSuspend__ ErtsMonitorSuspend;


struct ErtsMonitorSuspend__ {
    ErtsMonitorData md; /* origin = suspender; target = suspendee */
    ErtsMonitorSuspend *next;
    erts_atomic_t state;
};
#define ERTS_MSUSPEND_STATE_FLG_ACTIVE ((erts_aint_t) (((Uint) 1) << (sizeof(Uint)*8 - 1)))
#define ERTS_MSUSPEND_STATE_COUNTER_MASK (~ERTS_MSUSPEND_STATE_FLG_ACTIVE)

/* 
 * --- Monitor tree operations ---
 */

/**
 *
 * @brief Lookup a monitor in a monitor tree
 *
 *
 * @param[in]     root          Pointer to root of monitor tree
 *
 * @param[in]     key           Key of monitor to lookup
 *
 * @returns                     Pointer to a monitor with the
 *                              key 'key', or NULL if no such
 *                              monitor was found
 *
 */
ErtsMonitor *erts_monitor_tree_lookup(ErtsMonitor *root, Eterm key);

/**
 *
 * @brief Lookup or insert a monitor in a monitor tree
 *
 * When the function is called it is assumed that:
 * - 'mon' monitor is not part of any tree or list
 * If the above is not true, bad things will happen.
 *
 * @param[in,out] root          Pointer to pointer to root of monitor tree
 *
 * @param[in]     mon           Monitor to insert if no monitor
 *                              with the same key already exists
 *
 * @returns                     Pointer to a monitor with the
 *                              key 'key'. If no monitor with the key
 *                              'key' was found and 'mon' was inserted
 *                              'NULL' is returned.
 *
 */
ErtsMonitor *erts_monotor_tree_lookup_insert(ErtsMonitor **root,
                                             ErtsMonitor *mon);

/**
 *
 * @brief Lookup or create a node or a nodes monitor in a monitor tree.
 *
 * Looks up an origin monitor with the key 'target' in the monitor tree.
 * If it is not found, creates a monitor and returns a pointer to the
 * origin monitor.
 *
 * When the function is called it is assumed that:
 * - no target monitors with the key 'target' exists in the tree.
 * If the above is not true, bad things will happen.
 *
 * @param[in,out] root          Pointer to pointer to root of monitor tree
 *
 * @param[out]    created       Pointer to integer. The integer is set to
 *                              a non-zero value if no monitor with key
 *                              'target' was found, and a new monitor
 *                              was created. If a monitor was found, it
 *                              is set to zero.
 *
 * @param[in]     type          ERTS_MON_TYPE_NODE | ERTS_MON_TYPE_NODES
 *
 * @param[in]     origin        The key of the origin
 *
 * @param[in]     target        The key of the target
 *
 */
ErtsMonitor *erts_monitor_tree_lookup_create(ErtsMonitor **root, int *created,
                                             Uint16 type, Eterm origin,
                                             Eterm target);

/**
 *
 * @brief Insert a monitor in a monitor tree
 *
 * When the function is called it is assumed that:
 * - no monitors with the same key that 'mon' exist in the tree
 * - 'mon' is not part of any list of tree
 * If the above are not true, bad things will happen.
 *
 * @param[in,out] root          Pointer to pointer to root of monitor tree
 *
 * @param[in]     mon           Monitor to insert.
 *
 */
void erts_monitor_tree_insert(ErtsMonitor **root, ErtsMonitor *mon);

/**
 *
 * @brief Replace a monitor in a monitor tree
 *
 * When the function is called it is assumed that:
 * - 'old' monitor and 'new' monitor have exactly the same key
 * - 'old' monitor is part of the tree
 * - 'new' monitor is not part of any tree or list
 * If the above are not true, bad things will happen.
 *
 * @param[in,out] root          Pointer to pointer to root of monitor tree
 *
 * @param[in]     old           Monitor to remove from the tree
 *
 * @param[in]     new_           Monitor to insert into the tree
 *
 */
void erts_monitor_tree_replace(ErtsMonitor **root, ErtsMonitor *old,
                               ErtsMonitor *new_);

/**
 *
 * @brief Delete a monitor from a monitor tree
 *
 * When the function is called it is assumed that:
 * - 'mon' monitor is part of the tree
 * If the above is not true, bad things will happen.
 *
 * @param[in,out] root          Pointer to pointer to root of monitor tree
 *
 * @param[in]     mon           Monitor to remove from the tree
 *
 */
void erts_monitor_tree_delete(ErtsMonitor **root, ErtsMonitor *mon);

/**
 *
 * @brief Call a function for each monitor in a monitor tree
 *
 * The function 'func' will be called with a pointer to a monitor
 * as first argument and 'arg' as second argument for each monitor
 * in the tree referred to by 'root'.
 *
 * @param[in]     root          Pointer to root of monitor tree
 *
 * @param[in]     func          Pointer to function to call
 *
 * @param[in]     arg           Argument to pass as second argument in
 *                              calls to 'func'
 *
 */
void erts_monitor_tree_foreach(ErtsMonitor *root,
                               ErtsMonitorFunc func,
                               void *arg);

/**
 *
 * @brief Call a function for each monitor in a monitor tree. Yield
 *        if lots of monitors exist.
 *
 * The function 'func' will be called with a pointer to a monitor
 * as first argument and 'arg' as second argument for each monitor
 * in the tree referred to by 'root'. It should return the number of
 * reductions the operator took to perform.
 *
 * It is assumed that:
 * - *yspp equals NULL on first call
 * - this function is repetedly called with *yspp set
 *   as set when previous call returned until a non-zero
 *   value is returned.
 * - no modifications are made on the tree between first call
 *   and the call that returns a non-zero value
 * If the above are not true, bad things will happen.
 *
 * @param[in]     root          Pointer to root of monitor tree
 *
 * @param[in]     func          Pointer to function to call
 *
 * @param[in]     arg           Argument to pass as second argument in
 *                              calls to 'func'
 *
 * @param[in,out] vyspp         Pointer to a pointer to an internal state
 *                              used by this function. At initial call
 *                              *yspp should be NULL. When done *yspp
 *                              will be NULL.
 *
 * @param[in]     reds          Reductions available to execute before yielding.
 *
 * @returns                     The unconsumed reductions when all monitors
 *                              have been processed, and zero when more work
 *                              is needed.
 *
 */
int erts_monitor_tree_foreach_yielding(ErtsMonitor *root,
                                       ErtsMonitorFunc func,
                                       void *arg,
                                       void **vyspp,
                                       Sint reds);

/**
 *
 * @brief Delete all monitors from a monitor tree and call a function for
 *        each monitor
 *
 * The function 'func' will be called with a pointer to a monitor
 * as first argument and 'arg' as second argument for each monitor
 * in the tree referred to by 'root'. It should return the number of
 * reductions the operator took to perform.
 *
 * @param[in,out] root          Pointer to pointer to root of monitor tree
 *
 * @param[in]     func          Pointer to function to call
 *
 * @param[in]     arg           Argument to pass as second argument in
 *                              calls to 'func'
 *
 */
void erts_monitor_tree_foreach_delete(ErtsMonitor **root,
                                      ErtsMonitorFunc func,
                                      void *arg);

/**
 *
 * @brief Delete all monitors from a monitor tree and call a function for
 *        each monitor
 *
 * The function 'func' will be called with a pointer to a monitor
 * as first argument and 'arg' as second argument for each monitor
 * in the tree referred to by 'root'. It should return the number of
 * reductions the operator took to perform.
 *
 * It is assumed that:
 * - *yspp equals NULL on first call
 * - this function is repetededly called with *yspp set
 *   as set when previous call returned until a non-zero
 *   value is returned.
 * - no modifications are made on the tree between first call
 *   and the call that returns a non-zero value
 * If the above are not true, bad things will happen.
 *
 * @param[in,out] root          Pointer to pointer to root of monitor tree
 *
 * @param[in]     func          Pointer to function to call
 *
 * @param[in]     arg           Argument to pass as second argument in
 *                              calls to 'func'
 *
 * @param[in,out] vyspp         Pointer to a pointer to an internal state
 *                              used by this function. At initial call
 *                              *yspp should be NULL. When done *yspp
 *                              will be NULL.
 *
 * @param[in]     reds          Reductions available to execute before yielding.
 *
 * @returns                     The unconsumed reductions when all monitors
 *                              have been processed, and zero when more work
 *                              is needed.
 *
 */
int erts_monitor_tree_foreach_delete_yielding(ErtsMonitor **root,
                                              ErtsMonitorFunc func,
                                              void *arg,
                                              void **vyspp,
                                              Sint reds);

/*
 * --- Monitor list operations --
 */

/**
 *
 * @brief Insert a monitor in a monitor list
 *
 * When the function is called it is assumed that:
 * - 'mon' monitor is not part of any list or tree
 * If the above is not true, bad things will happen.
 *
 * @param[in,out] list          Pointer to pointer to monitor list
 *
 * @param[in]     mon           Monitor to insert
 *
 */
ERTS_GLB_INLINE void erts_monitor_list_insert(ErtsMonitor **list, ErtsMonitor *mon);

/**
 *
 * @brief Delete a monitor from a monitor list
 *
 * When the function is called it is assumed that:
 * - 'mon' monitor is part of the list
 * If the above is not true, bad things will happen.
 *
 * @param[in,out] list          Pointer to pointer to monitor list
 *
 * @param[in]     mon           Monitor to remove from the list
 *
 */
ERTS_GLB_INLINE void erts_monitor_list_delete(ErtsMonitor **list, ErtsMonitor *mon);

/**
 *
 * @brief Get a pointer to first monitor in a monitor list
 *
 * The monitor will still remain in the list after the return
 *
 * @param[in] list              Pointer to monitor list
 *
 * @returns                     Pointer to first monitor in list if
 *                              list is not empty. If list is empty
 *                              NULL is returned.
 *
 */
ERTS_GLB_INLINE ErtsMonitor *erts_monitor_list_first(ErtsMonitor *list);

/**
 *
 * @brief Get a pointer to last monitor in a monitor list
 *
 * The monitor will still remain in the list after the return
 *
 * @param[in] list              Pointer to monitor list
 *
 * @returns                     Pointer to last monitor in list if
 *                              list is not empty. If list is empty
 *                              NULL is returned.
 *
 */
ERTS_GLB_INLINE ErtsMonitor *erts_monitor_list_last(ErtsMonitor *list);

/**
 *
 * @brief Call a function for each monitor in a monitor list
 *
 * The function 'func' will be called with a pointer to a monitor
 * as first argument and 'arg' as second argument for each monitor
 * in the tree referred to by 'list'.
 *
 * @param[in]     list          Pointer to root of monitor list
 *
 * @param[in]     func          Pointer to function to call
 *
 * @param[in]     arg           Argument to pass as second argument in
 *                              calls to 'func'
 *
 */
void erts_monitor_list_foreach(ErtsMonitor *list,
                               ErtsMonitorFunc func,
                               void *arg);

/**
 *
 * @brief Call a function for each monitor in a monitor list. Yield
 *        if lots of monitors exist.
 *
 * The function 'func' will be called with a pointer to a monitor
 * as first argument and 'arg' as second argument for each monitor
 * in the tree referred to by 'root'. It should return the number of
 * reductions the operator took to perform.
 *
 * It is assumed that:
 * - *yspp equals NULL on first call
 * - this function is repetedly called with *yspp set
 *   as set when previous call returned until a non-zero
 *   value is returned.
 * - no modifications are made on the tree between first call
 *   and the call that returns a non-zero value
 * If the above are not true, bad things will happen.
 *
 * @param[in]     list          Pointer to monitor list
 *
 * @param[in]     func          Pointer to function to call
 *
 * @param[in]     arg           Argument to pass as second argument in
 *                              calls to 'func'
 *
 * @param[in,out] vyspp         Pointer to a pointer to an internal state
 *                              used by this function. At initial call
 *                              *yspp should be NULL. When done *yspp
 *                              will be NULL.
 *
 * @param[in]     reds          Reductions available to execute before yielding.
 *
 * @returns                     The unconsumed reductions when all monitors
 *                              have been processed, and zero when more work
 *                              is needed.
 *
 */
int erts_monitor_list_foreach_yielding(ErtsMonitor *list,
                                       ErtsMonitorFunc func,
                                       void *arg,
                                       void **vyspp,
                                       Sint reds);

/**
 *
 * @brief Delete all monitors from a monitor list and call a function for
 *        each monitor
 *
 * The function 'func' will be called with a pointer to a monitor
 * as first argument and 'arg' as second argument for each monitor
 * in the tree referred to by 'root'.
 *
 * @param[in,out] list          Pointer to pointer to monitor list
 *
 * @param[in]     func          Pointer to function to call
 *
 * @param[in]     arg           Argument to pass as second argument in
 *                              calls to 'func'
 *
 */
void erts_monitor_list_foreach_delete(ErtsMonitor **list,
                                      ErtsMonitorFunc func,
                                      void *arg);

/**
 *
 * @brief Delete all monitors from a monitor list and call a function for
 *        each monitor
 *
 * The function 'func' will be called with a pointer to a monitor
 * as first argument and 'arg' as second argument for each monitor
 * in the tree referred to by 'root'. It should return the number of
 * reductions the operator took to perform.
 *
 * It is assumed that:
 * - *yspp equals NULL on first call
 * - this function is repetededly called with *yspp set
 *   as set when previous call returned until a non-zero
 *   value is returned.
 * - no modifications are made on the tree between first
 *   and the call that returns a non-zero value
 * If the above are not true, bad things will happen.
 *
 * @param[in,out] list          Pointer to pointer to monitor list
 *
 * @param[in]     func          Pointer to function to call
 *
 * @param[in]     arg           Argument to pass as second argument in
 *                              calls to 'func'
 *
 * @param[in,out] vyspp         Pointer to a pointer to an internal state
 *                              used by this function. At initial call
 *                              *yspp should be NULL. When done *yspp
 *                              will be NULL.
 *
 * @param[in]     reds          Reductions available to execute before yielding.
 *
 * @returns                     The unconsumed reductions when all monitors
 *                              have been processed, and zero when more work
 *                              is needed.
 *
 */
int erts_monitor_list_foreach_delete_yielding(ErtsMonitor **list,
                                              ErtsMonitorFunc func,
                                              void *arg,
                                              void **vyspp,
                                              Sint reds);

/*
 * --- Misc monitor operations ---
 */

/**
 *
 * @brief Create a monitor
 *
 * Can create all types of monitors
 *
 * When the function is called it is assumed that:
 * - 'ref' is an ordinary internal reference or internal pid-reference if type
 *   is ERTS_MON_TYPE_PROC, ERTS_MON_TYPE_PORT, ERTS_MON_TYPE_DIST_PORT,
 *   ERTS_MON_TYPE_TIME_OFFSET, or ERTS_MON_TYPE_RESOURCE
 * - 'ref' is NIL if type is ERTS_MON_TYPE_NODE, ERTS_MON_TYPE_NODES, or
 *   ERTS_MON_TYPE_SUSPEND
 * - 'ref' is and ordinary internal reference, internal pid-reference or an
 *   external reference if type is ERTS_MON_TYPE_DIST_PROC
 * - 'name' is an atom or NIL if type is ERTS_MON_TYPE_PROC,
 *   ERTS_MON_TYPE_PORT, or ERTS_MON_TYPE_DIST_PROC
 * - 'name is NIL if type is ERTS_MON_TYPE_TIME_OFFSET, ERTS_MON_TYPE_RESOURCE,
 *   ERTS_MON_TYPE_NODE, ERTS_MON_TYPE_NODES, or ERTS_MON_TYPE_SUSPEND
 * If the above is not true, bad things will happen.
 *
 * @param[in]     type          ERTS_MON_TYPE_PROC, ERTS_MON_TYPE_PORT,
 *                              ERTS_MON_TYPE_TIME_OFFSET, ERTS_MON_TYPE_DIST_PROC,
 *                              ERTS_MON_TYPE_DIST_PORT, ERTS_MON_TYPE_RESOURCE,
 *                              ERTS_MON_TYPE_NODE, ERTS_MON_TYPE_NODES, or
 *                              ERTS_MON_TYPE_SUSPEND
 *
 * @param[in]     ref           A reference or NIL depending on type
 *
 * @param[in]     origin        The key of the origin
 *
 * @param[in]     target        The key of the target
 *
 * @param[in]     name          An atom (the name) or NIL depending on type
 *
 * @param[in]     tag           Tag to use in message when monitor is
 *                              triggered or THE_NON_VALUE if default
 *                              should be used.
 *
 * @returns                     A pointer to monitor data structure
 *
 */
ErtsMonitorData *erts_monitor_create(Uint16 type, Eterm ref, Eterm origin,
                                     Eterm target, Eterm name, Eterm tag);

/**
 *
 * @brief Get pointer to monitor data structure
 *
 * @param[in]    mon            Pointer to monitor
 *
 * @returns                     Pointer to monitor data structure
 *
 */
ERTS_GLB_INLINE ErtsMonitorData *erts_monitor_to_data(ErtsMonitor *mon);

/**
 *
 * @brief Check if monitor is a target monitor
 *
 * @param[in]    mon            Pointer to monitor to check
 *
 * @returns                     A non-zero value if target monitor;
 *                              otherwise zero
 *
 */
ERTS_GLB_INLINE int erts_monitor_is_target(ErtsMonitor *mon);

/**
 *
 * @brief Check if monitor is an origin monitor
 *
 * @param[in]    mon            Pointer to monitor to check
 *
 * @returns                     A non-zero value if origin monitor;
 *                              otherwise zero
 *
 */
ERTS_GLB_INLINE int erts_monitor_is_origin(ErtsMonitor *mon);

/**
 *
 * @brief Check if monitor is in tree or list
 *
 * @param[in]    mon            Pointer to monitor to check
 *
 * @returns                     A non-zero value if in tree or list;
 *                              otherwise zero
 *
 */
ERTS_GLB_INLINE int erts_monitor_is_in_table(ErtsMonitor *mon);

/**
 *
 * @brief Release monitor
 *
 * When both the origin and the target part of the monitor have
 * been released the monitor structure will be deallocated.
 *
 * When the function is called it is assumed that:
 * - 'mon' monitor is not part of any list or tree
 * - 'mon' is not referred to by any other structures
 * If the above are not true, bad things will happen.
 *
 * @param[in]    mon            Pointer to monitor
 *
 */
ERTS_GLB_INLINE void erts_monitor_release(ErtsMonitor *mon);

/**
 *
 * @brief Release both target and origin monitor structures simultaneously
 *
 * Release both the origin and target parts of the monitor
 * simultaneously and deallocate the structure.
 *
 * When the function is called it is assumed that:
 * - Neither the origin part nor the target part of the monitor
 *   are not part of any list or tree
 * - Neither the origin part nor the target part of the monitor
 *   are referred to by any other structures
 * If the above are not true, bad things will happen.
 *
 * @param[in]    mdp            Pointer to monitor data structure
 *
 */
ERTS_GLB_INLINE void erts_monitor_release_both(ErtsMonitorData *mdp);

/**
 *
 * @brief Insert monitor in dist monitor tree or list
 *
 * When the function is called it is assumed that:
 * - 'mon' monitor is not part of any list or tree
 * If the above is not true, bad things will happen.
 *
 * @param[in]    mon            Pointer to monitor
 *
 * @param[in]    dist           Pointer to dist structure
 *
 * @returns                     A non-zero value if inserted;
 *                              otherwise, zero. The monitor
 *                              is not inserted if the dist
 *                              structure has been set in a
 *                              dead state.
 *
 */
ERTS_GLB_INLINE int erts_monitor_dist_insert(ErtsMonitor *mon, ErtsMonLnkDist *dist);

/**
 *
 * @brief Delete monitor from dist monitor tree or list
 *
 * When the function is called it is assumed that:
 * - 'mon' monitor earlier has been inserted into 'dist'
 * If the above is not true, bad things will happen.
 *
 * @param[in]    mon            Pointer to monitor
 *
 * @param[in]    dist           Pointer to dist structure
 *
 * @returns                     A non-zero value if deleted;
 *                              otherwise, zero. The monitor
 *                              is not deleted if the dist
 *                              structure has been set in a
 *                              dead state or if it has already
 *                              been deleted.
 *
 */
ERTS_GLB_INLINE int erts_monitor_dist_delete(ErtsMonitor *mon);

/**
 *
 * @brief Set dead dist structure on monitor
 *
 * @param[in]    mon            Pointer to monitor
 *
 * @param[in]    nodename       Name of remote node
 *
 */
void
erts_monitor_set_dead_dist(ErtsMonitor *mon, Eterm nodename);

/**
 *
 * @brief Get charged size of monitor
 *
 * If the other side of the monitor has been released, the
 * whole size of the monitor data structure is returned; otherwise,
 * half of the size is returned.
 *
 * When the function is called it is assumed that:
 * - 'mon' has not been released
 * If the above is not true, bad things will happen.
 *
 * @param[in]    mon            Pointer to monitor
 *
 * @returns                     Charged size in bytes
 *
 */
Uint erts_monitor_size(ErtsMonitor *mon);


/* internal function... */
void erts_monitor_destroy__(ErtsMonitorData *mdp);

/* implementations for globally inlined monitor functions... */
#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE int
erts_monitor_is_target(ErtsMonitor *mon)
{
    return !!(mon->flags & ERTS_ML_FLG_TARGET);
}

ERTS_GLB_INLINE int
erts_monitor_is_origin(ErtsMonitor *mon)
{
    return !(mon->flags & ERTS_ML_FLG_TARGET);
}

ERTS_GLB_INLINE int
erts_monitor_is_in_table(ErtsMonitor *mon)
{
    return !!(mon->flags & ERTS_ML_FLG_IN_TABLE);
}

ERTS_GLB_INLINE void
erts_monitor_list_insert(ErtsMonitor **list, ErtsMonitor *mon)
{
    erts_ml_dl_list_insert__((ErtsMonLnkNode **) list, (ErtsMonLnkNode *) mon);
}

ERTS_GLB_INLINE void
erts_monitor_list_delete(ErtsMonitor **list, ErtsMonitor *mon)
{
    erts_ml_dl_list_delete__((ErtsMonLnkNode **) list, (ErtsMonLnkNode *) mon);
}

ERTS_GLB_INLINE ErtsMonitor *
erts_monitor_list_first(ErtsMonitor *list)
{
    return (ErtsMonitor *) erts_ml_dl_list_first__((ErtsMonLnkNode *) list);
}

ERTS_GLB_INLINE ErtsMonitor *
erts_monitor_list_last(ErtsMonitor *list)
{
    return (ErtsMonitor *) erts_ml_dl_list_last__((ErtsMonLnkNode *) list);
}

#ifdef ERTS_ML_DEBUG
extern size_t erts_monitor_origin_offset;
extern size_t erts_monitor_origin_key_offset;
extern size_t erts_monitor_target_offset;
extern size_t erts_monitor_target_key_offset;
extern size_t erts_monitor_node_key_offset;
#endif

ERTS_GLB_INLINE ErtsMonitorData *
erts_monitor_to_data(ErtsMonitor *mon)
{
    ErtsMonitorData *mdp = (ErtsMonitorData *)erts_ml_node_to_main_struct__((ErtsMonLnkNode *) mon);

#ifdef ERTS_ML_DEBUG
    ERTS_ML_ASSERT(!(mdp->origin.flags & ERTS_ML_FLG_TARGET));
    ERTS_ML_ASSERT(erts_monitor_origin_offset == (size_t) mdp->origin.offset);
    ERTS_ML_ASSERT(mon->type == ERTS_MON_TYPE_ALIAS
                   || !!(mdp->u.target.flags & ERTS_ML_FLG_TARGET));
    ERTS_ML_ASSERT(mon->type == ERTS_MON_TYPE_ALIAS
                   || erts_monitor_target_offset == (size_t) mdp->u.target.offset);
    if (mon->type == ERTS_MON_TYPE_NODE || mon->type == ERTS_MON_TYPE_NODES
        || mon->type == ERTS_MON_TYPE_SUSPEND) {
        ERTS_ML_ASSERT(erts_monitor_node_key_offset == (size_t) mdp->origin.key_offset);
        ERTS_ML_ASSERT(erts_monitor_node_key_offset == (size_t) mdp->u.target.key_offset);
    }
    else {
        ERTS_ML_ASSERT(erts_monitor_origin_key_offset == (size_t) mdp->origin.key_offset);
        ERTS_ML_ASSERT(mon->type == ERTS_MON_TYPE_ALIAS
                       || erts_monitor_target_key_offset == (size_t) mdp->u.target.key_offset);
    }
#endif

    return mdp;
}

ERTS_GLB_INLINE void
erts_monitor_release(ErtsMonitor *mon)
{
    ErtsMonitorData *mdp = erts_monitor_to_data(mon);
    ERTS_ML_ASSERT(erts_atomic32_read_nob(&mdp->refc) > 0);

    if (erts_atomic32_dec_read_mb(&mdp->refc) == 0) {
        ERTS_ML_ASSERT(!(mdp->origin.flags & ERTS_ML_FLG_IN_TABLE));
        ERTS_ML_ASSERT(mon->type == ERTS_MON_TYPE_ALIAS
                       || !(mdp->u.target.flags & ERTS_ML_FLG_IN_TABLE));

        erts_monitor_destroy__(mdp);
    }
}

ERTS_GLB_INLINE void
erts_monitor_release_both(ErtsMonitorData *mdp)
{
    ERTS_ML_ASSERT((mdp->origin.flags & ERTS_ML_FLGS_SAME)
                   == (mdp->u.target.flags & ERTS_ML_FLGS_SAME));
    ERTS_ML_ASSERT(erts_atomic32_read_nob(&mdp->refc) >= 2);

    if (erts_atomic32_add_read_mb(&mdp->refc, (erts_aint32_t) -2) == 0) {
        ERTS_ML_ASSERT(!(mdp->origin.flags & ERTS_ML_FLG_IN_TABLE));
        ERTS_ML_ASSERT(!(mdp->u.target.flags & ERTS_ML_FLG_IN_TABLE));

        erts_monitor_destroy__(mdp);
    }
}

ERTS_GLB_INLINE int
erts_monitor_dist_insert(ErtsMonitor *mon, ErtsMonLnkDist *dist)
{
    ErtsMonitorDataExtended *mdep;
    int insert;

    ERTS_ML_ASSERT(mon->flags & ERTS_ML_FLG_EXTENDED);
    ERTS_ML_ASSERT(mon->type == ERTS_MON_TYPE_DIST_PROC
                   || mon->type == ERTS_MON_TYPE_NODE);

    mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(mon);

    ERTS_ML_ASSERT(!mdep->dist);
    ERTS_ML_ASSERT(dist);

    erts_mtx_lock(&dist->mtx);

    insert = dist->alive;
    if (insert) {
        mdep->dist = dist;
        erts_mon_link_dist_inc_refc(dist);

        if ((mon->flags & (ERTS_ML_FLG_NAME
                           | ERTS_ML_FLG_TARGET)) == ERTS_ML_FLG_NAME)
            erts_monitor_tree_insert(&dist->orig_name_monitors, mon);
        else
            erts_monitor_list_insert(&dist->monitors, mon);
    }

    erts_mtx_unlock(&dist->mtx);

    return insert;
}

ERTS_GLB_INLINE int
erts_monitor_dist_delete(ErtsMonitor *mon)
{
    ErtsMonitorDataExtended *mdep;
    ErtsMonLnkDist *dist;
    Uint16 flags;
    int delete_;

    ERTS_ML_ASSERT(mon->flags & ERTS_ML_FLG_EXTENDED);
    ERTS_ML_ASSERT(mon->type == ERTS_MON_TYPE_DIST_PROC
                   || mon->type == ERTS_MON_TYPE_NODE);

    mdep = (ErtsMonitorDataExtended *) erts_monitor_to_data(mon);
    dist = mdep->dist;
    ERTS_ML_ASSERT(dist);

    erts_mtx_lock(&dist->mtx);

    flags = mon->flags;
    delete_ = !!dist->alive & !!(flags & ERTS_ML_FLG_IN_TABLE);
    if (delete_) {
        if ((flags & (ERTS_ML_FLG_NAME
                      | ERTS_ML_FLG_TARGET)) == ERTS_ML_FLG_NAME)
            erts_monitor_tree_delete(&dist->orig_name_monitors, mon);
        else
            erts_monitor_list_delete(&dist->monitors, mon);
    }

    erts_mtx_unlock(&dist->mtx);

    return delete_;
}


#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

/* suspend monitors... */
ErtsMonitorSuspend *erts_monitor_suspend_create(Eterm pid);
ErtsMonitorSuspend *erts_monitor_suspend_tree_lookup_create(ErtsMonitor **root,
                                                            int *created,
                                                            Eterm pid);
void erts_monitor_suspend_destroy(ErtsMonitorSuspend *msp);

ERTS_GLB_INLINE ErtsMonitorSuspend *erts_monitor_suspend(ErtsMonitor *mon);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE ErtsMonitorSuspend *erts_monitor_suspend(ErtsMonitor *mon)
{
    ERTS_ML_ASSERT(!mon || mon->type == ERTS_MON_TYPE_SUSPEND);
    return (ErtsMonitorSuspend *) mon;
}

#endif

void
erts_debug_monitor_tree_destroying_foreach(ErtsMonitor *root,
                                           ErtsMonitorFunc func,
                                           void *arg,
                                           void *vysp);
void
erts_debug_monitor_list_destroying_foreach(ErtsMonitor *list,
                                           ErtsMonitorFunc func,
                                           void *arg,
                                           void *vysp);

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Link Operations                                                           *
\*                                                                           */

typedef struct ErtsMonLnkNode__ ErtsLink;

typedef int (*ErtsLinkFunc)(ErtsLink *, void *, Sint);

/* Internal Link */
typedef struct {
    ErtsLink link;
    Uint64 unlinking;
} ErtsILink;

typedef struct {
    ErtsLink proc;
    ErtsLink dist;
    erts_atomic32_t refc;
} ErtsLinkData;

/* External Link */
typedef struct {
    ErtsLinkData ld;
    struct erl_off_heap_header *ohhp;
    ErtsMonLnkDist *dist;
    Uint64 unlinking;
    Eterm heap[1]; /* heap start... */
} ErtsELink;

/*
 * --- Link tree operations ---
 */

/**
 *
 * @brief Lookup a link in a link tree
 *
 *
 * @param[in]     root          Pointer to root of link tree
 *
 * @param[in]     key           Key of link to lookup
 *
 * @returns                     Pointer to a link with the
 *                              key 'key', or NULL if no such
 *                              link was found
 *
 */
ErtsLink *erts_link_tree_lookup(ErtsLink *root, Eterm item);

/**
 *
 * @brief Lookup or insert a link in a link tree
 *
 * When the function is called it is assumed that:
 * - 'lnk' link is not part of any tree or list
 * If the above is not true, bad things will happen.
 *
 * @param[in,out] root          Pointer to pointer to root of link tree
 *
 * @param[in]     lnk           Link to insert if no link
 *                              with the same key already exists
 *
 * @returns                     Pointer to a link with the
 *                              key 'key'. If no link with the key
 *                              'key' was found and 'lnk' was inserted
 *                              'NULL' is returned.
 *
 */
ErtsLink *erts_link_tree_lookup_insert(ErtsLink **root, ErtsLink *lnk);

/**
 *
 * @brief Lookup or create an external link in a link tree.
 *
 * Looks up a link with the key 'other' in the link tree. If it is not
 * found, creates and insert a link with the key 'other'.
 *
 * @param[in,out] root          Pointer to pointer to root of link tree
 *
 * @param[out]    created       Pointer to integer. The integer is set to
 *                              a non-zero value if no link with key
 *                              'other' was found, and a new link
 *                              was created. If a link was found, it
 *                              is set to zero.
 *
 * @param[in]     type          Type of link
 *
 * @param[in]     this          Id of this entity
 *
 * @param[in]     other         Id of other entity
 *
 * @returns                     Pointer to either an already existing
 *                              link in the tree or a newly created
 *                              and inserted link.
 *
 */
ErtsLink *erts_link_external_tree_lookup_create(ErtsLink **root, int *created,
                                                Uint16 type, Eterm this_, Eterm other);

/**
 *
 * @brief Lookup or create an internal link in a link tree.
 *
 * Looks up a link with the key 'other' in the link tree. If it is not
 * found, creates and insert a link with the key 'other'.
 *
 * @param[in,out] root          Pointer to pointer to root of link tree
 *
 * @param[out]    created       Pointer to integer. The integer is set to
 *                              a non-zero value if no link with key
 *                              'other' was found, and a new link
 *                              was created. If a link was found, it
 *                              is set to zero.
 *
 * @param[in]     type          Type of link
 *
 * @param[in]     other         Id of other entity
 *
 * @returns                     Pointer to either an already existing
 *                              link in the tree or a newly created
 *                              and inserted link.
 *
 */
ErtsLink *erts_link_internal_tree_lookup_create(ErtsLink **root, int *created,
                                                Uint16 type, Eterm other);

/**
 *
 * @brief Insert a link in a link tree
 *
 * When the function is called it is assumed that:
 * - no links with the same key that 'lnk' exist in the tree
 * - 'lnk' is not part of any list of tree
 * If the above are not true, bad things will happen.
 *
 * @param[in,out] root          Pointer to pointer to root of link tree
 *
 * @param[in]     lnk           Link to insert.
 *
 */
void erts_link_tree_insert(ErtsLink **root, ErtsLink *lnk);

/**
 *
 * @brief Replace a link in a link tree
 *
 * When the function is called it is assumed that:
 * - 'old' link and 'new' link have exactly the same key
 * - 'old' link is part of the tree
 * - 'new' link is not part of any tree or list
 * If the above are not true, bad things will happen.
 *
 * @param[in,out] root          Pointer to pointer to root of link tree
 *
 * @param[in]     old           Link to remove from the tree
 *
 * @param[in]     new           Link to insert into the tree
 *
 */
void erts_link_tree_replace(ErtsLink **root, ErtsLink *old, ErtsLink *new_);

/**
 *
 * @brief Replace a link in a link tree if key already exist based on address
 *
 * Inserts the link 'lnk' in the tree if no link with the same key
 * already exists in tree. If a link with the same key exists in
 * the tree and 'lnk' has a lower address than the link in the
 * tree, the existing link in the tree is replaced by 'lnk'.
 *
 * When the function is called it is assumed that:
 * - 'lnk' link is not part of any tree or list
 * If the above are not true, bad things will happen.
 *
 * @param[in,out] root          Pointer to pointer to root of link tree
 *
 * @param[in]     lnk           Link to insert into the tree
 *
 * @returns                     A pointer to the link that is not part
 *                              of the tree after this operation.
 *
 */
ERTS_GLB_INLINE ErtsLink *erts_link_tree_insert_addr_replace(ErtsLink **root,
                                                             ErtsLink *lnk);

/**
 *
 * @brief Delete a link from a link tree
 *
 * When the function is called it is assumed that:
 * - 'lnk' link is part of the tree
 * If the above is not true, bad things will happen.
 *
 * @param[in,out] root          Pointer to pointer to root of link tree
 *
 * @param[in]     lnk           Link to remove from the tree
 *
 */
void erts_link_tree_delete(ErtsLink **root, ErtsLink *lnk);

/**
 *
 * @brief Delete a link from a link tree based on key
 *
 * If link 'lnk' is in the tree, 'lnk' is deleted from the tree.
 * If link 'lnk' is not in the tree, another link with the same
 * key as 'lnk' is deleted from the tree if such a link exist.
 *
 * When the function is called it is assumed that:
 * - if 'lnk' link is part of a tree or list, it is part of this tree
 * If the above is not true, bad things will happen.
 *
 * @param[in,out] root          Pointer to pointer to root of link tree
 *
 * @param[in]     lnk           Link to remove from the tree
 *
 * @returns                     A pointer to the link that was deleted
 *                              from the tree, or NULL in case no link
 *                              was deleted from the tree
 *
 */
ERTS_GLB_INLINE ErtsLink *erts_link_tree_key_delete(ErtsLink **root, ErtsLink *lnk);

/**
 *
 * @brief Call a function for each link in a link tree
 *
 * The function 'func' will be called with a pointer to a link
 * as first argument and 'arg' as second argument for each link
 * in the tree referred to by 'root'.
 *
 * @param[in]     root          Pointer to root of link tree
 *
 * @param[in]     func          Pointer to function to call
 *
 * @param[in]     arg           Argument to pass as second argument in
 *                              calls to 'func'
 *
 */
void erts_link_tree_foreach(ErtsLink *root,
                            ErtsLinkFunc,
                            void *arg);

/**
 *
 * @brief Call a function for each link in a link tree. Yield if lots
 *        of links exist.
 *
 * The function 'func' will be called with a pointer to a link
 * as first argument and 'arg' as second argument for each link
 * in the tree referred to by 'root'. It should return the number of
 * reductions the operator took to perform.
 *
 * It is assumed that:
 * - *yspp equals NULL on first call
 * - this function is repetedly called with *yspp set
 *   as set when previous call returned until a non-zero
 *   value is returned.
 * - no modifications are made on the tree between first call
 *   and the call that returns a non-zero value
 * If the above are not true, bad things will happen.
 *
 * @param[in]     root          Pointer to root of link tree
 *
 * @param[in]     func          Pointer to function to call
 *
 * @param[in]     arg           Argument to pass as second argument in
 *                              calls to 'func'
 *
 * @param[in,out] vyspp         Pointer to a pointer to an internal state
 *                              used by this function. At initial call
 *                              *yspp should be NULL. When done *yspp
 *                              will be NULL.
 *
 * @param[in]     reds          Reductions available to execute before yielding.
 *
 * @returns                     The unconsumed reductions when all links
 *                              have been processed, and zero when more work
 *                              is needed.
 *
 */
int erts_link_tree_foreach_yielding(ErtsLink *root,
                                    ErtsLinkFunc func,
                                    void *arg,
                                    void **vyspp,
                                    Sint reds);

/**
 *
 * @brief Delete all links from a link tree and call a function for
 *        each link
 *
 * The function 'func' will be called with a pointer to a link
 * as first argument and 'arg' as second argument for each link
 * in the tree referred to by 'root'.
 *
 * @param[in,out] root          Pointer to pointer to root of link tree
 *
 * @param[in]     func          Pointer to function to call
 *
 * @param[in]     arg           Argument to pass as second argument in
 *                              calls to 'func'
 *
 */
void erts_link_tree_foreach_delete(ErtsLink **root,
                                   ErtsLinkFunc func,
                                   void *arg);

/**
 *
 * @brief Delete all links from a link tree and call a function for
 *        each link
 *
 * The function 'func' will be called with a pointer to a link
 * as first argument and 'arg' as second argument for each link
 * in the tree referred to by 'root'. It should return the number of
 * reductions the operator took to perform.
 *
 * It is assumed that:
 * - *yspp equals NULL on first call
 * - this function is repetededly called with *yspp set
 *   as set when previous call returned until a non-zero
 *   value is returned.
 * - no modifications are made on the tree between first call
 *   and the call that returns a non-zero value
 * If the above are not true, bad things will happen.
 *
 * @param[in,out] root          Pointer to pointer to root of link tree
 *
 * @param[in]     func          Pointer to function to call
 *
 * @param[in]     arg           Argument to pass as second argument in
 *                              calls to 'func'
 *
 * @param[in,out] vyspp         Pointer to a pointer to an internal state
 *                              used by this function. At initial call
 *                              *yspp should be NULL. When done *yspp
 *                              will be NULL.
 *
 * @param[in]     reds          Reductions available to execute before yielding.
 *
 * @returns                     The unconsumed reductions when all links
 *                              have been processed, and zero when more work
 *                              is needed.
 *
 */
int erts_link_tree_foreach_delete_yielding(ErtsLink **root,
                                           ErtsLinkFunc func,
                                           void *arg,
                                           void **vyspp,
                                           Sint reds);

/* 
 * --- Link list operations ---
 */

/**
 *
 * @brief Insert a link in a link list
 *
 * When the function is called it is assumed that:
 * - 'lnk' link is not part of any list or tree
 * If the above is not true, bad things will happen.
 *
 * @param[in,out] list          Pointer to pointer to link list
 *
 * @param[in]     lnk           Link to insert
 *
 */
ERTS_GLB_INLINE void erts_link_list_insert(ErtsLink **list, ErtsLink *lnk);

/**
 *
 * @brief Delete a link from a link list
 *
 * When the function is called it is assumed that:
 * - 'lnk' link is part of the list
 * If the above is not true, bad things will happen.
 *
 * @param[in,out] list          Pointer to pointer to link list
 *
 * @param[in]     lnk           Link to remove from the list
 *
 */
ERTS_GLB_INLINE void erts_link_list_delete(ErtsLink **list, ErtsLink *lnk);

/**
 *
 * @brief Get a pointer to first link in a link list
 *
 * The link will still remain in the list after the return
 *
 * @param[in] list              Pointer to link list
 *
 * @returns                     Pointer to first link in list if
 *                              list is not empty. If list is empty
 *                              NULL is returned.
 *
 */
ERTS_GLB_INLINE ErtsLink *erts_link_list_first(ErtsLink *list);

/**
 *
 * @brief Get a pointer to last link in a link list
 *
 * The link will still remain in the list after the return
 *
 * @param[in] list              Pointer to link list
 *
 * @returns                     Pointer to last link in list if
 *                              list is not empty. If list is empty
 *                              NULL is returned.
 *
 */
ERTS_GLB_INLINE ErtsLink *erts_link_list_last(ErtsLink *list);

/**
 *
 * @brief Call a function for each link in a link list
 *
 * The function 'func' will be called with a pointer to a link
 * as first argument and 'arg' as second argument for each link
 * in the tree referred to by 'list'.
 *
 * @param[in]     list          Pointer to root of link list
 *
 * @param[in]     func          Pointer to function to call
 *
 * @param[in]     arg           Argument to pass as second argument in
 *                              calls to 'func'
 *
 */
void erts_link_list_foreach(ErtsLink *list,
                            ErtsLinkFunc func,
                            void *arg);

/**
 *
 * @brief Call a function for each link in a link list. Yield
 *        if lots of links exist.
 *
 * The function 'func' will be called with a pointer to a link
 * as first argument and 'arg' as second argument for each link
 * in the tree referred to by 'root'. It should return the number of
 * reductions the operator took to perform.
 *
 * It is assumed that:
 * - *yspp equals NULL on first call
 * - this function is repetedly called with *yspp set
 *   as set when previous call returned until a non-zero
 *   value is returned.
 * - no modifications are made on the tree between first call
 *   and the call that returns a non-zero value
 * If the above are not true, bad things will happen.
 *
 * @param[in]     list          Pointer to link list
 *
 * @param[in]     func          Pointer to function to call
 *
 * @param[in]     arg           Argument to pass as second argument in
 *                              calls to 'func'
 *
 * @param[in,out] vyspp         Pointer to a pointer to an internal state
 *                              used by this function. At initial call
 *                              *yspp should be NULL. When done *yspp
 *                              will be NULL.
 *
 * @param[in]     reds          Reductions available to execute before yielding.
 *
 * @returns                     The unconsumed reductions when all links
 *                              have been processed, and zero when more work
 *                              is needed.
 *
 */
int erts_link_list_foreach_yielding(ErtsLink *list,
                                    ErtsLinkFunc func,
                                    void *arg,
                                    void **vyspp,
                                    Sint reds);

/**
 *
 * @brief Delete all links from a link list and call a function for
 *        each link
 *
 * The function 'func' will be called with a pointer to a link
 * as first argument and 'arg' as second argument for each link
 * in the tree referred to by 'root'.
 *
 * @param[in,out] list          Pointer to pointer to link list
 *
 * @param[in]     func          Pointer to function to call
 *
 * @param[in]     arg           Argument to pass as second argument in
 *                              calls to 'func'
 *
 */
void erts_link_list_foreach_delete(ErtsLink **list,
                                   ErtsLinkFunc func,
                                   void *arg);

/**
 *
 * @brief Delete all links from a link list and call a function for
 *        each link
 *
 * The function 'func' will be called with a pointer to a link
 * as first argument and 'arg' as second argument for each link
 * in the tree referred to by 'root'. It should return the number of
 * reductions the operator took to perform.
 *
 * It is assumed that:
 * - *yspp equals NULL on first call
 * - this function is repetededly called with *yspp set
 *   as set when previous call returned until a non-zero
 *   value is returned.
 * - no modifications are made on the tree between first
 *   and the call that returns a non-zero value
 * If the above are not true, bad things will happen.
 *
 * @param[in,out] list          Pointer to pointer to link list
 *
 * @param[in]     func          Pointer to function to call
 *
 * @param[in]     arg           Argument to pass as second argument in
 *                              calls to 'func'
 *
 * @param[in,out] vyspp         Pointer to a pointer to an internal state
 *                              used by this function. At initial call
 *                              *yspp should be NULL. When done *yspp
 *                              will be NULL.
 *
 * @param[in]     reds          Reductions available to execute before yielding.
 *
 * @returns                     The unconsumed reductions when all links
 *                              have been processed, and zero when more work
 *                              is needed.
 *
 */
int erts_link_list_foreach_delete_yielding(ErtsLink **list,
                                           ErtsLinkFunc func,
                                           void *arg,
                                           void **vyspp,
                                           Sint reds);

/*
 * --- Misc link operations ---
 */

/**
 *
 * @brief Create an external link
 *
 * An external link structure contains two links, one for usage in
 * the link tree of the process and one for usage in the dist entry.
 *
 *
 * @param[in]     type          ERTS_MON_TYPE_DIST_PROC
 *
 * @param[in]     this          The process identifier of the local
 *                              process. The link structure in the
 *                              'dist' field a will have its
 *                              'other.item' field set to 'this'.
 *                              The 'dist' link structure is to be
 *                              inserted on the distribution entry.
 *
 * @param[in]     other         The process identifier of the remote
 *                              process. The link structure in the
 *                              'proc' field a will have its
 *                              'other.item' field set to 'other'.
 *                              The 'proc' link structure is to be
 *                              inserted on the local process.
 *
 * @returns                     A pointer to the link data structure
 *                              containing the link structures. The
 *                              link data structure is in turn part
 *                              of the external link structure
 *                              (ErtsELink).
 *
 */
ErtsLinkData *erts_link_external_create(Uint16 type, Eterm this_, Eterm other);

/**
 *
 * @brief Create an internal link
 *
 * @param[in]     type          ERTS_MON_TYPE_PROC, ERTS_MON_TYPE_PORT,
 *
 * @param[in]     id            Id of the entity linked.
 *
 * @returns                     A pointer to the link structure.
 */
ErtsLink *erts_link_internal_create(Uint16 type, Eterm id);

/**
 *
 * @brief Get pointer to external link data structure
 *
 * @param[in]    lnk            Pointer to link
 *
 * @returns                     Pointer to external link structure
 *
 */
ERTS_GLB_INLINE ErtsELink *erts_link_to_elink(ErtsLink *lnk);

/**
 *
 * @brief Get pointer to the other link structure part of the link
 *
 * @param[in]    lnk            Pointer to link structure
 *
 * @param[out]   elnkpp         Pointer to pointer to external link
 *                              data structure, if a non-NULL value
 *                              is passed in the call
 *
 * @returns                     Pointer to other link structure
 *
 */
ERTS_GLB_INLINE ErtsLink *erts_link_to_other(ErtsLink *lnk, ErtsELink **elnkpp);

/**
 *
 * @brief Check if link is in tree or list
 *
 * @param[in]    lnk            Pointer to lnk to check
 *
 * @returns                     A non-zero value if in tree or list;
 *                              otherwise zero
 *
 */
ERTS_GLB_INLINE int erts_link_is_in_table(ErtsLink *lnk);

/**
 *
 * @brief Release an internal link
 *
 * When the function is called it is assumed that:
 * - 'lnk' link is not part of any list or tree
 * - 'lnk' is not referred to by any other structures
 * If the above are not true, bad things will happen.
 *
 * @param[in]    lnk            Pointer to link
 *
 */
ERTS_GLB_INLINE void erts_link_internal_release(ErtsLink *lnk);

/**
 *
 * @brief Release link
 *
 * Can be used to release a link half of an external
 * link as well as an internal link. In the external
 * case both link halves part of the external link have
 * to been released before the link structure will be
 * deallocated.
 *
 * When the function is called it is assumed that:
 * - 'lnk' link is not part of any list or tree
 * - 'lnk' is not referred to by any other structures
 * If the above are not true, bad things will happen.
 *
 * @param[in]    lnk            Pointer to link
 *
 */
ERTS_GLB_INLINE void erts_link_release(ErtsLink *lnk);

/**
 *
 * @brief Release both link halves of an external link
 *        simultaneously
 *
 * Release both halves of an external link simultaneously and 
 * deallocate the structure.
 *
 * When the function is called it is assumed that:
 * - Neither of the parts of the link are part of any list or tree
 * - Neither of the parts of the link or the link data structure
 *   are referred to by any other structures
 * If the above are not true, bad things will happen.
 *
 * @param[in]    mdp            Pointer to link data structure
 *
 */
ERTS_GLB_INLINE void erts_link_release_both(ErtsLinkData *ldp);

/**
 *
 * @brief Insert link in dist link list
 *
 * When the function is called it is assumed that:
 * - 'lnk' link is not part of any list or tree
 * If the above is not true, bad things will happen.
 *
 * @param[in]    lnk            Pointer to link
 *
 * @param[in]    dist           Pointer to dist structure
 *
 * @returns                     A non-zero value if inserted;
 *                              otherwise, zero. The link
 *                              is not inserted if the dist
 *                              structure has been set in a
 *                              dead state.
 *
 */
ERTS_GLB_INLINE int erts_link_dist_insert(ErtsLink *lnk, ErtsMonLnkDist *dist);

/**
 *
 * @brief Delete link from dist link list
 *
 * When the function is called it is assumed that:
 * - 'lnk' link earlier has been inserted into 'dist'
 * If the above is not true, bad things will happen.
 *
 * @param[in]    lnk            Pointer to link
 *
 * @param[in]    dist           Pointer to dist structure
 *
 * @returns                     A non-zero value if deleted;
 *                              otherwise, zero. The link
 *                              is not deleted if the dist
 *                              structure has been set in a
 *                              dead state or if it has already
 *                              been deleted.
 *
 */
ERTS_GLB_INLINE int erts_link_dist_delete(ErtsLink *lnk);

/**
 *
 * @brief Set dead dist structure on link
 *
 * @param[in]    lnk            Pointer to link
 *
 * @param[in]    nodename       Name of remote node
 *
 */
void
erts_link_set_dead_dist(ErtsLink *lnk, Eterm nodename);

/**
 *
 * @brief Get charged size of link
 *
 * If the other side of the link has been released, the
 * whole size of the link data structure is returned; otherwise,
 * half of the size is returned.
 *
 * When the function is called it is assumed that:
 * - 'lnk' has not been released
 * If the above is not true, bad things will happen.
 *
 * @param[in]    lnk            Pointer to link
 *
 * @returns                     Charged size in bytes
 *
 */
Uint erts_link_size(ErtsLink *lnk);

/* internal function... */
void erts_link_destroy_elink__(ErtsELink *elnk);

/* implementations for globally inlined link functions... */
#if ERTS_GLB_INLINE_INCL_FUNC_DEF

#ifdef ERTS_ML_DEBUG
extern size_t erts_link_proc_offset;
extern size_t erts_link_dist_offset;
extern size_t erts_link_key_offset;
#endif

ERTS_GLB_INLINE ErtsELink *
erts_link_to_elink(ErtsLink *lnk)
{
    ErtsELink *elnk;

    ERTS_ML_ASSERT(lnk->flags & ERTS_ML_FLG_EXTENDED);

    elnk = (ErtsELink *) erts_ml_node_to_main_struct__((ErtsMonLnkNode *) lnk);

#ifdef ERTS_ML_DEBUG
    ERTS_ML_ASSERT(erts_link_proc_offset == (size_t) elnk->ld.proc.offset);
    ERTS_ML_ASSERT(erts_link_key_offset == (size_t) elnk->ld.proc.key_offset);
    ERTS_ML_ASSERT(erts_link_dist_offset == (size_t) elnk->ld.dist.offset);
    ERTS_ML_ASSERT(erts_link_key_offset == (size_t) elnk->ld.dist.key_offset);
#endif

    return elnk;
}

ERTS_GLB_INLINE ErtsLink *
erts_link_to_other(ErtsLink *lnk, ErtsELink **elnkpp)
{
    ErtsELink *elnk = erts_link_to_elink(lnk);
    if (elnkpp)
        *elnkpp = elnk;
    return lnk == &elnk->ld.proc ? &elnk->ld.dist : &elnk->ld.proc;
}

ERTS_GLB_INLINE int
erts_link_is_in_table(ErtsLink *lnk)
{
    return !!(lnk->flags & ERTS_ML_FLG_IN_TABLE);
}

ERTS_GLB_INLINE void
erts_link_list_insert(ErtsLink **list, ErtsLink *lnk)
{
    erts_ml_dl_list_insert__((ErtsMonLnkNode **) list, (ErtsMonLnkNode *) lnk);
}

ERTS_GLB_INLINE void
erts_link_list_delete(ErtsLink **list, ErtsLink *lnk)
{
    erts_ml_dl_list_delete__((ErtsMonLnkNode **) list, (ErtsMonLnkNode *) lnk);
}

ERTS_GLB_INLINE ErtsLink *
erts_link_list_first(ErtsLink *list)
{
    return (ErtsLink *) erts_ml_dl_list_first__((ErtsMonLnkNode *) list);
}

ERTS_GLB_INLINE ErtsLink *
erts_link_list_last(ErtsLink *list)
{
    return (ErtsLink *) erts_ml_dl_list_last__((ErtsMonLnkNode *) list);
}

ERTS_GLB_INLINE void
erts_link_internal_release(ErtsLink *lnk)
{
    ERTS_ML_ASSERT(lnk->type == ERTS_LNK_TYPE_PROC
                   || lnk->type == ERTS_LNK_TYPE_PORT);
    ERTS_ML_ASSERT(!(lnk->flags & ERTS_ML_FLG_EXTENDED));
    erts_free(ERTS_ALC_T_LINK, lnk);
}

ERTS_GLB_INLINE void
erts_link_release(ErtsLink *lnk)
{
    if (!(lnk->flags & ERTS_ML_FLG_EXTENDED))
        erts_link_internal_release(lnk);
    else {
        ErtsELink *elnk = erts_link_to_elink(lnk);
        ERTS_ML_ASSERT(!(lnk->flags & ERTS_ML_FLG_IN_TABLE));
        ERTS_ML_ASSERT(erts_atomic32_read_nob(&elnk->ld.refc) > 0);
        if (erts_atomic32_dec_read_nob(&elnk->ld.refc) == 0)
            erts_link_destroy_elink__(elnk);
    }
}

ERTS_GLB_INLINE void
erts_link_release_both(ErtsLinkData *ldp)
{
    ERTS_ML_ASSERT(!(ldp->proc.flags & ERTS_ML_FLG_IN_TABLE));
    ERTS_ML_ASSERT(!(ldp->dist.flags & ERTS_ML_FLG_IN_TABLE));
    ERTS_ML_ASSERT(erts_atomic32_read_nob(&ldp->refc) >= 2);
    ERTS_ML_ASSERT(ldp->proc.flags & ERTS_ML_FLG_EXTENDED);
    ERTS_ML_ASSERT(ldp->dist.flags & ERTS_ML_FLG_EXTENDED);
    if (erts_atomic32_add_read_nob(&ldp->refc, (erts_aint32_t) -2) == 0)
        erts_link_destroy_elink__((ErtsELink *) ldp);
}

ERTS_GLB_INLINE ErtsLink *
erts_link_tree_insert_addr_replace(ErtsLink **root, ErtsLink *lnk)
{
    ErtsLink *lnk2 = erts_link_tree_lookup_insert(root, lnk);
    if (!lnk2)
        return NULL;
    if (lnk2 < lnk)
        return lnk;
    erts_link_tree_replace(root, lnk2, lnk);
    return lnk2;
}

ERTS_GLB_INLINE ErtsLink *
erts_link_tree_key_delete(ErtsLink **root, ErtsLink *lnk)
{
    ErtsLink *dlnk;
    if (erts_link_is_in_table(lnk))
        dlnk = lnk;
    else
        dlnk = erts_link_tree_lookup(*root, lnk->other.item);
    if (dlnk)
        erts_link_tree_delete(root, dlnk);
    return dlnk;
}

ERTS_GLB_INLINE int
erts_link_dist_insert(ErtsLink *lnk, ErtsMonLnkDist *dist)
{
    ErtsELink *elnk;
    int insert;

    ERTS_ML_ASSERT(lnk->flags & ERTS_ML_FLG_EXTENDED);
    ERTS_ML_ASSERT(lnk->type == ERTS_LNK_TYPE_DIST_PROC);

    elnk = erts_link_to_elink(lnk);

    ERTS_ML_ASSERT(!elnk->dist);
    ERTS_ML_ASSERT(dist);

    erts_mtx_lock(&dist->mtx);

    insert = dist->alive;
    if (insert) {
        elnk->dist = dist;
        erts_mon_link_dist_inc_refc(dist);
        erts_link_list_insert(&dist->links, lnk);
    }

    erts_mtx_unlock(&dist->mtx);

    return insert;
}

ERTS_GLB_INLINE int
erts_link_dist_delete(ErtsLink *lnk)
{
    ErtsELink *elnk;
    ErtsMonLnkDist *dist;
    int delete_;

    ERTS_ML_ASSERT(lnk->flags & ERTS_ML_FLG_EXTENDED);
    ERTS_ML_ASSERT(lnk->type == ERTS_LNK_TYPE_DIST_PROC);

    elnk = erts_link_to_elink(lnk);
    dist = elnk->dist;
    if (!dist)
        return -1;

    erts_mtx_lock(&dist->mtx);

    delete_ = !!dist->alive & !!(lnk->flags & ERTS_ML_FLG_IN_TABLE);
    if (delete_)
        erts_link_list_delete(&dist->links, lnk);

    erts_mtx_unlock(&dist->mtx);

    return delete_;
}


#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

void
erts_debug_link_tree_destroying_foreach(ErtsLink *root,
                                        ErtsLinkFunc func,
                                        void *arg,
                                        void *vysp);

#endif /* ERL_MONITOR_LINK_H__ */
