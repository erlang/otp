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

#ifndef ERL_NODE_TABLES_BASIC__
#define ERL_NODE_TABLES_BASIC__

typedef struct dist_entry_ DistEntry;
typedef struct ErtsDistOutputBuf_ ErtsDistOutputBuf;
void erts_ref_dist_entry(DistEntry *dep);
void erts_deref_dist_entry(DistEntry *dep);

#endif

#if !defined(ERL_NODE_TABLES_BASIC_ONLY) && !defined(ERL_NODE_TABLES_H__)
#define ERL_NODE_TABLES_H__

/*
 * The "node_tables module" contain two (hash) tables: the node_table
 * and the dist_table.
 *
 * The elements of the node_table represents a specific incarnation of
 * an Erlang node and has {Nodename, Creation} pairs as keys. Elements
 * in the node_table are referred to from node containers (see
 * node_container_utils.h).
 *
 * The elements of the dist_table represents a (potential) connection
 * to an Erlang node and has Nodename as key. Elements in the
 * dist_table are either referred to from elements in the node_table
 * or from the process or port structure of the entity controlling
 * the connection.
 *
 * Both tables are garbage collected by reference counting.
 */

#include "sys.h"
#include "hash.h"
#include "erl_alloc.h"
#define ERTS_PORT_TASK_ONLY_BASIC_TYPES__
#include "erl_port_task.h"
#undef ERTS_PORT_TASK_ONLY_BASIC_TYPES__
#include "erl_process.h"
#define ERTS_BINARY_TYPES_ONLY__
#include "erl_binary.h"
#undef ERTS_BINARY_TYPES_ONLY__
#include "erl_monitor_link.h"

#define ERTS_NODE_TAB_DELAY_GC_DEFAULT (60)
#define ERTS_NODE_TAB_DELAY_GC_MAX (100*1000*1000)
#define ERTS_NODE_TAB_DELAY_GC_INFINITY (ERTS_NODE_TAB_DELAY_GC_MAX+1)
 
#define ERST_INTERNAL_CHANNEL_NO 0

enum dist_entry_state {
    ERTS_DE_STATE_IDLE,
    ERTS_DE_STATE_PENDING,
    ERTS_DE_STATE_CONNECTED,
    ERTS_DE_STATE_EXITING
};

#define ERTS_DE_QFLG_BUSY			(((erts_aint32_t) 1) <<  0)
#define ERTS_DE_QFLG_EXIT			(((erts_aint32_t) 1) <<  1)
#define ERTS_DE_QFLG_REQ_INFO			(((erts_aint32_t) 1) <<  2)
#define ERTS_DE_QFLG_PORT_CTRL                  (((erts_aint32_t) 1) <<  3)
#define ERTS_DE_QFLG_PROC_CTRL                  (((erts_aint32_t) 1) <<  4)

#define ERTS_DE_QFLGS_ALL			(ERTS_DE_QFLG_BUSY \
						 | ERTS_DE_QFLG_EXIT \
                                                 | ERTS_DE_QFLG_REQ_INFO \
                                                 | ERTS_DE_QFLG_PORT_CTRL \
                                                 | ERTS_DE_QFLG_PROC_CTRL)

#if defined(ARCH_64)
#define ERTS_DIST_OUTPUT_BUF_DBG_PATTERN ((Uint) 0xf713f713f713f713UL)
#else
#define ERTS_DIST_OUTPUT_BUF_DBG_PATTERN ((Uint) 0xf713f713)
#endif

struct ErtsDistOutputBuf_ {
#ifdef DEBUG
    Uint dbg_pattern;
    byte *alloc_endp;
#endif
    ErtsDistOutputBuf *next;
    Uint hopefull_flags;
    byte *extp;
    byte *ext_endp;
    byte *msg_start;
    byte data[1];
};

typedef struct {
    ErtsDistOutputBuf *first;
    ErtsDistOutputBuf *last;
} ErtsDistOutputQueue;

struct ErtsProcList_;

/*
 * Lock order:
 *   1. dist_entry->rwmtx
 *   2. erts_node_table_rwmtx
 *   3. erts_dist_table_rwmtx
 *
 *   Lock mutexes with lower numbers before mutexes with higher numbers and
 *   unlock mutexes with higher numbers before mutexes with higher numbers.
 */

struct dist_entry_ {
    HashBucket hash_bucket;     /* Hash bucket */
    struct dist_entry_ *next;	/* Next entry in dist_table (not sorted) */
    struct dist_entry_ *prev;	/* Previous entry in dist_table (not sorted) */

    erts_rwmtx_t rwmtx;         /* Protects all fields below until lck_mtx. */
    Eterm sysname;		/* name@host atom for efficiency */
    Uint32 creation;		/* creation of connected node */
    erts_atomic_t input_handler; /* Input handler */
    Eterm cid;			/* connection handler (pid or port),
                                   NIL == free */
    Uint32 connection_id;	/* Connection id incremented on connect */
    enum dist_entry_state state;
    Uint32 flags;		/* Distribution flags, like hidden, 
				   atom cache etc. */
    unsigned long version;	/* Protocol version */

    ErtsMonLnkDist *mld;        /* Monitors and links */

    erts_mtx_t qlock;           /* Protects qflgs and out_queue */
    erts_atomic32_t qflgs;
    erts_atomic_t qsize;
    erts_atomic64_t in;
    erts_atomic64_t out;
    ErtsDistOutputQueue out_queue;
    struct ErtsProcList_ *suspended;

    ErtsDistOutputQueue tmp_out_queue;
    ErtsDistOutputQueue finalized_out_queue;
    erts_atomic_t dist_cmd_scheduled;
    ErtsPortTaskHandle dist_cmd;

    Uint (*send)(Port *prt, ErtsDistOutputBuf *obuf);

    struct cache* cache;	/* The atom cache */

    ErtsThrPrgrLaterOp later_op;

    struct transcode_context* transcode_ctx;
};

typedef struct erl_node_ {
  HashBucket hash_bucket;	/* Hash bucket */
  erts_refc_t refc;		/* Reference count */
  Eterm	sysname;		/* name@host atom for efficiency */
  Uint32 creation;		/* Creation */
  DistEntry *dist_entry;	/* Corresponding dist entry */
} ErlNode;


extern Hash erts_dist_table;
extern Hash erts_node_table;
extern erts_rwmtx_t erts_dist_table_rwmtx;
extern erts_rwmtx_t erts_node_table_rwmtx;

extern DistEntry *erts_hidden_dist_entries;
extern DistEntry *erts_visible_dist_entries;
extern DistEntry *erts_pending_dist_entries;
extern DistEntry *erts_not_connected_dist_entries;
extern Sint erts_no_of_hidden_dist_entries;
extern Sint erts_no_of_visible_dist_entries;
extern Sint erts_no_of_pending_dist_entries;
extern Sint erts_no_of_not_connected_dist_entries;

extern DistEntry *erts_this_dist_entry;
extern ErlNode *erts_this_node;
extern char *erts_this_node_sysname; /* must match erl_node_tables.c */

Uint erts_delayed_node_table_gc(void);
DistEntry *erts_channel_no_to_dist_entry(Uint);
DistEntry *erts_sysname_to_connected_dist_entry(Eterm);
DistEntry *erts_find_or_insert_dist_entry(Eterm);
DistEntry *erts_find_dist_entry(Eterm);
void erts_schedule_delete_dist_entry(DistEntry *);
Uint erts_dist_table_size(void);
void erts_dist_table_info(fmtfn_t, void *);
void erts_set_dist_entry_not_connected(DistEntry *);
void erts_set_dist_entry_pending(DistEntry *);
void erts_set_dist_entry_connected(DistEntry *, Eterm, Uint);
ErlNode *erts_find_or_insert_node(Eterm, Uint32);
void erts_schedule_delete_node(ErlNode *);
void erts_set_this_node(Eterm, Uint);
Uint erts_node_table_size(void);
void erts_init_node_tables(int);
void erts_node_table_info(fmtfn_t, void *);
void erts_print_node_info(fmtfn_t, void *, Eterm, int*, int*);
Eterm erts_get_node_and_dist_references(struct process *);
#if defined(ERTS_ENABLE_LOCK_CHECK)
int erts_lc_is_de_rwlocked(DistEntry *);
int erts_lc_is_de_rlocked(DistEntry *);
#endif
int erts_dist_entry_destructor(Binary *bin);
DistEntry *erts_dhandle_to_dist_entry(Eterm dhandle);
Eterm erts_build_dhandle(Eterm **hpp, ErlOffHeap*, DistEntry*);
Eterm erts_make_dhandle(Process *c_p, DistEntry *dep);

ERTS_GLB_INLINE void erts_deref_node_entry(ErlNode *np);
ERTS_GLB_INLINE void erts_de_rlock(DistEntry *dep);
ERTS_GLB_INLINE void erts_de_runlock(DistEntry *dep);
ERTS_GLB_INLINE void erts_de_rwlock(DistEntry *dep);
ERTS_GLB_INLINE void erts_de_rwunlock(DistEntry *dep);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_deref_node_entry(ErlNode *np)
{
    ASSERT(np);
    if (erts_refc_dectest(&np->refc, 0) == 0)
	erts_schedule_delete_node(np);
}

ERTS_GLB_INLINE void
erts_de_rlock(DistEntry *dep)
{
    erts_rwmtx_rlock(&dep->rwmtx);
}

ERTS_GLB_INLINE void
erts_de_runlock(DistEntry *dep)
{
    erts_rwmtx_runlock(&dep->rwmtx);
}

ERTS_GLB_INLINE void
erts_de_rwlock(DistEntry *dep)
{
    erts_rwmtx_rwlock(&dep->rwmtx);
}

ERTS_GLB_INLINE void
erts_de_rwunlock(DistEntry *dep)
{
    erts_rwmtx_rwunlock(&dep->rwmtx);
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

void erts_debug_test_node_tab_delayed_delete(Sint64 millisecs);
void erts_lcnt_update_distribution_locks(int enable);

#endif
