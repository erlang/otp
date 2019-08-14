/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
 * Description: Implementation of ETS ordered_set table type with
 *              fine-grained synchronization.
 *
 * Author: 	Kjell Winblad
 *
 * "erl_db_catree.c" contains more details about the implementation.
 *
 */

#ifndef _DB_CATREE_H
#define _DB_CATREE_H

struct DbTableCATreeNode;

typedef struct {
    Eterm term;
    struct erl_off_heap_header* oh;
    Uint size;
    Eterm heap[1];
} DbRouteKey;

typedef struct {
    erts_rwmtx_t lock; /* The lock for this base node */
    erts_atomic_t lock_statistics;
    int is_valid; /* If this base node is still valid */
    TreeDbTerm *root; /* The root of the sequential tree */
    ErtsThrPrgrLaterOp free_item; /* Used when freeing using thread progress */

    char end_of_struct__;
} DbTableCATreeBaseNode;

typedef struct {
#ifdef ERTS_ENABLE_LOCK_CHECK
    Sint lc_order;
#endif
    ErtsThrPrgrLaterOp free_item; /* Used when freeing using thread progress */
    erts_mtx_t lock; /* Used when joining route nodes */
    int is_valid; /* If this route node is still valid */
    erts_atomic_t left;
    erts_atomic_t right;
    DbRouteKey key;
} DbTableCATreeRouteNode;

typedef struct DbTableCATreeNode {
    int is_base_node;
    union {
        DbTableCATreeRouteNode route;
        DbTableCATreeBaseNode base;
    } u;
} DbTableCATreeNode;

typedef struct {
    Uint pos;          /* Current position on stack */
    Uint size;         /* The size of the stack array */
    DbTableCATreeNode** array; /* The stack */
} CATreeNodeStack;

typedef struct db_table_catree {
    DbTableCommon common;

    /* CA Tree-specific fields */
    erts_atomic_t root;         /* The tree root (DbTableCATreeNode*) */
    Uint deletion;		/* Being deleted */
    int is_routing_nodes_freed;
    /* The fields below are used by delete_all_objects and
       select_delete(DeleteAll)*/
    Uint nr_of_deleted_items;
    Binary* nr_of_deleted_items_wb;
} DbTableCATree;

typedef struct {
    DbTableCATree* tb;
    Eterm next_route_key;
    DbTableCATreeNode* locked_bnode;
    DbTableCATreeNode* bnode_parent;
    int bnode_level;
    int read_only;
    DbRouteKey* search_key;
} CATreeRootIterator;


void db_initialize_catree(void);

int db_create_catree(Process *p, DbTable *tbl);

TreeDbTerm** catree_find_root(Eterm key, CATreeRootIterator*);

TreeDbTerm** catree_find_next_from_pb_key_root(Eterm key, CATreeRootIterator*);
TreeDbTerm** catree_find_prev_from_pb_key_root(Eterm key, CATreeRootIterator*);
TreeDbTerm** catree_find_nextprev_root(CATreeRootIterator*, int next, Eterm* keyp);
TreeDbTerm** catree_find_next_root(CATreeRootIterator*, Eterm* keyp);
TreeDbTerm** catree_find_prev_root(CATreeRootIterator*, Eterm* keyp);
TreeDbTerm** catree_find_first_root(CATreeRootIterator*);
TreeDbTerm** catree_find_last_root(CATreeRootIterator*);


#ifdef ERTS_ENABLE_LOCK_COUNT
void erts_lcnt_enable_db_catree_lock_count(DbTableCATree *tb, int enable);
#endif

void db_catree_force_split(DbTableCATree*, int on);
void db_catree_debug_random_split_join(DbTableCATree*, int on);

typedef struct {
    Uint route_nodes;
    Uint base_nodes;
    Uint max_depth;
} DbCATreeStats;
void db_calc_stats_catree(DbTableCATree*, DbCATreeStats*);
void
erts_db_foreach_thr_prgr_offheap_catree(void (*func)(ErlOffHeap *, void *),
                                        void *arg);


#endif /* _DB_CATREE_H */
