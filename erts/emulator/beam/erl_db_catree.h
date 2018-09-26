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
    erts_rwmtx_t lock; /* The lock for this base node */
    Sint lock_statistics;
    int is_valid; /* If this base node is still valid */
    TreeDbTerm *root; /* The root of the sequential tree */
    DbTable * tab; /* Table ptr, used when freeing using thread progress */
    ErtsThrPrgrLaterOp free_item; /* Used when freeing using thread progress */
    struct DbTableCATreeNode * next; /* Used when gradually deleting */
} DbTableCATreeBaseNode;

typedef struct {
    ErtsThrPrgrLaterOp free_item; /* Used when freeing using thread progress */
    DbTable* tab; /* Table ptr, used when freeing using thread progress */
    erts_mtx_t lock; /* Used when joining route nodes */
    int is_valid; /* If this route node is still valid */
    erts_atomic_t left;
    erts_atomic_t right;
    DbTerm key;
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
    DbTreeStack free_stack_elems;/* Used for deletion ...*/
    CATreeNodeStack free_stack_rnodes;
    DbTableCATreeNode *base_nodes_to_free_list;
    int is_routing_nodes_freed;
} DbTableCATree;

void db_initialize_catree(void);

int db_create_catree(Process *p, DbTable *tbl);


#ifdef ERTS_ENABLE_LOCK_COUNT
void erts_lcnt_enable_db_catree_lock_count(DbTableCATree *tb, int enable);
#endif

#endif /* _DB_CATREE_H */
