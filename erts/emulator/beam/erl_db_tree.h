/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2009. All Rights Reserved.
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

#ifndef _DB_TREE_H
#define _DB_TREE_H

#include "erl_db_util.h"

typedef struct tree_db_term {
    struct  tree_db_term *left, *right;  /* left and right child */
    int  balance;                        /* tree balancing value */
    DbTerm dbterm;                       /* The actual term */
} TreeDbTerm;

typedef struct {
    Uint pos;          /* Current position on stack */
    Uint slot;         /* "Slot number" of top element or 0 if not set */
    TreeDbTerm** array; /* The stack */
} DbTreeStack;

typedef struct db_table_tree {
    DbTableCommon common;

    /* Tree-specific fields */
    TreeDbTerm *root;         /* The tree root */
    Uint deletion;		/* Being deleted */
    erts_smp_atomic_t is_stack_busy;
    DbTreeStack static_stack;
} DbTableTree;

/*
** Function prototypes, looks the same (except the suffix) for all 
** table types. The process is always an [in out] parameter.
*/
void db_initialize_tree(void);

int db_create_tree(Process *p, DbTable *tbl);

#endif /* _DB_TREE_H */
