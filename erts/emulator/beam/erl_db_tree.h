/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 1998-2025. All Rights Reserved.
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

#ifndef _DB_TREE_H
#define _DB_TREE_H

#include "erl_db_util.h"

typedef struct tree_db_term {
    struct tree_db_term *left, *right;   /* left and right child */
    DbTerm dbterm;                       /* The actual term */
} TreeDbTerm;

/*
** AVL balance factor is encoded in the 2 low bits of the `left` pointer.
** This works because ERTS_ALLOC_ALIGN_BYTES >= 8, giving 3 free low bits.
**
** Encoding: -1 → 0b11, 0 → 0b00, +1 → 0b01  (simply: balance & 3)
** Decoding: 2-bit sign extension — if bit 1 set, value is negative.
*/
#define TREE_TAG_MASK ((UWord)3)

/*
** AVL tree node access.
*/
#define TREE_GET_LEFT(p) ((TreeDbTerm *)((UWord)(p)->left & ~TREE_TAG_MASK))
#define TREE_GET_RIGHT(p) ((p)->right)
#define TREE_GET_BALANCE(p)                                                    \
    ((int)((UWord)(p)->left & TREE_TAG_MASK) |                                 \
     -((int)(((UWord)(p)->left & 2) >> 1) << 1))
#define TREE_SET_BALANCE(p, b)                                                 \
    ((p)->left = (TreeDbTerm *)(((UWord)(p)->left & ~TREE_TAG_MASK) |          \
                                ((UWord)(b) & TREE_TAG_MASK)))
#define TREE_SET_LEFT(p, child)                                                \
    ((p)->left = (TreeDbTerm *)(((UWord)(child)) |                             \
                                ((UWord)(p)->left & TREE_TAG_MASK)))
#define TREE_SET_RIGHT(p, child) ((p)->right = (child))

/*
** When traversing via TreeDbTerm** (pointer-to-pointer to a node field),
** the pointed-to location may be a `left` field with tag bits.
** TREE_DEREF_NODE strips tags; TREE_ASSIGN_NODE preserves existing tags.
*/
#define TREE_DEREF_NODE(pp) ((TreeDbTerm *)((UWord)(*(pp)) & ~TREE_TAG_MASK))
#define TREE_ASSIGN_NODE(pp, node)                                             \
    (*(pp) = (TreeDbTerm *)(((UWord)(node)) | ((UWord)(*(pp)) & TREE_TAG_MASK)))

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
    erts_atomic_t is_stack_busy;
    DbTreeStack static_stack;
} DbTableTree;

/*
** Function prototypes, looks the same (except the suffix) for all 
** table types. The process is always an [in out] parameter.
*/
void db_initialize_tree(void);

int db_create_tree(Process *p, DbTable *tbl);

void
erts_db_foreach_thr_prgr_offheap_tree(void (*func)(ErlOffHeap *, void *),
                                      void *arg);

#endif /* _DB_TREE_H */
