/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB and Kjell Winblad 1998-2018. All Rights Reserved.
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
 * This implementation is based on the contention adapting search tree
 * (CA tree). The CA tree is a concurrent data structure that
 * dynamically adapts its synchronization granularity based on how
 * much contention is detected in locks. The following publication
 * contains a detailed description of CA trees:
 * 
 * A Contention Adapting Approach to Concurrent Ordered Sets
 * Journal of Parallel and Distributed Computing, 2018
 * Kjell Winblad and Konstantinos Sagonas
 * https://doi.org/10.1016/j.jpdc.2017.11.007
 *
 * The following publication may also be interesting as it discusses
 * how the CA tree can be used as an ETS ordered_set table type
 * backend:
 *
 * More Scalable Ordered Set for ETS Using Adaptation
 * In Thirteenth ACM SIGPLAN workshop on Erlang (2014)
 * Kjell Winblad and Konstantinos Sagonas
 * https://doi.org/10.1145/2633448.2633455
 *
 * This implementation of the ordered_set ETS table type is only
 * activated when the options {write_concurrency, true}, public and
 * ordered_set are passed to the ets:new/2 function. This
 * implementation is expected to scale better than the default
 * implementation (located in "erl_db_tree.c") when concurrent
 * processes use the following ETS operations to operate on a table:
 * 
 * delete/2, delete_object/2, first/1, insert/2 (single object),
 * insert_new/2 (single object), lookup/2, lookup_element/2, member/2,
 * next/2, take/2 and update_element/3 (single object).
 *
 * Currently, the implementation does not have scalable support for
 * the other operations (e.g., select/2). These operations are handled
 * by merging all locks so that all terms get protected by a single
 * lock. This implementation may thus perform worse than the default
 * implementation in some scenarios. For example, when concurrent
 * processes access a table with the operations insert/2, delete/2 and
 * select/2, the insert/2 and delete/2 operations will trigger splits
 * of locks (to get more fine-grained synchronization) but this will
 * quickly be undone by the select/2 operation if this operation is
 * also called frequently.
 *
 * The default implementation has a static stack optimization (see
 * get_static_stack in erl_db_tree.c). This implementation does not
 * have such an optimization as it induces bad scalability when
 * concurrent read operations are frequent (they all try to get hold
 * of the same stack). The default implementation may thus perform
 * better compared to this implementation in scenarios where the
 * static stack optimization is useful. One such scenario is when only
 * one process is accessing the table and this process is traversing
 * the table with a sequence of next/2 calls.
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

#include "erl_db_catree.h"
#include "erl_db_tree.h"
#include "erl_db_tree_util.h"

/*
** Forward declarations
*/

static SWord do_free_base_node_cont(DbTableCATree *tb, SWord num_left);
static SWord do_free_routing_nodes_catree_cont(DbTableCATree *tb, SWord num_left);
static DbTableCATreeNode *catree_first_base_node_from_free_list(DbTableCATree *tb);

/* Method interface functions */
static int db_first_catree(Process *p, DbTable *tbl,
                           Eterm *ret);
static int db_next_catree(Process *p, DbTable *tbl,
                          Eterm key, Eterm *ret);
static int db_last_catree(Process *p, DbTable *tbl,
                          Eterm *ret);
static int db_prev_catree(Process *p, DbTable *tbl,
                          Eterm key,
                          Eterm *ret);
static int db_put_catree(DbTable *tbl, Eterm obj, int key_clash_fail);
static int db_get_catree(Process *p, DbTable *tbl,
                         Eterm key,  Eterm *ret);
static int db_member_catree(DbTable *tbl, Eterm key, Eterm *ret);
static int db_get_element_catree(Process *p, DbTable *tbl,
                                 Eterm key,int ndex,
                                 Eterm *ret);
static int db_erase_catree(DbTable *tbl, Eterm key, Eterm *ret);
static int db_erase_object_catree(DbTable *tbl, Eterm object,Eterm *ret);
static int db_slot_catree(Process *p, DbTable *tbl,
                          Eterm slot_term,  Eterm *ret);
static int db_select_catree(Process *p, DbTable *tbl, Eterm tid,
                            Eterm pattern, int reversed, Eterm *ret);
static int db_select_count_catree(Process *p, DbTable *tbl, Eterm tid,
                                  Eterm pattern,  Eterm *ret);
static int db_select_chunk_catree(Process *p, DbTable *tbl, Eterm tid,
                                  Eterm pattern, Sint chunk_size,
                                  int reversed, Eterm *ret);
static int db_select_continue_catree(Process *p, DbTable *tbl,
                                     Eterm continuation, Eterm *ret);
static int db_select_count_continue_catree(Process *p, DbTable *tbl,
                                           Eterm continuation, Eterm *ret);
static int db_select_delete_catree(Process *p, DbTable *tbl, Eterm tid,
                                   Eterm pattern,  Eterm *ret);
static int db_select_delete_continue_catree(Process *p, DbTable *tbl, 
                                            Eterm continuation, Eterm *ret);
static int db_select_replace_catree(Process *p, DbTable *tbl, Eterm tid,
                                    Eterm pattern, Eterm *ret);
static int db_select_replace_continue_catree(Process *p, DbTable *tbl,
                                             Eterm continuation, Eterm *ret);
static int db_take_catree(Process *, DbTable *, Eterm, Eterm *);
static void db_print_catree(fmtfn_t to, void *to_arg,
                            int show, DbTable *tbl);
static int db_free_table_catree(DbTable *tbl);
static SWord db_free_table_continue_catree(DbTable *tbl, SWord);
static void db_foreach_offheap_catree(DbTable *,
                                      void (*)(ErlOffHeap *, void *),
                                      void *);
static SWord db_delete_all_objects_catree(Process* p, DbTable* tbl, SWord reds);
static int
db_lookup_dbterm_catree(Process *, DbTable *, Eterm key, Eterm obj,
                        DbUpdateHandle*);
static void db_finalize_dbterm_catree(int cret, DbUpdateHandle *);


/*
** External interface
*/
DbTableMethod db_catree =
{
    db_create_catree,
    db_first_catree,
    db_next_catree,
    db_last_catree,
    db_prev_catree,
    db_put_catree,
    db_get_catree,
    db_get_element_catree,
    db_member_catree,
    db_erase_catree,
    db_erase_object_catree,
    db_slot_catree,
    db_select_chunk_catree,
    db_select_catree,
    db_select_delete_catree,
    db_select_continue_catree,
    db_select_delete_continue_catree,
    db_select_count_catree,
    db_select_count_continue_catree,
    db_select_replace_catree,
    db_select_replace_continue_catree,
    db_take_catree,
    db_delete_all_objects_catree,
    db_free_table_catree,
    db_free_table_continue_catree,
    db_print_catree,
    db_foreach_offheap_catree,
    db_lookup_dbterm_catree,
    db_finalize_dbterm_catree

};

/*
 * Constants
 */

#define ERL_DB_CATREE_LOCK_FAILURE_CONTRIBUTION 200
#define ERL_DB_CATREE_LOCK_SUCCESS_CONTRIBUTION (-1)
#define ERL_DB_CATREE_LOCK_MORE_THAN_ONE_CONTRIBUTION (-10)
#define ERL_DB_CATREE_HIGH_CONTENTION_LIMIT 1000
#define ERL_DB_CATREE_LOW_CONTENTION_LIMIT (-1000)
#define ERL_DB_CATREE_MAX_ROUTE_NODE_LAYER_HEIGHT 14

/*
 * Internal CA tree related helper functions and macros
 */

#define GET_ROUTE_NODE_KEY(node) (node->u.route.key.term)
#define GET_BASE_NODE_LOCK(node) (&(node->u.base.lock))
#define GET_ROUTE_NODE_LOCK(node) (&(node->u.route.lock))


/* Helpers for reading and writing shared atomic variables */

/* No memory barrier */
#define GET_ROOT(tb) ((DbTableCATreeNode*)erts_atomic_read_nob(&(tb->root)))
#define GET_LEFT(ca_tree_route_node) ((DbTableCATreeNode*)erts_atomic_read_nob(&(ca_tree_route_node->u.route.left)))
#define GET_RIGHT(ca_tree_route_node) ((DbTableCATreeNode*)erts_atomic_read_nob(&(ca_tree_route_node->u.route.right)))
#define SET_ROOT(tb, v) erts_atomic_set_nob(&((tb)->root), (erts_aint_t)(v))
#define SET_LEFT(ca_tree_route_node, v) erts_atomic_set_nob(&(ca_tree_route_node->u.route.left), (erts_aint_t)(v));
#define SET_RIGHT(ca_tree_route_node, v) erts_atomic_set_nob(&(ca_tree_route_node->u.route.right), (erts_aint_t)(v));


/* Release or acquire barriers */
#define GET_ROOT_ACQB(tb) ((DbTableCATreeNode*)erts_atomic_read_acqb(&(tb->root)))
#define GET_LEFT_ACQB(ca_tree_route_node) ((DbTableCATreeNode*)erts_atomic_read_acqb(&(ca_tree_route_node->u.route.left)))
#define GET_RIGHT_ACQB(ca_tree_route_node) ((DbTableCATreeNode*)erts_atomic_read_acqb(&(ca_tree_route_node->u.route.right)))
#define SET_ROOT_RELB(tb, v) erts_atomic_set_relb(&((tb)->root), (erts_aint_t)(v))
#define SET_LEFT_RELB(ca_tree_route_node, v) erts_atomic_set_relb(&(ca_tree_route_node->u.route.left), (erts_aint_t)(v));
#define SET_RIGHT_RELB(ca_tree_route_node, v) erts_atomic_set_relb(&(ca_tree_route_node->u.route.right), (erts_aint_t)(v));

/* Compares a key to the key in a route node */
static ERTS_INLINE Sint cmp_key_route(Eterm key,
                                      DbTableCATreeNode *obj)
{
    return CMP(key, GET_ROUTE_NODE_KEY(obj));
}

static ERTS_INLINE void push_node_dyn_array(DbTable *tb,
                                            CATreeNodeStack *stack,
                                            DbTableCATreeNode *node)
{
    int i;
    if (stack->pos == stack->size) {
        DbTableCATreeNode **newArray =
            erts_db_alloc(ERTS_ALC_T_DB_STK, tb,
                          sizeof(DbTableCATreeNode*) * (stack->size*2));
        for (i = 0; i < stack->pos; i++) {
            newArray[i] = stack->array[i];
        }
        if (stack->size > STACK_NEED) {
            /* Dynamically allocated array that needs to be deallocated */
            erts_db_free(ERTS_ALC_T_DB_STK, tb,
                         stack->array,
                         sizeof(DbTableCATreeNode *) * stack->size);
        }
        stack->array = newArray;
        stack->size = stack->size*2;
    }
    PUSH_NODE(stack, node);
}

/*
 * Used by the split_tree function
 */
static ERTS_INLINE
int less_than_two_elements(TreeDbTerm *root)
{
    return root == NULL || (root->left == NULL && root->right == NULL);
}

/*
 * Inserts a TreeDbTerm into a tree. Returns the new root.
 */
static ERTS_INLINE
TreeDbTerm* insert_TreeDbTerm(DbTableCATree *tb,
                              TreeDbTerm *insert_to_root,
                              TreeDbTerm *value_to_insert) {
    /* Non recursive insertion in AVL tree, building our own stack */
    TreeDbTerm **tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 0;
    int state = 0;
    TreeDbTerm * base = insert_to_root;
    TreeDbTerm **this = &base;
    Sint c;
    Eterm key;
    int dir;
    TreeDbTerm *p1, *p2, *p;

    key = GETKEY(tb, value_to_insert->dbterm.tpl);

    dstack[dpos++] = DIR_END;
    for (;;)
	if (!*this) { /* Found our place */
	    state = 1;
	    *this = value_to_insert;
	    (*this)->balance = 0;
	    (*this)->left = (*this)->right = NULL;
	    break;
	} else if ((c = cmp_key(&tb->common, key, *this)) < 0) {
	    /* go lefts */
	    dstack[dpos++] = DIR_LEFT;
	    tstack[tpos++] = this;
	    this = &((*this)->left);
	} else { /* go right */
	    dstack[dpos++] = DIR_RIGHT;
	    tstack[tpos++] = this;
	    this = &((*this)->right);
	}

    while (state && ( dir = dstack[--dpos] ) != DIR_END) {
	this = tstack[--tpos];
	p = *this;
	if (dir == DIR_LEFT) {
	    switch (p->balance) {
	    case 1:
		p->balance = 0;
		state = 0;
		break;
	    case 0:
		p->balance = -1;
		break;
	    case -1: /* The icky case */
		p1 = p->left;
		if (p1->balance == -1) { /* Single LL rotation */
		    p->left = p1->right;
		    p1->right = p;
		    p->balance = 0;
		    (*this) = p1;
		} else { /* Double RR rotation */
		    p2 = p1->right;
		    p1->right = p2->left;
		    p2->left = p1;
		    p->left = p2->right;
		    p2->right = p;
		    p->balance = (p2->balance == -1) ? +1 : 0;
		    p1->balance = (p2->balance == 1) ? -1 : 0;
		    (*this) = p2;
		}
		(*this)->balance = 0;
		state = 0;
		break;
	    }
	} else { /* dir == DIR_RIGHT */
	    switch (p->balance) {
	    case -1:
		p->balance = 0;
		state = 0;
		break;
	    case 0:
		p->balance = 1;
		break;
	    case 1:
		p1 = p->right;
		if (p1->balance == 1) { /* Single RR rotation */
		    p->right = p1->left;
		    p1->left = p;
		    p->balance = 0;
		    (*this) = p1;
		} else { /* Double RL rotation */
		    p2 = p1->left;
		    p1->left = p2->right;
		    p2->right = p1;
		    p->right = p2->left;
		    p2->left = p;
		    p->balance = (p2->balance == 1) ? -1 : 0;
		    p1->balance = (p2->balance == -1) ? 1 : 0;
		    (*this) = p2;
		}
		(*this)->balance = 0;
		state = 0;
		break;
	    }
	}
    }
    return base;
}

/*
 * Split an AVL tree into two trees. The function stores the node
 * containing the "split key" in the write back parameter
 * split_key_wb. The function stores the left tree containing the keys
 * that are smaller than the "split key" in the write back parameter
 * left_wb and the tree containing the rest of the keys in the write
 * back parameter right_wb.
 */
static void split_tree(DbTableCATree *tb,
                       TreeDbTerm *root,
                       TreeDbTerm **split_key_node_wb,
                       TreeDbTerm **left_wb,
                       TreeDbTerm **right_wb) {
    TreeDbTerm * split_node = NULL;
    TreeDbTerm * left_root;
    TreeDbTerm * right_root;
    if (root->left == NULL) { /* To get non empty split */
        *right_wb = root->right;
        *split_key_node_wb = root->right;
        root->right = NULL;
        root->balance = 0;
        *left_wb = root;
        return;
    }
    split_node = root;
    left_root = split_node->left;
    split_node->left = NULL;
    right_root = split_node->right;
    split_node->right = NULL;
    right_root = insert_TreeDbTerm(tb, right_root, split_node);
    *split_key_node_wb = split_node;
    *left_wb = left_root;
    *right_wb = right_root;
}

/*
 * Used by the join_trees function
 */
static ERTS_INLINE int compute_tree_hight(TreeDbTerm * root)
{
    if(root == NULL) {
        return 0;
    } else {
        TreeDbTerm * current_node = root;
        int hight_so_far = 1;
        while (current_node->left != NULL || current_node->right != NULL) {
            if (current_node->balance == -1) {
                current_node = current_node->left;
            } else {
                current_node = current_node->right;
            }
            hight_so_far = hight_so_far + 1;
        }
        return hight_so_far;
    }
}

/*
 * Used by the join_trees function
 */
static ERTS_INLINE
TreeDbTerm* linkout_min_or_max_tree_node(TreeDbTerm **root, int is_min)
{
    TreeDbTerm **tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 0;
    int state = 0;
    TreeDbTerm **this = root;
    int dir;
    TreeDbTerm *q = NULL;

    dstack[dpos++] = DIR_END;
    for (;;) {
        if (!*this) { /* Failure */
            return NULL;
        } else if (is_min && (*this)->left != NULL) {
            dstack[dpos++] = DIR_LEFT;
            tstack[tpos++] = this;
            this = &((*this)->left);
        } else if (!is_min && (*this)->right != NULL) {
            dstack[dpos++] = DIR_RIGHT;
            tstack[tpos++] = this;
            this = &((*this)->right);
        } else { /* Min value, found the one to splice out */
            q = (*this);
            if (q->right == NULL) {
                (*this) = q->left;
                state = 1;
            } else if (q->left == NULL) {
                (*this) = q->right;
                state = 1;
            }
            break;
        }
    }
    while (state && ( dir = dstack[--dpos] ) != DIR_END) {
        this = tstack[--tpos];
        if (dir == DIR_LEFT) {
            state = tree_balance_left(this);
        } else {
            state = tree_balance_right(this);
        }
    }
    return q;
}

#define LINKOUT_MIN_TREE_NODE(root) linkout_min_or_max_tree_node(root, 1)
#define LINKOUT_MAX_TREE_NODE(root) linkout_min_or_max_tree_node(root, 0)

/*
 * Joins two AVL trees where all the keys in the left one are smaller
 * then the keys in the right one and returns the resulting tree.
 *
 * The algorithm is described on page 474 in D. E. Knuth. The Art of
 * Computer Programming: Sorting and Searching,
 * vol. 3. Addison-Wesley, 2nd edition, 1998.
 */
static TreeDbTerm* join_trees(TreeDbTerm *left_root_param,
                               TreeDbTerm *right_root_param)
{
    TreeDbTerm **tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 0;
    int state = 1;
    TreeDbTerm **this;
    int dir;
    TreeDbTerm *p1, *p2, *p;
    TreeDbTerm *left_root = left_root_param;
    TreeDbTerm *right_root = right_root_param;
    int left_height;
    int right_height;
    int current_height;
    dstack[dpos++] = DIR_END;
    if (left_root == NULL) {
        return right_root;
    } else if (right_root == NULL) {
        return left_root;
    }

    left_height = compute_tree_hight(left_root);
    right_height = compute_tree_hight(right_root);
    if (left_height >= right_height) {
        TreeDbTerm * new_root =
            LINKOUT_MIN_TREE_NODE(&right_root);
        int new_right_height = compute_tree_hight(right_root);
        TreeDbTerm * current_node = left_root;
        this = &left_root;
        current_height = left_height;
        while(current_height > new_right_height + 1) {
            if (current_node->balance == -1) {
                current_height = current_height - 2;
            } else {
                current_height = current_height - 1;
            }
            dstack[dpos++] = DIR_RIGHT;
            tstack[tpos++] = this;
            this = &((*this)->right);
            current_node = current_node->right;
        }
        new_root->left = current_node;
        new_root->right = right_root;
        new_root->balance = new_right_height - current_height;
        *this = new_root;
    } else {
        /* This case is symmetric to the previous case */
        TreeDbTerm * new_root =
            LINKOUT_MAX_TREE_NODE(&left_root);
        int new_left_height = compute_tree_hight(left_root);
        TreeDbTerm * current_node = right_root;
        this = &right_root;
        current_height = right_height;
        while (current_height > new_left_height + 1) {
            if (current_node->balance == 1) {
                current_height = current_height - 2;
            } else {
                current_height = current_height - 1;
            }
            dstack[dpos++] = DIR_LEFT;
            tstack[tpos++] = this;
            this = &((*this)->left);
            current_node = current_node->left;
        }
        new_root->right = current_node;
        new_root->left = left_root;
        new_root->balance = current_height - new_left_height;
        *this = new_root;
    }
    /* Now we need to continue as if this was during the insert */
    while (state && ( dir = dstack[--dpos] ) != DIR_END) {
        this = tstack[--tpos];
        p = *this;
        if (dir == DIR_LEFT) {
            switch (p->balance) {
                case 1:
                p->balance = 0;
                state = 0;
                break;
                case 0:
                p->balance = -1;
                break;
                case -1: /* The icky case */
                p1 = p->left;
                if (p1->balance == -1) { /* Single LL rotation */
                    p->left = p1->right;
                    p1->right = p;
                    p->balance = 0;
                    (*this) = p1;
                } else { /* Double RR rotation */
                    p2 = p1->right;
                    p1->right = p2->left;
                    p2->left = p1;
                    p->left = p2->right;
                    p2->right = p;
                    p->balance = (p2->balance == -1) ? +1 : 0;
                    p1->balance = (p2->balance == 1) ? -1 : 0;
                    (*this) = p2;
                }
                (*this)->balance = 0;
                state = 0;
                break;
            }
        } else { /* dir == DIR_RIGHT */
            switch (p->balance) {
                case -1:
                p->balance = 0;
                state = 0;
                break;
                case 0:
                p->balance = 1;
                break;
                case 1:
                p1 = p->right;
                if (p1->balance == 1) { /* Single RR rotation */
                    p->right = p1->left;
                    p1->left = p;
                    p->balance = 0;
                    (*this) = p1;
                } else { /* Double RL rotation */
                    p2 = p1->left;
                    p1->left = p2->right;
                    p2->right = p1;
                    p->right = p2->left;
                    p2->left = p;
                    p->balance = (p2->balance == 1) ? -1 : 0;
                    p1->balance = (p2->balance == -1) ? 1 : 0;
                    (*this) = p2;
                }
                (*this)->balance = 0;
                state = 0;
                break;
            }
        }
    }
    /* Return the joined tree */
    if (left_height >= right_height) {
        return left_root;
    } else {
        return right_root;
    }
}


static ERTS_INLINE
int try_wlock_base_node(DbTableCATreeBaseNode *base_node)
{
    return EBUSY == erts_rwmtx_tryrwlock(&base_node->lock);
}

/*
 * Locks a base node without adjusting the lock statistics
 */
static ERTS_INLINE
void wlock_base_node_no_stats(DbTableCATreeBaseNode *base_node)
{
    erts_rwmtx_rwlock(&base_node->lock);
}

/*
 * Locks a base node and adjusts the lock statistics according to if
 * the lock was contended or not
 */
static ERTS_INLINE
void wlock_base_node(DbTableCATreeBaseNode *base_node)
{
    if (try_wlock_base_node(base_node)) {
        /* The lock is contended */
        wlock_base_node_no_stats(base_node);
        base_node->lock_statistics += ERL_DB_CATREE_LOCK_FAILURE_CONTRIBUTION;
    } else {
        base_node->lock_statistics += ERL_DB_CATREE_LOCK_SUCCESS_CONTRIBUTION;
    }
}

static ERTS_INLINE
void wunlock_base_node(DbTableCATreeBaseNode *base_node)
{
    erts_rwmtx_rwunlock(&base_node->lock);
}

static ERTS_INLINE
void rlock_base_node(DbTableCATreeBaseNode *base_node)
{
    erts_rwmtx_rlock(&base_node->lock);
}

static ERTS_INLINE
void runlock_base_node(DbTableCATreeBaseNode *base_node)
{
    erts_rwmtx_runlock(&base_node->lock);
}

static ERTS_INLINE
void lock_route_node(DbTableCATreeNode *route_node)
{
    ASSERT(!route_node->is_base_node);
    erts_mtx_lock(&route_node->u.route.lock);
}

static ERTS_INLINE
void unlock_route_node(DbTableCATreeNode *route_node)
{
    ASSERT(!route_node->is_base_node);
    erts_mtx_unlock(&route_node->u.route.lock);
}

static ERTS_INLINE
void init_root_iterator(DbTableCATree* tb, CATreeRootIterator* iter,
                        int read_only)
{
    iter->tb = tb;
    iter->read_only = read_only;
    iter->locked_bnode = NULL;
    iter->next_route_key = THE_NON_VALUE;
}

static ERTS_INLINE
void lock_iter_base_node(CATreeRootIterator* iter,
                         DbTableCATreeBaseNode *base_node)
{
    ASSERT(!iter->locked_bnode);
    if (iter->read_only)
        rlock_base_node(base_node);
    else
        wlock_base_node(base_node);
    iter->locked_bnode = base_node;
}

static ERTS_INLINE
void unlock_iter_base_node(CATreeRootIterator* iter)
{
    ASSERT(iter->locked_bnode);
    if (iter->read_only)
        runlock_base_node(iter->locked_bnode);
    else
        wunlock_base_node(iter->locked_bnode);
    iter->locked_bnode = NULL;
}



/*
 * The following macros are used to create the ETS functions that only
 * need to access one element (e.g. db_put_catree, db_get_catree and
 * db_erase_catree).
 */


typedef struct
{
    DbTableCATreeNode *parent;
    int current_level;
} FindBaseNode;

static ERTS_INLINE
DbTableCATreeBaseNode* find_base_node(DbTableCATree* tb, Eterm key,
                                      FindBaseNode* fbn)
{
    DbTableCATreeNode* ERTS_RESTRICT node = GET_ROOT_ACQB(tb);
    fbn->parent = NULL;
    fbn->current_level = 0;
    while (!node->is_base_node) {
        fbn->current_level++;
        fbn->parent = node;
        if (cmp_key_route(key, node) < 0) {
            node = GET_LEFT_ACQB(node);
        } else {
            node = GET_RIGHT_ACQB(node);
        }
    }
    return &node->u.base;
}

#define ERL_DB_CATREE_CREATE_DO_OPERATION_FUNCTION_FIND_BASE_NODE_PART(LOCK,UNLOCK) \
        int retry;                                                      \
        DbTableCATreeNode *current_node;                                \
        DbTableCATreeNode *prev_node;                                   \
        DbTableCATreeBaseNode *base_node;                               \
        int current_level;                                              \
        (void)prev_node;                                                \
        do {                                                            \
            retry = 0;                                                  \
            current_node = GET_ROOT_ACQB(tb);                           \
            prev_node = NULL;                                           \
            current_level = 0;                                          \
            while ( ! current_node->is_base_node ) {                    \
                current_level = current_level + 1;                      \
                prev_node = current_node;                               \
                if (cmp_key_route(key,current_node) < 0) { \
                    current_node = GET_LEFT_ACQB(current_node);         \
                } else {                                                \
                    current_node = GET_RIGHT_ACQB(current_node);        \
                }                                                       \
            }                                                           \
            base_node = &current_node->u.base;                \
            LOCK(base_node);                                            \
            if ( ! base_node->is_valid ) {                              \
                /* Retry */                                             \
                UNLOCK(base_node);                                      \
                retry = 1;                                              \
            }                                                           \
        } while(retry);                                                 


#define ERL_DB_CATREE_CREATE_DO_OPERATION_FUNCTION_ADAPT_AND_UNLOCK_PART \
    if (base_node->lock_statistics > ERL_DB_CATREE_HIGH_CONTENTION_LIMIT \
        && current_level < ERL_DB_CATREE_MAX_ROUTE_NODE_LAYER_HEIGHT) { \
        split_catree(tb, prev_node, current_node);             \
    } else if (base_node->lock_statistics < ERL_DB_CATREE_LOW_CONTENTION_LIMIT) { \
        join_catree(tb, prev_node, current_node);                       \
    } else {                                                            \
        wunlock_base_node(base_node);                                    \
    }

#define ERL_DB_CATREE_CREATE_DO_OPERATION_FUNCTION(FUN_POSTFIX,PARAMETERS,SEQUENTAIL_CALL) \
    static int erl_db_catree_do_operation_##FUN_POSTFIX(DbTableCATree *tb, \
                                                        Eterm key,      \
                                                        PARAMETERS){    \
        int result;                                                     \
        ERL_DB_CATREE_CREATE_DO_OPERATION_FUNCTION_FIND_BASE_NODE_PART(wlock_base_node,wunlock_base_node)  \
        result = SEQUENTAIL_CALL;                                   \
        ERL_DB_CATREE_CREATE_DO_OPERATION_FUNCTION_ADAPT_AND_UNLOCK_PART \
        return result;                                                  \
    }


#define ERL_DB_CATREE_CREATE_DO_READ_OPERATION_FUNCTION(FUN_POSTFIX,PARAMETERS,SEQUENTAIL_CALL) \
    static int erl_db_catree_do_operation_##FUN_POSTFIX(DbTableCATree *tb, \
                                                        Eterm key,      \
                                                        PARAMETERS){    \
        int result;                                                     \
        ERL_DB_CATREE_CREATE_DO_OPERATION_FUNCTION_FIND_BASE_NODE_PART(rlock_base_node,runlock_base_node) \
        result = SEQUENTAIL_CALL;                                       \
        runlock_base_node(base_node);                                   \
        return result;                                                  \
    }


static ERTS_INLINE
void copy_route_key(DbRouteKey* dst, Eterm key, Uint key_size)
{
    dst->size = key_size;
    if (key_size != 0) {
        Eterm* hp = &dst->heap[0];
        ErlOffHeap tmp_offheap;
        tmp_offheap.first  = NULL;
        dst->term = copy_struct(key, key_size, &hp, &tmp_offheap);
        dst->oh = tmp_offheap.first;
    }
    else {
        ASSERT(is_immed(key));
        dst->term = key;
        dst->oh = NULL;
    }
}

static ERTS_INLINE
void destroy_route_key(DbRouteKey* key)
{
    if (key->oh) {
        ErlOffHeap oh;
        oh.first = key->oh;
        erts_cleanup_offheap(&oh);
    }
}


#ifdef ERTS_ENABLE_LOCK_CHECK
#  define sizeof_base_node(KEY_SZ) \
          (offsetof(DbTableCATreeNode, u.base.lc_key.heap) \
           + (KEY_SZ)*sizeof(Eterm))
#  define LC_ORDER(ORDER) ORDER
#else
#  define sizeof_base_node(KEY_SZ) \
          offsetof(DbTableCATreeNode, u.base.end_of_struct__)
#  define LC_ORDER(ORDER) NIL
#endif

static DbTableCATreeNode *create_base_node(DbTableCATree *tb,
                                           TreeDbTerm* root,
                                           Eterm lc_key)
{
    DbTableCATreeNode *p;
    erts_rwmtx_opt_t rwmtx_opt = ERTS_RWMTX_OPT_DEFAULT_INITER;
#ifdef ERTS_ENABLE_LOCK_CHECK
    Eterm lc_key_size = size_object(lc_key);
#endif
    p = erts_db_alloc(ERTS_ALC_T_DB_TABLE, (DbTable *) tb,
                      sizeof_base_node(lc_key_size));

    p->is_base_node = 1;
    p->u.base.root = root;
    if (tb->common.type & DB_FREQ_READ)
        rwmtx_opt.type = ERTS_RWMTX_TYPE_FREQUENT_READ;
    if (erts_ets_rwmtx_spin_count >= 0)
        rwmtx_opt.main_spincount = erts_ets_rwmtx_spin_count;

#ifdef ERTS_ENABLE_LOCK_CHECK
    copy_route_key(&p->u.base.lc_key, lc_key, lc_key_size);
#endif
    erts_rwmtx_init_opt(&p->u.base.lock, &rwmtx_opt,
                        "erl_db_catree_base_node",
                        lc_key,
                        ERTS_LOCK_FLAGS_CATEGORY_DB);
    p->u.base.lock_statistics = 0;
    p->u.base.is_valid = 1;
    return p;
}

static ERTS_INLINE
DbTableCATreeNode *create_wlocked_base_node(DbTableCATree *tb,
                                            TreeDbTerm* root,
                                            Eterm lc_key)
{
    DbTableCATreeNode* p = create_base_node(tb, root, lc_key);
    ethr_rwmutex_rwlock(&p->u.base.lock.rwmtx);
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_trylock_flg(-1, &p->u.base.lock.lc, ERTS_LOCK_OPTIONS_RDWR);
#endif
    return p;
}


static ERTS_INLINE Uint sizeof_route_node(Uint key_size)
{
    return (offsetof(DbTableCATreeNode, u.route.key.heap)
            + key_size*sizeof(Eterm));
}

static DbTableCATreeNode*
create_route_node(DbTableCATree *tb,
                  DbTableCATreeNode *left,
                  DbTableCATreeNode *right,
                  DbTerm * keyTerm,
                  DbTableCATreeNode* lc_parent)
{
    Eterm key = GETKEY(tb,keyTerm->tpl);
    int key_size = size_object(key);
    DbTableCATreeNode* p = erts_db_alloc(ERTS_ALC_T_DB_TABLE,
                                         (DbTable *) tb,
                                         sizeof_route_node(key_size));

    copy_route_key(&p->u.route.key, key, key_size);
    p->is_base_node = 0;
    p->u.route.is_valid = 1;
    erts_atomic_init_nob(&p->u.route.left, (erts_aint_t)left);
    erts_atomic_init_nob(&p->u.route.right, (erts_aint_t)right);
#ifdef ERTS_ENABLE_LOCK_CHECK
    /* Route node lock order is inverse tree depth (from leafs toward root) */
    p->u.route.lc_order = (lc_parent == NULL ? MAX_SMALL :
                           lc_parent->u.route.lc_order - 1);
    /*
     * This assert may eventually fail as we don't increase 'lc_order' in join
     * operations when route nodes move up in the tree.
     * Tough luck if you run a lock-checking VM for such a long time on 32-bit.
     */
    ERTS_LC_ASSERT(p->u.route.lc_order >= 0);
#endif
    erts_mtx_init(&p->u.route.lock, "erl_db_catree_route_node",
                  LC_ORDER(make_small(p->u.route.lc_order)),
                  ERTS_LOCK_FLAGS_CATEGORY_DB);
    return p;
}

static void do_free_base_node(void* vptr)
{
    DbTableCATreeNode *p = (DbTableCATreeNode *)vptr;
    ASSERT(p->is_base_node);
    erts_rwmtx_destroy(&p->u.base.lock);
#ifdef ERTS_ENABLE_LOCK_CHECK
    destroy_route_key(&p->u.base.lc_key);
#endif
    erts_free(ERTS_ALC_T_DB_TABLE, p);
}

static void free_catree_base_node(DbTableCATree* tb, DbTableCATreeNode* p)
{
    ASSERT(p->is_base_node);
    ERTS_DB_ALC_MEM_UPDATE_(tb, sizeof_base_node(p->u.base.lc_key.size), 0);
    do_free_base_node(p);
}

static void do_free_route_node(void *vptr)
{
    DbTableCATreeNode *p = (DbTableCATreeNode *)vptr;
    ASSERT(!p->is_base_node);
    erts_mtx_destroy(&p->u.route.lock);
    destroy_route_key(&p->u.route.key);
    erts_free(ERTS_ALC_T_DB_TABLE, p);
}

static void free_catree_route_node(DbTableCATree* tb, DbTableCATreeNode* p)
{
    ASSERT(!p->is_base_node);
    ERTS_DB_ALC_MEM_UPDATE_(tb, sizeof_route_node(p->u.route.key.size), 0);
    do_free_route_node(p);
}


/*
 * Returns the parent routing node of the specified
 * route node 'child' if such a parent exists
 * or NULL if 'child' is attached to the root.
 */
static ERTS_INLINE DbTableCATreeNode *
parent_of(DbTableCATree *tb,
          DbTableCATreeNode *child)
{
    Eterm key = GET_ROUTE_NODE_KEY(child);
    DbTableCATreeNode *current = GET_ROOT_ACQB(tb);
    DbTableCATreeNode *prev = NULL;

    while (current != child) {
        prev = current;
        if (cmp_key_route(key, current) < 0) {
            current = GET_LEFT_ACQB(current);
        } else {
            current = GET_RIGHT_ACQB(current);
        }
    }
    return prev;
}


static ERTS_INLINE DbTableCATreeNode *
leftmost_base_node(DbTableCATreeNode *root)
{
    DbTableCATreeNode *node = root;
    while (!node->is_base_node) {
        node = GET_LEFT_ACQB(node);
    }
    return node;
}


static ERTS_INLINE DbTableCATreeNode *
rightmost_base_node(DbTableCATreeNode *root)
{
    DbTableCATreeNode *node = root;
    while (!node->is_base_node) {
        node = GET_RIGHT_ACQB(node);
    }
    return node;
}


static ERTS_INLINE DbTableCATreeNode *
leftmost_route_node(DbTableCATreeNode *root)
{
    DbTableCATreeNode *node = root;
    DbTableCATreeNode *prev_node = NULL;
    while (!node->is_base_node) {
        prev_node = node;
        node = GET_LEFT_ACQB(node);
    }
    return prev_node;
}

static ERTS_INLINE DbTableCATreeNode*
rightmost_route_node(DbTableCATreeNode *root)
{
    DbTableCATreeNode * node = root;
    DbTableCATreeNode * prev_node = NULL;
    while (!node->is_base_node) {
        prev_node = node;
        node = GET_RIGHT_ACQB(node);
    }
    return prev_node;
}

static ERTS_INLINE DbTableCATreeNode*
leftmost_base_node_and_path(DbTableCATreeNode *root, CATreeNodeStack * stack)
{
    DbTableCATreeNode * node = root;
    while (!node->is_base_node) {
        PUSH_NODE(stack, node);
        node = GET_LEFT_ACQB(node);
    }
    return node;
}

static ERTS_INLINE DbTableCATreeNode*
get_next_base_node_and_path(DbTableCATreeNode *base_node,
                            CATreeNodeStack *stack)
{
    if (EMPTY_NODE(stack)) { /* The parent of b is the root */
        return NULL;
    } else {
        if (GET_LEFT(TOP_NODE(stack)) == base_node) {
            return leftmost_base_node_and_path(
                        GET_RIGHT_ACQB(TOP_NODE(stack)),
                        stack);
        } else {
            Eterm pkey =
                TOP_NODE(stack)->u.route.key.term; /* pKey = key of parent */
            POP_NODE(stack);
            while (!EMPTY_NODE(stack)) {
                if (TOP_NODE(stack)->u.route.is_valid &&
                   cmp_key_route(pkey, TOP_NODE(stack)) <= 0) {
                    return leftmost_base_node_and_path(GET_RIGHT_ACQB(TOP_NODE(stack)), stack);
                } else {
                  POP_NODE(stack);
                }
            }
        }
        return NULL;
    }
}

static ERTS_INLINE void
clone_stack(CATreeNodeStack *search_stack_ptr,
            CATreeNodeStack *search_stack_copy_ptr)
{
    int i;
    search_stack_copy_ptr->pos = search_stack_ptr->pos;
    for (i = 0; i < search_stack_ptr->pos; i++) {
        search_stack_copy_ptr->array[i] = search_stack_ptr->array[i];
    }
}



static ERTS_INLINE DbTableCATreeNode*
lock_first_base_node(DbTable *tbl,
                     CATreeNodeStack *search_stack_ptr,
                     CATreeNodeStack *locked_base_nodes_stack_ptr)
{
    DbTableCATreeNode *current_node;
    DbTableCATreeBaseNode *base_node;
    DbTableCATree* tb = &tbl->catree;
    while (1) {
        current_node = GET_ROOT_ACQB(tb);
        while ( ! current_node->is_base_node ) {
            PUSH_NODE(search_stack_ptr, current_node);
            current_node = GET_LEFT_ACQB(current_node);
        }
        base_node = &current_node->u.base;
        rlock_base_node(base_node);
        if (base_node->is_valid)
            break;
        /* Retry */
        runlock_base_node(base_node);
        search_stack_ptr->pos = 0;
    }
    push_node_dyn_array(tbl, locked_base_nodes_stack_ptr, current_node);
    return current_node;
}

static ERTS_INLINE DbTableCATreeBaseNode*
find_and_lock_next_base_node_and_path(DbTable *tbl,
                                      CATreeNodeStack **search_stack_ptr_ptr,
                                      CATreeNodeStack **search_stack_copy_ptr_ptr,
                                      CATreeNodeStack *locked_base_nodes_stack_ptr)
{
    DbTableCATreeNode *current_node;
    DbTableCATreeBaseNode *base_node;
    CATreeNodeStack * tmp_stack_ptr;

    while (1) {
        current_node = TOP_NODE(locked_base_nodes_stack_ptr);
        clone_stack(*search_stack_ptr_ptr, *search_stack_copy_ptr_ptr);
        current_node =
            get_next_base_node_and_path(current_node, *search_stack_ptr_ptr);
        if (current_node == NULL) {
            return NULL;
        }
        base_node = &current_node->u.base;
        rlock_base_node(base_node);
        if (base_node->is_valid)
            break;

        /* Retry */
        runlock_base_node(base_node);
        /* Revert to previous state */
        current_node = TOP_NODE(locked_base_nodes_stack_ptr);
        tmp_stack_ptr = *search_stack_ptr_ptr;
        *search_stack_ptr_ptr = *search_stack_copy_ptr_ptr;
        *search_stack_copy_ptr_ptr = tmp_stack_ptr;
    }

    push_node_dyn_array(tbl, locked_base_nodes_stack_ptr, current_node);
    return base_node;
}

static ERTS_INLINE
void unlock_and_release_locked_base_node_stack(DbTable *tbl,
                                               CATreeNodeStack *locked_base_nodes_stack_ptr)
{
    DbTableCATreeNode *current_node;
    DbTableCATreeBaseNode *base_node;
    int i;
    for (i = 0; i < locked_base_nodes_stack_ptr->pos; i++) {
        current_node = locked_base_nodes_stack_ptr->array[i];
        base_node = &current_node->u.base;
        if (locked_base_nodes_stack_ptr->pos > 1) {
            base_node->lock_statistics =     /* This is not atomic which is fine as */
                base_node->lock_statistics + /* correctness does not depend on that. */
                ERL_DB_CATREE_LOCK_MORE_THAN_ONE_CONTRIBUTION;
        }
        runlock_base_node(base_node);
    }
    if (locked_base_nodes_stack_ptr->size > STACK_NEED) {
        erts_db_free(ERTS_ALC_T_DB_STK, tbl,
                     locked_base_nodes_stack_ptr->array,
                     sizeof(DbTableCATreeNode *) * locked_base_nodes_stack_ptr->size);
    }
}

static ERTS_INLINE
void init_stack(CATreeNodeStack *stack,
                DbTableCATreeNode **stack_array,
                Uint init_size)
{
    stack->array = stack_array;
    stack->pos = 0;
    stack->size = init_size;
}

static ERTS_INLINE
void init_tree_stack(DbTreeStack *stack,
                     TreeDbTerm **stack_array,
                     Uint init_slot)
{
    stack->array = stack_array;
    stack->pos = 0;
    stack->slot = init_slot;
}

#define DEC_ROUTE_NODE_STACK_AND_ARRAY(STACK_NAME) \
    CATreeNodeStack STACK_NAME; \
    CATreeNodeStack * STACK_NAME##_ptr = &(STACK_NAME); \
    DbTableCATreeNode *STACK_NAME##_array[STACK_NEED];

#define DECLARE_AND_INIT_BASE_NODE_SEARCH_STACKS \
DEC_ROUTE_NODE_STACK_AND_ARRAY(search_stack) \
DEC_ROUTE_NODE_STACK_AND_ARRAY(search_stack_copy) \
DEC_ROUTE_NODE_STACK_AND_ARRAY(locked_base_nodes_stack) \
init_stack(&search_stack, search_stack_array, 0); \
init_stack(&search_stack_copy, search_stack_copy_array, 0); \
init_stack(&locked_base_nodes_stack, locked_base_nodes_stack_array, STACK_NEED);/* Abuse as stack array size*/

/*
 * Locks and returns the base node that contains the specified key if
 * it is present. The function saves the search path to the found base
 * node in search_stack_ptr and adds the found base node to
 * locked_base_nodes_stack_ptr.
 */
static ERTS_INLINE DbTableCATreeBaseNode *
lock_base_node_with_key(DbTable *tbl,
                        Eterm key,
                        CATreeNodeStack * search_stack_ptr,
                        CATreeNodeStack * locked_base_nodes_stack_ptr)
{
    int retry;
    DbTableCATreeNode *current_node;
    DbTableCATreeBaseNode *base_node;
    DbTableCATree* tb = &tbl->catree;
    do {
        retry = 0;
        current_node = GET_ROOT_ACQB(tb);
        while ( ! current_node->is_base_node ) {
            PUSH_NODE(search_stack_ptr, current_node);
            if( cmp_key_route(key,current_node) < 0 ) {
                current_node = GET_LEFT_ACQB(current_node);
            } else {
                current_node = GET_RIGHT_ACQB(current_node);
            }
        }
        base_node = &current_node->u.base;
        rlock_base_node(base_node);
        if ( ! base_node->is_valid ) {
            /* Retry */
            runlock_base_node(base_node);
            retry = 1;
        }
    } while(retry);
    push_node_dyn_array(tbl, locked_base_nodes_stack_ptr, current_node);
    return base_node;
}

/*
 * Joins a base node with it's right neighbor. Returns the base node
 * resulting from the join in locked state or NULL if there is no base
 * node to join with.
 */
static DbTableCATreeNode*
erl_db_catree_force_join_right(DbTableCATree *tb,
                               DbTableCATreeNode *parent,
                               DbTableCATreeNode *thiz,
                               DbTableCATreeNode **result_parent_wb)
{
    DbTableCATreeNode *gparent;
    DbTableCATreeNode *neighbor;
    DbTableCATreeNode *new_neighbor;
    DbTableCATreeNode *neighbor_parent;
    TreeDbTerm* new_root;

    if (parent == NULL) {
        return NULL;
    }
    ASSERT(thiz == GET_LEFT(parent));
    while (1) {
        neighbor = leftmost_base_node(GET_RIGHT_ACQB(parent));
        wlock_base_node_no_stats(&neighbor->u.base);
        if (neighbor->u.base.is_valid)
            break;
        wunlock_base_node(&neighbor->u.base);
    }
    lock_route_node(parent);
    parent->u.route.is_valid = 0;
    neighbor->u.base.is_valid = 0;
    thiz->u.base.is_valid = 0;
    while (1) {
        gparent = parent_of(tb, parent);
        if (gparent == NULL)
            break;
        lock_route_node(gparent);
        if (gparent->u.route.is_valid)
            break;
        unlock_route_node(gparent);
    }
    if (gparent == NULL) {
        SET_ROOT_RELB(tb, GET_RIGHT(parent));
    } else if (GET_LEFT(gparent) == parent) {
        SET_LEFT_RELB(gparent, GET_RIGHT(parent));
    } else {
        SET_RIGHT_RELB(gparent, GET_RIGHT(parent));
    }
    unlock_route_node(parent);
    if (gparent != NULL) {
        unlock_route_node(gparent);
    }

    new_root = join_trees(thiz->u.base.root, neighbor->u.base.root);
    new_neighbor = create_wlocked_base_node(tb, new_root,
                                            LC_ORDER(thiz->u.base.lc_key.term));

    if (GET_RIGHT(parent) == neighbor) {
        neighbor_parent = gparent;
    } else {
        neighbor_parent = leftmost_route_node(GET_RIGHT(parent));
    }
    if(neighbor_parent == NULL) {
        SET_ROOT_RELB(tb, new_neighbor);
    } else if (GET_LEFT(neighbor_parent) == neighbor) {
        SET_LEFT_RELB(neighbor_parent, new_neighbor);
    } else {
        SET_RIGHT_RELB(neighbor_parent, new_neighbor);
    }
    wunlock_base_node(&thiz->u.base);
    wunlock_base_node(&neighbor->u.base);
    /* Free the parent and base */
    erts_schedule_db_free(&tb->common,
                          do_free_route_node,
                          parent,
                          &parent->u.route.free_item,
                          sizeof_route_node(parent->u.route.key.size));
    erts_schedule_db_free(&tb->common,
                          do_free_base_node,
                          thiz,
                          &thiz->u.base.free_item,
                          sizeof_base_node(thiz->u.base.lc_key.size));
    erts_schedule_db_free(&tb->common,
                          do_free_base_node,
                          neighbor,
                          &neighbor->u.base.free_item,
                          sizeof_base_node(neighbor->u.base.lc_key.size));

    *result_parent_wb = neighbor_parent;
    return new_neighbor;
}

/*
 * Used to merge together all base nodes for operations such as last
 * and select_*. Returns the base node resulting from the merge in
 * locked state.
 */
static DbTableCATreeNode *
merge_to_one_locked_base_node(DbTableCATree* tb)
{
    DbTableCATreeNode *parent;
    DbTableCATreeNode *new_parent;
    DbTableCATreeNode *base;
    DbTableCATreeNode *new_base;
    int is_not_valid;
    /* Find first base node */
    do {
        parent = NULL;
        base = GET_ROOT_ACQB(tb);
        while ( ! base->is_base_node ) {
            parent = base;
            base = GET_LEFT_ACQB(base);
        }
        wlock_base_node_no_stats(&(base->u.base));
        is_not_valid = ! base->u.base.is_valid;
        if (is_not_valid) {
            wunlock_base_node(&(base->u.base));
        }
    } while(is_not_valid);
    do {
        new_base = erl_db_catree_force_join_right(tb,
                                                  parent,
                                                  base,
                                                  &new_parent);
        if (new_base != NULL) {
            base = new_base;
            parent = new_parent;
        }
    } while(new_base != NULL);
    return base;
}


static void join_catree(DbTableCATree *tb,
                        DbTableCATreeNode *parent,
                        DbTableCATreeNode *thiz)
{
    DbTableCATreeNode *gparent;
    DbTableCATreeNode *neighbor;
    DbTableCATreeNode *new_neighbor;
    DbTableCATreeNode *neighbor_parent;

    ASSERT(thiz->is_base_node);
    if (parent == NULL) {
        thiz->u.base.lock_statistics = 0;
        wunlock_base_node(&thiz->u.base);
        return;
    }
    ASSERT(!parent->is_base_node);
    if (GET_LEFT(parent) == thiz) {
        neighbor = leftmost_base_node(GET_RIGHT_ACQB(parent));
        if (try_wlock_base_node(&neighbor->u.base)) {
            /* Failed to acquire lock */
            thiz->u.base.lock_statistics = 0;
            wunlock_base_node(&thiz->u.base);
            return;
        } else if (!neighbor->u.base.is_valid) {
            thiz->u.base.lock_statistics = 0;
            wunlock_base_node(&thiz->u.base);
            wunlock_base_node(&neighbor->u.base);
            return;
        } else {
            lock_route_node(parent);
            parent->u.route.is_valid = 0;
            neighbor->u.base.is_valid = 0;
            thiz->u.base.is_valid = 0;
            gparent = NULL;
            do {
                if (gparent != NULL) {
                    unlock_route_node(gparent);
                }
                gparent = parent_of(tb, parent);
                if (gparent != NULL)
                    lock_route_node(gparent);
            } while (gparent != NULL && !gparent->u.route.is_valid);

            if (gparent == NULL) {
                SET_ROOT_RELB(tb, GET_RIGHT(parent));
            } else if (GET_LEFT(gparent) == parent) {
                SET_LEFT_RELB(gparent, GET_RIGHT(parent));
            } else {
                SET_RIGHT_RELB(gparent, GET_RIGHT(parent));
            }
            unlock_route_node(parent);
            if (gparent != NULL) {
                unlock_route_node(gparent);
            }
            {
                TreeDbTerm* new_root = join_trees(thiz->u.base.root,
                                                  neighbor->u.base.root);
                new_neighbor = create_base_node(tb, new_root,
                                                LC_ORDER(thiz->u.base.lc_key.term));
            }
            if (GET_RIGHT(parent) == neighbor) {
                neighbor_parent = gparent;
            } else {
                neighbor_parent = leftmost_route_node(GET_RIGHT(parent));
            }
        }
    } else { /* Symetric case */
        ASSERT(GET_RIGHT(parent) == thiz);
        neighbor = rightmost_base_node(GET_LEFT_ACQB(parent));
        if (try_wlock_base_node(&neighbor->u.base)) {
            /* Failed to acquire lock */
            thiz->u.base.lock_statistics = 0;
            wunlock_base_node(&thiz->u.base);
            return;
        } else if (!neighbor->u.base.is_valid) {
            thiz->u.base.lock_statistics = 0;
            wunlock_base_node(&thiz->u.base);
            wunlock_base_node(&neighbor->u.base);
            return;
        } else {
            lock_route_node(parent);
            parent->u.route.is_valid = 0;
            neighbor->u.base.is_valid = 0;
            thiz->u.base.is_valid = 0;
            gparent = NULL;
            do {
                if (gparent != NULL) {
                    unlock_route_node(gparent);
                }
                gparent = parent_of(tb, parent);
                if (gparent != NULL) {
                    lock_route_node(gparent);
                } else {
                    gparent = NULL;
                }
            } while (gparent != NULL && !gparent->u.route.is_valid);
            if (gparent == NULL) {
                SET_ROOT_RELB(tb, GET_LEFT(parent));
            } else if (GET_RIGHT(gparent) == parent) {
                SET_RIGHT_RELB(gparent, GET_LEFT(parent));
            } else {
                SET_LEFT_RELB(gparent, GET_LEFT(parent));
            }
            unlock_route_node(parent);
            if (gparent != NULL) {
                unlock_route_node(gparent);
            }
            {
                TreeDbTerm* new_root = join_trees(neighbor->u.base.root,
                                                  thiz->u.base.root);
                new_neighbor = create_base_node(tb, new_root,
                                                LC_ORDER(thiz->u.base.lc_key.term));
            }
            if (GET_LEFT(parent) == neighbor) {
                neighbor_parent = gparent;
            } else {
                neighbor_parent =
                    rightmost_route_node(GET_LEFT(parent));
            }
        }
    }
    /* Link in new neighbor and free nodes that are no longer in the tree */
    if (neighbor_parent == NULL) {
        SET_ROOT_RELB(tb, new_neighbor);
    } else if (GET_LEFT(neighbor_parent) == neighbor) {
        SET_LEFT_RELB(neighbor_parent, new_neighbor);
    } else {
        SET_RIGHT_RELB(neighbor_parent, new_neighbor);
    }
    wunlock_base_node(&thiz->u.base);
    wunlock_base_node(&neighbor->u.base);
    /* Free the parent and base */
    erts_schedule_db_free(&tb->common,
                          do_free_route_node,
                          parent,
                          &parent->u.route.free_item,
                          sizeof_route_node(parent->u.route.key.size));
    erts_schedule_db_free(&tb->common,
                          do_free_base_node,
                          thiz,
                          &thiz->u.base.free_item,
                          sizeof_base_node(thiz->u.base.lc_key.size));
    erts_schedule_db_free(&tb->common,
                          do_free_base_node,
                          neighbor,
                          &neighbor->u.base.free_item,
                          sizeof_base_node(neighbor->u.base.lc_key.size));
}

static void split_catree(DbTableCATree *tb,
                         DbTableCATreeNode* ERTS_RESTRICT parent,
                         DbTableCATreeNode* ERTS_RESTRICT base) {
    TreeDbTerm *splitOutWriteBack;
    DbTableCATreeNode* ERTS_RESTRICT new_left;
    DbTableCATreeNode* ERTS_RESTRICT new_right;
    DbTableCATreeNode* ERTS_RESTRICT new_route;

    if (less_than_two_elements(base->u.base.root)) {
        base->u.base.lock_statistics = 0;
        wunlock_base_node(&base->u.base);
        return;
    } else {
        TreeDbTerm *left_tree;
        TreeDbTerm *right_tree;

        split_tree(tb, base->u.base.root, &splitOutWriteBack,
                   &left_tree, &right_tree);

        new_left = create_base_node(tb, left_tree,
                                    LC_ORDER(GETKEY(tb, left_tree->dbterm.tpl)));
        new_right = create_base_node(tb, right_tree,
                                     LC_ORDER(GETKEY(tb, right_tree->dbterm.tpl)));
        new_route = create_route_node(tb,
                                      new_left,
                                      new_right,
                                      &splitOutWriteBack->dbterm,
                                      parent);
        if (parent == NULL) {
            SET_ROOT_RELB(tb, new_route);
        } else if(GET_LEFT(parent) == base) {
            SET_LEFT_RELB(parent, new_route);
        } else {
            SET_RIGHT_RELB(parent, new_route);
        }
        base->u.base.is_valid = 0;
        wunlock_base_node(&base->u.base);
        erts_schedule_db_free(&tb->common,
                              do_free_base_node,
                              base,
                              &base->u.base.free_item,
                              sizeof_base_node(base->u.base.lc_key.size));
    }
}

/*
 * Helper functions for removing the table
 */

static void catree_add_base_node_to_free_list(
        DbTableCATree *tb,
        DbTableCATreeNode *base_node_container)
{
    base_node_container->u.base.next =
        tb->base_nodes_to_free_list;
    tb->base_nodes_to_free_list = base_node_container;
}

static void catree_deque_base_node_from_free_list(DbTableCATree *tb)
{
    if (tb->base_nodes_to_free_list == NULL) {
        return; /* List empty */
    } else {
        DbTableCATreeNode *first = tb->base_nodes_to_free_list;
        tb->base_nodes_to_free_list = first->u.base.next;
    }
}

static DbTableCATreeNode *catree_first_base_node_from_free_list(
        DbTableCATree *tb)
{
    return tb->base_nodes_to_free_list;
}

static SWord do_free_routing_nodes_catree_cont(DbTableCATree *tb, SWord num_left)
{
    DbTableCATreeNode *root;
    DbTableCATreeNode *p;
    for (;;) {
        root = POP_NODE(&tb->free_stack_rnodes);
    	if (root == NULL) break;
        else if(root->is_base_node) {
            catree_add_base_node_to_free_list(tb, root);
            break;
        }
    	for (;;) {
            if ((GET_LEFT(root) != NULL) &&
                (p = GET_LEFT(root))->is_base_node) {
                SET_LEFT(root, NULL);
                catree_add_base_node_to_free_list(tb, p);
            } else if ((GET_RIGHT(root) != NULL) &&
                       (p = GET_RIGHT(root))->is_base_node) {
                SET_RIGHT(root, NULL);
                catree_add_base_node_to_free_list(tb, p);
            } else if ((p = GET_LEFT(root)) != NULL) {
                SET_LEFT(root, NULL);
                PUSH_NODE(&tb->free_stack_rnodes, root);
                root = p;
            } else if ((p = GET_RIGHT(root)) != NULL) {
                SET_RIGHT(root, NULL);
                PUSH_NODE(&tb->free_stack_rnodes, root);
                root = p;
            } else {
                free_catree_route_node(tb, root);
                if (--num_left >= 0) {
                    break;
                } else {
                    return num_left;	/* Done enough for now */
                }
            }
        }
    }
    return num_left;
}

static SWord do_free_base_node_cont(DbTableCATree *tb, SWord num_left)
{
    TreeDbTerm *root;
    TreeDbTerm *p;
    DbTableCATreeNode *base_node_container =
        catree_first_base_node_from_free_list(tb);
    for (;;) {
        root = POP_NODE(&tb->free_stack_elems);
        if (root == NULL) break;
        for (;;) {
            if ((p = root->left) != NULL) {
                root->left = NULL;
                PUSH_NODE(&tb->free_stack_elems, root);
                root = p;
            } else if ((p = root->right) != NULL) {
                root->right = NULL;
                PUSH_NODE(&tb->free_stack_elems, root);
                root = p;
            } else {
                free_term((DbTable*)tb, root);
                if (--num_left >= 0) {
                    break;
                } else {
                    return num_left;	/* Done enough for now */
                }
            }
        }
    }
    catree_deque_base_node_from_free_list(tb);
    free_catree_base_node(tb, base_node_container);
    base_node_container = catree_first_base_node_from_free_list(tb);
    if (base_node_container != NULL) {
        PUSH_NODE(&tb->free_stack_elems, base_node_container->u.base.root);
    }
    return num_left;
}


/*
** Initialization function
*/

void db_initialize_catree(void)
{
    return;
};

/*
** Table interface routines (i.e., what's called by the bif's)
*/

int db_create_catree(Process *p, DbTable *tbl)
{
    DbTableCATree *tb = &tbl->catree;
    DbTableCATreeNode *root;

    root = create_base_node(tb, NULL,
                            NIL);/* lc_key of first base node does not matter */
    tb->deletion = 0;
    tb->base_nodes_to_free_list = NULL;
    erts_atomic_init_relb(&(tb->root), (erts_aint_t)root);
    return DB_ERROR_NONE;
}

static int db_first_catree(Process *p, DbTable *tbl, Eterm *ret)
{
    DbTableCATreeBaseNode *base_node;
    int result;
    DECLARE_AND_INIT_BASE_NODE_SEARCH_STACKS;
    /* Find first base node */
    base_node = &(lock_first_base_node(tbl, &search_stack, &locked_base_nodes_stack)->u.base);
    /* Find next base node until non-empty base node is found */
    while (base_node != NULL && base_node->root == NULL) {
        base_node = find_and_lock_next_base_node_and_path(tbl, &search_stack_ptr, &search_stack_copy_ptr, locked_base_nodes_stack_ptr);
    }
    /* Get the return value */
    result = db_first_tree_common(p, tbl, (base_node == NULL ? NULL : base_node->root), ret, NULL);
    /* Unlock base nodes */
    unlock_and_release_locked_base_node_stack(tbl, locked_base_nodes_stack_ptr);
    return result;
}

static int db_next_catree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTreeStack next_search_stack;
    TreeDbTerm *next_search_stack_array[STACK_NEED];
    DbTableCATreeBaseNode *base_node;
    int result = 0;
    DECLARE_AND_INIT_BASE_NODE_SEARCH_STACKS;
    init_tree_stack(&next_search_stack, next_search_stack_array, 0);
    /* Lock base node with key */
    base_node = lock_base_node_with_key(tbl, key, &search_stack, &locked_base_nodes_stack);
    /* Continue until key is not >= greatest key in base_node */
    while (base_node != NULL) {
        result = db_next_tree_common(p, tbl, base_node->root, key,
                                     ret, &next_search_stack);
        if (result != DB_ERROR_NONE || *ret != am_EOT) {
            break;
        }
        base_node =
            find_and_lock_next_base_node_and_path(tbl,
                                                  &search_stack_ptr,
                                                  &search_stack_copy_ptr,
                                                  locked_base_nodes_stack_ptr);
    }
    unlock_and_release_locked_base_node_stack(tbl, &locked_base_nodes_stack);
    return result;
}

static int db_last_catree(Process *p, DbTable *tbl, Eterm *ret)
{
    DbTableCATreeNode *base = merge_to_one_locked_base_node(&tbl->catree);
    int result = db_last_tree_common(p, tbl, base->u.base.root, ret, NULL);
    wunlock_base_node(&(base->u.base));
    return result;
    
}

static int db_prev_catree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTreeStack stack;
    TreeDbTerm * stack_array[STACK_NEED];
    int result;
    DbTableCATreeNode *base;
    init_tree_stack(&stack, stack_array, 0);
    base = merge_to_one_locked_base_node(&tbl->catree);
    result = db_prev_tree_common(p, tbl, base->u.base.root, key, ret, &stack);
    wunlock_base_node(&(base->u.base));
    return result;
}

#define ERL_DB_CATREE_DO_OPERATION_PUT_PARAMS Eterm obj, int key_clash_fail
ERL_DB_CATREE_CREATE_DO_OPERATION_FUNCTION(put,
                                           ERL_DB_CATREE_DO_OPERATION_PUT_PARAMS,
                                           db_put_tree_common(&tb->common,
                                                              &base_node->root,
                                                              obj,
                                                              key_clash_fail,
                                                              NULL);)

static int db_put_catree(DbTable *tbl, Eterm obj, int key_clash_fail)
{
    DbTableCATree *tb = &tbl->catree;
    Eterm key = GETKEY(&tb->common, tuple_val(obj));
    return erl_db_catree_do_operation_put(tb,
                                          key,
                                          obj,
                                          key_clash_fail);
}

#define ERL_DB_CATREE_DO_OPERATION_GET_PARAMS Process *p, Eterm *ret
ERL_DB_CATREE_CREATE_DO_READ_OPERATION_FUNCTION(get,
                                                ERL_DB_CATREE_DO_OPERATION_GET_PARAMS,
                                                db_get_tree_common(p, &tb->common,
                                                                   base_node->root,
                                                                   key, ret, NULL);)

static int db_get_catree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableCATree *tb = &tbl->catree;
    return erl_db_catree_do_operation_get(tb, key, p, ret);
}



TreeDbTerm** catree_find_root(Eterm key, CATreeRootIterator* iter)
{
    FindBaseNode fbn;
    DbTableCATreeBaseNode* base_node;

    while (1) {
        base_node = find_base_node(iter->tb, key, &fbn);
        lock_iter_base_node(iter, base_node);
        if (base_node->is_valid)
            break;
        unlock_iter_base_node(iter);
    }
    return &base_node->root;
}


TreeDbTerm** catree_find_prev_from_pb_key_root(Eterm key,
                                               CATreeRootIterator* iter)
{
#ifdef DEBUG
    DbTableCATreeNode *rejected_base = NULL;
#endif
    DbTableCATreeNode *node;
    DbTableCATreeNode* next_route_node;

    ASSERT(!iter->locked_bnode);

    while (1) {
        node = GET_ROOT_ACQB(iter->tb);
        next_route_node = NULL;
        while (!node->is_base_node) {
            if (cmp_partly_bound(key, GET_ROUTE_NODE_KEY(node)) <= 0) {
                node = GET_LEFT_ACQB(node);
            } else {
                next_route_node = node;
                node = GET_RIGHT_ACQB(node);
            }
        }
        ASSERT(node != rejected_base);
        lock_iter_base_node(iter, &node->u.base);
        if (node->u.base.is_valid) {
            if (node->u.base.root || !next_route_node) {
                iter->next_route_key = (next_route_node ?
                                        next_route_node->u.route.key.term :
                                        THE_NON_VALUE);
                return &node->u.base.root;
            }
            unlock_iter_base_node(iter);
            iter->next_route_key = next_route_node->u.route.key.term;
            return catree_find_next_root(iter);
        }
        /* Retry */
        unlock_iter_base_node(iter);
#ifdef DEBUG
        rejected_base = node;
#endif
    }
}

TreeDbTerm** catree_find_nextprev_root(CATreeRootIterator *iter, int next)
{
#ifdef DEBUG
    DbTableCATreeNode *rejected_base = NULL;
#endif
    DbTableCATreeNode *node;
    DbTableCATreeNode* next_route_node;
    Eterm key = iter->next_route_key;

    if (iter->locked_bnode)
        unlock_iter_base_node(iter);

    if (is_non_value(key))
        return NULL;

    while (1) {
        node = GET_ROOT_ACQB(iter->tb);
        next_route_node = NULL;
        while (!node->is_base_node) {
            if (next) {
                if (cmp_key_route(key,node) < 0) {
                    next_route_node = node;
                    node = GET_LEFT_ACQB(node);
                } else {
                    node = GET_RIGHT_ACQB(node);
                }
            }
            else {
                if (cmp_key_route(key,node) > 0) {
                    next_route_node = node;
                    node = GET_RIGHT_ACQB(node);
                } else {
                    node = GET_LEFT_ACQB(node);
                }
            }
        }
        ASSERT(node != rejected_base);
        lock_iter_base_node(iter, &node->u.base);
        if (node->u.base.is_valid) {
            if (node->u.base.root) {
                iter->next_route_key = (next_route_node ?
                                        next_route_node->u.route.key.term :
                                        THE_NON_VALUE);
                iter->locked_bnode = &node->u.base;
                return &node->u.base.root;
            }
            if (!next_route_node) {
                unlock_iter_base_node(iter);
                return NULL;
            }
            key = next_route_node->u.route.key.term;
        }
        /* Retry */
        unlock_iter_base_node(iter);
#ifdef DEBUG
        rejected_base = node;
#endif
    }
}

TreeDbTerm** catree_find_next_root(CATreeRootIterator *iter)
{
    return catree_find_nextprev_root(iter, 1);
}

TreeDbTerm** catree_find_prev_root(CATreeRootIterator *iter)
{
    return catree_find_nextprev_root(iter, 0);
}


TreeDbTerm** catree_find_next_from_pb_key_root(Eterm key,
                                               CATreeRootIterator* iter)
{
#ifdef DEBUG
    DbTableCATreeNode *rejected_base = NULL;
#endif
    DbTableCATreeNode *node;
    DbTableCATreeNode* next_route_node;

    ASSERT(!iter->locked_bnode);

    while (1) {
        node = GET_ROOT_ACQB(iter->tb);
        next_route_node = NULL;
        while (!node->is_base_node) {
            if (cmp_partly_bound(key, GET_ROUTE_NODE_KEY(node)) >= 0) {
                next_route_node = node;
                node = GET_RIGHT_ACQB(node);
            } else {
                node = GET_LEFT_ACQB(node);
            }
        }
        ASSERT(node != rejected_base);
        lock_iter_base_node(iter, &node->u.base);
        if (node->u.base.is_valid) {
            if (node->u.base.root || !next_route_node) {
                iter->next_route_key = (next_route_node ?
                                        next_route_node->u.route.key.term :
                                        THE_NON_VALUE);
                iter->locked_bnode = &node->u.base;
                return &node->u.base.root;
            }
            unlock_iter_base_node(iter);
            iter->next_route_key = next_route_node->u.route.key.term;
            return catree_find_prev_root(iter);
        }
        /* Retry */
        unlock_iter_base_node(iter);
#ifdef DEBUG
        rejected_base = node;
#endif
    }
}

static TreeDbTerm** catree_find_firstlast_root(CATreeRootIterator* iter,
                                               int first)
{
#ifdef DEBUG
    DbTableCATreeNode *rejected_base = NULL;
#endif
    DbTableCATreeNode *node;
    DbTableCATreeNode* next_route_node;

    while (1) {
        node = GET_ROOT_ACQB(iter->tb);
        next_route_node = NULL;
        while (!node->is_base_node) {
            next_route_node = node;
            node = first ? GET_LEFT_ACQB(node) : GET_RIGHT_ACQB(node);
        }
        ASSERT(node != rejected_base);
        lock_iter_base_node(iter, &node->u.base);
        if (node->u.base.is_valid) {
            iter->next_route_key = (next_route_node ?
                                    next_route_node->u.route.key.term :
                                    THE_NON_VALUE);
            iter->locked_bnode = &node->u.base;
            return &node->u.base.root;
        }
        /* Retry */
        unlock_iter_base_node(iter);
#ifdef DEBUG
        rejected_base = node;
#endif
    }
}

TreeDbTerm** catree_find_first_root(CATreeRootIterator* iter)
{
    return catree_find_firstlast_root(iter, 1);
}

TreeDbTerm** catree_find_last_root(CATreeRootIterator* iter)
{
    return catree_find_firstlast_root(iter, 0);
}


#define ERL_DB_CATREE_DO_OPERATION_MEMBER_PARAMS Eterm *ret
ERL_DB_CATREE_CREATE_DO_READ_OPERATION_FUNCTION(member,
                                                ERL_DB_CATREE_DO_OPERATION_MEMBER_PARAMS,
                                                db_member_tree_common(&tb->common,
                                                                      base_node->root,
                                                                      key,
                                                                      ret,
                                                                      NULL);)

static int db_member_catree(DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableCATree *tb = &tbl->catree;
    return erl_db_catree_do_operation_member(tb, key, ret);
}

#define ERL_DB_CATREE_DO_OPERATION_GET_ELEMENT_PARAMS Process *p, int ndex, Eterm *ret
ERL_DB_CATREE_CREATE_DO_READ_OPERATION_FUNCTION(get_element,
                                                ERL_DB_CATREE_DO_OPERATION_GET_ELEMENT_PARAMS,
                                                db_get_element_tree_common(p, &tb->common,
                                                                           base_node->root,
                                                                           key, ndex,
                                                                           ret, NULL))

static int db_get_element_catree(Process *p, DbTable *tbl,
			       Eterm key, int ndex, Eterm *ret)
{
    DbTableCATree *tb = &tbl->catree;
    return erl_db_catree_do_operation_get_element(tb, key, p, ndex, ret);
}

#define ERL_DB_CATREE_DO_OPERATION_ERASE_PARAMS DbTable *tbl, Eterm *ret
ERL_DB_CATREE_CREATE_DO_OPERATION_FUNCTION(erase,
                                           ERL_DB_CATREE_DO_OPERATION_ERASE_PARAMS,
                                           db_erase_tree_common(tbl,
                                                                &base_node->root,
                                                                key,
                                                                ret,
                                                                NULL);)

static int db_erase_catree(DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableCATree *tb = &tbl->catree;
    return erl_db_catree_do_operation_erase(tb, key, tbl, ret);
}

#define ERL_DB_CATREE_DO_OPERATION_ERASE_OBJECT_PARAMS DbTable *tbl, Eterm object, Eterm *ret
ERL_DB_CATREE_CREATE_DO_OPERATION_FUNCTION(erase_object,
                                           ERL_DB_CATREE_DO_OPERATION_ERASE_OBJECT_PARAMS,
                                           db_erase_object_tree_common(tbl,
                                                                       &base_node->root,
                                                                       object,
                                                                       ret,
                                                                       NULL);)

static int db_erase_object_catree(DbTable *tbl, Eterm object, Eterm *ret)
{
    DbTableCATree *tb = &tbl->catree;
    Eterm key = GETKEY(&tb->common, tuple_val(object));
    return erl_db_catree_do_operation_erase_object(tb, key, tbl, object, ret);
}


static int db_slot_catree(Process *p, DbTable *tbl,
                          Eterm slot_term, Eterm *ret)
{
    int result;
    DbTableCATreeNode *base;
    base = merge_to_one_locked_base_node(&tbl->catree);
    result = db_slot_tree_common(p, tbl, base->u.base.root,
                                 slot_term, ret, NULL);
    wunlock_base_node(&(base->u.base));
    return result;
}

static int db_select_continue_catree(Process *p,
                                     DbTable *tbl,
                                     Eterm continuation,
                                     Eterm *ret)
{
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 1);
    result = db_select_continue_tree_common(p, &tbl->common,
                                            continuation, ret, NULL, &iter);
    if (iter.locked_bnode)
        runlock_base_node(iter.locked_bnode);

    return result;
}

static int db_select_catree(Process *p, DbTable *tbl, Eterm tid,
                            Eterm pattern, int reverse, Eterm *ret)
{
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 1);
    result = db_select_tree_common(p, tbl, tid, pattern, reverse, ret,
                                   NULL, &iter);
    if (iter.locked_bnode)
        runlock_base_node(iter.locked_bnode);
    return result;
}

static int db_select_count_continue_catree(Process *p,
                                           DbTable *tbl,
                                           Eterm continuation,
                                           Eterm *ret)
{
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 1);
    result = db_select_count_continue_tree_common(p, tbl,
                                                  continuation, ret, NULL,
                                                  &iter);
    if (iter.locked_bnode)
        runlock_base_node(iter.locked_bnode);
    return result;
}

static int db_select_count_catree(Process *p, DbTable *tbl, Eterm tid,
                                  Eterm pattern, Eterm *ret)
{
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 1);
    result = db_select_count_tree_common(p, tbl,
                                         tid, pattern, ret, NULL, &iter);
    if (iter.locked_bnode)
        runlock_base_node(iter.locked_bnode);

    return result;
}

static int db_select_chunk_catree(Process *p, DbTable *tbl, Eterm tid,
                                  Eterm pattern, Sint chunk_size,
                                  int reversed, Eterm *ret)
{
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 1);
    result = db_select_chunk_tree_common(p, tbl,
                                         tid, pattern, chunk_size, reversed, ret,
                                         NULL, &iter);
    if (iter.locked_bnode)
        runlock_base_node(iter.locked_bnode);
    return result;
}

static int db_select_delete_continue_catree(Process *p,
                                            DbTable *tbl,
                                            Eterm continuation,
                                            Eterm *ret)
{
    DbTreeStack stack;
    TreeDbTerm * stack_array[STACK_NEED];
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 0);
    init_tree_stack(&stack, stack_array, 0);
    result = db_select_delete_continue_tree_common(p, tbl, continuation, ret,
                                                   &stack, &iter);
    if (iter.locked_bnode)
        wunlock_base_node(iter.locked_bnode);
    return result;
}

static int db_select_delete_catree(Process *p, DbTable *tbl, Eterm tid,
                                   Eterm pattern, Eterm *ret)
{
    DbTreeStack stack;
    TreeDbTerm * stack_array[STACK_NEED];
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 0);
    init_tree_stack(&stack, stack_array, 0);
    result = db_select_delete_tree_common(p, tbl,
                                          tid, pattern, ret, &stack,
                                          &iter);
    if (iter.locked_bnode)
        wunlock_base_node(iter.locked_bnode);
    return result;
}

static int db_select_replace_catree(Process *p, DbTable *tbl, Eterm tid,
                                    Eterm pattern, Eterm *ret)
{
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 0);
    result = db_select_replace_tree_common(p, tbl,
                                           tid, pattern, ret, NULL, &iter);
    if (iter.locked_bnode)
        wunlock_base_node(iter.locked_bnode);
    return result;
}

static int db_select_replace_continue_catree(Process *p, DbTable *tbl,
                                             Eterm continuation, Eterm *ret)
{
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 0);
    result = db_select_replace_continue_tree_common(p, tbl, continuation, ret,
                                                    NULL, &iter);
    if (iter.locked_bnode)
        wunlock_base_node(iter.locked_bnode);
    return result;
}

#define ERL_DB_CATREE_DO_OPERATION_TAKE_PARAMS Process *p, Eterm *ret, DbTable *tbl
ERL_DB_CATREE_CREATE_DO_OPERATION_FUNCTION(take,
                                           ERL_DB_CATREE_DO_OPERATION_TAKE_PARAMS,
                                           db_take_tree_common(p, tbl,
                                                               &base_node->root,
                                                               key, ret, NULL);)

static int db_take_catree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableCATree *tb = &tbl->catree;
    return erl_db_catree_do_operation_take(tb, key, p, ret, tbl);
}

/*
** Other interface routines (not directly coupled to one bif)
*/


/* Display tree contents (for dump) */
static void db_print_catree(fmtfn_t to, void *to_arg,
                            int show, DbTable *tbl)
{
    DbTableCATreeNode *base = merge_to_one_locked_base_node(&tbl->catree);
    db_print_tree_common(to, to_arg, show, base->u.base.root, tbl);
    wunlock_base_node(&(base->u.base));
}

/* Release all memory occupied by a single table */
static int db_free_table_catree(DbTable *tbl)
{
    while (db_free_table_continue_catree(tbl, ERTS_SWORD_MAX) < 0)
	;
    return 1;
}

static SWord db_free_table_continue_catree(DbTable *tbl, SWord reds)
{
    DbTableCATreeNode *first_base_node;
    DbTableCATree *tb = &tbl->catree;
    if (!tb->deletion) {
        tb->deletion = 1;
        tb->free_stack_elems.array =
            erts_db_alloc(ERTS_ALC_T_DB_STK,
                          (DbTable *) tb,
                          sizeof(TreeDbTerm *) * STACK_NEED);
        tb->free_stack_elems.pos = 0;
        tb->free_stack_elems.slot = 0;
        tb->free_stack_rnodes.array =
            erts_db_alloc(ERTS_ALC_T_DB_STK,
                          (DbTable *) tb,
                          sizeof(DbTableCATreeNode *) * STACK_NEED);
        tb->free_stack_rnodes.pos = 0;
        tb->free_stack_rnodes.size = STACK_NEED;
        PUSH_NODE(&tb->free_stack_rnodes, GET_ROOT(tb));
        tb->is_routing_nodes_freed = 0;
        tb->base_nodes_to_free_list = NULL;
    }
    if ( ! tb->is_routing_nodes_freed ) {
        reds = do_free_routing_nodes_catree_cont(tb, reds);
        if (reds < 0) {
            return reds; /* Not finished */
        } else {
            tb->is_routing_nodes_freed = 1; /* Ready with the routing nodes */
            first_base_node = catree_first_base_node_from_free_list(tb);
            PUSH_NODE(&tb->free_stack_elems, first_base_node->u.base.root);
        }
    }
    while (catree_first_base_node_from_free_list(tb) != NULL) {
        reds = do_free_base_node_cont(tb, reds);
        if (reds < 0) {
            return reds; /* Continue later */
        }
    }
    /* Time to free the main structure*/
    erts_db_free(ERTS_ALC_T_DB_STK,
                 (DbTable *) tb,
                 (void *) tb->free_stack_elems.array,
                 sizeof(TreeDbTerm *) * STACK_NEED);
    erts_db_free(ERTS_ALC_T_DB_STK,
                 (DbTable *) tb,
                 (void *) tb->free_stack_rnodes.array,
                 sizeof(DbTableCATreeNode *) * STACK_NEED);
    return 1;
}

static SWord db_delete_all_objects_catree(Process* p, DbTable* tbl, SWord reds)
{
    reds = db_free_table_continue_catree(tbl, reds);
    if (reds < 0)
        return reds;
    db_create_catree(p, tbl);
    erts_atomic_set_nob(&tbl->catree.common.nitems, 0);
    return reds;
}


static void db_foreach_offheap_catree(DbTable *tbl,
                                      void (*func)(ErlOffHeap *, void *),
                                      void *arg)
{
    DbTableCATreeNode *base = merge_to_one_locked_base_node(&tbl->catree);
    db_foreach_offheap_tree_common(base->u.base.root, func, arg);
    wunlock_base_node(&(base->u.base));
}

static int db_lookup_dbterm_catree(Process *p, DbTable *tbl, Eterm key, Eterm obj,
                                   DbUpdateHandle *handle)
{
    DbTableCATree *tb = &tbl->catree;
    int res;
    ERL_DB_CATREE_CREATE_DO_OPERATION_FUNCTION_FIND_BASE_NODE_PART(wlock_base_node,wunlock_base_node);
    res =  db_lookup_dbterm_tree_common(p, tbl, &base_node->root, key, obj, handle, NULL);
    if (res == 0) {
        ERL_DB_CATREE_CREATE_DO_OPERATION_FUNCTION_ADAPT_AND_UNLOCK_PART;
    } else {
        /* db_finalize_dbterm_catree will unlock */
        handle->lck = prev_node;
        handle->lck2 = current_node;
        handle->current_level = current_level;
    }
    return res;
}

static void db_finalize_dbterm_catree(int cret, DbUpdateHandle *handle)
{
    DbTableCATree *tb = &(handle->tb->catree);
    DbTableCATreeNode *prev_node = handle->lck;    
    DbTableCATreeNode *current_node = handle->lck2;
    int current_level = handle->current_level;
    DbTableCATreeBaseNode *base_node = &current_node->u.base;
    db_finalize_dbterm_tree_common(cret, handle, NULL);
    ERL_DB_CATREE_CREATE_DO_OPERATION_FUNCTION_ADAPT_AND_UNLOCK_PART;
    return;
}

#ifdef ERTS_ENABLE_LOCK_COUNT
static void erts_lcnt_enable_db_catree_lock_count_helper(DbTableCATree *tb,
                                                         DbTableCATreeNode *node,
                                                         int enable)
{
    erts_lcnt_ref_t *lcnt_ref;
    erts_lock_flags_t lock_type;
    if (node->is_base_node) {
        lcnt_ref = &GET_BASE_NODE_LOCK(node)->lcnt;
        lock_type = ERTS_LOCK_TYPE_RWMUTEX;
    } else {
        erts_lcnt_enable_db_catree_lock_count_helper(tb, GET_LEFT(node), enable);
        erts_lcnt_enable_db_catree_lock_count_helper(tb, GET_RIGHT(node), enable);
        lcnt_ref = &GET_ROUTE_NODE_LOCK(node)->lcnt;
        lock_type = ERTS_LOCK_TYPE_MUTEX;
    }
    if (enable) {
        erts_lcnt_install_new_lock_info(lcnt_ref, "db_hash_slot", tb->common.the_name,
                                        lock_type | ERTS_LOCK_FLAGS_CATEGORY_DB);
    } else {
        erts_lcnt_uninstall(lcnt_ref);
    }
}

void erts_lcnt_enable_db_catree_lock_count(DbTableCATree *tb, int enable)
{
    erts_lcnt_enable_db_catree_lock_count_helper(tb, GET_ROOT(tb), enable);
}
#endif /* ERTS_ENABLE_LOCK_COUNT */


#ifdef HARDDEBUG

/*
 * Not called, but kept as it might come to use
 */
static inline int my_check_table_tree(TreeDbTerm *t)
{
    int lh, rh;
    if (t == NULL)
	return 0;
    lh = my_check_table_tree(t->left);
    rh = my_check_table_tree(t->right);
    if ((rh - lh) != t->balance) {
	erts_fprintf(stderr, "Invalid tree balance for this node:\n");
	erts_fprintf(stderr,"balance = %d, left = 0x%08X, right = 0x%08X\n",
		     t->balance, t->left, t->right);
	erts_fprintf(stderr,"\nDump:\n---------------------------------\n");
	erts_fprintf(stderr,"\n---------------------------------\n");
        abort();
    }
    return ((rh > lh) ? rh : lh) + 1;
}

#endif
