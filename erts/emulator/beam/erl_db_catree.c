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
 * implementation located in "erl_db_tree.c".
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

#ifdef DEBUG
#  define IF_DEBUG(X) X
#else
#  define IF_DEBUG(X)
#endif

/*
** Forward declarations
*/

static SWord do_delete_base_node_cont(DbTableCATree *tb,
                                    DbTableCATreeNode *base_node,
                                    SWord num_left);

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
static int db_put_catree(DbTable *tbl, Eterm obj, int key_clash_fail,
                         SWord *consumed_reds_p);
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
                            Eterm pattern, int reversed, Eterm *ret,
                            enum DbIterSafety);
static int db_select_count_catree(Process *p, DbTable *tbl, Eterm tid,
                                  Eterm pattern,  Eterm *ret, enum DbIterSafety);
static int db_select_chunk_catree(Process *p, DbTable *tbl, Eterm tid,
                                  Eterm pattern, Sint chunk_size,
                                  int reversed, Eterm *ret, enum DbIterSafety);
static int db_select_continue_catree(Process *p, DbTable *tbl,
                                     Eterm continuation, Eterm *ret,
                                     enum DbIterSafety*);
static int db_select_count_continue_catree(Process *p, DbTable *tbl,
                                           Eterm continuation, Eterm *ret,
                                           enum DbIterSafety*);
static int db_select_delete_catree(Process *p, DbTable *tbl, Eterm tid,
                                   Eterm pattern,  Eterm *ret,
                                   enum DbIterSafety);
static int db_select_delete_continue_catree(Process *p, DbTable *tbl, 
                                            Eterm continuation, Eterm *ret,
                                            enum DbIterSafety*);
static int db_select_replace_catree(Process *p, DbTable *tbl, Eterm tid,
                                    Eterm pattern, Eterm *ret,
                                    enum DbIterSafety);
static int db_select_replace_continue_catree(Process *p, DbTable *tbl,
                                             Eterm continuation, Eterm *ret,
                                             enum DbIterSafety*);
static int db_take_catree(Process *, DbTable *, Eterm, Eterm *);
static void db_print_catree(fmtfn_t to, void *to_arg,
                            int show, DbTable *tbl);
static int db_free_table_catree(DbTable *tbl);
static SWord db_free_table_continue_catree(DbTable *tbl, SWord);
static void db_foreach_offheap_catree(DbTable *,
                                      void (*)(ErlOffHeap *, void *),
                                      void *);
static SWord db_delete_all_objects_catree(Process* p,
                                          DbTable* tbl,
                                          SWord reds,
                                          Eterm* nitems_holder_wb);
static Eterm db_delete_all_objects_get_nitems_from_holder_catree(Process* p,
                                                                 Eterm nitems_holder);
static int
db_lookup_dbterm_catree(Process *, DbTable *, Eterm key, Eterm obj,
                        DbUpdateHandle*);
static void db_finalize_dbterm_catree(int cret, DbUpdateHandle *);
static int db_get_binary_info_catree(Process*, DbTable*, Eterm key, Eterm *ret);
static int db_put_dbterm_catree(DbTable* tbl,
                                void* obj,
                                int key_clash_fail,
                                SWord *consumed_reds_p);

static void split_catree(DbTableCATree *tb,
                         DbTableCATreeNode* ERTS_RESTRICT base,
                         DbTableCATreeNode* ERTS_RESTRICT parent);
static void join_catree(DbTableCATree *tb,
                        DbTableCATreeNode *thiz,
                        DbTableCATreeNode *parent);
static ERTS_INLINE
int try_wlock_base_node(DbTableCATreeBaseNode *base_node);
static ERTS_INLINE
void wunlock_base_node(DbTableCATreeNode *base_node);
static ERTS_INLINE
void wlock_base_node_no_stats(DbTableCATreeNode *base_node);
static ERTS_INLINE
void wunlock_adapt_base_node(DbTableCATree* tb,
                             DbTableCATreeNode* node,
                             DbTableCATreeNode* parent,
                             int current_level);
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
    db_delete_all_objects_get_nitems_from_holder_catree,
    db_free_table_catree,
    db_free_table_continue_catree,
    db_print_catree,
    db_foreach_offheap_catree,
    db_lookup_dbterm_catree,
    db_finalize_dbterm_catree,
    db_eterm_to_dbterm_tree_common,
    db_dbterm_list_prepend_tree_common,
    db_dbterm_list_remove_first_tree_common,
    db_put_dbterm_catree,
    db_free_dbterm_tree_common,
    db_get_dbterm_key_tree_common,
    db_get_binary_info_catree,
    db_first_catree, /* raw_first same as first */
    db_next_catree   /* raw_next same as next */
};

/*
 * Constants
 */

#define ERL_DB_CATREE_LOCK_FAILURE_CONTRIBUTION 250
#define ERL_DB_CATREE_LOCK_SUCCESS_CONTRIBUTION (-1)
#define ERL_DB_CATREE_LOCK_GRAVITY_CONTRIBUTION (-500)
#define ERL_DB_CATREE_LOCK_GRAVITY_PATTERN (0xFF800000)
#define ERL_DB_CATREE_LOCK_MORE_THAN_ONE_CONTRIBUTION (-10)
#define ERL_DB_CATREE_HIGH_CONTENTION_LIMIT 1000
#define ERL_DB_CATREE_LOW_CONTENTION_LIMIT (-1000)
#define ERL_DB_CATREE_MAX_ROUTE_NODE_LAYER_HEIGHT 16
#define ERL_DB_CATREE_LOCK_LOW_NO_CONTRIBUTION_LIMIT (-20000)
#define ERL_DB_CATREE_LOCK_HIGH_NO_CONTRIBUTION_LIMIT (20000)

/*
 * Internal CA tree related helper functions and macros
 */

#define GET_ROUTE_NODE_KEY(node) (node->u.route.key.term)
#define GET_BASE_NODE_LOCK(node) (&(node->u.base.lock))
#define GET_ROUTE_NODE_LOCK(node) (&(node->u.route.lock))


/* Helpers for reading and writing shared atomic variables */

/* No memory barrier */
#define GET_ROOT(tb) ((DbTableCATreeNode*)erts_atomic_read_nob(&((tb)->root)))
#define GET_LEFT(ca_tree_route_node) ((DbTableCATreeNode*)erts_atomic_read_nob(&(ca_tree_route_node->u.route.left)))
#define GET_RIGHT(ca_tree_route_node) ((DbTableCATreeNode*)erts_atomic_read_nob(&(ca_tree_route_node->u.route.right)))
#define SET_ROOT(tb, v) erts_atomic_set_nob(&((tb)->root), (erts_aint_t)(v))
#define SET_LEFT(ca_tree_route_node, v) erts_atomic_set_nob(&(ca_tree_route_node->u.route.left), (erts_aint_t)(v));
#define SET_RIGHT(ca_tree_route_node, v) erts_atomic_set_nob(&(ca_tree_route_node->u.route.right), (erts_aint_t)(v));


/* Release or acquire barriers */
#define GET_ROOT_ACQB(tb) ((DbTableCATreeNode*)erts_atomic_read_acqb(&((tb)->root)))
#define GET_LEFT_ACQB(ca_tree_route_node) ((DbTableCATreeNode*)erts_atomic_read_acqb(&(ca_tree_route_node->u.route.left)))
#define GET_RIGHT_ACQB(ca_tree_route_node) ((DbTableCATreeNode*)erts_atomic_read_acqb(&(ca_tree_route_node->u.route.right)))
#define SET_ROOT_RELB(tb, v) erts_atomic_set_relb(&((tb)->root), (erts_aint_t)(v))
#define SET_LEFT_RELB(ca_tree_route_node, v) erts_atomic_set_relb(&(ca_tree_route_node->u.route.left), (erts_aint_t)(v));
#define SET_RIGHT_RELB(ca_tree_route_node, v) erts_atomic_set_relb(&(ca_tree_route_node->u.route.right), (erts_aint_t)(v));

/* Change base node lock statistics */
#define BASE_NODE_STAT_SET(NODE, VALUE) erts_atomic_set_nob(&(NODE)->u.base.lock_statistics, VALUE)
#define BASE_NODE_STAT_READ(NODE) erts_atomic_read_nob(&(NODE)->u.base.lock_statistics)
#define BASE_NODE_STAT_ADD(NODE, VALUE)                                 \
    do {                                                                \
        Sint v = erts_atomic_read_nob(&((NODE)->u.base.lock_statistics)); \
        ASSERT(VALUE > 0);                                              \
        if(v < ERL_DB_CATREE_LOCK_HIGH_NO_CONTRIBUTION_LIMIT) {          \
            erts_atomic_set_nob(&(NODE->u.base.lock_statistics), v + VALUE); \
        }                                                               \
    }while(0);
#define BASE_NODE_STAT_SUB(NODE, VALUE)                                 \
    do {                                                                \
        Sint v = erts_atomic_read_nob(&((NODE)->u.base.lock_statistics)); \
        ASSERT(VALUE < 0);                                              \
        if(v > ERL_DB_CATREE_LOCK_LOW_NO_CONTRIBUTION_LIMIT) {          \
            erts_atomic_set_nob(&(NODE->u.base.lock_statistics), v + VALUE); \
        }                                                               \
    }while(0);


/* Compares a key to the key in a route node */
static ERTS_INLINE Sint cmp_key_route(Eterm key,
                                      DbTableCATreeNode *obj)
{
    return CMP(key, GET_ROUTE_NODE_KEY(obj));
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

#ifdef DEBUG
#  define PROVOKE_RANDOM_SPLIT_JOIN
#endif
#ifdef PROVOKE_RANDOM_SPLIT_JOIN
static int dbg_fastrand(void)
{
    static int g_seed = 648835;
    g_seed = (214013*g_seed+2531011);
    return (g_seed>>16)&0x7FFF;
}

static void dbg_provoke_random_splitjoin(DbTableCATree* tb,
                                         DbTableCATreeNode* base_node)
{
    if (tb->common.status & DB_CATREE_FORCE_SPLIT ||
        !(tb->common.status & DB_CATREE_DEBUG_RANDOM_SPLIT_JOIN))
        return;

    switch (dbg_fastrand() % 8) {
    case 1:
        BASE_NODE_STAT_ADD(base_node, 1+ERL_DB_CATREE_HIGH_CONTENTION_LIMIT);
        break;
    case 2:
        BASE_NODE_STAT_SUB(base_node, -1+ERL_DB_CATREE_LOW_CONTENTION_LIMIT);
        break;
    }
}
#else
#  define dbg_provoke_random_splitjoin(T,N)
#endif /* PROVOKE_RANDOM_SPLIT_JOIN */

static ERTS_NOINLINE
void do_random_join(DbTableCATree* tb, Uint rand)
{
    DbTableCATreeNode* node = GET_ROOT_ACQB(tb);
    DbTableCATreeNode* parent = NULL;
    int level = 0;
    Sint stat;
    while (!node->is_base_node) {
        parent = node;
        if ((rand & (1 << level)) == 0) {
            node = GET_LEFT_ACQB(node);
        } else {
            node = GET_RIGHT_ACQB(node);
        }
        level++;
    }
    BASE_NODE_STAT_SUB(node, ERL_DB_CATREE_LOCK_GRAVITY_CONTRIBUTION);
    stat = BASE_NODE_STAT_READ(node);
    if (stat >= ERL_DB_CATREE_LOW_CONTENTION_LIMIT &&
        stat <= ERL_DB_CATREE_HIGH_CONTENTION_LIMIT) {
        return; /* No adaptation */
    }
    if (parent != NULL && !try_wlock_base_node(&node->u.base)) {
        if (!node->u.base.is_valid) {
            wunlock_base_node(node);
            return;
        }
        wunlock_adapt_base_node(tb, node, parent, level);
    }
}

static ERTS_INLINE
void do_random_join_with_low_probability(DbTableCATree* tb, Uint seed)
{
#ifndef ERTS_DB_CA_TREE_NO_RANDOM_JOIN_WITH_LOW_PROBABILITY
    Uint32 rand = erts_sched_local_random(seed);
    if (((rand & ERL_DB_CATREE_LOCK_GRAVITY_PATTERN)) == 0) {
        do_random_join(tb, rand);
    }
#endif
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
void wlock_base_node_no_stats(DbTableCATreeNode *base_node)
{
    ASSERT(base_node->is_base_node);
    erts_rwmtx_rwlock(&base_node->u.base.lock);
}

/*
 * Locks a base node and adjusts the lock statistics according to if
 * the lock was contended or not
 */
static ERTS_INLINE
void wlock_base_node(DbTableCATreeNode *base_node)
{
    ASSERT(base_node->is_base_node);
    if (try_wlock_base_node(&base_node->u.base)) {
        /* The lock is contended */
        wlock_base_node_no_stats(base_node);
        BASE_NODE_STAT_ADD(base_node, ERL_DB_CATREE_LOCK_FAILURE_CONTRIBUTION);
    } else {
        BASE_NODE_STAT_SUB(base_node, ERL_DB_CATREE_LOCK_SUCCESS_CONTRIBUTION);
    }
}

static ERTS_INLINE
void wunlock_base_node(DbTableCATreeNode *base_node)
{
    erts_rwmtx_rwunlock(&base_node->u.base.lock);
}

static ERTS_INLINE
void wunlock_adapt_base_node(DbTableCATree* tb,
                             DbTableCATreeNode* node,
                             DbTableCATreeNode* parent,
                             int current_level)
{
    Sint base_node_lock_stat = BASE_NODE_STAT_READ(node);
    dbg_provoke_random_splitjoin(tb,node);
    if ((!node->u.base.root && parent && !(tb->common.status
                                           & DB_CATREE_FORCE_SPLIT))
        || base_node_lock_stat < ERL_DB_CATREE_LOW_CONTENTION_LIMIT) {
        join_catree(tb, node, parent);
    }
    else if (base_node_lock_stat > ERL_DB_CATREE_HIGH_CONTENTION_LIMIT
        && current_level < ERL_DB_CATREE_MAX_ROUTE_NODE_LAYER_HEIGHT) {
        split_catree(tb, node, parent);
    }
    else {
        wunlock_base_node(node);
    }
}

static ERTS_INLINE
void rlock_base_node(DbTableCATreeNode *base_node)
{
    ASSERT(base_node->is_base_node);
    if (EBUSY == erts_rwmtx_tryrlock(&base_node->u.base.lock)) {
        /* The lock is contended */
        BASE_NODE_STAT_ADD(base_node, ERL_DB_CATREE_LOCK_FAILURE_CONTRIBUTION);
        erts_rwmtx_rlock(&base_node->u.base.lock);
    }
}

static ERTS_INLINE
void runlock_base_node(DbTableCATreeNode *base_node, DbTableCATree* tb)
{
    ASSERT(base_node->is_base_node);
    erts_rwmtx_runlock(&base_node->u.base.lock);
    do_random_join_with_low_probability(tb, (Uint)base_node);
}

static ERTS_INLINE
void runlock_base_node_no_rand(DbTableCATreeNode *base_node)
{
    ASSERT(base_node->is_base_node);
    erts_rwmtx_runlock(&base_node->u.base.lock);
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
Eterm copy_route_key(DbRouteKey* dst, Eterm key, Uint key_size)
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
    return dst->term;
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

static ERTS_INLINE
void init_root_iterator(DbTableCATree* tb, CATreeRootIterator* iter,
                        int read_only)
{
    iter->tb = tb;
    iter->read_only = read_only;
    iter->locked_bnode = NULL;
    iter->next_route_key = THE_NON_VALUE;
    iter->search_key = NULL;
}

static ERTS_INLINE
void lock_iter_base_node(CATreeRootIterator* iter,
                         DbTableCATreeNode *base_node,
                         DbTableCATreeNode *parent,
                         int current_level)
{
    ASSERT(!iter->locked_bnode);
    if (iter->read_only)
        rlock_base_node(base_node);
    else {
        wlock_base_node(base_node);
        iter->bnode_parent = parent;
        iter->bnode_level = current_level;
    }
    iter->locked_bnode = base_node;
}

static ERTS_INLINE
void unlock_iter_base_node(CATreeRootIterator* iter)
{
    ASSERT(iter->locked_bnode);
    if (iter->read_only)
        runlock_base_node(iter->locked_bnode, iter->tb);
    else if (iter->locked_bnode->u.base.is_valid) {
        wunlock_adapt_base_node(iter->tb, iter->locked_bnode,
                                iter->bnode_parent, iter->bnode_level);
    }
    else
        wunlock_base_node(iter->locked_bnode);
    iter->locked_bnode = NULL;
}

static ERTS_INLINE
void destroy_root_iterator(CATreeRootIterator* iter)
{
    if (iter->locked_bnode)
        unlock_iter_base_node(iter);
    if (iter->search_key) {
        destroy_route_key(iter->search_key);
        erts_free(ERTS_ALC_T_DB_TMP, iter->search_key);
    }
}

typedef struct
{
    DbTableCATreeNode *parent;
    int current_level;
} FindBaseNode;

static ERTS_INLINE
DbTableCATreeNode* find_base_node(DbTableCATree* tb, Eterm key,
                                  FindBaseNode* fbn)
{
    DbTableCATreeNode* ERTS_RESTRICT node = GET_ROOT_ACQB(tb);
    if (fbn) {
        fbn->parent = NULL;
        fbn->current_level = 0;
    }
    while (!node->is_base_node) {
        if (fbn) {
            fbn->current_level++;
            fbn->parent = node;
        }
        if (cmp_key_route(key, node) < 0) {
            node = GET_LEFT_ACQB(node);
        } else {
            node = GET_RIGHT_ACQB(node);
        }
    }
    return node;
}

static ERTS_INLINE
DbTableCATreeNode* find_rlock_valid_base_node(DbTableCATree* tb, Eterm key)
{
    DbTableCATreeNode* base_node;

    while (1) {
        base_node = find_base_node(tb, key, NULL);
        rlock_base_node(base_node);
        if (base_node->u.base.is_valid)
            break;
        runlock_base_node_no_rand(base_node);
    }
    return base_node;
}

static ERTS_INLINE
DbTableCATreeNode* find_wlock_valid_base_node(DbTableCATree* tb, Eterm key,
                                              FindBaseNode* fbn)
{
    DbTableCATreeNode* base_node;

    while (1) {
        base_node = find_base_node(tb, key, fbn);
        wlock_base_node(base_node);
        if (base_node->u.base.is_valid)
            break;
        wunlock_base_node(base_node);
    }
    return base_node;
}

#ifdef ERTS_ENABLE_LOCK_CHECK
#  define LC_ORDER(ORDER) ORDER
#else
#  define LC_ORDER(ORDER) NIL
#endif

#define sizeof_base_node() \
          offsetof(DbTableCATreeNode, u.base.end_of_struct__)

static DbTableCATreeNode *create_base_node(DbTableCATree *tb,
                                           TreeDbTerm* root)
{
    DbTableCATreeNode *p;
    erts_rwmtx_opt_t rwmtx_opt = ERTS_RWMTX_OPT_DEFAULT_INITER;
    p = erts_db_alloc(ERTS_ALC_T_DB_TABLE, (DbTable *) tb,
                      sizeof_base_node());

    p->is_base_node = 1;
    p->u.base.root = root;
    if (tb->common.type & DB_FREQ_READ)
        rwmtx_opt.type = ERTS_RWMTX_TYPE_FREQUENT_READ;
    if (erts_ets_rwmtx_spin_count >= 0)
        rwmtx_opt.main_spincount = erts_ets_rwmtx_spin_count;

    erts_rwmtx_init_opt(&p->u.base.lock, &rwmtx_opt,
                        "erl_db_catree_base_node",
                        NIL,
                        ERTS_LOCK_FLAGS_CATEGORY_DB);
    BASE_NODE_STAT_SET(p, ((tb->common.status & DB_CATREE_FORCE_SPLIT)
                           ? INT_MAX : 0));
    p->u.base.is_valid = 1;
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
    erts_free(ERTS_ALC_T_DB_TABLE, p);
}

static void free_catree_base_node(DbTableCATree* tb, DbTableCATreeNode* p)
{
    ASSERT(p->is_base_node);
    ERTS_DB_ALC_MEM_UPDATE_(tb, sizeof_base_node(), 0);
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

static ERTS_INLINE
void init_tree_stack(DbTreeStack *stack,
                     TreeDbTerm **stack_array,
                     Uint init_slot)
{
    stack->array = stack_array;
    stack->pos = 0;
    stack->slot = init_slot;
}

static void join_catree(DbTableCATree *tb,
                        DbTableCATreeNode *thiz,
                        DbTableCATreeNode *parent)
{
    DbTableCATreeNode *gparent;
    DbTableCATreeNode *neighbor;
    DbTableCATreeNode *new_neighbor;
    DbTableCATreeNode *neighbor_parent;

    ASSERT(thiz->is_base_node);
    if (parent == NULL) {
        BASE_NODE_STAT_SET(thiz, 0);
        wunlock_base_node(thiz);
        return;
    }
    ASSERT(!parent->is_base_node);
    if (GET_LEFT(parent) == thiz) {
        neighbor = leftmost_base_node(GET_RIGHT_ACQB(parent));
        if (try_wlock_base_node(&neighbor->u.base)) {
            /* Failed to acquire lock */
            BASE_NODE_STAT_SET(thiz, 0);
            wunlock_base_node(thiz);
            return;
        } else if (!neighbor->u.base.is_valid) {
            BASE_NODE_STAT_SET(thiz, 0);
            wunlock_base_node(thiz);
            wunlock_base_node(neighbor);
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
                new_neighbor = create_base_node(tb, new_root);
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
            BASE_NODE_STAT_SET(thiz, 0);
            wunlock_base_node(thiz);
            return;
        } else if (!neighbor->u.base.is_valid) {
            BASE_NODE_STAT_SET(thiz, 0);
            wunlock_base_node(thiz);
            wunlock_base_node(neighbor);
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
                new_neighbor = create_base_node(tb, new_root);
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
    wunlock_base_node(thiz);
    wunlock_base_node(neighbor);
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
                          sizeof_base_node());
    erts_schedule_db_free(&tb->common,
                          do_free_base_node,
                          neighbor,
                          &neighbor->u.base.free_item,
                          sizeof_base_node());
}

static void split_catree(DbTableCATree *tb,
                         DbTableCATreeNode* ERTS_RESTRICT base,
                         DbTableCATreeNode* ERTS_RESTRICT parent)
{
    TreeDbTerm *splitOutWriteBack;
    DbTableCATreeNode* ERTS_RESTRICT new_left;
    DbTableCATreeNode* ERTS_RESTRICT new_right;
    DbTableCATreeNode* ERTS_RESTRICT new_route;

    if (less_than_two_elements(base->u.base.root)) {
        if (!(tb->common.status & DB_CATREE_FORCE_SPLIT))
            BASE_NODE_STAT_SET(base, 0);
        wunlock_base_node(base);
        return;
    } else {
        TreeDbTerm *left_tree;
        TreeDbTerm *right_tree;

        split_tree(tb, base->u.base.root, &splitOutWriteBack,
                   &left_tree, &right_tree);

        new_left = create_base_node(tb, left_tree);
        new_right = create_base_node(tb, right_tree);
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
        wunlock_base_node(base);
        erts_schedule_db_free(&tb->common,
                              do_free_base_node,
                              base,
                              &base->u.base.free_item,
                              sizeof_base_node());
    }
}

/** @brief Free the entire catree and its sub-trees.
 *
 * @param reds Reductions to spend.
 * @return Reductions left. Negative value if not done.
 */
static SWord db_free_table_continue_catree(DbTable *tbl, SWord reds)
{
    DbTableCATree *tb = &tbl->catree;
    DbTableCATreeNode *node;
    DbTableCATreeNode *parent;
    CATreeNodeStack rnode_stack;
    DbTableCATreeNode *rnode_stack_array[STACK_NEED];

    if (!tb->deletion) {
        /* First call */
        tb->deletion = 1;
        tb->nr_of_deleted_items = 0;
    }

    /*
     * The route tree is traversed and freed while keeping it consistent
     * during yields.
     */
    rnode_stack.array = rnode_stack_array;
    rnode_stack.pos = 0;
    rnode_stack.size = STACK_NEED;

    node = GET_ROOT(tb);
    if (node->is_base_node) {
        if (node->u.base.root) {
            reds = do_delete_base_node_cont(tb, node, reds);
            if (reds < 0)
                return reds; /* Yield */
        }
        free_catree_base_node(tb, node);
    }
    else {
        for (;;) {
            DbTableCATreeNode* left = GET_LEFT(node);
            DbTableCATreeNode* right = GET_RIGHT(node);

            if (!left->is_base_node) {
                PUSH_NODE(&rnode_stack, node);
                node = left;
            }
            else if (!right->is_base_node) {
                PUSH_NODE(&rnode_stack, node);
                node = right;
            }
            else {
                if (left->u.base.root) {
                    reds = do_delete_base_node_cont(tb, left, reds);
                    if (reds < 0)
                        return reds; /* Yield */
                }
                if (right->u.base.root) {
                    reds = do_delete_base_node_cont(tb, right, reds);
                    if (reds < 0)
                        return reds; /* Yield */
                }

                free_catree_base_node(tb, right);
                free_catree_route_node(tb, node);
                /*
                 * Keep empty left base node to join with its grandparent
                 * for tree consistency during yields.
                 */

                parent = POP_NODE(&rnode_stack);
                if (parent) {
                    if (node == GET_LEFT(parent)) {
                        SET_LEFT(parent, left);
                    }
                    else {
                        ASSERT(node == GET_RIGHT(parent));
                        SET_RIGHT(parent, left);
                    }

                    reds -= 2;
                    if (reds < 0)
                        return reds; /* Yield */

                    node = parent;
                }
                else {  /* Done */
                    free_catree_base_node(tb, left);
                    break;
                }
            }
        }
    }

    ASSERT(reds >= 0);
    SET_ROOT(tb, NULL);
    return reds;
}

/** @brief Free all objects of a base node, but keep the base node.
 *
 * @param reds Reductions to spend.
 * @return Reductions left. Negative value if not done.
 */
static SWord do_delete_base_node_cont(DbTableCATree *tb,
                                      DbTableCATreeNode *base_node,
                                      SWord reds)
{
    TreeDbTerm *p;
    DbTreeStack stack;
    TreeDbTerm* stack_array[STACK_NEED];

    stack.pos = 0;
    stack.array = stack_array;

    p = base_node->u.base.root;
    for (;;) {
        if (p->left) {
            PUSH_NODE(&stack, p);
            p = p->left;
        }
        else if (p->right) {
            PUSH_NODE(&stack, p);
            p = p->right;
        }
        else {
            TreeDbTerm *parent;

            DEC_NITEMS((DbTable*)tb);
            tb->nr_of_deleted_items++;
            free_term((DbTable*)tb, p);

            parent = POP_NODE(&stack);
            if (!parent)
                break;
            if (parent->left == p)
                parent->left = NULL;
            else {
                ASSERT(parent->right == p);
                parent->right = NULL;
            }
            if (--reds < 0)
                return reds;	/* Yield */
            p = parent;
        }
    }
    base_node->u.base.root = NULL;
    return reds;
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

    root = create_base_node(tb, NULL);
    tb->deletion = 0;
    tb->nr_of_deleted_items = 0;
#ifdef DEBUG
    tbl->common.status |= DB_CATREE_DEBUG_RANDOM_SPLIT_JOIN;
#endif
    erts_atomic_init_relb(&(tb->root), (erts_aint_t)root);
    return DB_ERROR_NONE;
}

static int db_first_catree(Process *p, DbTable *tbl, Eterm *ret)
{
    TreeDbTerm *root;
    CATreeRootIterator iter;
    int result;

    init_root_iterator(&tbl->catree, &iter, 1);
    root = *catree_find_first_root(&iter);
    if (!root) {
        TreeDbTerm **pp = catree_find_next_root(&iter, NULL);
        root = pp ? *pp : NULL;
    }

    result = db_first_tree_common(p, tbl, root, ret, NULL);

    destroy_root_iterator(&iter);
    return result;
}

static int db_next_catree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTreeStack stack;
    TreeDbTerm * stack_array[STACK_NEED];
    TreeDbTerm **rootp;
    CATreeRootIterator iter;
    int result;

    init_root_iterator(&tbl->catree, &iter, 1);
    iter.next_route_key = key;
    rootp = catree_find_next_root(&iter, NULL);

    do {
        init_tree_stack(&stack, stack_array, 0);
        result = db_next_tree_common(p, tbl, (rootp ? *rootp : NULL), key, ret, &stack);
        if (result != DB_ERROR_NONE || *ret != am_EOT)
            break;

        rootp = catree_find_next_root(&iter, NULL);
    } while (rootp);

    destroy_root_iterator(&iter);
    return result;
}

static int db_last_catree(Process *p, DbTable *tbl, Eterm *ret)
{
    TreeDbTerm *root;
    CATreeRootIterator iter;
    int result;

    init_root_iterator(&tbl->catree, &iter, 1);
    root = *catree_find_last_root(&iter);
    if (!root) {
        TreeDbTerm **pp = catree_find_prev_root(&iter, NULL);
        root = pp ? *pp : NULL;
    }

    result = db_last_tree_common(p, tbl, root, ret, NULL);

    destroy_root_iterator(&iter);
    return result;
}

static int db_prev_catree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTreeStack stack;
    TreeDbTerm * stack_array[STACK_NEED];
    TreeDbTerm **rootp;
    CATreeRootIterator iter;
    int result;

    init_root_iterator(&tbl->catree, &iter, 1);
    iter.next_route_key = key;
    rootp = catree_find_prev_root(&iter, NULL);

    do {
        init_tree_stack(&stack, stack_array, 0);
        result = db_prev_tree_common(p, tbl, (rootp ? *rootp : NULL), key, ret,
                                     &stack);
        if (result != DB_ERROR_NONE || *ret != am_EOT)
            break;
        rootp = catree_find_prev_root(&iter, NULL);
    } while (rootp);

    destroy_root_iterator(&iter);
    return result;
}

static int db_put_dbterm_catree(DbTable* tbl,
                                void* obj,
                                int key_clash_fail,
                                SWord *consumed_reds_p)
{
    TreeDbTerm *value_to_insert = obj;
    DbTableCATree *tb = &tbl->catree;
    Eterm key = GETKEY(tb, value_to_insert->dbterm.tpl);
    FindBaseNode fbn;
    DbTableCATreeNode* node = find_wlock_valid_base_node(tb, key, &fbn);
    int result = db_put_dbterm_tree_common(&tb->common,
                                           &node->u.base.root,
                                           value_to_insert,
                                           key_clash_fail,
                                           NULL);
    wunlock_adapt_base_node(tb, node, fbn.parent, fbn.current_level);
    return result;
}

static int db_put_catree(DbTable *tbl, Eterm obj, int key_clash_fail,
                         SWord *consumed_reds_p)
{
    DbTableCATree *tb = &tbl->catree;
    Eterm key = GETKEY(&tb->common, tuple_val(obj));
    FindBaseNode fbn;
    DbTableCATreeNode* node = find_wlock_valid_base_node(tb, key, &fbn);
    int result = db_put_tree_common(&tb->common, &node->u.base.root, obj,
                                    key_clash_fail, NULL);
    wunlock_adapt_base_node(tb, node, fbn.parent, fbn.current_level);
    return result;
}

static int db_get_catree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableCATree *tb = &tbl->catree;
    DbTableCATreeNode* node = find_rlock_valid_base_node(tb, key);
    int result = db_get_tree_common(p, &tb->common,
                                    node->u.base.root,
                                    key, ret, NULL);
    runlock_base_node(node, tb);
    return result;
}

TreeDbTerm** catree_find_root(Eterm key, CATreeRootIterator* iter)
{
    FindBaseNode fbn;
    DbTableCATreeNode* base_node;

    while (1) {
        base_node = find_base_node(iter->tb, key, &fbn);
        lock_iter_base_node(iter, base_node, fbn.parent, fbn.current_level);
        if (base_node->u.base.is_valid)
            break;
        unlock_iter_base_node(iter);
    }
    return &base_node->u.base.root;
}

static Eterm save_iter_search_key(CATreeRootIterator* iter, Eterm key)
{
    Uint key_size;

    if (is_immed(key))
        return key;

    if (iter->search_key) {
        if (key == iter->search_key->term)
            return key; /* already saved */
        destroy_route_key(iter->search_key);
    }
    key_size = size_object(key);
    if (!iter->search_key || key_size > iter->search_key->size) {
        iter->search_key = erts_realloc(ERTS_ALC_T_DB_TMP,
                                        iter->search_key,
                                        (offsetof(DbRouteKey, heap)
                                         + key_size*sizeof(Eterm)));
    }
    return copy_route_key(iter->search_key, key, key_size);
}

TreeDbTerm** catree_find_nextprev_root(CATreeRootIterator *iter,
                                       int forward,
                                       Eterm *search_keyp)
{
#ifdef DEBUG
    DbTableCATreeNode *rejected_invalid = NULL;
    DbTableCATreeNode *rejected_empty = NULL;
#endif
    DbTableCATreeNode *node;
    DbTableCATreeNode *parent;
    DbTableCATreeNode* next_route_node;
    Eterm route_key = iter->next_route_key;
    int current_level;

    if (iter->locked_bnode) {
        if (search_keyp)
            *search_keyp = save_iter_search_key(iter, *search_keyp);
        unlock_iter_base_node(iter);
    }

    if (is_non_value(route_key))
        return NULL;

    while (1) {
        node = GET_ROOT_ACQB(iter->tb);
        current_level = 0;
        parent = NULL;
        next_route_node = NULL;
        while (!node->is_base_node) {
            current_level++;
            parent = node;
            if (forward) {
                if (cmp_key_route(route_key,node) < 0) {
                    next_route_node = node;
                    node = GET_LEFT_ACQB(node);
                } else {
                    node = GET_RIGHT_ACQB(node);
                }
            }
            else {
                if (cmp_key_route(route_key,node) > 0) {
                    next_route_node = node;
                    node = GET_RIGHT_ACQB(node);
                } else {
                    node = GET_LEFT_ACQB(node);
                }
            }
        }
        ASSERT(node != rejected_invalid);
        lock_iter_base_node(iter, node, parent, current_level);
        if (node->u.base.is_valid) {
            ASSERT(node != rejected_empty);
            if (node->u.base.root) {
                iter->next_route_key = (next_route_node ?
                                        next_route_node->u.route.key.term :
                                        THE_NON_VALUE);
                iter->locked_bnode = node;
                return &node->u.base.root;
            }
            if (!next_route_node) {
                unlock_iter_base_node(iter);
                return NULL;
            }
            route_key = next_route_node->u.route.key.term;
            IF_DEBUG(rejected_empty = node);
        }
        else
            IF_DEBUG(rejected_invalid = node);

        /* Retry */
        unlock_iter_base_node(iter);
    }
}

TreeDbTerm** catree_find_next_root(CATreeRootIterator *iter, Eterm* keyp)
{
    return catree_find_nextprev_root(iter, 1, keyp);
}

TreeDbTerm** catree_find_prev_root(CATreeRootIterator *iter, Eterm* keyp)
{
    return catree_find_nextprev_root(iter, 0, keyp);
}

/** @brief Find root of tree where object with smallest key of all larger than
 * partially bound key may reside. Can be used as a starting point for
 * a reverse iteration with pb_key.
 *
 * @param pb_key The partially bound key. Example {42, '$1'}
 * @param iter An initialized root iterator.
 *
 * @return Pointer to found root pointer. May not be NULL.
 */
TreeDbTerm** catree_find_next_from_pb_key_root(Eterm pb_key,
                                               CATreeRootIterator* iter)
{
#ifdef DEBUG
    DbTableCATreeNode *rejected_base = NULL;
#endif
    DbTableCATreeNode *node;
    DbTableCATreeNode *parent;
    DbTableCATreeNode* next_route_node;
    int current_level;

    ASSERT(!iter->locked_bnode);

    while (1) {
        node = GET_ROOT_ACQB(iter->tb);
        current_level = 0;
        parent = NULL;
        next_route_node = NULL;
        while (!node->is_base_node) {
            current_level++;
            parent = node;
            if (cmp_partly_bound(pb_key, GET_ROUTE_NODE_KEY(node)) >= 0) {
                next_route_node = node;
                node = GET_RIGHT_ACQB(node);
            } else {
                node = GET_LEFT_ACQB(node);
            }
        }
        ASSERT(node != rejected_base);
        lock_iter_base_node(iter, node, parent, current_level);
        if (node->u.base.is_valid) {
            iter->next_route_key = (next_route_node ?
                                    next_route_node->u.route.key.term :
                                    THE_NON_VALUE);
            return &node->u.base.root;
        }
        /* Retry */
        unlock_iter_base_node(iter);
#ifdef DEBUG
        rejected_base = node;
#endif
    }
}

/** @brief Find root of tree where object with largest key of all smaller than
 * partially bound key may reside. Can be used as a starting point for
 * a forward iteration with pb_key.
 *
 * @param pb_key The partially bound key. Example {42, '$1'}
 * @param iter An initialized root iterator.
 *
 * @return Pointer to found root pointer. May not be NULL.
 */
TreeDbTerm** catree_find_prev_from_pb_key_root(Eterm key,
                                               CATreeRootIterator* iter)
{
#ifdef DEBUG
    DbTableCATreeNode *rejected_base = NULL;
#endif
    DbTableCATreeNode *node;
    DbTableCATreeNode *parent;
    DbTableCATreeNode* next_route_node;
    int current_level;

    ASSERT(!iter->locked_bnode);

    while (1) {
        node = GET_ROOT_ACQB(iter->tb);
        current_level = 0;
        parent = NULL;
        next_route_node = NULL;
        while (!node->is_base_node) {
            current_level++;
            parent = node;
            if (cmp_partly_bound(key, GET_ROUTE_NODE_KEY(node)) <= 0) {
                next_route_node = node;
                node = GET_LEFT_ACQB(node);
            } else {
                node = GET_RIGHT_ACQB(node);
            }
        }
        ASSERT(node != rejected_base);
        lock_iter_base_node(iter, node, parent, current_level);
        if (node->u.base.is_valid) {
            iter->next_route_key = (next_route_node ?
                                    next_route_node->u.route.key.term :
                                    THE_NON_VALUE);
            return &node->u.base.root;
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
    int current_level;

    while (1) {
        node = GET_ROOT_ACQB(iter->tb);
        current_level = 0;
        next_route_node = NULL;
        while (!node->is_base_node) {
            current_level++;
            next_route_node = node;
            node = first ? GET_LEFT_ACQB(node) : GET_RIGHT_ACQB(node);
        }
        ASSERT(node != rejected_base);
        lock_iter_base_node(iter, node, next_route_node, current_level);
        if (node->u.base.is_valid) {
            iter->next_route_key = (next_route_node ?
                                    next_route_node->u.route.key.term :
                                    THE_NON_VALUE);
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

static int db_member_catree(DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableCATree *tb = &tbl->catree;
    DbTableCATreeNode* node = find_rlock_valid_base_node(tb, key);
    int result = db_member_tree_common(&tb->common,
                                       node->u.base.root,
                                       key, ret, NULL);
    runlock_base_node(node, tb);
    return result;
}

static int db_get_element_catree(Process *p, DbTable *tbl,
			       Eterm key, int ndex, Eterm *ret)
{
    DbTableCATree *tb = &tbl->catree;
    DbTableCATreeNode* node = find_rlock_valid_base_node(tb, key);
    int result = db_get_element_tree_common(p, &tb->common,
                                            node->u.base.root,
                                            key, ndex, ret, NULL);
    runlock_base_node(node, tb);
    return result;
}

static int db_erase_catree(DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableCATree *tb = &tbl->catree;
    FindBaseNode fbn;
    DbTableCATreeNode* node = find_wlock_valid_base_node(tb, key, &fbn);
    int result = db_erase_tree_common(tbl, &node->u.base.root, key,
                                      ret, NULL);
    wunlock_adapt_base_node(tb, node, fbn.parent, fbn.current_level);
    return result;
}

static int db_erase_object_catree(DbTable *tbl, Eterm object, Eterm *ret)
{
    DbTableCATree *tb = &tbl->catree;
    Eterm key = GETKEY(&tb->common, tuple_val(object));
    FindBaseNode fbn;
    DbTableCATreeNode* node = find_wlock_valid_base_node(tb, key, &fbn);
    int result = db_erase_object_tree_common(tbl,
                                             &node->u.base.root,
                                             object,
                                             ret,
                                             NULL);
    wunlock_adapt_base_node(tb, node, fbn.parent, fbn.current_level);
    return result;
}


static int db_slot_catree(Process *p, DbTable *tbl,
                          Eterm slot_term, Eterm *ret)
{
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 1);
    result = db_slot_tree_common(p, tbl, *catree_find_first_root(&iter),
                                 slot_term, ret, NULL, &iter);
    destroy_root_iterator(&iter);
    return result;
}

static int db_select_continue_catree(Process *p,
                                     DbTable *tbl,
                                     Eterm continuation,
                                     Eterm *ret,
                                     enum DbIterSafety* safety_p)
{
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 1);
    result = db_select_continue_tree_common(p, &tbl->common,
                                            continuation, ret, NULL, &iter);
    destroy_root_iterator(&iter);
    return result;
}

static int db_select_catree(Process *p, DbTable *tbl, Eterm tid,
                            Eterm pattern, int reverse, Eterm *ret,
                            enum DbIterSafety safety)
{
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 1);
    result = db_select_tree_common(p, tbl, tid, pattern, reverse, ret,
                                   NULL, &iter);
    destroy_root_iterator(&iter);
    return result;
}

static int db_select_count_continue_catree(Process *p,
                                           DbTable *tbl,
                                           Eterm continuation,
                                           Eterm *ret,
                                           enum DbIterSafety* safety_p)
{
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 1);
    result = db_select_count_continue_tree_common(p, tbl,
                                                  continuation, ret, NULL,
                                                  &iter);
    destroy_root_iterator(&iter);
    return result;
}

static int db_select_count_catree(Process *p, DbTable *tbl, Eterm tid,
                                  Eterm pattern, Eterm *ret,
                                  enum DbIterSafety safety)
{
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 1);
    result = db_select_count_tree_common(p, tbl,
                                         tid, pattern, ret, NULL, &iter);
    destroy_root_iterator(&iter);
    return result;
}

static int db_select_chunk_catree(Process *p, DbTable *tbl, Eterm tid,
                                  Eterm pattern, Sint chunk_size,
                                  int reversed, Eterm *ret,
                                  enum DbIterSafety safety)
{
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 1);
    result = db_select_chunk_tree_common(p, tbl,
                                         tid, pattern, chunk_size, reversed, ret,
                                         NULL, &iter);
    destroy_root_iterator(&iter);
    return result;
}

static int db_select_delete_continue_catree(Process *p,
                                            DbTable *tbl,
                                            Eterm continuation,
                                            Eterm *ret,
                                            enum DbIterSafety* safety_p)
{
    DbTreeStack stack;
    TreeDbTerm * stack_array[STACK_NEED];
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 0);
    init_tree_stack(&stack, stack_array, 0);
    result = db_select_delete_continue_tree_common(p, tbl, continuation, ret,
                                                   &stack, &iter);
    destroy_root_iterator(&iter);
    return result;
}

static int db_select_delete_catree(Process *p, DbTable *tbl, Eterm tid,
                                   Eterm pattern, Eterm *ret,
                                   enum DbIterSafety safety)
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
    destroy_root_iterator(&iter);
    return result;
}

static int db_select_replace_catree(Process *p, DbTable *tbl, Eterm tid,
                                    Eterm pattern, Eterm *ret,
                                    enum DbIterSafety safety_p)
{
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 0);
    result = db_select_replace_tree_common(p, tbl,
                                           tid, pattern, ret, NULL, &iter);
    destroy_root_iterator(&iter);
    return result;
}

static int db_select_replace_continue_catree(Process *p, DbTable *tbl,
                                             Eterm continuation, Eterm *ret,
                                             enum DbIterSafety* safety_p)
{
    int result;
    CATreeRootIterator iter;

    init_root_iterator(&tbl->catree, &iter, 0);
    result = db_select_replace_continue_tree_common(p, tbl, continuation, ret,
                                                    NULL, &iter);
    destroy_root_iterator(&iter);
    return result;
}

static int db_take_catree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableCATree *tb = &tbl->catree;
    FindBaseNode fbn;
    DbTableCATreeNode* node = find_wlock_valid_base_node(tb, key, &fbn);
    int result = db_take_tree_common(p, tbl, &node->u.base.root, key,
                                     ret, NULL);
    wunlock_adapt_base_node(tb, node, fbn.parent, fbn.current_level);
    return result;
}

/*
** Other interface routines (not directly coupled to one bif)
*/


/* Display tree contents (for dump) */
static void db_print_catree(fmtfn_t to, void *to_arg,
                            int show, DbTable *tbl)
{
    CATreeRootIterator iter;
    TreeDbTerm** root;

    init_root_iterator(&tbl->catree, &iter, 1);
    root = catree_find_first_root(&iter);
    do {
        db_print_tree_common(to, to_arg, show, *root, tbl);
        root = catree_find_next_root(&iter, NULL);
    } while (root);
    destroy_root_iterator(&iter);
}

/* Release all memory occupied by a single table */
static int db_free_table_catree(DbTable *tbl)
{
    while (db_free_table_continue_catree(tbl, ERTS_SWORD_MAX) < 0)
	;
    return 1;
}

static
int db_catree_nr_of_items_deleted_wb_dtor(Binary *context_bin) {
    (void)context_bin;
    return 1;
}

typedef struct {
    Uint nr_of_deleted_items;
} DbCATreeNrOfItemsDeletedWb;

static Eterm
create_and_install_num_of_deleted_items_wb_bin(Process *p, DbTableCATree *tb)
{
    Binary* bin =
        erts_create_magic_binary(sizeof(DbCATreeNrOfItemsDeletedWb),
                                 db_catree_nr_of_items_deleted_wb_dtor);
    DbCATreeNrOfItemsDeletedWb* data = ERTS_MAGIC_BIN_DATA(bin);
    Eterm* hp = HAlloc(p, ERTS_MAGIC_REF_THING_SIZE);
    Eterm mref = erts_mk_magic_ref(&hp, &MSO(p), bin);
    data->nr_of_deleted_items = 0;
    tb->nr_of_deleted_items_wb = bin;
    erts_refc_inctest(&bin->intern.refc, 2);   
    return mref;
}

static Eterm db_delete_all_objects_get_nitems_from_holder_catree(Process* p,
                                                                 Eterm mref)
{
    Binary* bin = erts_magic_ref2bin(mref);
    DbCATreeNrOfItemsDeletedWb* data = ERTS_MAGIC_BIN_DATA(bin);
    return erts_make_integer(data->nr_of_deleted_items, p);
}

static SWord db_delete_all_objects_catree(Process* p,
                                          DbTable* tbl,
                                          SWord reds,
                                          Eterm* nitems_holder_wb)
{
    DbTableCATree *tb = &tbl->catree;
    DbCATreeNrOfItemsDeletedWb* data;
    if (!tb->deletion) {
        *nitems_holder_wb =
            create_and_install_num_of_deleted_items_wb_bin(p, tb);
    }
    reds = db_free_table_continue_catree(tbl, reds);
    if (reds < 0)
        return reds;
    data = ERTS_MAGIC_BIN_DATA(tb->nr_of_deleted_items_wb);
    data->nr_of_deleted_items = tb->nr_of_deleted_items;
    erts_bin_release(tb->nr_of_deleted_items_wb);
    db_create_catree(p, tbl);
    return reds;
}


static void do_for_route_nodes(DbTableCATreeNode* node,
                               void (*func)(ErlOffHeap *, void *),
                               void *arg)
{
    ErlOffHeap tmp_offheap;

    if (!GET_LEFT(node)->is_base_node)
        do_for_route_nodes(GET_LEFT(node), func, arg);

    tmp_offheap.first = node->u.route.key.oh;
    tmp_offheap.overhead = 0;
    (*func)(&tmp_offheap, arg);

    if (!GET_RIGHT(node)->is_base_node)
        do_for_route_nodes(GET_RIGHT(node), func, arg);
}

static void db_foreach_offheap_catree(DbTable *tbl,
                                      void (*func)(ErlOffHeap *, void *),
                                      void *arg)
{
    DbTableCATree* tb = &tbl->catree;
    CATreeRootIterator iter;
    TreeDbTerm** root;

    if (!GET_ROOT(tb)) {
        ASSERT(tb->common.status & DB_DELETE);
        return;
    }
    init_root_iterator(tb, &iter, 1);
    root = catree_find_first_root(&iter);
    do {
        db_foreach_offheap_tree_common(*root, func, arg);
        root = catree_find_next_root(&iter, NULL);
    } while (root);
    destroy_root_iterator(&iter);

    do_for_route_nodes(GET_ROOT(tb), func, arg);
}

static int db_lookup_dbterm_catree(Process *p, DbTable *tbl, Eterm key, Eterm obj,
                                   DbUpdateHandle *handle)
{
    DbTableCATree *tb = &tbl->catree;
    FindBaseNode fbn;
    DbTableCATreeNode* node = find_wlock_valid_base_node(tb, key, &fbn);
    int res = db_lookup_dbterm_tree_common(p, tbl, &node->u.base.root, key,
                                           obj, handle, NULL);
    if (res == 0) {
        wunlock_adapt_base_node(tb, node, fbn.parent, fbn.current_level);
    } else {
        /* db_finalize_dbterm_catree will unlock */
        handle->u.catree.base_node = node;
        handle->u.catree.parent = fbn.parent;
        handle->u.catree.current_level = fbn.current_level;
    }
    return res;
}

static void db_finalize_dbterm_catree(int cret, DbUpdateHandle *handle)
{
    DbTableCATree *tb = &(handle->tb->catree);
    db_finalize_dbterm_tree_common(cret,
                                   handle,
                                   &handle->u.catree.base_node->u.base.root,
                                   NULL);
    wunlock_adapt_base_node(tb, handle->u.catree.base_node,
                            handle->u.catree.parent,
                            handle->u.catree.current_level);
    return;
}

static int db_get_binary_info_catree(Process *p, DbTable *tbl, Eterm key,
                                     Eterm *ret)
{
    DbTableCATree *tb = &tbl->catree;
    FindBaseNode fbn;
    DbTableCATreeNode* node = find_wlock_valid_base_node(tb, key, &fbn);
    TreeDbTerm* this = db_find_tree_node_common(&tbl->common, node->u.base.root,
                                                key);
    *ret = db_binary_info_tree_common(p, this);
    wunlock_base_node(node);
    return DB_ERROR_NONE;
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

void db_catree_force_split(DbTableCATree* tb, int on)
{
    CATreeRootIterator iter;
    TreeDbTerm** root;

    init_root_iterator(tb, &iter, 1);
    root = catree_find_first_root(&iter);
    do {
        BASE_NODE_STAT_SET(iter.locked_bnode, (on ? INT_MAX : 0));
        root = catree_find_next_root(&iter, NULL);
    } while (root);
    destroy_root_iterator(&iter);

    if (on)
        tb->common.status |= DB_CATREE_FORCE_SPLIT;
    else
        tb->common.status &= ~DB_CATREE_FORCE_SPLIT;
}

void db_catree_debug_random_split_join(DbTableCATree* tb, int on)
{
    if (on)
        tb->common.status |= DB_CATREE_DEBUG_RANDOM_SPLIT_JOIN;
    else
        tb->common.status &= ~DB_CATREE_DEBUG_RANDOM_SPLIT_JOIN;
}

void db_calc_stats_catree(DbTableCATree* tb, DbCATreeStats* stats)
{
    DbTableCATreeNode* stack[ERL_DB_CATREE_MAX_ROUTE_NODE_LAYER_HEIGHT];
    DbTableCATreeNode* node;
    Uint depth = 0;

    stats->route_nodes = 0;
    stats->base_nodes = 0;
    stats->max_depth = 0;

    node = GET_ROOT(tb);
    do {
        while (!node->is_base_node) {
            stats->route_nodes++;
            ASSERT(depth < sizeof(stack)/sizeof(*stack));
            stack[depth++] = node;  /* PUSH parent */
            if (stats->max_depth < depth)
                stats->max_depth = depth;
            node = GET_LEFT(node);
        }
        stats->base_nodes++;

        while (depth > 0) {
            DbTableCATreeNode* parent = stack[depth-1];
            if (node == GET_LEFT(parent)) {
                node = GET_RIGHT(parent);
                break;
            }
            else {
                ASSERT(node == GET_RIGHT(parent));
                node = parent;
                depth--; /* POP parent */
            }
        }
    } while (depth > 0);
}

struct debug_catree_fa {
    void (*func)(ErlOffHeap *, void *);
    void *arg;
};

static void debug_free_route_node(void *vfap, ErtsThrPrgrVal val, void *vnp)
{
    DbTableCATreeNode *np = vnp;
    if (np->u.route.key.oh) {
        struct debug_catree_fa *fap = vfap;
        ErlOffHeap oh;
        ERTS_INIT_OFF_HEAP(&oh);
        oh.first = np->u.route.key.oh;
        (*fap->func)(&oh, fap->arg);
    }
}

void
erts_db_foreach_thr_prgr_offheap_catree(void (*func)(ErlOffHeap *, void *),
                                        void *arg)
{
    struct debug_catree_fa fa;
    fa.func = func;
    fa.arg = arg;
    erts_debug_later_op_foreach(do_free_route_node, debug_free_route_node, &fa);
}


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
