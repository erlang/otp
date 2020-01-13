/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2018. All Rights Reserved.
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
** Implementation of ordered ETS tables.
** The tables are implemented as AVL trees (Published by Adelson-Velski 
** and Landis). A nice source for learning about these trees is
** Wirth's Algorithms + Datastructures = Programs.
** The implementation here is however not made with recursion
** as the examples in Wirths book are.
*/

/*
#ifdef DEBUG
#define HARDDEBUG 1
#endif
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

#include "erl_db_tree.h"
#include "erl_db_tree_util.h"

#define GETKEY_WITH_POS(Keypos, Tplp) (*((Tplp) + Keypos))

#define NITEMS_CENTRALIZED(tb)                                          \
    ((Sint)erts_flxctr_read_centralized(&(tb)->common.counters,          \
                                        ERTS_DB_TABLE_NITEMS_COUNTER_ID))
#define ADD_NITEMS(DB, TO_ADD)                                          \
    erts_flxctr_add(&(DB)->common.counters, ERTS_DB_TABLE_NITEMS_COUNTER_ID, TO_ADD)
#define INC_NITEMS(DB)                                                  \
    erts_flxctr_inc(&(DB)->common.counters, ERTS_DB_TABLE_NITEMS_COUNTER_ID)
#define INC_NITEMS_CENTRALIZED(DB)                                      \
    erts_flxctr_inc_read_centralized(&(DB)->common.counters, ERTS_DB_TABLE_NITEMS_COUNTER_ID)
#define RESET_NITEMS(DB)                                                \
    erts_flxctr_reset(&(DB)->common.counters, ERTS_DB_TABLE_NITEMS_COUNTER_ID)
#define IS_CENTRALIZED_CTR(tb) (!(tb)->common.counters.is_decentralized)
#define APPROX_MEM_CONSUMED(tb) erts_flxctr_read_approx(&(tb)->common.counters, ERTS_DB_TABLE_MEM_COUNTER_ID)

#define TOPN_NODE(Dtt, Pos)                   \
     (((Pos) < Dtt->pos) ? 			\
      (Dtt)->array[(Dtt)->pos - ((Pos) + 1)] : NULL)

#define REPLACE_TOP_NODE(Dtt, Node)          \
     if ((Dtt)->pos) (Dtt)->array[(Dtt)->pos - 1] = (Node)

#define EMPTY_NODE(Dtt) (TOP_NODE(Dtt) == NULL)


/* Obtain table static stack if available. NULL if not.
** Must be released with release_stack()
*/
ERTS_INLINE static DbTreeStack* get_static_stack(DbTableTree* tb)
{
    if (tb != NULL) {
        ASSERT(IS_TREE_TABLE(tb->common.type));
        if (!erts_atomic_xchg_acqb(&tb->is_stack_busy, 1))
            return &tb->static_stack;
    }
    return NULL;
}

/* Obtain static stack if available, otherwise empty dynamic stack.
** Must be released with release_stack()
*/
static DbTreeStack* get_any_stack(DbTable* tb, DbTableTree* stack_container)
{
    DbTreeStack* stack;
    if (stack_container != NULL) {
        ASSERT(IS_TREE_TABLE(stack_container->common.type));
        if (!erts_atomic_xchg_acqb(&stack_container->is_stack_busy, 1))
            return &stack_container->static_stack;
    }
    stack = erts_db_alloc(ERTS_ALC_T_DB_STK, tb,
			  sizeof(DbTreeStack) + sizeof(TreeDbTerm*) * STACK_NEED);
    stack->pos = 0;
    stack->slot = 0;
    stack->array = (TreeDbTerm**) (stack + 1);
    return stack;
}

static void release_stack(DbTable* tb, DbTableTree* stack_container, DbTreeStack* stack)
{
    if (stack_container != NULL) {
        ASSERT(IS_TREE_TABLE(stack_container->common.type));
        if (stack == &stack_container->static_stack) {
            ASSERT(erts_atomic_read_nob(&stack_container->is_stack_busy) == 1);
            erts_atomic_set_relb(&stack_container->is_stack_busy, 0);
            return;
        }
    }
    erts_db_free(ERTS_ALC_T_DB_STK, tb,
                 (void *) stack, sizeof(DbTreeStack) + sizeof(TreeDbTerm*) * STACK_NEED);
}

static ERTS_INLINE void reset_stack(DbTreeStack* stack)
{
    if (stack != NULL) {
        stack->pos = 0;
        stack->slot = 0;
    }
}

static ERTS_INLINE void reset_static_stack(DbTableTree* tb)
{
    if (tb != NULL) {
        ASSERT(IS_TREE_TABLE(tb->common.type));
        reset_stack(&tb->static_stack);
    }
}

static ERTS_INLINE TreeDbTerm* new_dbterm(DbTableCommon *tb, Eterm obj)
{
    TreeDbTerm* p;
    if (tb->compress) {
	p = db_store_term_comp(tb, NULL, offsetof(TreeDbTerm,dbterm), obj);
    }
    else {
	p = db_store_term(tb, NULL, offsetof(TreeDbTerm,dbterm), obj);
    }
    return p;
}
static ERTS_INLINE TreeDbTerm* replace_dbterm(DbTableCommon *tb, TreeDbTerm* old,
					      Eterm obj)
{
    TreeDbTerm* p;
    ASSERT(old != NULL);
    if (tb->compress) {
	p = db_store_term_comp(tb, &(old->dbterm), offsetof(TreeDbTerm,dbterm), obj);
    }
    else {
	p = db_store_term(tb, &(old->dbterm), offsetof(TreeDbTerm,dbterm), obj);
    }
    return p;
}

/*
 * Number of records to delete before trapping.
 */
#define DELETE_RECORD_LIMIT 12000

/* 
** Debugging
*/
#ifdef HARDDEBUG
static TreeDbTerm *traverse_until(TreeDbTerm *t, int *current, int to);
static void check_slot_pos(DbTableTree *tb);
static void check_saved_stack(DbTableTree *tb);

#define TREE_DEBUG
#endif

#ifdef TREE_DEBUG
/*
** Primitive trace macro
*/
#define DBG erts_fprintf(stderr,"%d\n",__LINE__)

/*
** Debugging dump
*/

static void do_dump_tree2(DbTableTree*, int to, void *to_arg, int show,
			  TreeDbTerm *t, int offset);

#else

#define DBG /* nothing */

#endif

/*
** Datatypes
*/

enum ms_key_boundness {
    /* Order significant, larger means more "boundness" => less iteration */
    MS_KEY_UNBOUND           = 0,
    MS_KEY_PARTIALLY_BOUND   = 1,
    MS_KEY_BOUND             = 2,
    MS_KEY_IMPOSSIBLE        = 3
};

/* 
 * This structure is filled in by analyze_pattern() for the select 
 * functions.
 */
struct mp_info {
    enum ms_key_boundness key_boundness;
    Eterm least;		/* The lowest matching key (possibly 
				 * partially bound expression) */
    Eterm most;                 /* The highest matching key (possibly 
				 * partially bound expression) */
    Binary *mp;                 /* The compiled match program */
};

struct select_common {
    TreeDbTerm **root;
};


/*
 * Used by doit_select(_chunk)
 */
struct select_context {
    struct select_common common;
    Process *p;
    Eterm accum;
    Binary *mp;
    Eterm end_condition;
    Eterm *lastobj;
    Sint32 max;
    int keypos;
    Sint got;
    Sint chunk_size;
};

/*
 * Used by doit_select_count
 */
struct select_count_context {
    struct select_common common;
    Process *p;
    Binary *mp;
    Eterm end_condition;
    Eterm *lastobj;
    Sint32 max;
    int keypos;
    Sint got;
};

/*
 * Used by doit_select_delete
 */
struct select_delete_context {
    struct select_common common;
    Process *p;
    DbTableCommon *tb;
    DbTreeStack *stack;
    Uint accum;
    Binary *mp;
    Eterm end_condition;
    int erase_lastterm;
    TreeDbTerm *lastterm;
    Sint32 max;
    int keypos;
};

/*
 * Used by doit_select_replace
 */
struct select_replace_context {
    struct select_common common;
    Process *p;
    DbTableCommon *tb;
    Binary *mp;
    Eterm end_condition;
    Eterm *lastobj;
    Sint32 max;
    int keypos;
    Sint replaced;
};

/* Used by select_replace on analyze_pattern */
typedef int (*extra_match_validator_t)(int keypos, Eterm match, Eterm guard, Eterm body);

/*
** Forward declarations 
*/
static TreeDbTerm *linkout_tree(DbTableCommon *tb, TreeDbTerm **root,
                                Eterm key, DbTreeStack *stack);
static TreeDbTerm *linkout_object_tree(DbTableCommon *tb,  TreeDbTerm **root,
				       Eterm object, DbTableTree *stack);
static SWord do_free_tree_continue(DbTableTree *tb, SWord reds);
static void free_term(DbTable *tb, TreeDbTerm* p);
int tree_balance_left(TreeDbTerm **this); 
int tree_balance_right(TreeDbTerm **this); 
static int delsub(TreeDbTerm **this); 
static TreeDbTerm *slot_search(Process *p, TreeDbTerm *root, Sint slot,
                               DbTable *tb, DbTableTree *stack_container,
                               CATreeRootIterator *iter, int* is_EOT);
static TreeDbTerm *find_node(DbTableCommon *tb, TreeDbTerm *root,
                             Eterm key, DbTableTree *stack_container);
static TreeDbTerm **find_node2(DbTableCommon *tb, TreeDbTerm **root, Eterm key);
static TreeDbTerm **find_ptr(DbTableCommon *tb, TreeDbTerm **root,
                             DbTreeStack *stack, TreeDbTerm *this);
static TreeDbTerm *find_next(DbTableCommon *tb, TreeDbTerm *root,
                             DbTreeStack* stack, Eterm key);
static TreeDbTerm *find_prev(DbTableCommon *tb, TreeDbTerm *root,
                             DbTreeStack* stack, Eterm key);
static TreeDbTerm *find_next_from_pb_key(DbTable*, TreeDbTerm*** rootpp,
                                         DbTreeStack* stack, Eterm key,
                                         CATreeRootIterator*);
static TreeDbTerm *find_prev_from_pb_key(DbTable*,  TreeDbTerm*** rootpp,
                                         DbTreeStack* stack, Eterm key,
                                         CATreeRootIterator*);
typedef int traverse_doit_funcT(DbTableCommon*, TreeDbTerm*,
                                struct select_common*, int forward);

static void traverse_backwards(DbTableCommon *tb,
			       DbTreeStack*,
			       Eterm lastkey,
			       traverse_doit_funcT*,
			       struct select_common *context,
                               CATreeRootIterator*);
static void traverse_forward(DbTableCommon *tb,
			     DbTreeStack*,
			     Eterm lastkey,
                             traverse_doit_funcT*,
			     struct select_common *context,
                             CATreeRootIterator*);
static void traverse_update_backwards(DbTableCommon *tb,
                                      DbTreeStack*,
                                      Eterm lastkey,
                                      int (*doit)(DbTableCommon *tb,
                                                  TreeDbTerm **, // out
                                                  struct select_common*,
                                                  int),
                                      struct select_common*,
                                      CATreeRootIterator*);
static enum ms_key_boundness key_boundness(DbTableCommon *tb,
                                           Eterm pattern, Eterm *keyp);
static Sint do_cmp_partly_bound(Eterm a, Eterm b, int *done);

static int analyze_pattern(DbTableCommon *tb, Eterm pattern,
                           extra_match_validator_t extra_validator, /* Optional callback */
                           struct mp_info *mpi);
static int doit_select(DbTableCommon *tb,
                       TreeDbTerm *this,
                       struct select_common* ptr,
		       int forward);
static int doit_select_count(DbTableCommon *tb,
			     TreeDbTerm *this,
                             struct select_common*,
			     int forward);
static int doit_select_chunk(DbTableCommon *tb,
			     TreeDbTerm *this,
                             struct select_common*,
			     int forward);
static int doit_select_delete(DbTableCommon *tb,
			      TreeDbTerm *this,
			      struct select_common*,
			      int forward);
static int doit_select_replace(DbTableCommon *tb,
                               TreeDbTerm **this_ptr,
                               struct select_common*,
                               int forward);

static int partly_bound_can_match_lesser(Eterm partly_bound_1, 
					 Eterm partly_bound_2);
static int partly_bound_can_match_greater(Eterm partly_bound_1, 
					  Eterm partly_bound_2); 
static int do_partly_bound_can_match_lesser(Eterm a, Eterm b, 
					    int *done);
static int do_partly_bound_can_match_greater(Eterm a, Eterm b, 
					     int *done);
static BIF_RETTYPE ets_select_reverse(BIF_ALIST_3);


/* Method interface functions */
static int db_first_tree(Process *p, DbTable *tbl, 
		  Eterm *ret);
static int db_next_tree(Process *p, DbTable *tbl, 
			Eterm key, Eterm *ret);
static int db_last_tree(Process *p, DbTable *tbl, 
			Eterm *ret);
static int db_prev_tree(Process *p, DbTable *tbl, 
			Eterm key,
			Eterm *ret);
static int db_put_tree(DbTable *tbl, Eterm obj, int key_clash_fail);
static int db_get_tree(Process *p, DbTable *tbl, 
		       Eterm key,  Eterm *ret);
static int db_member_tree(DbTable *tbl, Eterm key, Eterm *ret);
static int db_get_element_tree(Process *p, DbTable *tbl, 
			       Eterm key,int ndex,
			       Eterm *ret);
static int db_erase_tree(DbTable *tbl, Eterm key, Eterm *ret);
static int db_erase_object_tree(DbTable *tbl, Eterm object,Eterm *ret);
static int db_slot_tree(Process *p, DbTable *tbl, 
			Eterm slot_term,  Eterm *ret);
static int db_select_tree(Process *p, DbTable *tbl, Eterm tid,
			  Eterm pattern, int reversed, Eterm *ret,
                          enum DbIterSafety);
static int db_select_count_tree(Process *p, DbTable *tbl, Eterm tid,
				Eterm pattern,  Eterm *ret, enum DbIterSafety);
static int db_select_chunk_tree(Process *p, DbTable *tbl, Eterm tid,
				Eterm pattern, Sint chunk_size,
				int reversed, Eterm *ret, enum DbIterSafety);
static int db_select_continue_tree(Process *p, DbTable *tbl,
				   Eterm continuation, Eterm *ret,
                                   enum DbIterSafety*);
static int db_select_count_continue_tree(Process *p, DbTable *tbl,
					 Eterm continuation, Eterm *ret,
                                         enum DbIterSafety*);
static int db_select_delete_tree(Process *p, DbTable *tbl, Eterm tid,
				 Eterm pattern,  Eterm *ret,
                                 enum DbIterSafety);
static int db_select_delete_continue_tree(Process *p, DbTable *tbl, 
					  Eterm continuation, Eterm *ret,
                                          enum DbIterSafety*);
static int db_select_replace_tree(Process *p, DbTable *tbl, Eterm tid,
                                  Eterm pattern, Eterm *ret,
                                  enum DbIterSafety);
static int db_select_replace_continue_tree(Process *p, DbTable *tbl,
                                           Eterm continuation, Eterm *ret,
                                           enum DbIterSafety*);
static int db_take_tree(Process *, DbTable *, Eterm, Eterm *);
static void db_print_tree(fmtfn_t to, void *to_arg,
			  int show, DbTable *tbl);
static int db_free_empty_table_tree(DbTable *tbl);

static SWord db_free_table_continue_tree(DbTable *tbl, SWord);

static void db_foreach_offheap_tree(DbTable *,
				    void (*)(ErlOffHeap *, void *),
				    void *);

static SWord db_delete_all_objects_tree(Process* p,
                                        DbTable* tbl,
                                        SWord reds,
                                        Eterm* nitems_holder_wb);
static Eterm db_delete_all_objects_get_nitems_from_holder_tree(Process* p,
                                                               Eterm nitems_holder);
#ifdef HARDDEBUG
static void db_check_table_tree(DbTable *tbl);
#endif
static int
db_lookup_dbterm_tree(Process *, DbTable *, Eterm key, Eterm obj,
                      DbUpdateHandle*);
static void
db_finalize_dbterm_tree(int cret, DbUpdateHandle *);

static int db_get_binary_info_tree(Process*, DbTable*, Eterm key, Eterm *ret);


/*
** Static variables
*/

Export ets_select_reverse_exp;

/*
** External interface 
*/
DbTableMethod db_tree =
{
    db_create_tree,
    db_first_tree,
    db_next_tree,
    db_last_tree,
    db_prev_tree,
    db_put_tree,
    db_get_tree,
    db_get_element_tree,
    db_member_tree,
    db_erase_tree,
    db_erase_object_tree,
    db_slot_tree,
    db_select_chunk_tree,
    db_select_tree, /* why not chunk size=0 ??? */
    db_select_delete_tree,
    db_select_continue_tree,
    db_select_delete_continue_tree,
    db_select_count_tree,
    db_select_count_continue_tree,
    db_select_replace_tree,
    db_select_replace_continue_tree,
    db_take_tree,
    db_delete_all_objects_tree,
    db_delete_all_objects_get_nitems_from_holder_tree,
    db_free_empty_table_tree,
    db_free_table_continue_tree,
    db_print_tree,
    db_foreach_offheap_tree,
    db_lookup_dbterm_tree,
    db_finalize_dbterm_tree,
    db_get_binary_info_tree,
    db_first_tree, /* raw_first same as first */
    db_next_tree   /* raw_next same as next */
};





void db_initialize_tree(void)
{
    erts_init_trap_export(&ets_select_reverse_exp, am_ets, am_reverse, 3,
			  &ets_select_reverse);
    return;
};

/*
** Table interface routines ie what's called by the bif's 
*/

int db_create_tree(Process *p, DbTable *tbl)
{
    DbTableTree *tb = &tbl->tree;
    tb->root = NULL;
    tb->static_stack.array = erts_db_alloc(ERTS_ALC_T_DB_STK,
					   (DbTable *) tb,
					   sizeof(TreeDbTerm *) * STACK_NEED);
    tb->static_stack.pos = 0;
    tb->static_stack.slot = 0;
    erts_atomic_init_nob(&tb->is_stack_busy, 0);
    tb->deletion = 0;
    return DB_ERROR_NONE;
}

int db_first_tree_common(Process *p, DbTable *tbl, TreeDbTerm *root,
                         Eterm *ret, DbTableTree *stack_container)
{
    DbTreeStack* stack;
    TreeDbTerm *this;

    if (( this = root ) == NULL) {
	*ret = am_EOT;
	return DB_ERROR_NONE;
    }
    /* Walk down the tree to the left */
    if ((stack = get_static_stack(stack_container)) != NULL) {
	stack->pos = stack->slot = 0;
    }
    while (this->left != NULL) {
	if (stack) PUSH_NODE(stack, this);
	this = this->left;
    }
    if (stack) {
	PUSH_NODE(stack, this);
	stack->slot = 1;
	release_stack(tbl,stack_container,stack);
    }
    *ret = db_copy_key(p, tbl, &this->dbterm);
    return DB_ERROR_NONE;
}

static int db_first_tree(Process *p, DbTable *tbl, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    return db_first_tree_common(p, tbl, tb->root, ret, tb);
}

int db_next_tree_common(Process *p, DbTable *tbl,
                        TreeDbTerm *root, Eterm key,
                        Eterm *ret, DbTreeStack* stack)
{
    TreeDbTerm *this;

    if (key == am_EOT)
	return DB_ERROR_BADKEY;
    this = find_next(&tbl->common, root, stack, key);
    if (this == NULL) {
	*ret = am_EOT;
	return DB_ERROR_NONE;
    }
    *ret = db_copy_key(p, tbl, &this->dbterm);
    return DB_ERROR_NONE;
}

static int db_next_tree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    DbTreeStack* stack = get_any_stack(tbl, tb);
    int ret_val = db_next_tree_common(p, tbl, tb->root, key, ret, stack);
    release_stack(tbl,tb,stack);
    return ret_val;
}

int db_last_tree_common(Process *p, DbTable *tbl, TreeDbTerm *root,
                        Eterm *ret, DbTableTree *stack_container)
{
    TreeDbTerm *this;
    DbTreeStack* stack;

    if (( this = root ) == NULL) {
	*ret = am_EOT;
	return DB_ERROR_NONE;
    }
    /* Walk down the tree to the right */
    if ((stack = get_static_stack(stack_container)) != NULL) {
	stack->pos = stack->slot = 0;
    }    
    while (this->right != NULL) {
	if (stack) PUSH_NODE(stack, this);
	this = this->right;
    }
    if (stack) {
	PUSH_NODE(stack, this);
        /* Always centralized counters when static stack is used */
	stack->slot = NITEMS_CENTRALIZED(tbl);
	release_stack(tbl,stack_container,stack);
    }
    *ret = db_copy_key(p, tbl, &this->dbterm);
    return DB_ERROR_NONE;
}

static int db_last_tree(Process *p, DbTable *tbl, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    return db_last_tree_common(p, tbl, tb->root, ret, tb);
}

int db_prev_tree_common(Process *p, DbTable *tbl, TreeDbTerm *root, Eterm key,
                 Eterm *ret, DbTreeStack* stack)
{
    TreeDbTerm *this;

    if (key == am_EOT)
	return DB_ERROR_BADKEY;
    this = find_prev(&tbl->common, root, stack, key);
    if (this == NULL) {
	*ret = am_EOT;
	return DB_ERROR_NONE;
    }
    *ret = db_copy_key(p, tbl, &this->dbterm);
    return DB_ERROR_NONE;
}

static int db_prev_tree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    DbTreeStack* stack = get_any_stack(tbl, tb);
    int res = db_prev_tree_common(p, tbl, tb->root, key, ret, stack);
    release_stack(tbl,tb,stack);
    return res;
}

static ERTS_INLINE int cmp_key_eq(DbTableCommon* tb, Eterm key, TreeDbTerm* obj) {
    Eterm obj_key = GETKEY(tb,obj->dbterm.tpl);
    return is_same(key, obj_key) || CMP(key, obj_key) == 0;
}

int db_put_tree_common(DbTableCommon *tb, TreeDbTerm **root, Eterm obj,
                       int key_clash_fail, DbTableTree *stack_container)
{
    /* Non recursive insertion in AVL tree, building our own stack */
    TreeDbTerm **tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 0;
    int state = 0;
    TreeDbTerm **this = root;
    Sint c;
    Eterm key;
    int dir;
    TreeDbTerm *p1, *p2, *p;

    key = GETKEY(tb, tuple_val(obj));

    reset_static_stack(stack_container);

    dstack[dpos++] = DIR_END;
    for (;;)
	if (!*this) { /* Found our place */
	    state = 1;
            INC_NITEMS(((DbTable*)tb));
	    *this = new_dbterm(tb, obj);
	    (*this)->balance = 0;
	    (*this)->left = (*this)->right = NULL;
	    break;
	} else if ((c = cmp_key(tb, key, *this)) < 0) {
	    /* go lefts */
	    dstack[dpos++] = DIR_LEFT;
	    tstack[tpos++] = this;
	    this = &((*this)->left);
	} else if (c > 0) { /* go right */
	    dstack[dpos++] = DIR_RIGHT;
	    tstack[tpos++] = this;
	    this = &((*this)->right);
	} else if (!key_clash_fail) { /* Equal key and this is a set, replace. */
	    *this = replace_dbterm(tb, *this, obj);
	    break;
	} else {
	    return DB_ERROR_BADKEY; /* key already exists */
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
    return DB_ERROR_NONE;
}

static int db_put_tree(DbTable *tbl, Eterm obj, int key_clash_fail)
{
    DbTableTree *tb = &tbl->tree;
    return db_put_tree_common(&tb->common, &tb->root, obj, key_clash_fail, tb);
}

int db_get_tree_common(Process *p, DbTableCommon *tb, TreeDbTerm *root, Eterm key,
                       Eterm *ret, DbTableTree *stack_container)
{
    Eterm copy;
    Eterm *hp, *hend;
    TreeDbTerm *this;

    /*
     * This is always a set, so we know exactly how large
     * the data is when we have found it.
     * The list created around it is purely for interface conformance.
     */
    
    this = find_node(tb,root,key,stack_container);
    if (this == NULL) {
	*ret = NIL;
    } else {
	hp = HAlloc(p, this->dbterm.size + 2);
	hend = hp + this->dbterm.size + 2;
	copy = db_copy_object_from_ets(tb, &this->dbterm, &hp, &MSO(p));
	*ret = CONS(hp, copy, NIL);
	hp += 2;
	HRelease(p,hend,hp);
    }
    return DB_ERROR_NONE;
}

static int db_get_tree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    return db_get_tree_common(p, &tb->common, tb->root, key, ret, tb);
}

int db_member_tree_common(DbTableCommon *tb, TreeDbTerm *root, Eterm key, Eterm *ret,
                          DbTableTree *stack_container)
{
    *ret = (find_node(tb,root,key,stack_container) == NULL) ? am_false : am_true;
    return DB_ERROR_NONE;
}

static int db_member_tree(DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    return db_member_tree_common(&tb->common, tb->root, key, ret, tb);
}

int db_get_element_tree_common(Process *p, DbTableCommon *tb, TreeDbTerm *root, Eterm key,
                               int ndex, Eterm *ret, DbTableTree *stack_container)
{
    /*
     * Look the node up:
     */
    Eterm *hp;
    TreeDbTerm *this;

    /*
     * This is always a set, so we know exactly how large
     * the data is when we have found it.
     * No list is created around elements in set's so there are no list
     * around the element here either.
     */
    
    this = find_node(tb,root,key,stack_container);
    if (this == NULL) {
	return DB_ERROR_BADKEY;
    } else {
	if (ndex > arityval(this->dbterm.tpl[0])) {
	    return DB_ERROR_BADPARAM;
	}
	*ret = db_copy_element_from_ets(tb, p, &this->dbterm, ndex, &hp, 0);
    }
    return DB_ERROR_NONE;
}

static int db_get_element_tree(Process *p, DbTable *tbl,
			       Eterm key, int ndex, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    return db_get_element_tree_common(p, &tb->common, tb->root, key,
                                      ndex, ret, tb);
}

int db_erase_tree_common(DbTable *tbl, TreeDbTerm **root, Eterm key, Eterm *ret,
                         DbTreeStack *stack /* NULL if no static stack */)
{
    TreeDbTerm *res;

    *ret = am_true;

    if ((res = linkout_tree(&tbl->common, root,key, stack)) != NULL) {
	free_term(tbl, res);
    }
    return DB_ERROR_NONE;
}

static int db_erase_tree(DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    return db_erase_tree_common(tbl, &tb->root, key, ret, &tb->static_stack);
}

int db_erase_object_tree_common(DbTable *tbl, TreeDbTerm **root, Eterm object,
                                Eterm *ret, DbTableTree *stack_container)
{
    TreeDbTerm *res;

    *ret = am_true;

    if ((res = linkout_object_tree(&tbl->common, root, object, stack_container)) != NULL) {
	free_term(tbl, res);
    }
    return DB_ERROR_NONE;
}

static int db_erase_object_tree(DbTable *tbl, Eterm object, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    return  db_erase_object_tree_common(tbl, &tb->root, object, ret, tb);
}

int db_slot_tree_common(Process *p, DbTable *tbl, TreeDbTerm *root,
                        Eterm slot_term, Eterm *ret,
                        DbTableTree *stack_container,
                        CATreeRootIterator *iter)
{
    Sint slot;
    TreeDbTerm *st;
    Eterm *hp, *hend;
    Eterm copy;
    int is_EOT = 0;
    /*
     * The notion of a "slot" is not natural in a tree, but we try to
     * simulate it by giving the n'th node in the tree instead.
     * Traversing a tree in this way is not very convenient, but by
     * using the saved stack we at least sometimes will get acceptable 
     * performance.
     */

    if (is_not_small(slot_term) ||
	((slot = signed_val(slot_term)) < 0) ||
	(IS_CENTRALIZED_CTR(tbl) && slot > NITEMS_CENTRALIZED(tbl)))
	return DB_ERROR_BADPARAM;

    if (IS_CENTRALIZED_CTR(tbl) && slot == NITEMS_CENTRALIZED(tbl)) {
	*ret = am_EOT;
	return DB_ERROR_NONE;
    }

    /* 
     * We use the slot position and search from there, slot positions 
     * are counted from 1 and up.
     */
    ++slot;
    st = slot_search(p, root, slot, tbl, stack_container, iter, &is_EOT);
    if (is_EOT) {
        *ret = am_EOT;
	return DB_ERROR_NONE;
    }
    if (st == NULL) {
	*ret = am_false;
	return DB_ERROR_UNSPEC;
    }
    hp = HAlloc(p, st->dbterm.size + 2);
    hend = hp + st->dbterm.size + 2;
    copy = db_copy_object_from_ets(&tbl->common, &st->dbterm, &hp, &MSO(p));
    *ret = CONS(hp, copy, NIL);
    hp += 2;
    HRelease(p,hend,hp);
    return DB_ERROR_NONE;
}

static int db_slot_tree(Process *p, DbTable *tbl, 
			Eterm slot_term, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    return db_slot_tree_common(p, tbl, tb->root, slot_term, ret, tb, NULL);
}



static BIF_RETTYPE ets_select_reverse(BIF_ALIST_3)
{
    Process *p = BIF_P;
    Eterm a1 = BIF_ARG_1;
    Eterm a2 = BIF_ARG_2;
    Eterm a3 = BIF_ARG_3;
    Eterm list;
    Eterm result;
    Eterm* hp;
    Eterm* hend;

    int max_iter = CONTEXT_REDS * 10;

    if (is_nil(a1)) {
	hp = HAlloc(p, 3);
	BIF_RET(TUPLE2(hp,a2,a3));
    } else if (is_not_list(a1)) {
    error:
	BIF_ERROR(p, BADARG);
    }
    
    list = a1;
    result = a2;
    hp = hend = NULL;
    while (is_list(list)) {
	Eterm* pair = list_val(list);
	if (--max_iter == 0) {
	    BUMP_ALL_REDS(p);
	    HRelease(p, hend, hp);
	    BIF_TRAP3(&ets_select_reverse_exp, p, list, result, a3);
	}
	if (hp == hend) {
	    hp = HAlloc(p, 64);
	    hend = hp + 64;
	}
	result = CONS(hp, CAR(pair), result);
	hp += 2;
	list = CDR(pair);
    }
    if (is_not_nil(list))  {
	goto error;
    }
    HRelease(p, hend, hp);
    BUMP_REDS(p,CONTEXT_REDS - max_iter / 10);
    hp = HAlloc(p,3);
    BIF_RET(TUPLE2(hp, result, a3));
}

static BIF_RETTYPE bif_trap1(Export *bif,
			     Process *p, 
			     Eterm p1) 
{
    BIF_TRAP1(bif, p, p1);
}
    
static BIF_RETTYPE bif_trap3(Export *bif,
			     Process *p, 
			     Eterm p1, 
			     Eterm p2,
			     Eterm p3) 
{
    BIF_TRAP3(bif, p, p1, p2, p3);
}

int db_select_continue_tree_common(Process *p, 
                                   DbTableCommon *tb,
                                   Eterm continuation,
                                   Eterm *ret,
                                   DbTableTree *stack_container,
                                   CATreeRootIterator* iter)
{
    DbTreeStack* stack;
    struct select_context sc;
    unsigned sz;
    Eterm *hp; 
    Eterm lastkey;
    Eterm end_condition; 
    Binary *mp;
    Eterm key;
    Eterm *tptr;
    Sint chunk_size;
    Sint reverse;

#define RET_TO_BIF(Term, State) do { *ret = (Term); return State; } while(0);

    /* Decode continuation. We know it's a tuple but not the arity or 
       anything else */

    tptr = tuple_val(continuation);

    if (arityval(*tptr) != 8)
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    
    if (!is_small(tptr[4]) ||
	!(is_list(tptr[6]) || tptr[6] == NIL) || !is_small(tptr[7]) ||
	!is_small(tptr[8]))
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    
    lastkey = tptr[2];
    end_condition = tptr[3];
    mp = erts_db_get_match_prog_binary(tptr[5]);
    if (!mp)
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    chunk_size = signed_val(tptr[4]);

    sc.p = p;
    sc.accum = tptr[6];
    sc.mp = mp;
    sc.end_condition = NIL;
    sc.lastobj = NULL;
    sc.max = 1000;
    sc.keypos = tb->keypos;
    sc.chunk_size = chunk_size;
    reverse = unsigned_val(tptr[7]);
    sc.got = signed_val(tptr[8]);

    if (iter) {
        iter->next_route_key = lastkey;
        sc.common.root = catree_find_nextprev_root(iter, !!reverse != !!chunk_size, NULL);
    }
    else
        sc.common.root = &((DbTableTree*)tb)->root;

    if (sc.common.root) {
        stack = get_any_stack((DbTable*)tb, stack_container);
        if (chunk_size) {
            if (reverse) {
                traverse_backwards(tb, stack, lastkey, &doit_select_chunk, &sc.common, iter);
            } else {
                traverse_forward(tb, stack, lastkey, &doit_select_chunk, &sc.common, iter);
            }
        } else {
            if (reverse) {
                traverse_forward(tb, stack, lastkey, &doit_select, &sc.common, iter);
            } else {
                traverse_backwards(tb, stack, lastkey, &doit_select, &sc.common, iter);
            }
        }
        release_stack((DbTable*)tb,stack_container,stack);

        BUMP_REDS(p, 1000 - sc.max);
    }

    if (sc.max > 0 || (chunk_size && sc.got == chunk_size)) {
	if (chunk_size) {
	    Eterm *hp; 
	    unsigned sz;

	    if (sc.got < chunk_size || sc.lastobj == NULL) { 
		/* end of table, sc.lastobj may be NULL as we may have been
		   at the very last object in the table when trapping. */
		if (!sc.got) {
		    RET_TO_BIF(am_EOT, DB_ERROR_NONE);
		} else {
		    RET_TO_BIF(bif_trap3(&ets_select_reverse_exp, p,
					 sc.accum, NIL, am_EOT), 
			       DB_ERROR_NONE);
		}
	    }

	    key = GETKEY(tb, sc.lastobj);
	    sz = size_object(key);
	    hp = HAlloc(p, 9 + sz);
	    key = copy_struct(key, sz, &hp, &MSO(p));
	    continuation = TUPLE8
		(hp,
		 tptr[1],
		 key,
		 tptr[3], 
		 tptr[4],
		 tptr[5],
		 NIL,
		 tptr[7],
		 make_small(0));
	    RET_TO_BIF(bif_trap3(&ets_select_reverse_exp, p,
				 sc.accum, NIL, continuation), 
		       DB_ERROR_NONE);
	} else {
	    RET_TO_BIF(sc.accum, DB_ERROR_NONE);
	}
    }	
    key = GETKEY(tb, sc.lastobj);
    if (chunk_size) {
	if (end_condition != NIL && 
	    ((!reverse && cmp_partly_bound(end_condition,key) < 0) ||
	     (reverse && cmp_partly_bound(end_condition,key) > 0))) {
	    /* done anyway */
	    if (!sc.got) {
		RET_TO_BIF(am_EOT, DB_ERROR_NONE);
	    } else {
		RET_TO_BIF(bif_trap3(&ets_select_reverse_exp, p, 
				     sc.accum, NIL, am_EOT), 
			   DB_ERROR_NONE);
	    }
	}
    } else {
	if (end_condition != NIL && 
	    ((!reverse && cmp_partly_bound(end_condition,key) > 0) ||
	     (reverse && cmp_partly_bound(end_condition,key) < 0))) {
	    /* done anyway */
	    RET_TO_BIF(sc.accum,DB_ERROR_NONE);
	}
    }
    /* Not done yet, let's trap. */
    sz = size_object(key);
    hp = HAlloc(p, 9 + sz);
    key = copy_struct(key, sz, &hp, &MSO(p));
    continuation = TUPLE8
	(hp,
	 tptr[1],
	 key,
	 tptr[3], 
	 tptr[4],
	 tptr[5],
	 sc.accum,
	 tptr[7],
	 make_small(sc.got));
    RET_TO_BIF(bif_trap1(bif_export[BIF_ets_select_1], p, continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF
}
    
/*
** This is called either when the select bif traps or when ets:select/1 
** is called. It does mostly the same as db_select_tree and may in either case
** trap to itself again (via the ets:select/1 bif).
** Note that this is common for db_select_tree and db_select_chunk_tree.
*/
static int db_select_continue_tree(Process *p, 
				   DbTable *tbl,
				   Eterm continuation,
				   Eterm *ret,
                                   enum DbIterSafety* safety_p)
{
    DbTableTree *tb = &tbl->tree;
    return db_select_continue_tree_common(p, &tb->common,
                                          continuation, ret, tb, NULL);
}

int db_select_tree_common(Process *p, DbTable *tb,
                          Eterm tid, Eterm pattern, int reverse, Eterm *ret,
                          DbTableTree *stack_container,
                          CATreeRootIterator* iter)
{
    /* Strategy: Traverse backwards to build resulting list from tail to head */
    DbTreeStack* stack;
    struct select_context sc;
    struct mp_info mpi;
    Eterm lastkey = THE_NON_VALUE;
    Eterm key;
    Eterm continuation;
    unsigned sz;
    Eterm *hp; 
    TreeDbTerm *this;
    int errcode;
    Eterm mpb;


#define RET_TO_BIF(Term,RetVal) do { 	       	\
	if (mpi.mp != NULL) {			\
	    erts_bin_free(mpi.mp);       	\
	}					\
	*ret = (Term); 				\
	return RetVal; 			        \
    } while(0)

    mpi.mp = NULL;

    sc.accum = NIL;
    sc.lastobj = NULL;
    sc.p = p;
    sc.max = 1000; 
    sc.end_condition = NIL;
    sc.keypos = tb->common.keypos;
    sc.got = 0;
    sc.chunk_size = 0;

    if ((errcode = analyze_pattern(&tb->common, pattern, NULL, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(NIL,errcode);
    }

    if (mpi.key_boundness == MS_KEY_IMPOSSIBLE) {
	RET_TO_BIF(NIL,DB_ERROR_NONE);  
	/* can't possibly match anything */
    }

    sc.mp = mpi.mp;

    if (mpi.key_boundness == MS_KEY_BOUND) {
        ASSERT(CMP_EQ(mpi.least, mpi.most));
        if (iter)
            sc.common.root = catree_find_root(mpi.least, iter);
        else
            sc.common.root = &tb->tree.root;
        this = find_node(&tb->common, *sc.common.root, mpi.least, NULL);
        if (this)
            doit_select(&tb->common, this, &sc.common, 0 /* direction doesn't matter */);
	RET_TO_BIF(sc.accum,DB_ERROR_NONE);
    }

    stack = get_any_stack((DbTable*)tb,stack_container);
    if (reverse) {
	if (mpi.key_boundness == MS_KEY_PARTIALLY_BOUND) {
            this = find_prev_from_pb_key(tb, &sc.common.root, stack, mpi.least, iter);
	    if (this)
		lastkey = GETKEY(tb, this->dbterm.tpl);
	    sc.end_condition = mpi.most;
	}
        else {
            ASSERT(mpi.key_boundness == MS_KEY_UNBOUND);
            if (iter)
                sc.common.root = catree_find_first_root(iter);
            else
                sc.common.root = &tb->tree.root;
        }
	traverse_forward(&tb->common, stack, lastkey, &doit_select, &sc.common, iter);
    } else {
	if (mpi.key_boundness == MS_KEY_PARTIALLY_BOUND) {
            this = find_next_from_pb_key(tb, &sc.common.root, stack, mpi.most, iter);
	    if (this)
                lastkey = GETKEY(tb, this->dbterm.tpl);
	    sc.end_condition = mpi.least;
	}
        else {
            ASSERT(mpi.key_boundness == MS_KEY_UNBOUND);
            if (iter)
                sc.common.root = catree_find_last_root(iter);
            else
                sc.common.root = &tb->tree.root;
        }
	traverse_backwards(&tb->common, stack, lastkey, &doit_select, &sc.common, iter);
    }
    release_stack((DbTable*)tb,stack_container,stack);
#ifdef HARDDEBUG
	erts_fprintf(stderr,"Least: %T\n", mpi.least);
	erts_fprintf(stderr,"Most: %T\n", mpi.most);
#endif
    BUMP_REDS(p, 1000 - sc.max);
    if (sc.max > 0) {
	RET_TO_BIF(sc.accum,DB_ERROR_NONE);
    }

    key = GETKEY(tb, sc.lastobj);
    sz = size_object(key);
    hp = HAlloc(p, 9 + sz + ERTS_MAGIC_REF_THING_SIZE);
    key = copy_struct(key, sz, &hp, &MSO(p));
    mpb= erts_db_make_match_prog_ref(p,mpi.mp,&hp);
	    
    continuation = TUPLE8
	(hp,
	 tid,
	 key,
	 sc.end_condition, /* From the match program, needn't be copied */
	 make_small(0), /* Chunk size of zero means not chunked to the
			   continuation BIF */
	 mpb,
	 sc.accum,
	 make_small(reverse),
	 make_small(sc.got));

    /* Don't free mpi.mp, so don't use macro */
    *ret = bif_trap1(bif_export[BIF_ets_select_1], p, continuation); 
    return DB_ERROR_NONE;

#undef RET_TO_BIF

}

static int db_select_tree(Process *p, DbTable *tbl, Eterm tid,
			  Eterm pattern, int reverse, Eterm *ret,
                          enum DbIterSafety safety)
{
    return db_select_tree_common(p, tbl, tid,
                                 pattern, reverse, ret, &tbl->tree, NULL);
}

int db_select_count_continue_tree_common(Process *p, 
                                         DbTable *tb,
                                         Eterm continuation,
                                         Eterm *ret,
                                         DbTableTree *stack_container,
                                         CATreeRootIterator* iter)
{
    DbTreeStack* stack;
    struct select_count_context sc;
    unsigned sz;
    Eterm *hp; 
    Eterm lastkey;
    Eterm end_condition; 
    Binary *mp;
    Eterm key;
    Eterm *tptr;
    Eterm egot;

#define RET_TO_BIF(Term, State) do { *ret = (Term); return State; } while(0);

    /* Decode continuation. We know it's a tuple and everything else as
     this is only called by ourselves */

    /* continuation: 
       {Table, Lastkey, EndCondition, MatchProgBin, HowManyGot}*/

    tptr = tuple_val(continuation);

    if (arityval(*tptr) != 5)
	erts_exit(ERTS_ERROR_EXIT,"Internal error in ets:select_count/1");
    
    lastkey = tptr[2];
    end_condition = tptr[3];
    mp = erts_db_get_match_prog_binary(tptr[4]);
    if (!mp)
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);

    sc.p = p;
    sc.mp = mp;
    sc.end_condition = NIL;
    sc.lastobj = NULL;
    sc.max = 1000;
    sc.keypos = tb->common.keypos;
    if (is_big(tptr[5])) {
	sc.got = big_to_uint32(tptr[5]);
    } else {
	sc.got = unsigned_val(tptr[5]);
    }

    if (iter) {
        iter->next_route_key = lastkey;
        sc.common.root = catree_find_prev_root(iter, NULL);
    }
    else {
        sc.common.root = &tb->tree.root;
    }

    if (sc.common.root) {
        stack = get_any_stack(tb, stack_container);
        traverse_backwards(&tb->common, stack, lastkey, &doit_select_count, &sc.common, iter);
        release_stack(tb,stack_container,stack);

        BUMP_REDS(p, 1000 - sc.max);
    }

    if (sc.max > 0) {
	RET_TO_BIF(erts_make_integer(sc.got,p), DB_ERROR_NONE);
    }	
    key = GETKEY(tb, sc.lastobj);
    if (end_condition != NIL && 
	(cmp_partly_bound(end_condition,key) > 0)) {
	/* done anyway */
	RET_TO_BIF(make_small(sc.got),DB_ERROR_NONE);
    }
    /* Not done yet, let's trap. */
    sz = size_object(key);
    if (IS_USMALL(0, sc.got)) {
	hp = HAlloc(p, sz + 6);
	egot = make_small(sc.got);
    }
    else {
	hp = HAlloc(p, BIG_UINT_HEAP_SIZE + sz + 6);
	egot = uint_to_big(sc.got, hp);
	hp += BIG_UINT_HEAP_SIZE;
    }
    key = copy_struct(key, sz, &hp, &MSO(p));
    continuation = TUPLE5
	(hp,
	 tptr[1],
	 key,
	 tptr[3], 
	 tptr[4],
	 egot);
    RET_TO_BIF(bif_trap1(&ets_select_count_continue_exp, p, continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF
}

/*
** This is called either when the select_count bif traps.
*/
static int db_select_count_continue_tree(Process *p, 
                                         DbTable *tbl,
                                         Eterm continuation,
                                         Eterm *ret,
                                         enum DbIterSafety* safety_p)
{
    DbTableTree *tb = &tbl->tree;
    return db_select_count_continue_tree_common(p, tbl,
                                                continuation, ret, tb, NULL);
}


int db_select_count_tree_common(Process *p, DbTable *tb,
                                Eterm tid, Eterm pattern, Eterm *ret,
                                DbTableTree *stack_container,
                                CATreeRootIterator* iter)
{
    DbTreeStack* stack;
    struct select_count_context sc;
    struct mp_info mpi;
    Eterm lastkey = THE_NON_VALUE;
    Eterm key;
    Eterm continuation;
    unsigned sz;
    Eterm *hp; 
    TreeDbTerm *this;
    int errcode;
    Eterm egot;
    Eterm mpb;

#define RET_TO_BIF(Term,RetVal) do { 	       	\
	if (mpi.mp != NULL) {			\
	    erts_bin_free(mpi.mp);       	\
	}					\
	*ret = (Term); 				\
	return RetVal; 			        \
    } while(0)

    mpi.mp = NULL;

    sc.lastobj = NULL;
    sc.p = p;
    sc.max = 1000; 
    sc.end_condition = NIL;
    sc.keypos = tb->common.keypos;
    sc.got = 0;

    if ((errcode = analyze_pattern(&tb->common, pattern, NULL, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(NIL,errcode);
    }

    if (mpi.key_boundness == MS_KEY_IMPOSSIBLE) {
	RET_TO_BIF(make_small(0),DB_ERROR_NONE);  
	/* can't possibly match anything */
    }

    sc.mp = mpi.mp;

    if (mpi.key_boundness == MS_KEY_BOUND) {
        ASSERT(CMP_EQ(mpi.least, mpi.most));
        if (iter)
            sc.common.root = catree_find_root(mpi.least, iter);
        else
            sc.common.root = &((DbTable*)tb)->tree.root;
        this =  find_node(&tb->common, *sc.common.root, mpi.least, NULL);
        if (this)
            doit_select_count(&tb->common, this, &sc.common, 0 /* dummy */);
	RET_TO_BIF(erts_make_integer(sc.got,p),DB_ERROR_NONE);
    }

    stack = get_any_stack((DbTable*)tb, stack_container);
    if (mpi.key_boundness == MS_KEY_PARTIALLY_BOUND) {
        this = find_next_from_pb_key(tb, &sc.common.root, stack, mpi.most, iter);
	if (this)
            lastkey = GETKEY(tb, this->dbterm.tpl);
	sc.end_condition = mpi.least;
    }
    else {
        ASSERT(mpi.key_boundness == MS_KEY_UNBOUND);
        if (iter)
            sc.common.root = catree_find_last_root(iter);
        else
            sc.common.root = &tb->tree.root;
    }
    
    traverse_backwards(&tb->common, stack, lastkey, &doit_select_count, &sc.common, iter);
    release_stack((DbTable*)tb,stack_container,stack);
    BUMP_REDS(p, 1000 - sc.max);
    if (sc.max > 0) {
	RET_TO_BIF(erts_make_integer(sc.got,p),DB_ERROR_NONE);
    }

    key = GETKEY(tb, sc.lastobj);
    sz = size_object(key);
    if (IS_USMALL(0, sc.got)) {
	hp = HAlloc(p, sz + ERTS_MAGIC_REF_THING_SIZE + 6);
	egot = make_small(sc.got);
    }
    else {
	hp = HAlloc(p, BIG_UINT_HEAP_SIZE + sz + ERTS_MAGIC_REF_THING_SIZE + 6);
	egot = uint_to_big(sc.got, hp);
	hp += BIG_UINT_HEAP_SIZE;
    }
    key = copy_struct(key, sz, &hp, &MSO(p));
    mpb = erts_db_make_match_prog_ref(p,mpi.mp,&hp);
	    
    continuation = TUPLE5
	(hp,
	 tid,
	 key,
	 sc.end_condition, /* From the match program, needn't be copied */
	 mpb,
	 egot);

    /* Don't free mpi.mp, so don't use macro */
    *ret = bif_trap1(&ets_select_count_continue_exp, p, continuation); 
    return DB_ERROR_NONE;

#undef RET_TO_BIF

}

static int db_select_count_tree(Process *p, DbTable *tbl, Eterm tid,
                                Eterm pattern, Eterm *ret,
                                enum DbIterSafety safety)
{
    DbTableTree *tb = &tbl->tree;
    return db_select_count_tree_common(p, tbl,
                                       tid, pattern, ret, tb, NULL);
}


int db_select_chunk_tree_common(Process *p, DbTable *tb,
                                Eterm tid, Eterm pattern, Sint chunk_size,
                                int reverse, Eterm *ret,
                                DbTableTree *stack_container,
                                CATreeRootIterator* iter)
{
    DbTreeStack* stack;
    struct select_context sc;
    struct mp_info mpi;
    Eterm lastkey = THE_NON_VALUE;
    Eterm key;
    Eterm continuation;
    unsigned sz;
    Eterm *hp; 
    TreeDbTerm *this;
    int errcode;
    Eterm mpb;

#define RET_TO_BIF(Term,RetVal) do { 		\
	if (mpi.mp != NULL) {			\
	    erts_bin_free(mpi.mp);		\
	}					\
	*ret = (Term); 				\
	return RetVal; 			        \
    } while(0)

    mpi.mp = NULL;

    sc.accum = NIL;
    sc.lastobj = NULL;
    sc.p = p;
    sc.max = 1000; 
    sc.end_condition = NIL;
    sc.keypos = tb->common.keypos;
    sc.got = 0;
    sc.chunk_size = chunk_size;

    if ((errcode = analyze_pattern(&tb->common, pattern, NULL, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(NIL,errcode);
    }

    if (mpi.key_boundness == MS_KEY_IMPOSSIBLE) {
	RET_TO_BIF(am_EOT,DB_ERROR_NONE);
	/* can't possibly match anything */
    }

    sc.mp = mpi.mp;

    if (mpi.key_boundness == MS_KEY_BOUND) {
        ASSERT(CMP_EQ(mpi.least, mpi.most));
        if (iter)
            sc.common.root = catree_find_root(mpi.least, iter);
        else
            sc.common.root = &tb->tree.root;
        this =  find_node(&tb->common, *sc.common.root, mpi.least, NULL);
        if (this)
            doit_select(&tb->common, this, &sc.common, 0 /* direction doesn't matter */);
	if (sc.accum != NIL) {
	    hp=HAlloc(p, 3);
	    RET_TO_BIF(TUPLE2(hp,sc.accum,am_EOT),DB_ERROR_NONE);
	} else {
	    RET_TO_BIF(am_EOT,DB_ERROR_NONE);
	}
    }

    stack = get_any_stack((DbTable*)tb,stack_container);
    if (reverse) {
	if (mpi.key_boundness == MS_KEY_PARTIALLY_BOUND) {
            this = find_next_from_pb_key(tb, &sc.common.root, stack, mpi.most, iter);
	    if (this)
                lastkey = GETKEY(tb, this->dbterm.tpl);
	    sc.end_condition = mpi.least;
	}
        else {
            ASSERT(mpi.key_boundness == MS_KEY_UNBOUND);
            if (iter)
                sc.common.root = catree_find_last_root(iter);
            else
                sc.common.root = &tb->tree.root;
        }
	traverse_backwards(&tb->common, stack, lastkey, &doit_select_chunk, &sc.common, iter);
    } else {
	if (mpi.key_boundness == MS_KEY_PARTIALLY_BOUND) {
            this = find_prev_from_pb_key(tb, &sc.common.root, stack, mpi.least, iter);
	    if (this)
                lastkey = GETKEY(tb, this->dbterm.tpl);
	    sc.end_condition = mpi.most;
	}
        else {
            ASSERT(mpi.key_boundness == MS_KEY_UNBOUND);
            if (iter)
                sc.common.root = catree_find_first_root(iter);
            else
                sc.common.root = &tb->tree.root;
        }
	traverse_forward(&tb->common, stack, lastkey, &doit_select_chunk, &sc.common, iter);
    }
    release_stack((DbTable*)tb,stack_container,stack);

    BUMP_REDS(p, 1000 - sc.max);
    if (sc.max > 0 || sc.got == chunk_size) {
	Eterm *hp; 
	unsigned sz;

	if (sc.got < chunk_size ||
	    sc.lastobj == NULL) { 
	    /* We haven't got all and we haven't trapped 
	       which should mean we are at the end of the 
	       table, sc.lastobj may be NULL if the table was empty */
	    
	    if (!sc.got) {
		RET_TO_BIF(am_EOT, DB_ERROR_NONE);
	    } else {
		RET_TO_BIF(bif_trap3(&ets_select_reverse_exp, p,
				     sc.accum, NIL, am_EOT), 
			   DB_ERROR_NONE);
	    }
	}

	key = GETKEY(tb, sc.lastobj);
	sz = size_object(key);
	hp = HAlloc(p, 9 + sz + ERTS_MAGIC_REF_THING_SIZE);
	key = copy_struct(key, sz, &hp, &MSO(p));
	mpb = erts_db_make_match_prog_ref(p,mpi.mp,&hp);
	
	continuation = TUPLE8
	    (hp,
	     tid,
	     key,
	     sc.end_condition, /* From the match program, 
				  needn't be copied */
	     make_small(chunk_size),
	     mpb,
	     NIL,
	     make_small(reverse),
	     make_small(0));
	/* Don't let RET_TO_BIF macro free mpi.mp*/
	*ret = bif_trap3(&ets_select_reverse_exp, p,
			 sc.accum, NIL, continuation);
	return DB_ERROR_NONE; 
    }

    key = GETKEY(tb, sc.lastobj);
    sz = size_object(key);
    hp = HAlloc(p, 9 + sz + ERTS_MAGIC_REF_THING_SIZE);
    key = copy_struct(key, sz, &hp, &MSO(p));

    mpb = erts_db_make_match_prog_ref(p,mpi.mp,&hp);
    continuation = TUPLE8
	(hp,
	 tid,
	 key,
	 sc.end_condition, /* From the match program, needn't be copied */
	 make_small(chunk_size),
	 mpb,
	 sc.accum,
	 make_small(reverse),
	 make_small(sc.got));
    /* Don't let RET_TO_BIF macro free mpi.mp*/
    *ret = bif_trap1(bif_export[BIF_ets_select_1], p, continuation);
    return DB_ERROR_NONE;

#undef RET_TO_BIF

}

static int db_select_chunk_tree(Process *p, DbTable *tbl, Eterm tid,
                                Eterm pattern, Sint chunk_size,
                                int reverse,
                                Eterm *ret, enum DbIterSafety safety)
{
    DbTableTree *tb = &tbl->tree;
    return db_select_chunk_tree_common(p, tbl,
                                       tid, pattern, chunk_size,
                                       reverse, ret, tb, NULL);
}


int db_select_delete_continue_tree_common(Process *p,
                                          DbTable *tbl,
                                          Eterm continuation,
                                          Eterm *ret,
                                          DbTreeStack* stack,
                                          CATreeRootIterator* iter)
{
    struct select_delete_context sc;
    unsigned sz;
    Eterm *hp; 
    Eterm lastkey;
    Eterm end_condition; 
    Binary *mp;
    Eterm key;
    Eterm *tptr;
    Eterm eaccsum;

#define RET_TO_BIF(Term, State) do { 		\
	if (sc.erase_lastterm) {		\
	    free_term(tbl, sc.lastterm);		\
	}					\
	*ret = (Term); 				\
	return State; 				\
    } while(0);

    /* Decode continuation. We know it's correct, this can only be called
       by trapping */

    tptr = tuple_val(continuation);

    lastkey = tptr[2];
    end_condition = tptr[3];

    sc.erase_lastterm = 0; /* Before first RET_TO_BIF */
    sc.lastterm = NULL;

    mp = erts_db_get_match_prog_binary_unchecked(tptr[4]);
    sc.p = p;
    sc.tb = &tbl->common;
    sc.stack = stack;
    if (is_big(tptr[5])) {
	sc.accum = big_to_uint32(tptr[5]);
    } else {
	sc.accum = unsigned_val(tptr[5]);
    }
    sc.mp = mp;
    sc.end_condition = NIL;
    sc.max = 1000;
    sc.keypos = tbl->common.keypos;

    if (iter) {
        iter->next_route_key = lastkey;
        sc.common.root = catree_find_prev_root(iter, NULL);
    }
    else {
        sc.common.root = &tbl->tree.root;
    }

    if (sc.common.root) {
        traverse_backwards(&tbl->common, stack, lastkey, &doit_select_delete, &sc.common, iter);

        BUMP_REDS(p, 1000 - sc.max);
    }

    if (sc.max > 0) {
	RET_TO_BIF(erts_make_integer(sc.accum, p), DB_ERROR_NONE);
    }	
    key = GETKEY(&tbl->common, (sc.lastterm)->dbterm.tpl);
    if (end_condition != NIL && 
	cmp_partly_bound(end_condition,key) > 0) { /* done anyway */
	RET_TO_BIF(erts_make_integer(sc.accum,p),DB_ERROR_NONE);
    }
    /* Not done yet, let's trap. */
    sz = size_object(key);
    if (IS_USMALL(0, sc.accum)) {
	hp = HAlloc(p, sz + 6);
	eaccsum = make_small(sc.accum);
    }
    else {
	hp = HAlloc(p, BIG_UINT_HEAP_SIZE + sz + 6);
	eaccsum = uint_to_big(sc.accum, hp);
	hp += BIG_UINT_HEAP_SIZE;
    }
    key = copy_struct(key, sz, &hp, &MSO(p));
    continuation = TUPLE5
	(hp,
	 tptr[1],
	 key,
	 tptr[3], 
	 tptr[4],
	 eaccsum);
    RET_TO_BIF(bif_trap1(&ets_select_delete_continue_exp, p, continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF
}

static int db_select_delete_continue_tree(Process *p, 
					  DbTable *tbl,
					  Eterm continuation,
					  Eterm *ret,
                                          enum DbIterSafety* safety_p)
{
    DbTableTree *tb = &tbl->tree;
    ASSERT(!erts_atomic_read_nob(&tb->is_stack_busy));
    return db_select_delete_continue_tree_common(p, tbl, continuation, ret,
                                                 &tb->static_stack, NULL);
}

int db_select_delete_tree_common(Process *p, DbTable *tbl,
                                 Eterm tid, Eterm pattern,
                                 Eterm *ret,
                                 DbTreeStack* stack,
                                 CATreeRootIterator* iter)
{
    struct select_delete_context sc;
    struct mp_info mpi;
    Eterm lastkey = THE_NON_VALUE;
    Eterm key;
    Eterm continuation;
    unsigned sz;
    Eterm *hp; 
    TreeDbTerm *this;
    int errcode;
    Eterm mpb;
    Eterm eaccsum;

#define RET_TO_BIF(Term,RetVal) do { 	       	\
	if (mpi.mp != NULL) {			\
	    erts_bin_free(mpi.mp);       	\
	}					\
	if (sc.erase_lastterm) {                \
	    free_term(tbl, sc.lastterm);         \
	}                                       \
	*ret = (Term); 				\
	return RetVal; 			        \
    } while(0)

    mpi.mp = NULL;

    sc.accum = 0;
    sc.erase_lastterm = 0;
    sc.lastterm = NULL;
    sc.p = p;
    sc.max = 1000; 
    sc.end_condition = NIL;
    sc.keypos = tbl->common.keypos;
    sc.tb = &tbl->common;
    sc.stack = stack;
    
    if ((errcode = analyze_pattern(&tbl->common, pattern, NULL, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(0,errcode);
    }

    if (mpi.key_boundness == MS_KEY_IMPOSSIBLE) {
	RET_TO_BIF(make_small(0),DB_ERROR_NONE);  
	/* can't possibly match anything */
    }

    sc.mp = mpi.mp;

    if (mpi.key_boundness == MS_KEY_BOUND) {
        ASSERT(CMP_EQ(mpi.least, mpi.most));
        if (iter)
            sc.common.root = catree_find_root(mpi.least, iter);
        else
            sc.common.root = &tbl->tree.root;
        this =  find_node(&tbl->common, *sc.common.root, mpi.least, NULL);
        if (this)
            doit_select_delete(&tbl->common, this, &sc.common, 0 /* direction doesn't
						      matter */);
	RET_TO_BIF(erts_make_integer(sc.accum,p),DB_ERROR_NONE);
    }

    if (mpi.key_boundness == MS_KEY_PARTIALLY_BOUND) {
        this = find_next_from_pb_key(tbl, &sc.common.root, stack, mpi.most, iter);
        if (this)
            lastkey = GETKEY(&tbl->common, this->dbterm.tpl);
	sc.end_condition = mpi.least;
    }
    else {
        ASSERT(mpi.key_boundness == MS_KEY_UNBOUND);
        if (iter)
            sc.common.root = catree_find_last_root(iter);
        else
            sc.common.root = &tbl->tree.root;
    }

    traverse_backwards(&tbl->common, stack, lastkey,
                       &doit_select_delete, &sc.common, iter);
    BUMP_REDS(p, 1000 - sc.max);

    if (sc.max > 0) {
	RET_TO_BIF(erts_make_integer(sc.accum,p), DB_ERROR_NONE);
    }

    key = GETKEY(&tbl->common, (sc.lastterm)->dbterm.tpl);
    sz = size_object(key);
    if (IS_USMALL(0, sc.accum)) {
	hp = HAlloc(p, sz + ERTS_MAGIC_REF_THING_SIZE + 6);
	eaccsum = make_small(sc.accum);
    }
    else {
	hp = HAlloc(p, BIG_UINT_HEAP_SIZE + sz + ERTS_MAGIC_REF_THING_SIZE + 6);
	eaccsum = uint_to_big(sc.accum, hp);
	hp += BIG_UINT_HEAP_SIZE;
    }
    key = copy_struct(key, sz, &hp, &MSO(p));
    mpb = erts_db_make_match_prog_ref(p,mpi.mp,&hp);
    
    continuation = TUPLE5
	(hp,
	 tid,
	 key,
	 sc.end_condition, /* From the match program, needn't be copied */
	 mpb,
	 eaccsum);

    /* Don't free mpi.mp, so don't use macro */
    if (sc.erase_lastterm) {
	free_term(tbl, sc.lastterm);
    }
    *ret = bif_trap1(&ets_select_delete_continue_exp, p, continuation); 
    return DB_ERROR_NONE;

#undef RET_TO_BIF

}

static int db_select_delete_tree(Process *p, DbTable *tbl, Eterm tid,
				 Eterm pattern, Eterm *ret,
                                 enum DbIterSafety safety)
{
    DbTableTree *tb = &tbl->tree;
    return db_select_delete_tree_common(p, tbl, tid, pattern, ret,
                                        &tb->static_stack, NULL);
}

int db_select_replace_continue_tree_common(Process *p,
                                           DbTable *tbl,
                                           Eterm continuation,
                                           Eterm *ret,
                                           DbTableTree *stack_container,
                                           CATreeRootIterator* iter)
{
    DbTreeStack* stack;
    struct select_replace_context sc;
    unsigned sz;
    Eterm *hp;
    Eterm lastkey;
    Eterm end_condition;
    Binary *mp;
    Eterm key;
    Eterm *tptr;
    Eterm ereplaced;
    Sint prev_replaced;


#define RET_TO_BIF(Term, State) do { *ret = (Term); return State; } while(0);

    /* Decode continuation. We know it's a tuple and everything else as
       this is only called by ourselves */

    /* continuation:
       {Table, Lastkey, EndCondition, MatchProgBin, HowManyReplaced}*/

    tptr = tuple_val(continuation);

    if (arityval(*tptr) != 5)
        erts_exit(ERTS_ERROR_EXIT,"Internal error in ets:select_replace/1");

    lastkey = tptr[2];
    end_condition = tptr[3];
    mp = erts_db_get_match_prog_binary_unchecked(tptr[4]);

    sc.p = p;
    sc.mp = mp;
    sc.end_condition = NIL;
    sc.lastobj = NULL;
    sc.max = 1000;
    sc.keypos = tbl->common.keypos;
    if (is_big(tptr[5])) {
        sc.replaced = big_to_uint32(tptr[5]);
    } else {
        sc.replaced = unsigned_val(tptr[5]);
    }
    prev_replaced = sc.replaced;

    if (iter) {
        iter->next_route_key = lastkey;
        sc.common.root = catree_find_prev_root(iter, NULL);
    }
    else {
        sc.common.root = &tbl->tree.root;
    }

    stack = get_any_stack(tbl, stack_container);
    traverse_update_backwards(&tbl->common, stack, lastkey, &doit_select_replace,
                              &sc.common, iter);
    release_stack(tbl, stack_container,stack);

    // the more objects we've replaced, the more reductions we've consumed
    BUMP_REDS(p, MIN(2000, (1000 - sc.max) + (sc.replaced - prev_replaced)));

    if (sc.max > 0) {
        RET_TO_BIF(erts_make_integer(sc.replaced,p), DB_ERROR_NONE);
    }
    key = GETKEY(tbl, sc.lastobj);
    if (end_condition != NIL &&
            (cmp_partly_bound(end_condition,key) > 0)) {
        /* done anyway */
        RET_TO_BIF(make_small(sc.replaced),DB_ERROR_NONE);
    }
    /* Not done yet, let's trap. */
    sz = size_object(key);
    if (IS_USMALL(0, sc.replaced)) {
        hp = HAlloc(p, sz + 6);
        ereplaced = make_small(sc.replaced);
    }
    else {
        hp = HAlloc(p, BIG_UINT_HEAP_SIZE + sz + 6);
        ereplaced = uint_to_big(sc.replaced, hp);
        hp += BIG_UINT_HEAP_SIZE;
    }
    key = copy_struct(key, sz, &hp, &MSO(p));
    continuation = TUPLE5
        (hp,
         tptr[1],
         key,
         tptr[3],
         tptr[4],
         ereplaced);
    RET_TO_BIF(bif_trap1(&ets_select_replace_continue_exp, p, continuation),
            DB_ERROR_NONE);

#undef RET_TO_BIF
}

static int db_select_replace_continue_tree(Process *p,
                                           DbTable *tbl,
                                           Eterm continuation,
                                           Eterm *ret,
                                           enum DbIterSafety* safety_p)
{
    return db_select_replace_continue_tree_common(p, tbl, continuation, ret,
                                                  &tbl->tree, NULL);
}

int db_select_replace_tree_common(Process *p, DbTable *tbl,
                                  Eterm tid, Eterm pattern, Eterm *ret,
                                  DbTableTree *stack_container,
                                  CATreeRootIterator* iter)
{
    DbTreeStack* stack;
    struct select_replace_context sc;
    struct mp_info mpi;
    Eterm lastkey = THE_NON_VALUE;
    Eterm key;
    Eterm continuation;
    unsigned sz;
    Eterm *hp;
    TreeDbTerm *this;
    int errcode;
    Eterm ereplaced;
    Eterm mpb;


#define RET_TO_BIF(Term,RetVal) do { 	       	\
	if (mpi.mp != NULL) {			\
	    erts_bin_free(mpi.mp);       	\
	}					\
	*ret = (Term); 				\
	return RetVal; 			        \
    } while(0)

    mpi.mp = NULL;

    sc.lastobj = NULL;
    sc.p = p;
    sc.tb = &tbl->common;
    sc.max = 1000;
    sc.end_condition = NIL;
    sc.keypos = tbl->common.keypos;
    sc.replaced = 0;

    if ((errcode = analyze_pattern(&tbl->common, pattern, db_match_keeps_key, &mpi)) != DB_ERROR_NONE) {
        RET_TO_BIF(NIL,errcode);
    }

    if (mpi.key_boundness == MS_KEY_IMPOSSIBLE) {
        RET_TO_BIF(make_small(0),DB_ERROR_NONE);
        /* can't possibly match anything */
    }

    sc.mp = mpi.mp;

    if (mpi.key_boundness == MS_KEY_BOUND) {
        TreeDbTerm** pp;
        ASSERT(CMP_EQ(mpi.least, mpi.most));
        if (iter)
            sc.common.root = catree_find_root(mpi.least, iter);
        else
            sc.common.root = &tbl->tree.root;
        pp = find_node2(&tbl->common, sc.common.root, mpi.least);
        if (pp) {
            doit_select_replace(&tbl->common, pp, &sc.common, 0 /* dummy */);
            reset_static_stack(stack_container); /* may refer replaced term */
        }
        RET_TO_BIF(erts_make_integer(sc.replaced,p),DB_ERROR_NONE);
    }

    stack = get_any_stack(tbl,stack_container);

    if (mpi.key_boundness == MS_KEY_PARTIALLY_BOUND) {
        this = find_next_from_pb_key(tbl, &sc.common.root, stack, mpi.most, iter);
        if (this)
            lastkey = GETKEY(tbl, this->dbterm.tpl);
        sc.end_condition = mpi.least;
    }
    else {
        ASSERT(mpi.key_boundness == MS_KEY_UNBOUND);
        if (iter)
            sc.common.root = catree_find_last_root(iter);
        else
            sc.common.root = &tbl->tree.root;
    }

    traverse_update_backwards(&tbl->common, stack, lastkey, &doit_select_replace,
                              &sc.common, iter);
    release_stack(tbl,stack_container,stack);
    // the more objects we've replaced, the more reductions we've consumed
    BUMP_REDS(p, MIN(2000, (1000 - sc.max) + sc.replaced));
    if (sc.max > 0) {
        RET_TO_BIF(erts_make_integer(sc.replaced,p),DB_ERROR_NONE);
    }

    key = GETKEY(tbl, sc.lastobj);
    sz = size_object(key);
    if (IS_USMALL(0, sc.replaced)) {
        hp = HAlloc(p, sz + ERTS_MAGIC_REF_THING_SIZE + 6);
        ereplaced = make_small(sc.replaced);
    }
    else {
        hp = HAlloc(p, BIG_UINT_HEAP_SIZE + sz + ERTS_MAGIC_REF_THING_SIZE + 6);
        ereplaced = uint_to_big(sc.replaced, hp);
        hp += BIG_UINT_HEAP_SIZE;
    }
    key = copy_struct(key, sz, &hp, &MSO(p));
    mpb = erts_db_make_match_prog_ref(p,mpi.mp,&hp);

    continuation = TUPLE5
        (hp,
         tid,
         key,
         sc.end_condition, /* From the match program, needn't be copied */
         mpb,
         ereplaced);

    /* Don't free mpi.mp, so don't use macro */
    *ret = bif_trap1(&ets_select_replace_continue_exp, p, continuation);
    return DB_ERROR_NONE;

#undef RET_TO_BIF

}

static int db_select_replace_tree(Process *p, DbTable *tbl, Eterm tid,
                                  Eterm pattern, Eterm *ret,
                                  enum DbIterSafety safety)
{
    return db_select_replace_tree_common(p, tbl, tid, pattern, ret,
                                         &tbl->tree, NULL);
}

int db_take_tree_common(Process *p, DbTable *tbl, TreeDbTerm **root,
                        Eterm key, Eterm *ret,
                        DbTreeStack *stack /* NULL if no static stack */)
{
    TreeDbTerm *this;

    *ret = NIL;
    this = linkout_tree(&tbl->common, root, key, stack);
    if (this) {
        Eterm copy, *hp, *hend;

        hp = HAlloc(p, this->dbterm.size + 2);
        hend = hp + this->dbterm.size + 2;
        copy = db_copy_object_from_ets(&tbl->common,
                                       &this->dbterm, &hp, &MSO(p));
        *ret = CONS(hp, copy, NIL);
        hp += 2;
        HRelease(p, hend, hp);
        free_term(tbl, this);
    }
    return DB_ERROR_NONE;
}

static int db_take_tree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    return db_take_tree_common(p, tbl, &tb->root,
                               key, ret, &tb->static_stack);
}

/*
** Other interface routines (not directly coupled to one bif)
*/

void db_print_tree_common(fmtfn_t to, void *to_arg,
                          int show, TreeDbTerm *root, DbTable *tbl)
{
#ifdef TREE_DEBUG
    if (show)
	erts_print(to, to_arg, "\nTree data dump:\n"
		   "------------------------------------------------\n");
    do_dump_tree2(&tbl->common, to, to_arg, show, root, 0);
    if (show)
	erts_print(to, to_arg, "\n"
		   "------------------------------------------------\n");
#else
    erts_print(to, to_arg, "Ordered set (AVL tree), Elements: %d\n",
               erts_flxctr_read_approx(&tbl->common.counters, ERTS_DB_TABLE_NITEMS_COUNTER_ID));
#endif
}

/* Display tree contents (for dump) */
static void db_print_tree(fmtfn_t to, void *to_arg,
			  int show,
			  DbTable *tbl)
{
    DbTableTree *tb = &tbl->tree;
    db_print_tree_common(to, to_arg, show, tb->root, tbl);
}

/* release all memory occupied by a single table */
static int db_free_empty_table_tree(DbTable *tbl)
{
    ASSERT(tbl->tree.root == NULL);
    while (db_free_table_continue_tree(tbl, ERTS_SWORD_MAX) < 0)
	;
    return 1;
}

static SWord db_free_table_continue_tree(DbTable *tbl, SWord reds)
{
    DbTableTree *tb = &tbl->tree;

    if (!tb->deletion) {
	tb->static_stack.pos = 0;
	tb->deletion = 1;
	PUSH_NODE(&tb->static_stack, tb->root);
    }
    reds = do_free_tree_continue(tb, reds);
    if (reds >= 0) {		/* Completely done. */
	erts_db_free(ERTS_ALC_T_DB_STK,
		     (DbTable *) tb,
		     (void *) tb->static_stack.array,
		     sizeof(TreeDbTerm *) * STACK_NEED);
	ASSERT(erts_flxctr_is_snapshot_ongoing(&tb->common.counters) ||
               ((APPROX_MEM_CONSUMED(tb)
                 == sizeof(DbTable)) ||
                (APPROX_MEM_CONSUMED(tb)
                 == (sizeof(DbTable) + sizeof(DbFixation)))));
    }
    return reds;
}

static SWord db_delete_all_objects_tree(Process* p,
                                        DbTable* tbl,
                                        SWord reds,
                                        Eterm* nitems_holder_wb)
{
    if (nitems_holder_wb != NULL) {
        Uint nr_of_items =
            erts_flxctr_read_centralized(&tbl->common.counters,
                                         ERTS_DB_TABLE_NITEMS_COUNTER_ID);
        *nitems_holder_wb = erts_make_integer(nr_of_items, p);
    }
    reds = db_free_table_continue_tree(tbl, reds);
    if (reds < 0)
        return reds;
    db_create_tree(p, tbl);
    RESET_NITEMS(tbl);
    return reds;
}

static Eterm db_delete_all_objects_get_nitems_from_holder_tree(Process* p,
                                                               Eterm holder)
{
    (void)p;
    return holder;
}

static void do_db_tree_foreach_offheap(TreeDbTerm *,
				       void (*)(ErlOffHeap *, void *),
				       void *);

void db_foreach_offheap_tree_common(TreeDbTerm *root,
                                    void (*func)(ErlOffHeap *, void *),
                                    void * arg)
{
    do_db_tree_foreach_offheap(root, func, arg);
}

static void db_foreach_offheap_tree(DbTable *tbl,
				    void (*func)(ErlOffHeap *, void *),
				    void * arg)
{
    db_foreach_offheap_tree_common(tbl->tree.root, func, arg);
}


/*
** Functions for internal use
*/


static void
do_db_tree_foreach_offheap(TreeDbTerm *tdbt,
			   void (*func)(ErlOffHeap *, void *),
			   void * arg)
{
    ErlOffHeap tmp_offheap;
    if(!tdbt)
	return;
    do_db_tree_foreach_offheap(tdbt->left, func, arg);
    tmp_offheap.first = tdbt->dbterm.first_oh;
    tmp_offheap.overhead = 0;
    (*func)(&tmp_offheap, arg);
    tdbt->dbterm.first_oh = tmp_offheap.first;
    do_db_tree_foreach_offheap(tdbt->right, func, arg);
}

static TreeDbTerm *linkout_tree(DbTableCommon *tb, TreeDbTerm **root,
                                Eterm key, DbTreeStack *stack) {
    TreeDbTerm **tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 0;
    int state = 0;
    TreeDbTerm **this = root;
    Sint c;
    int dir;
    TreeDbTerm *q = NULL;

    /*
     * Somewhat complicated, deletion in an AVL tree,
     * The two helpers balance_left and balance_right are used to
     * keep the balance. As in insert, we do the stacking ourselves.
     */

    reset_stack(stack);
    dstack[dpos++] = DIR_END;
    for (;;) {
	if (!*this) { /* Failure */
	    return NULL;
	} else if ((c = cmp_key(tb, key, *this)) < 0) {
	    dstack[dpos++] = DIR_LEFT;
	    tstack[tpos++] = this;
	    this = &((*this)->left);
	} else if (c > 0) { /* go right */
	    dstack[dpos++] = DIR_RIGHT;
	    tstack[tpos++] = this;
	    this = &((*this)->right);
	} else { /* Equal key, found the one to delete*/
	    q = (*this);
	    if (q->right == NULL) {
		(*this) = q->left;
		state = 1;
	    } else if (q->left == NULL) {
		(*this) = q->right;
		state = 1;
	    } else {
		dstack[dpos++] = DIR_LEFT;
		tstack[tpos++] = this;
		state = delsub(this);
	    }
            DEC_NITEMS(((DbTable*)tb));
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

static TreeDbTerm *linkout_object_tree(DbTableCommon *tb,  TreeDbTerm **root,
				       Eterm object, DbTableTree *stack)
{
    TreeDbTerm **tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 0;
    int state = 0;
    TreeDbTerm **this = root;
    Sint c;
    int dir;
    TreeDbTerm *q = NULL;
    Eterm key;

    /*
     * Somewhat complicated, deletion in an AVL tree,
     * The two helpers balance_left and balance_right are used to
     * keep the balance. As in insert, we do the stacking ourselves.
     */

    
    key = GETKEY(tb, tuple_val(object));

    reset_static_stack(stack);
    dstack[dpos++] = DIR_END;
    for (;;) {
	if (!*this) { /* Failure */
	    return NULL;
	} else if ((c = cmp_key(tb,key,*this)) < 0) {
	    dstack[dpos++] = DIR_LEFT;
	    tstack[tpos++] = this;
	    this = &((*this)->left);
	} else if (c > 0) { /* go right */
	    dstack[dpos++] = DIR_RIGHT;
	    tstack[tpos++] = this;
	    this = &((*this)->right);
	} else { /* Equal key, found the only possible matching object*/
	    if (!db_eq(tb,object,&(*this)->dbterm)) {
		return NULL;
	    }
	    q = (*this);
	    if (q->right == NULL) {
		(*this) = q->left;
		state = 1;
	    } else if (q->left == NULL) {
		(*this) = q->right;
		state = 1;
	    } else {
		dstack[dpos++] = DIR_LEFT;
		tstack[tpos++] = this;
		state = delsub(this);
	    }
            DEC_NITEMS(((DbTable*)tb));
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

/*
** For the select functions, analyzes the pattern and determines which
** part of the tree should be searched. Also compiles the match program
*/
static int analyze_pattern(DbTableCommon *tb, Eterm pattern,
                           extra_match_validator_t extra_validator, /* Optional callback */
                           struct mp_info *mpi)
{
    Eterm lst, tpl, ttpl;
    Eterm *matches,*guards, *bodies;
    Eterm sbuff[30];
    Eterm *buff = sbuff;
    Eterm *ptpl;
    int i;
    int num_heads = 0;
    Eterm least = THE_NON_VALUE;
    Eterm most = THE_NON_VALUE;
    enum ms_key_boundness boundness;

    mpi->key_boundness = MS_KEY_IMPOSSIBLE;
    mpi->mp = NULL;

    for (lst = pattern; is_list(lst); lst = CDR(list_val(lst)))
	++num_heads;

    if (lst != NIL) {/* proper list... */
	return DB_ERROR_BADPARAM;
    }
    if (num_heads > 10) {
	buff = erts_alloc(ERTS_ALC_T_DB_TMP, sizeof(Eterm) * num_heads * 3);
    }

    matches = buff;
    guards = buff + num_heads;
    bodies = buff + (num_heads * 2);

    i = 0;
    for(lst = pattern; is_list(lst); lst = CDR(list_val(lst))) {
        Eterm match;
        Eterm guard;
        Eterm body;
        Eterm key;

	ttpl = CAR(list_val(lst));
	if (!is_tuple(ttpl)) {
	    if (buff != sbuff) { 
		erts_free(ERTS_ALC_T_DB_TMP, buff);
	    }
	    return DB_ERROR_BADPARAM;
	}
	ptpl = tuple_val(ttpl);
	if (ptpl[0] != make_arityval(3U)) {
	    if (buff != sbuff) { 
		erts_free(ERTS_ALC_T_DB_TMP, buff);
	    }
	    return DB_ERROR_BADPARAM;
	}
	matches[i] = match = tpl = ptpl[1];
	guards[i] = guard = ptpl[2];
	bodies[i] = body = ptpl[3];

        if(extra_validator != NULL && !extra_validator(tb->keypos, match, guard, body)) {
	    if (buff != sbuff) {
		erts_free(ERTS_ALC_T_DB_TMP, buff);
	    }
            return DB_ERROR_BADPARAM;
        }

	if (!is_list(body) || CDR(list_val(body)) != NIL ||
	    CAR(list_val(body)) != am_DollarUnderscore) {
	}
	++i;

        boundness = key_boundness(tb, tpl, &key);
	switch (boundness)
        {
        case MS_KEY_BOUND:
        case MS_KEY_PARTIALLY_BOUND:
            if (is_non_value(least) || partly_bound_can_match_lesser(key,least)) {
                least = key;
            }
            if (is_non_value(most) || partly_bound_can_match_greater(key,most)) {
                most = key;
            }
            break;
        case MS_KEY_IMPOSSIBLE:
        case MS_KEY_UNBOUND:
            break;
        }
        if (mpi->key_boundness > boundness)
            mpi->key_boundness = boundness;
    }

    if (mpi->key_boundness == MS_KEY_BOUND && !CMP_EQ(least, most)) {
        /* Several different bound keys */
        mpi->key_boundness = MS_KEY_PARTIALLY_BOUND;
    }
    mpi->least = least;
    mpi->most = most;

    /*
     * It would be nice not to compile the match_spec if nothing could match,
     * but then the select calls would not fail like they should on bad 
     * match specs that happen to specify non existent keys etc.
     */
    if ((mpi->mp = db_match_compile(matches, guards, bodies,
				    num_heads, DCOMP_TABLE, NULL)) 
	== NULL) {
	if (buff != sbuff) { 
	    erts_free(ERTS_ALC_T_DB_TMP, buff);
	}
	return DB_ERROR_BADPARAM;
    }
    if (buff != sbuff) { 
	erts_free(ERTS_ALC_T_DB_TMP, buff);
    }
    return DB_ERROR_NONE;
}

static SWord do_free_tree_continue(DbTableTree *tb, SWord reds)
{
    TreeDbTerm *root;
    TreeDbTerm *p;

    for (;;) {
	root = POP_NODE(&tb->static_stack);
	if (root == NULL) break;
	for (;;) {
	    if ((p = root->left) != NULL) {
		root->left = NULL;
		PUSH_NODE(&tb->static_stack, root);
		root = p;
	    } else if ((p = root->right) != NULL) {
		root->right = NULL;
		PUSH_NODE(&tb->static_stack, root);
		root = p;
	    } else {
		free_term((DbTable*)tb, root);
		if (--reds < 0) {
                    return reds;   /* Done enough for now */
                }
                break;
	    }
	}
    }
    return reds;
}

/*
 * Deletion helpers
 */
int tree_balance_left(TreeDbTerm **this) 
{
    TreeDbTerm *p, *p1, *p2;
    int b1, b2, h = 1;
    
    p = *this;
    switch (p->balance) {
    case -1:
	p->balance = 0;
	break;
    case 0:
	p->balance = 1;
	h = 0;
	break;
    case 1:
	p1 = p->right;
	b1 = p1->balance;
	if (b1 >= 0) { /* Single RR rotation */
	    p->right = p1->left;
	    p1->left = p;
	    if (b1 == 0) {
		p->balance = 1;
		p1->balance = -1;
		h = 0;
	    } else {
		p->balance = p1->balance = 0;
	    }
	    (*this) = p1;
	} else { /* Double RL rotation */
	    p2 = p1->left;
	    b2 = p2->balance;
	    p1->left = p2->right;
	    p2->right = p1;
	    p->right = p2->left;
	    p2->left = p;
	    p->balance = (b2 == 1) ? -1 : 0;
	    p1->balance = (b2 == -1) ? 1 : 0;
	    p2->balance = 0;
	    (*this) = p2;
	}
	break;
    }
    return h;
}

int tree_balance_right(TreeDbTerm **this) 
{
    TreeDbTerm *p, *p1, *p2;
    int b1, b2, h = 1;
    
    p = *this;
    switch (p->balance) {
    case 1:
	p->balance = 0;
	break;
    case 0:
	p->balance = -1;
	h = 0;
	break;
    case -1:
	p1 = p->left;
	b1 = p1->balance;
	if (b1 <= 0) { /* Single LL rotation */
	    p->left = p1->right;
	    p1->right = p;
	    if (b1 == 0) {
		p->balance = -1;
		p1->balance = 1;
		h = 0;
	    } else {
		p->balance = p1->balance = 0;
	    }
	    (*this) = p1;
	} else { /* Double LR rotation */
	    p2 = p1->right;
	    b2 = p2->balance;
	    p1->right = p2->left;
	    p2->left = p1;
	    p->left = p2->right;
	    p2->right = p;
	    p->balance = (b2 == -1) ? 1 : 0;
	    p1->balance = (b2 == 1) ? -1 : 0;
	    p2->balance = 0;
	    (*this) = p2;
	}
    }
    return h;
}

static int delsub(TreeDbTerm **this) 
{
    TreeDbTerm **tstack[STACK_NEED];
    int tpos = 0;
    TreeDbTerm *q = (*this);
    TreeDbTerm **r = &(q->left);
    int h;

    /*
     * Walk down the tree to the right and search 
     * for a void right child, pick that child out
     * and return it to be put in the deleted 
     * object's place.
     */
    
    while ((*r)->right != NULL) {
	tstack[tpos++] = r;
	r = &((*r)->right);
    }
    *this = *r;
    *r = (*r)->left;
    (*this)->left = q->left;
    (*this)->right = q->right;
    (*this)->balance = q->balance;
    tstack[0] = &((*this)->left);
    h = 1;
    while (tpos && h) {
	r = tstack[--tpos];
	h = tree_balance_right(r);
    }
    return h;
}

/*
 * Helper for db_slot
 */

static TreeDbTerm *slot_search(Process *p, TreeDbTerm *root,
                               Sint slot, DbTable *tb,
                               DbTableTree *stack_container,
                               CATreeRootIterator *iter,
                               int* is_EOT)
{
    TreeDbTerm *this;
    TreeDbTerm *tmp;
    TreeDbTerm *lastobj;
    Eterm lastkey;
    TreeDbTerm **pp;
    DbTreeStack* stack;

    if (iter) {
        /* Find first non-empty tree */
        while (!root) {
            TreeDbTerm** pp = catree_find_next_root(iter, NULL);
            if (!pp)
                return NULL;
            root = *pp;
        }
    }

    stack = get_any_stack(tb,stack_container);
    ASSERT(stack != NULL);

    if (slot == 1) { /* Don't search from where we are if we are 
			looking for the first slot */
	stack->slot = 0;
    }

    if (stack->slot == 0) { /* clear stack if slot positions 
				are not recorded */
	stack->pos = 0;
    }
    while (1) {
        if (EMPTY_NODE(stack)) {
            this = root;
            if (this == NULL)
                goto next_root;
            while (this->left != NULL){
                PUSH_NODE(stack, this);
                this = this->left;
            }
            PUSH_NODE(stack, this);
            stack->slot++;
        }
        this = TOP_NODE(stack);
        while (stack->slot != slot) {
            ASSERT(this);
            lastobj = this;
            if (slot > stack->slot) {
                if (this->right != NULL) {
                    this = this->right;
                    while (this->left != NULL) {
                        PUSH_NODE(stack, this);
                        this = this->left;
                    }
                    PUSH_NODE(stack, this);
                } else {
                    for (;;) {
                        tmp = POP_NODE(stack);
                        this = TOP_NODE(stack);
                        if (!this)
                            goto next_root;
                        if (this->left == tmp)
                            break;
                    }
                }
                ++(stack->slot);
            } else {
                if (this->left != NULL) {
                    this = this->left;
                    while (this->right != NULL) {
                        PUSH_NODE(stack, this);
                        this = this->right;
                    }
                    PUSH_NODE(stack, this);
                } else {
                    for (;;) {
                        tmp = POP_NODE(stack);
                        this = TOP_NODE(stack);
                        if (!this)
                            goto next_root;
                        if (this->right == tmp)
                            break;
                    }
                }
                --(stack->slot);
            }
        }
         /* Found slot */
        ASSERT(this);
        break;

next_root:
        if (!iter) {
            if (stack->slot == (slot-1)) {
                *is_EOT = 1;
            }
            break; /* EOT */
        }

        ASSERT(slot > stack->slot);
        if (lastobj) {
            lastkey = GETKEY(tb, lastobj->dbterm.tpl);
            lastobj = NULL;
        }
        pp = catree_find_next_root(iter, &lastkey);
        if (!pp) {
            if (stack->slot == (slot-1)) {
                *is_EOT = 1;
            }
            break; /* EOT */
        }
        root = *pp;
        stack->pos = 0;
        find_next(&tb->common, root, stack, lastkey);
    }

    release_stack(tb,stack_container,stack);
    return this;
}

/*
 * Find next and previous in sort order
 */

static TreeDbTerm *find_next(DbTableCommon *tb, TreeDbTerm *root,
                             DbTreeStack* stack, Eterm key) {
    TreeDbTerm *this;
    TreeDbTerm *tmp;
    Sint c;

    if(( this = TOP_NODE(stack)) != NULL) {
	if (!cmp_key_eq(tb,key,this)) {
	    /* Start from the beginning */
	    stack->pos = stack->slot = 0;
	}
    }
    if (EMPTY_NODE(stack)) { /* Have to rebuild the stack */
	if (( this = root ) == NULL)
	    return NULL;
	for (;;) {
	    PUSH_NODE(stack, this);
	    if (( c = cmp_key(tb,key,this) ) > 0) {
		if (this->right == NULL) /* We are at the previos 
					    and the element does
					    not exist */
		    break;
		else
		    this = this->right;
	    } else if (c < 0) {
		if (this->left == NULL) /* Done */
                    goto found_next;
		else
		    this = this->left;
	    } else
		break;
	}
    }
    /* The next element from this... */
    if (this->right != NULL) {
	this = this->right;
	PUSH_NODE(stack,this);
	while (this->left != NULL) {
	    this = this->left;
	    PUSH_NODE(stack, this);
	}
    } else {
	do {
	    tmp = POP_NODE(stack);
	    if (( this = TOP_NODE(stack)) == NULL) {
		stack->slot = 0;
		return NULL;
	    }
	} while (this->right == tmp);
    }

found_next:
    if (stack->slot > 0)
        ++(stack->slot);

    return this;
}

static TreeDbTerm *find_prev(DbTableCommon *tb, TreeDbTerm *root,
                             DbTreeStack* stack, Eterm key) {
    TreeDbTerm *this;
    TreeDbTerm *tmp;
    Sint c;

    if(( this = TOP_NODE(stack)) != NULL) {
	if (!cmp_key_eq(tb,key,this)) {
	    /* Start from the beginning */
	    stack->pos = stack->slot = 0;
	}
    }
    if (EMPTY_NODE(stack)) { /* Have to rebuild the stack */
	if (( this = root ) == NULL)
	    return NULL;
	for (;;) {
	    PUSH_NODE(stack, this);
	    if (( c = cmp_key(tb,key,this) ) < 0) {
		if (this->left == NULL) /* We are at the next 
					   and the element does
					   not exist */
		    break;
		else
		    this = this->left;
	    } else if (c > 0) {
		if (this->right == NULL) /* Done */
                    goto found_prev;
		else
		    this = this->right;
	    } else
		break;
	}
    }
    /* The previous element from this... */
    if (this->left != NULL) {
	this = this->left;
	PUSH_NODE(stack,this);
	while (this->right != NULL) {
	    this = this->right;
	    PUSH_NODE(stack, this);
	}
    } else {
	do {
	    tmp = POP_NODE(stack);
	    if (( this = TOP_NODE(stack)) == NULL) {
		stack->slot = 0;
		return NULL;
	    }
	} while (this->left == tmp);
    }

found_prev:
    if (stack->slot > 0)
        --(stack->slot);

    return this;
}


/* @brief Find object with smallest key of all larger than partially bound key.
 * Can be used as a starting point for a reverse iteration with pb_key.
 *
 * @param pb_key The partially bound key. Example {42, '$1'}
 * @param *rootpp Will return pointer to root pointer of tree with found object.
 * @param iter Root iterator or NULL for plain DbTableTree.
 * @param stack A stack to use. Will be cleared.
 *
 * @return found object or NULL if no such key exists.
 */
static TreeDbTerm *find_next_from_pb_key(DbTable *tbl,  TreeDbTerm*** rootpp,
                                         DbTreeStack* stack, Eterm pb_key,
                                         CATreeRootIterator* iter)
{
    TreeDbTerm* root;
    TreeDbTerm *this;
    Uint candidate = 0;
    Sint c;

    if (iter) {
        *rootpp = catree_find_next_from_pb_key_root(pb_key, iter);
        ASSERT(*rootpp);
        root = **rootpp;
    }
    else {
        *rootpp = &tbl->tree.root;
        root = tbl->tree.root;
    }

    /* spool the stack, we have to "re-search" */
    stack->pos = stack->slot = 0;
    if (( this = root ) == NULL)
	return NULL;
    for (;;) {
	PUSH_NODE(stack, this);
	if (( c = cmp_partly_bound(pb_key,GETKEY(tbl, this->dbterm.tpl))) >= 0) {
	    if (this->right == NULL) {
                stack->pos = candidate;
                return TOP_NODE(stack);
	    }
            this = this->right;
	} else /*if (c < 0)*/ {
	    if (this->left == NULL) /* Done */
		return this;
            candidate = stack->pos;
            this = this->left;
	} 
    }
}

/* @brief Find object with largest key of all smaller than partially bound key.
 * Can be used as a starting point for a forward iteration with pb_key.
 *
 * @param pb_key The partially bound key. Example {42, '$1'}
 * @param *rootpp Will return pointer to root pointer of found object.
 * @param iter Root iterator or NULL for plain DbTableTree.
 * @param stack A stack to use. Will be cleared.
 *
 * @return found object or NULL if no such key exists.
 */
static TreeDbTerm *find_prev_from_pb_key(DbTable *tbl, TreeDbTerm*** rootpp,
                                         DbTreeStack* stack, Eterm pb_key,
                                         CATreeRootIterator* iter)
{
    TreeDbTerm* root;
    TreeDbTerm *this;
    Uint candidate = 0;
    Sint c;

    if (iter) {
        *rootpp = catree_find_prev_from_pb_key_root(pb_key, iter);
        ASSERT(*rootpp);
        root = **rootpp;
    }
    else {
        *rootpp = &tbl->tree.root;
        root = tbl->tree.root;
    }

    /* spool the stack, we have to "re-search" */
    stack->pos = stack->slot = 0;
    if (( this = root ) == NULL)
	return NULL;
    for (;;) {
	PUSH_NODE(stack, this);
	if (( c = cmp_partly_bound(pb_key,GETKEY(tbl, this->dbterm.tpl))) <= 0) {
	    if (this->left == NULL) {
                stack->pos = candidate;
                return TOP_NODE(stack);
	    }
            this = this->left;
	} else /*if (c > 0)*/ {
	    if (this->right == NULL) /* Done */
		return this;
            candidate = stack->pos;
            this = this->right;
	} 
    }
}


/*
 * Just lookup a node
 */
static TreeDbTerm *find_node(DbTableCommon *tb, TreeDbTerm *root,
                             Eterm key, DbTableTree *stack_container)
{
    TreeDbTerm *this;
    Sint res;
    DbTreeStack* stack = get_static_stack(stack_container);

    if(!stack || EMPTY_NODE(stack)
       || !cmp_key_eq(tb, key, (this=TOP_NODE(stack)))) {

	this = root;
	while (this != NULL && (res = cmp_key(tb,key,this)) != 0) {
	    if (res < 0)
		this = this->left;
	    else
		this = this->right;
	}
    }
    if (stack) {
	release_stack((DbTable*)tb,stack_container,stack);
    }
    return this;
}


TreeDbTerm *db_find_tree_node_common(DbTableCommon *tb, TreeDbTerm *root,
                                     Eterm key)
{
    return find_node(tb, root, key, NULL);
}


/*
 * Lookup a node and return the address of the node pointer in the tree
 */
static TreeDbTerm **find_node2(DbTableCommon *tb, TreeDbTerm **root, Eterm key)
{
    TreeDbTerm **this;
    Sint res;

    this = root;
    while ((*this) != NULL && (res = cmp_key(tb, key, *this)) != 0) {
	if (res < 0)
	    this = &((*this)->left);
	else
	    this = &((*this)->right);
    }
    if (*this == NULL)
	return NULL;
    return this;
}

/*
 * Find node and return the address of the node pointer (NULL if not found)
 * Tries to reuse the existing stack for performance.
 */

static TreeDbTerm **find_ptr(DbTableCommon *tb, TreeDbTerm **root,
                             DbTreeStack *stack, TreeDbTerm *this) {
    Eterm key = GETKEY(tb, this->dbterm.tpl);
    TreeDbTerm *tmp;
    TreeDbTerm *parent;
    Sint c;

    if(( tmp = TOP_NODE(stack)) != NULL) {
	if (!cmp_key_eq(tb,key,tmp)) {
	    /* Start from the beginning */
	    stack->pos = stack->slot = 0;
	}
    }
    if (EMPTY_NODE(stack)) { /* Have to rebuild the stack */
	if (( tmp = *root ) == NULL)
	    return NULL;
	for (;;) {
	    PUSH_NODE(stack, tmp);
	    if (( c = cmp_key(tb,key,tmp) ) < 0) {
		if (tmp->left == NULL) /* We are at the next
					   and the element does
					   not exist */
		    break;
		else
		    tmp = tmp->left;
	    } else if (c > 0) {
		if (tmp->right == NULL) /* Done */
		    return NULL;
		else
		    tmp = tmp->right;
	    } else
		break;
	}
    }

    if (TOP_NODE(stack) != this)
        return NULL;

    parent = TOPN_NODE(stack, 1);
    if (parent == NULL)
        return ((this != *root) ? NULL : root);
    if (parent->left == this)
        return &(parent->left);
    if (parent->right == this)
        return &(parent->right);
    return NULL;
}

int db_lookup_dbterm_tree_common(Process *p, DbTable *tbl, TreeDbTerm **root,
                                 Eterm key, Eterm obj, DbUpdateHandle* handle,
                                 DbTableTree *stack_container)
{
    TreeDbTerm **pp = find_node2(&tbl->common, root, key);
    int flags = 0;

    if (pp == NULL) {
        if (obj == THE_NON_VALUE) {
            return 0;
        } else {
            Eterm *objp = tuple_val(obj);
            int arity = arityval(*objp);
            Eterm *htop, *hend;

            ASSERT(arity >= tbl->common.keypos);
            htop = HAlloc(p, arity + 1);
            hend = htop + arity + 1;
            sys_memcpy(htop, objp, sizeof(Eterm) * (arity + 1));
            htop[tbl->common.keypos] = key;
            obj = make_tuple(htop);

            if (db_put_tree_common(&tbl->common, root,
                                   obj, 1, stack_container) != DB_ERROR_NONE) {
                return 0;
            }

            pp = find_node2(&tbl->common, root, key);
            ASSERT(pp != NULL);
            HRelease(p, hend, htop);
            flags |= DB_NEW_OBJECT;
        }
    }

    handle->tb = tbl;
    handle->dbterm = &(*pp)->dbterm;
    handle->flags = flags;
    handle->bp = (void**) pp;
    handle->new_size = (*pp)->dbterm.size;
    return 1;
}

static int
db_lookup_dbterm_tree(Process *p, DbTable *tbl, Eterm key, Eterm obj,
                      DbUpdateHandle* handle)
{
    DbTableTree *tb = &tbl->tree;
    return db_lookup_dbterm_tree_common(p, tbl, &tb->root, key, obj, handle, tb);
}

void db_finalize_dbterm_tree_common(int cret,
                                    DbUpdateHandle *handle,
                                    TreeDbTerm **root,
                                    DbTableTree *stack_container)
{
    DbTable *tbl = handle->tb;
    TreeDbTerm *bp = (TreeDbTerm *) *handle->bp;

    if (handle->flags & DB_NEW_OBJECT && cret != DB_ERROR_NONE) {
        Eterm ret;
        db_erase_tree_common(tbl,
                             root,
                             GETKEY(&tbl->common, bp->dbterm.tpl),
                             &ret,
                             (stack_container == NULL ?
                              NULL : &stack_container->static_stack));
    } else if (handle->flags & DB_MUST_RESIZE) {
	db_finalize_resize(handle, offsetof(TreeDbTerm,dbterm));
        reset_static_stack(stack_container);

        free_term(tbl, bp);
    }
#ifdef DEBUG
    handle->dbterm = 0;
#endif
    return;
}   

static void
db_finalize_dbterm_tree(int cret, DbUpdateHandle *handle)
{
    DbTable *tbl = handle->tb;
    DbTableTree *tb = &tbl->tree;
    db_finalize_dbterm_tree_common(cret, handle, &tb->root, tb);
}

static int db_get_binary_info_tree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    *ret = db_binary_info_tree_common(p, find_node(&tbl->common, tbl->tree.root,
                                                   key, &tbl->tree));
    return DB_ERROR_NONE;
}

Eterm db_binary_info_tree_common(Process* p, TreeDbTerm* this)
{
    Eterm *hp, *hp_end;
    Uint hsz;
    Eterm ret;

    if (this == NULL) {
	ret = NIL;
    } else {
        ErlOffHeap oh;
        hsz = 0;

        oh.first = this->dbterm.first_oh;
        erts_bld_bin_list(NULL, &hsz, &oh, NIL);

        hp = HAlloc(p, hsz);
        hp_end = hp + hsz;
        oh.first = this->dbterm.first_oh;
        ret = erts_bld_bin_list(&hp, NULL, &oh, NIL);
        ASSERT(hp == hp_end); (void)hp_end;
    }
    return ret;
}


/*
 * Traverse the tree with a callback function, used by db_match_xxx
 */
static void traverse_backwards(DbTableCommon *tb,
			       DbTreeStack* stack,
			       Eterm lastkey,
                               traverse_doit_funcT* doit,
                               struct select_common *context,
                               CATreeRootIterator* iter)
{
    TreeDbTerm *this, *next;
    TreeDbTerm** root = context->root;

    if (lastkey == THE_NON_VALUE) {
        if (iter) {
            while (*root == NULL) {
                root = catree_find_prev_root(iter, NULL);
                if (!root)
                    return;
            }
            context->root = root;
        }
        stack->pos = stack->slot = 0;
        next = *root;
        while (next != NULL) {
            PUSH_NODE(stack, next);
            next = next->right;
        }
        next = TOP_NODE(stack);
    } else {
        next = find_prev(tb, *root, stack, lastkey);
    }

    while (1) {
        while (next) {
            this = next;
            lastkey = GETKEY(tb, this->dbterm.tpl);
            next = find_prev(tb, *root, stack, lastkey);
            if (!((*doit)(tb, this, context, 0)))
                return;
        }

        if (!iter)
            return;
        ASSERT(is_value(lastkey));
        root = catree_find_prev_root(iter, &lastkey);
        if (!root)
            return;
        context->root = root;
        stack->pos = stack->slot = 0;
        next = find_prev(tb, *root, stack, lastkey);
    }
}

/*
 * Traverse the tree with a callback function, used by db_match_xxx
 */
static void traverse_forward(DbTableCommon *tb,
			     DbTreeStack* stack,
			     Eterm lastkey,
                             traverse_doit_funcT* doit,
                             struct select_common *context,
                             CATreeRootIterator* iter)
{
    TreeDbTerm *this, *next;
    TreeDbTerm **root = context->root;

    if (lastkey == THE_NON_VALUE) {
        if (iter) {
            while (*root == NULL) {
                root = catree_find_next_root(iter, NULL);
                if (!root)
                    return;
            }
            context->root = root;
        }
        stack->pos = stack->slot = 0;
        next = *root;
        while (next != NULL) {
            PUSH_NODE(stack, next);
            next = next->left;
        }
        next = TOP_NODE(stack);
    } else {
        next = find_next(tb, *root, stack, lastkey);
    }

    while (1) {
        while (next) {
            this = next;
            lastkey = GETKEY(tb, this->dbterm.tpl);
            next = find_next(tb, *root, stack, lastkey);
            if (!((*doit)(tb, this, context, 1)))
                return;
        }

        if (!iter)
            return;
        ASSERT(is_value(lastkey));
        root = catree_find_next_root(iter, &lastkey);
        if (!root)
            return;
        context->root = root;
        stack->pos = stack->slot = 0;
        next = find_next(tb, *root, stack, lastkey);
    }
}

/*
 * Traverse the tree with an update callback function, used by db_select_replace
 */
static void traverse_update_backwards(DbTableCommon *tb,
                                      DbTreeStack* stack,
                                      Eterm lastkey,
                                      int (*doit)(DbTableCommon*,
                                                  TreeDbTerm**,
                                                  struct select_common*,
                                                  int),
                                      struct select_common* context,
                                      CATreeRootIterator* iter)
{
    int res;
    TreeDbTerm *this, *next, **this_ptr;
    TreeDbTerm** root = context->root;

    if (lastkey == THE_NON_VALUE) {
        if (iter) {
            while (*root == NULL) {
                root = catree_find_prev_root(iter, NULL);
                if (!root)
                    return;
                context->root = root;
            }
        }
        stack->pos = stack->slot = 0;
        next = *root;
        while (next) {
            PUSH_NODE(stack, next);
            next = next->right;
        }
        next = TOP_NODE(stack);
    }
    else
        next = find_prev(tb, *root, stack, lastkey);


    while (1) {
        while (next) {
            this = next;
            this_ptr = find_ptr(tb, root, stack, this);
            ASSERT(this_ptr != NULL);
            res = (*doit)(tb, this_ptr, context, 0);
            this = *this_ptr;
            REPLACE_TOP_NODE(stack, this);
            if (!res)
                return;
            lastkey = GETKEY(tb, this->dbterm.tpl);
            next = find_prev(tb, *root, stack, lastkey);
        }

        if (!iter)
            return;
        ASSERT(is_value(lastkey));
        root = catree_find_prev_root(iter, &lastkey);
        if (!root)
            return;
        context->root = root;
        stack->pos = stack->slot = 0;
        next = find_prev(tb, *root, stack, lastkey);
    }
}

static enum ms_key_boundness key_boundness(DbTableCommon *tb,
                                           Eterm pattern, Eterm *keyp)
{
    Eterm key;

    if (pattern == am_Underscore || db_is_variable(pattern) != -1)
	return MS_KEY_UNBOUND;
    key = db_getkey(tb->keypos, pattern);
    if (is_non_value(key))
	return MS_KEY_IMPOSSIBLE;  /* can't possibly match anything */
    if (!db_has_variable(key)) {   /* Bound key */
        *keyp = key;
	return MS_KEY_BOUND;
    } else if (key != am_Underscore &&
	       db_is_variable(key) < 0 && !db_has_map(key)) {

	*keyp = key;
        return MS_KEY_PARTIALLY_BOUND;
    }
	
    return MS_KEY_UNBOUND;
}



static Sint do_cmp_partly_bound(Eterm a, Eterm b, int *done)
{
    Eterm* aa;
    Eterm* bb;
    Eterm a_hdr;
    Eterm b_hdr;
    int i;
    Sint j;

    /* A variable matches anything */
    if (is_atom(a) && (a == am_Underscore || (db_is_variable(a) >= 0))) {
	*done = 1;
	return 0;
    }
    if (is_same(a,b))
	return 0;
    
    switch (a & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_LIST:
	if (!is_list(b)) {
	    return CMP(a,b);
	}
	aa = list_val(a);
	bb = list_val(b);
	while (1) {
	    if ((j = do_cmp_partly_bound(*aa++, *bb++, done)) != 0 || *done)
		return j;
	    if (is_same(*aa, *bb))
		return 0;
	    if (is_not_list(*aa) || is_not_list(*bb))
		return do_cmp_partly_bound(*aa, *bb, done);
	    aa = list_val(*aa);
	    bb = list_val(*bb);
	}
    case TAG_PRIMARY_BOXED:
	if ((b & _TAG_PRIMARY_MASK) != TAG_PRIMARY_BOXED) {
	    return CMP(a,b);
	}
	a_hdr = ((*boxed_val(a)) & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE;
	b_hdr = ((*boxed_val(b)) & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE;
	if (a_hdr != b_hdr) {
	    return CMP(a,b);
	}
	if (a_hdr == (_TAG_HEADER_ARITYVAL >> _TAG_PRIMARY_SIZE)) {
	    aa = tuple_val(a);
	    bb = tuple_val(b);
	    /* compare the arities */
	    i = arityval(*aa);	/* get the arity*/
	    if (i < arityval(*bb)) return(-1);
	    if (i > arityval(*bb)) return(1);
	    while (i--) {
		if ((j = do_cmp_partly_bound(*++aa, *++bb, done)) != 0
		    || *done) 
		    return j;
	    }
	    return 0;
	}
	/* Drop through */
      default:
	  return CMP(a,b);
    }
}

Sint cmp_partly_bound(Eterm partly_bound_key, Eterm bound_key)
{
    int done = 0;
    Sint ret = do_cmp_partly_bound(partly_bound_key, bound_key, &done);
#ifdef HARDDEBUG
    erts_fprintf(stderr,"\ncmp_partly_bound: %T", partly_bound_key);
    if (ret < 0)
	erts_fprintf(stderr," < ");
    else if (ret > 0)
	erts_fprintf(stderr," > ");
    else
	erts_fprintf(stderr," == ");
    erts_fprintf(stderr,"%T\n", bound_key);
#endif
    return ret;
}

/*
** For partly_bound debugging....
**
BIF_RETTYPE ets_testnisse_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Eterm r1 = make_small(partly_bound_can_match_lesser(BIF_ARG_1,
							BIF_ARG_2));
    Eterm r2 = make_small(partly_bound_can_match_greater(BIF_ARG_1,
							 BIF_ARG_2));
    Eterm *hp = HAlloc(BIF_P,3);
    Eterm ret;

    ret = TUPLE2(hp,r1,r2);
    BIF_RET(ret);
}
**
*/
static int partly_bound_can_match_lesser(Eterm partly_bound_1, 
					 Eterm partly_bound_2) 
{
    int done = 0;
    int ret = do_partly_bound_can_match_lesser(partly_bound_1, 
					       partly_bound_2, 
					       &done);
#ifdef HARDDEBUG
    erts_fprintf(stderr,"\npartly_bound_can_match_lesser: %T",partly_bound_1);
    if (ret)
	erts_fprintf(stderr," can match lesser than ");
    else
	erts_fprintf(stderr," cannot match lesser than ");
    erts_fprintf(stderr,"%T\n",partly_bound_2);
#endif
    return ret;
}

static int partly_bound_can_match_greater(Eterm partly_bound_1, 
					  Eterm partly_bound_2) 
{
    int done = 0;
    int ret = do_partly_bound_can_match_greater(partly_bound_1, 
						partly_bound_2, 
						&done);
#ifdef HARDDEBUG
    erts_fprintf(stderr,"\npartly_bound_can_match_greater: %T",partly_bound_1);
    if (ret)
	erts_fprintf(stderr," can match greater than ");
    else
	erts_fprintf(stderr," cannot match greater than ");
    erts_fprintf(stderr,"%T\n",partly_bound_2);
#endif
    return ret;
}

static int do_partly_bound_can_match_lesser(Eterm a, Eterm b, 
					    int *done)
{
    Eterm* aa;
    Eterm* bb;
    Sint i;
    int j;

    if (is_atom(a) && (a == am_Underscore || 
		       (db_is_variable(a) >= 0))) {
	*done = 1;
	if (is_atom(b) && (b == am_Underscore || 
			   (db_is_variable(b) >= 0))) {
	    return 0;
	} else {
	    return 1;
	}
    } else if (is_atom(b) && (b == am_Underscore || 
			      (db_is_variable(b) >= 0))) {
	*done = 1;
	return 0;
    }

    if (a == b)
	return 0;

    if (not_eq_tags(a,b)) {
	*done = 1;
	return (CMP(a, b) < 0) ? 1 : 0;
    }

    /* we now know that tags are the same */
    switch (tag_val_def(a)) {
    case TUPLE_DEF:
	aa = tuple_val(a);
	bb = tuple_val(b);
	/* compare the arities */
	if (arityval(*aa) < arityval(*bb)) return 1;
	if (arityval(*aa) > arityval(*bb)) return 0;
	i = arityval(*aa);	/* get the arity*/
	while (i--) {
	    if ((j = do_partly_bound_can_match_lesser(*++aa, *++bb, 
						      done)) != 0 
		|| *done) 
		return j;
	}
	return 0;
    case LIST_DEF:
	aa = list_val(a);
	bb = list_val(b);
	while (1) {
	    if ((j = do_partly_bound_can_match_lesser(*aa++, *bb++, 
						      done)) != 0 
		|| *done) 
		return j;
	    if (*aa==*bb)
		return 0;
	    if (is_not_list(*aa) || is_not_list(*bb))
		return do_partly_bound_can_match_lesser(*aa, *bb, 
							done);
	    aa = list_val(*aa);
	    bb = list_val(*bb);
	}
    default:
	if((i = CMP(a, b)) != 0) {
	    *done = 1;
	}
	return (i < 0) ? 1 : 0;
    }
}

static int do_partly_bound_can_match_greater(Eterm a, Eterm b, 
					    int *done)
{
    Eterm* aa;
    Eterm* bb;
    Sint i;
    int j;

    if (is_atom(a) && (a == am_Underscore || 
		       (db_is_variable(a) >= 0))) {
	*done = 1;
	if (is_atom(b) && (b == am_Underscore || 
			   (db_is_variable(b) >= 0))) {
	    return 0;
	} else {
	    return 1;
	}
    } else if (is_atom(b) && (b == am_Underscore || 
			      (db_is_variable(b) >= 0))) {
	*done = 1;
	return 0;
    }

    if (a == b)
	return 0;

    if (not_eq_tags(a,b)) {
	*done = 1;
	return (CMP(a, b) > 0) ? 1 : 0;
    }

    /* we now know that tags are the same */
    switch (tag_val_def(a)) {
    case TUPLE_DEF:
	aa = tuple_val(a);
	bb = tuple_val(b);
	/* compare the arities */
	if (arityval(*aa) < arityval(*bb)) return 0;
	if (arityval(*aa) > arityval(*bb)) return 1;
	i = arityval(*aa);	/* get the arity*/
	while (i--) {
	    if ((j = do_partly_bound_can_match_greater(*++aa, *++bb, 
						      done)) != 0 
		|| *done) 
		return j;
	}
	return 0;
    case LIST_DEF:
	aa = list_val(a);
	bb = list_val(b);
	while (1) {
	    if ((j = do_partly_bound_can_match_greater(*aa++, *bb++, 
						      done)) != 0 
		|| *done) 
		return j;
	    if (*aa==*bb)
		return 0;
	    if (is_not_list(*aa) || is_not_list(*bb))
		return do_partly_bound_can_match_greater(*aa, *bb, 
							done);
	    aa = list_val(*aa);
	    bb = list_val(*bb);
	}
    default:
	if((i = CMP(a, b)) != 0) {
	    *done = 1;
	}
	return (i > 0) ? 1 : 0;
    }
}

/*
 * Callback functions for the different match functions
 */

static int doit_select(DbTableCommon *tb, TreeDbTerm *this,
                       struct select_common* ptr,
		       int forward)
{
    struct select_context *sc = (struct select_context *) ptr;
    Eterm ret;
    Eterm* hp;

    sc->lastobj = this->dbterm.tpl;
    
    if (sc->end_condition != NIL && 
	((forward && 
	  cmp_partly_bound(sc->end_condition, 
			   GETKEY_WITH_POS(sc->keypos, this->dbterm.tpl)) < 0) ||
	 (!forward && 
	  cmp_partly_bound(sc->end_condition, 
			   GETKEY_WITH_POS(sc->keypos, this->dbterm.tpl)) > 0))) {
	return 0;
    }
    ret = db_match_dbterm(tb, sc->p,sc->mp, &this->dbterm, &hp, 2);
    if (is_value(ret)) {
	sc->accum = CONS(hp, ret, sc->accum);
    }
    if (--(sc->max) <= 0) {
	return 0;
    }
    return 1;
}

static int doit_select_count(DbTableCommon *tb, TreeDbTerm *this,
                             struct select_common* ptr,
			     int forward)
{
    struct select_count_context *sc = (struct select_count_context *) ptr;
    Eterm ret;

    sc->lastobj = this->dbterm.tpl;
    
    /* Always backwards traversing */
    if (sc->end_condition != NIL && 
	(cmp_partly_bound(sc->end_condition, 
			  GETKEY_WITH_POS(sc->keypos, this->dbterm.tpl)) > 0)) {
	return 0;
    }
    ret = db_match_dbterm(tb, sc->p, sc->mp, &this->dbterm, NULL, 0);
    if (ret == am_true) {
	++(sc->got);
    }
    if (--(sc->max) <= 0) {
	return 0;
    }
    return 1;
}

static int doit_select_chunk(DbTableCommon *tb, TreeDbTerm *this,
                             struct select_common* ptr,
			     int forward)
{
    struct select_context *sc = (struct select_context *) ptr;
    Eterm ret;
    Eterm* hp;

    sc->lastobj = this->dbterm.tpl;
    
    if (sc->end_condition != NIL && 
	((forward && 
	  cmp_partly_bound(sc->end_condition, 
			   GETKEY_WITH_POS(sc->keypos, this->dbterm.tpl)) < 0) ||
	 (!forward && 
	  cmp_partly_bound(sc->end_condition, 
			   GETKEY_WITH_POS(sc->keypos, this->dbterm.tpl)) > 0))) {
	return 0;
    }

    ret = db_match_dbterm(tb, sc->p, sc->mp, &this->dbterm, &hp, 2);
    if (is_value(ret)) {
	++(sc->got);
	sc->accum = CONS(hp, ret, sc->accum);
    }
    if (--(sc->max) <= 0 || sc->got == sc->chunk_size) {
	return 0;
    }
    return 1;
}


static int doit_select_delete(DbTableCommon *tb, TreeDbTerm *this,
                              struct select_common *ptr,
			      int forward)
{
    struct select_delete_context *sc = (struct select_delete_context *) ptr;
    Eterm ret;
    Eterm key;

    if (sc->erase_lastterm)
	free_term((DbTable*)tb, sc->lastterm);
    sc->erase_lastterm = 0;
    sc->lastterm = this;
    
    if (sc->end_condition != NIL && 
	cmp_partly_bound(sc->end_condition, 
			 GETKEY_WITH_POS(sc->keypos, this->dbterm.tpl)) > 0)
	return 0;
    ret = db_match_dbterm(tb, sc->p, sc->mp, &this->dbterm, NULL, 0);
    if (ret == am_true) {
	key = GETKEY(sc->tb, this->dbterm.tpl);
	linkout_tree(sc->tb, sc->common.root, key, sc->stack);
	sc->erase_lastterm = 1;
	++sc->accum;
    }
    if (--(sc->max) <= 0) {
	return 0;
    }
    return 1;
}

static int doit_select_replace(DbTableCommon *tb, TreeDbTerm **this,
                               struct select_common* ptr,
                               int forward)
{
    struct select_replace_context *sc = (struct select_replace_context *) ptr;
    Eterm ret;

    sc->lastobj = (*this)->dbterm.tpl;

    /* Always backwards traversing */
    if (sc->end_condition != NIL &&
	(cmp_partly_bound(sc->end_condition,
			  GETKEY_WITH_POS(sc->keypos, (*this)->dbterm.tpl)) > 0)) {
	return 0;
    }
    ret = db_match_dbterm(tb, sc->p, sc->mp, &(*this)->dbterm, NULL, 0);

    if (is_value(ret)) {
        TreeDbTerm* new;
        TreeDbTerm* old = *this;
#ifdef DEBUG
        Eterm key = db_getkey(tb->keypos, ret);
        ASSERT(is_value(key));
        ASSERT(cmp_key(tb, key, old) == 0);
#endif
        new = new_dbterm(tb, ret);
        new->left = old->left;
        new->right = old->right;
        new->balance = old->balance;
        sc->lastobj = new->dbterm.tpl;
        *this = new;
        free_term((DbTable*)tb, old);
        ++(sc->replaced);
    }
    if (--(sc->max) <= 0) {
	return 0;
    }
    return 1;
}

void
erts_db_foreach_thr_prgr_offheap_tree(void (*func)(ErlOffHeap *, void *),
                                      void *arg)
{
}

#ifdef TREE_DEBUG
static void do_dump_tree2(DbTableCommon* tb, int to, void *to_arg, int show,
			  TreeDbTerm *t, int offset)
{
    if (t == NULL)
	return;
    do_dump_tree2(tb, to, to_arg, show, t->right, offset + 4);
    if (show) {
	const char* prefix;
	Eterm term;
	if (tb->compress) {
	    prefix = "key=";
	    term = GETKEY(tb, t->dbterm.tpl);
	}
	else {
	    prefix = "";
	    term = make_tuple(t->dbterm.tpl);
	}
	erts_print(to, to_arg, "%*s%s%T (addr = %p, bal = %d)\n",
		   offset, "", prefix, term, t, t->balance);
    }
    do_dump_tree2(tb, to, to_arg, show, t->left, offset + 4); 
}

#endif

#ifdef HARDDEBUG

/*
 * No called, but kept as it might come to use
 */
void db_check_table_tree(DbTable *tbl)
{
    DbTableTree *tb = &tbl->tree;
    check_table_tree(tb, tb->root);
    check_saved_stack(tb);
    check_slot_pos(tb);
}

static TreeDbTerm *traverse_until(TreeDbTerm *t, int *current, int to)
{
    TreeDbTerm *tmp;
    if (t == NULL) 
	return NULL;
    tmp = traverse_until(t->left, current, to);
    if (tmp != NULL)
	return tmp;
    ++(*current);
    if (*current == to)
	return t;
    return traverse_until(t->right, current, to);
}

static void check_slot_pos(DbTableTree *tb)
{
    int pos = 0;
    TreeDbTerm *t;
    if (tb->stack.slot == 0 || tb->stack.pos == 0)
	return;
    t = traverse_until(tb->root, &pos, tb->stack.slot);
    if (t != tb->stack.array[tb->stack.pos - 1]) {
	erts_fprintf(stderr, "Slot position does not correspont with stack, "
		   "element position %d is really 0x%08X, when stack says "
		   "it's 0x%08X\n", tb->stack.slot, t, 
		   tb->stack.array[tb->stack.pos - 1]);
	do_dump_tree2(&tb->common, ERTS_PRINT_STDERR, NULL, 1, tb->root, 0);
    }
}
	

static void check_saved_stack(DbTableTree *tb)
{
     TreeDbTerm *t = tb->root;
     DbTreeStack* stack = &tb->static_stack;
     int n = 0;
     if (stack->pos == 0)
	 return;
     if (t != stack->array[0]) {
	 erts_fprintf(stderr,"tb->stack[0] is 0x%08X, should be 0x%08X\n",
		      stack->array[0], t);
	 do_dump_tree2(&tb->common, ERTS_PRINT_STDERR, NULL, 1, tb->root, 0);
	 return;
     }
     while (n < stack->pos) {
	 if (t == NULL) {
	     erts_fprintf(stderr, "NULL pointer in tree when stack not empty,"
			" stack depth is %d\n", n);
	     do_dump_tree2(&tb->common, ERTS_PRINT_STDERR, NULL, 1, tb->root, 0);
	     return;
	 }
	 n++;
	 if (n < stack->pos) {
	     if (stack->array[n] == t->left)
		 t = t->left;
	     else if (stack->array[n] == t->right)
		 t = t->right;
	     else {
		 erts_fprintf(stderr, "tb->stack[%d] == 0x%08X does not "
			    "represent child pointer in tree!"
			    "(left == 0x%08X, right == 0x%08X\n", 
			    n, tb->stack[n], t->left, t->right);
		 do_dump_tree2(&tb->common, ERTS_PRINT_STDERR, NULL, 1, tb->root, 0);
		 return;
	     }
	 }
     }
}

static int check_table_tree(DbTableTree* tb, TreeDbTerm *t)
{
    int lh, rh;
    if (t == NULL)
	return 0;
    lh = check_table_tree(tb, t->left);
    rh = check_table_tree(tb, t->right);
    if ((rh - lh) != t->balance) {
	erts_fprintf(stderr, "Invalid tree balance for this node:\n");
	erts_fprintf(stderr,"balance = %d, left = 0x%08X, right = 0x%08X\n",
		     t->balance, t->left, t->right);
	erts_fprintf(stderr,"\nDump:\n---------------------------------\n");
	do_dump_tree2(&tb->common, ERTS_PRINT_STDERR, NULL, 1, t, 0);
	erts_fprintf(stderr,"\n---------------------------------\n");
    }
    return ((rh > lh) ? rh : lh) + 1;
}
	
#endif
