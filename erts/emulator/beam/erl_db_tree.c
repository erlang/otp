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

#define GETKEY_WITH_POS(Keypos, Tplp) (*((Tplp) + Keypos))
#define NITEMS(tb) ((int)erts_smp_atomic_read_nob(&(tb)->common.nitems))

/*
** A stack of this size is enough for an AVL tree with more than
** 0xFFFFFFFF elements. May be subject to change if
** the datatype of the element counter is changed to a 64 bit integer.
** The Maximal height of an AVL tree is calculated as:
** h(n) <= 1.4404 * log(n + 2) - 0.328
** Where n denotes the number of nodes, h(n) the height of the tree
** with n nodes and log is the binary logarithm.
*/

#define STACK_NEED 50
#define TREE_MAX_ELEMENTS 0xFFFFFFFFUL

#define PUSH_NODE(Dtt, Tdt)                     \
    ((Dtt)->array[(Dtt)->pos++] = Tdt)

#define POP_NODE(Dtt)			\
     (((Dtt)->pos) ? 			\
      (Dtt)->array[--((Dtt)->pos)] : NULL)

#define TOP_NODE(Dtt)                   \
     ((Dtt->pos) ? 			\
      (Dtt)->array[(Dtt)->pos - 1] : NULL)

#define TOPN_NODE(Dtt, Pos)                   \
     (((Pos) < Dtt->pos) ? 			\
      (Dtt)->array[(Dtt)->pos - ((Pos) + 1)] : NULL)

#define REPLACE_TOP_NODE(Dtt, Node)          \
     if ((Dtt)->pos) (Dtt)->array[(Dtt)->pos - 1] = (Node)

#define EMPTY_NODE(Dtt) (TOP_NODE(Dtt) == NULL)

#ifndef MIN
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#endif

/* Obtain table static stack if available. NULL if not.
** Must be released with release_stack()
*/
static DbTreeStack* get_static_stack(DbTableTree* tb)
{
    if (!erts_smp_atomic_xchg_acqb(&tb->is_stack_busy, 1)) {
	return &tb->static_stack;
    }
    return NULL;
}

/* Obtain static stack if available, otherwise empty dynamic stack.
** Must be released with release_stack()
*/
static DbTreeStack* get_any_stack(DbTableTree* tb)
{
    DbTreeStack* stack;
    if (!erts_smp_atomic_xchg_acqb(&tb->is_stack_busy, 1)) {
	return &tb->static_stack;
    }
    stack = erts_db_alloc(ERTS_ALC_T_DB_STK, (DbTable *) tb,
			  sizeof(DbTreeStack) + sizeof(TreeDbTerm*) * STACK_NEED);
    stack->pos = 0;
    stack->slot = 0;
    stack->array = (TreeDbTerm**) (stack + 1);
    return stack;
}

static void release_stack(DbTableTree* tb, DbTreeStack* stack)
{
    if (stack == &tb->static_stack) {
	ASSERT(erts_smp_atomic_read_nob(&tb->is_stack_busy) == 1);
	erts_smp_atomic_set_relb(&tb->is_stack_busy, 0);
    }
    else {
	erts_db_free(ERTS_ALC_T_DB_STK, (DbTable *) tb,
		     (void *) stack, sizeof(DbTreeStack) + sizeof(TreeDbTerm*) * STACK_NEED);
    }
}

static ERTS_INLINE void reset_static_stack(DbTableTree* tb)
{
    tb->static_stack.pos = 0;
    tb->static_stack.slot = 0;
}

static ERTS_INLINE void free_term(DbTableTree *tb, TreeDbTerm* p)
{
    db_free_term((DbTable*)tb, p, offsetof(TreeDbTerm, dbterm));
}

static ERTS_INLINE TreeDbTerm* new_dbterm(DbTableTree *tb, Eterm obj)
{
    TreeDbTerm* p;
    if (tb->common.compress) {
	p = db_store_term_comp(&tb->common, NULL, offsetof(TreeDbTerm,dbterm), obj);
    }
    else {
	p = db_store_term(&tb->common, NULL, offsetof(TreeDbTerm,dbterm), obj);
    }
    return p;
}
static ERTS_INLINE TreeDbTerm* replace_dbterm(DbTableTree *tb, TreeDbTerm* old,
					      Eterm obj)
{
    TreeDbTerm* p;
    ASSERT(old != NULL);
    if (tb->common.compress) {
	p = db_store_term_comp(&tb->common, &(old->dbterm), offsetof(TreeDbTerm,dbterm), obj);
    }
    else {
	p = db_store_term(&tb->common, &(old->dbterm), offsetof(TreeDbTerm,dbterm), obj);
    }
    return p;
}

/*
** Some macros for "direction stacks"
*/
#define DIR_LEFT 0
#define DIR_RIGHT 1
#define DIR_END 2 

/*
 * Special binary flag
 */
#define BIN_FLAG_ALL_OBJECTS         BIN_FLAG_USR1

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

/* 
 * This structure is filled in by analyze_pattern() for the select 
 * functions.
 */
struct mp_info {
    int all_objects;		/* True if complete objects are always
				 * returned from the match_spec (can use 
				 * copy_shallow on the return value) */
    int something_can_match;	/* The match_spec is not "impossible" */
    int some_limitation;	/* There is some limitation on the search
				 * area, i. e. least and/or most is set.*/
    int got_partial;		/* The limitation has a partially bound
				 * key */
    Eterm least;		/* The lowest matching key (possibly 
				 * partially bound expression) */
    Eterm most;                 /* The highest matching key (possibly 
				 * partially bound expression) */

    TreeDbTerm **save_term;      /* If the key is completely bound, this
	 			  * will be the Tree node we're searching
				  * for, otherwise it will be useless */
    Binary *mp;                 /* The compiled match program */
};

/*
 * Used by doit_select(_chunk)
 */
struct select_context {
    Process *p;
    Eterm accum;
    Binary *mp;
    Eterm end_condition;
    Eterm *lastobj;
    Sint32 max;
    int keypos;
    int all_objects;
    Sint got;
    Sint chunk_size;
};

/*
 * Used by doit_select_count
 */
struct select_count_context {
    Process *p;
    Binary *mp;
    Eterm end_condition;
    Eterm *lastobj;
    Sint32 max;
    int keypos;
    int all_objects;
    Sint got;
};

/*
 * Used by doit_select_delete
 */
struct select_delete_context {
    Process *p;
    DbTableTree *tb;
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
    Process *p;
    DbTableTree *tb;
    Binary *mp;
    Eterm end_condition;
    Eterm *lastobj;
    Sint32 max;
    int keypos;
    int all_objects;
    Sint replaced;
};

/* Used by select_replace on analyze_pattern */
typedef int (*extra_match_validator_t)(int keypos, Eterm match, Eterm guard, Eterm body);

/*
** Forward declarations 
*/
static TreeDbTerm *linkout_tree(DbTableTree *tb, Eterm key);
static TreeDbTerm *linkout_object_tree(DbTableTree *tb, 
				       Eterm object);
static SWord do_free_tree_continue(DbTableTree *tb, SWord reds);
static void free_term(DbTableTree *tb, TreeDbTerm* p);
static int balance_left(TreeDbTerm **this); 
static int balance_right(TreeDbTerm **this); 
static int delsub(TreeDbTerm **this); 
static TreeDbTerm *slot_search(Process *p, DbTableTree *tb, Sint slot);
static TreeDbTerm *find_node(DbTableTree *tb, Eterm key);
static TreeDbTerm **find_node2(DbTableTree *tb, Eterm key);
static TreeDbTerm **find_ptr(DbTableTree *tb, DbTreeStack*, TreeDbTerm *this);
static TreeDbTerm *find_next(DbTableTree *tb, DbTreeStack*, Eterm key);
static TreeDbTerm *find_prev(DbTableTree *tb, DbTreeStack*, Eterm key);
static TreeDbTerm *find_next_from_pb_key(DbTableTree *tb, DbTreeStack*,
					 Eterm key);
static TreeDbTerm *find_prev_from_pb_key(DbTableTree *tb, DbTreeStack*,
					 Eterm key);
static void traverse_backwards(DbTableTree *tb,
			       DbTreeStack*,
			       Eterm lastkey,
			       int (*doit)(DbTableTree *tb,
					   TreeDbTerm *,
					   void *,
					   int),
			       void *context); 
static void traverse_forward(DbTableTree *tb,
			     DbTreeStack*,
			     Eterm lastkey,
			     int (*doit)(DbTableTree *tb,
					 TreeDbTerm *,
					 void *,
					 int),
			     void *context);
static void traverse_update_backwards(DbTableTree *tb,
                                      DbTreeStack*,
                                      Eterm lastkey,
                                      int (*doit)(DbTableTree *tb,
                                                  TreeDbTerm **, // out
                                                  void *,
                                                  int),
                                      void *context);
static int key_given(DbTableTree *tb, Eterm pattern, TreeDbTerm ***ret,
		     Eterm *partly_bound_key);
static Sint cmp_partly_bound(Eterm partly_bound_key, Eterm bound_key);
static Sint do_cmp_partly_bound(Eterm a, Eterm b, int *done);

static int analyze_pattern(DbTableTree *tb, Eterm pattern, 
                           extra_match_validator_t extra_validator, /* Optional callback */
                           struct mp_info *mpi);
static int doit_select(DbTableTree *tb,
		       TreeDbTerm *this,
		       void *ptr,
		       int forward);
static int doit_select_count(DbTableTree *tb,
			     TreeDbTerm *this,
			     void *ptr,
			     int forward);
static int doit_select_chunk(DbTableTree *tb,
			     TreeDbTerm *this,
			     void *ptr,
			     int forward);
static int doit_select_delete(DbTableTree *tb,
			      TreeDbTerm *this,
			      void *ptr,
			      int forward);
static int doit_select_replace(DbTableTree *tb,
                               TreeDbTerm **this_ptr,
                               void *ptr,
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
			  Eterm pattern, int reversed, Eterm *ret);
static int db_select_count_tree(Process *p, DbTable *tbl, Eterm tid,
				Eterm pattern,  Eterm *ret);
static int db_select_chunk_tree(Process *p, DbTable *tbl, Eterm tid,
				Eterm pattern, Sint chunk_size,
				int reversed, Eterm *ret);
static int db_select_continue_tree(Process *p, DbTable *tbl,
				   Eterm continuation, Eterm *ret);
static int db_select_count_continue_tree(Process *p, DbTable *tbl,
					 Eterm continuation, Eterm *ret);
static int db_select_delete_tree(Process *p, DbTable *tbl, Eterm tid,
				 Eterm pattern,  Eterm *ret);
static int db_select_delete_continue_tree(Process *p, DbTable *tbl, 
					  Eterm continuation, Eterm *ret);
static int db_select_replace_tree(Process *p, DbTable *tbl, Eterm tid,
                                  Eterm pattern, Eterm *ret);
static int db_select_replace_continue_tree(Process *p, DbTable *tbl,
                                           Eterm continuation, Eterm *ret);
static int db_take_tree(Process *, DbTable *, Eterm, Eterm *);
static void db_print_tree(fmtfn_t to, void *to_arg,
			  int show, DbTable *tbl);
static int db_free_table_tree(DbTable *tbl);

static SWord db_free_table_continue_tree(DbTable *tbl, SWord);

static void db_foreach_offheap_tree(DbTable *,
				    void (*)(ErlOffHeap *, void *),
				    void *);

static int db_delete_all_objects_tree(Process* p, DbTable* tbl);

#ifdef HARDDEBUG
static void db_check_table_tree(DbTable *tbl);
#endif
static int
db_lookup_dbterm_tree(Process *, DbTable *, Eterm key, Eterm obj,
                      DbUpdateHandle*);
static void
db_finalize_dbterm_tree(int cret, DbUpdateHandle *);

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
    db_free_table_tree,
    db_free_table_continue_tree,
    db_print_tree,
    db_foreach_offheap_tree,
    db_lookup_dbterm_tree,
    db_finalize_dbterm_tree

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
    erts_smp_atomic_init_nob(&tb->is_stack_busy, 0);
    tb->deletion = 0;
    return DB_ERROR_NONE;
}

static int db_first_tree(Process *p, DbTable *tbl, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    DbTreeStack* stack;
    TreeDbTerm *this;

    if (( this = tb->root ) == NULL) {
	*ret = am_EOT;
	return DB_ERROR_NONE;
    }
    /* Walk down the tree to the left */
    if ((stack = get_static_stack(tb)) != NULL) {
	stack->pos = stack->slot = 0;
    }
    while (this->left != NULL) {
	if (stack) PUSH_NODE(stack, this);
	this = this->left;
    }
    if (stack) {
	PUSH_NODE(stack, this);
	stack->slot = 1;
	release_stack(tb,stack);
    }
    *ret = db_copy_key(p, tbl, &this->dbterm);
    return DB_ERROR_NONE;
}

static int db_next_tree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    DbTreeStack* stack;
    TreeDbTerm *this;

    if (is_atom(key) && key == am_EOT)
	return DB_ERROR_BADKEY;
    stack = get_any_stack(tb);
    this = find_next(tb, stack, key);
    release_stack(tb,stack);
    if (this == NULL) {
	*ret = am_EOT;
	return DB_ERROR_NONE;
    }
    *ret = db_copy_key(p, tbl, &this->dbterm);
    return DB_ERROR_NONE;
}

static int db_last_tree(Process *p, DbTable *tbl, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    TreeDbTerm *this;
    DbTreeStack* stack;

    if (( this = tb->root ) == NULL) {
	*ret = am_EOT;
	return DB_ERROR_NONE;
    }
    /* Walk down the tree to the right */
    if ((stack = get_static_stack(tb)) != NULL) {
	stack->pos = stack->slot = 0;
    }    
    while (this->right != NULL) {
	if (stack) PUSH_NODE(stack, this);
	this = this->right;
    }
    if (stack) {
	PUSH_NODE(stack, this);
	stack->slot = NITEMS(tb);
	release_stack(tb,stack);
    }
    *ret = db_copy_key(p, tbl, &this->dbterm);
    return DB_ERROR_NONE;
}

static int db_prev_tree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    TreeDbTerm *this;
    DbTreeStack* stack;

    if (is_atom(key) && key == am_EOT)
	return DB_ERROR_BADKEY;
    stack = get_any_stack(tb);
    this = find_prev(tb, stack, key);
    release_stack(tb,stack);
    if (this == NULL) {
	*ret = am_EOT;
	return DB_ERROR_NONE;
    }
    *ret = db_copy_key(p, tbl, &this->dbterm);
    return DB_ERROR_NONE;
}

static ERTS_INLINE Sint cmp_key(DbTableTree* tb, Eterm key, TreeDbTerm* obj) {
    return CMP(key, GETKEY(tb,obj->dbterm.tpl));
}

static ERTS_INLINE int cmp_key_eq(DbTableTree* tb, Eterm key, TreeDbTerm* obj) {
    Eterm obj_key = GETKEY(tb,obj->dbterm.tpl);
    return is_same(key, obj_key) || CMP(key, obj_key) == 0;
}

static int db_put_tree(DbTable *tbl, Eterm obj, int key_clash_fail)
{
    DbTableTree *tb = &tbl->tree;
    /* Non recursive insertion in AVL tree, building our own stack */
    TreeDbTerm **tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 0;
    int state = 0;
    TreeDbTerm **this = &tb->root;
    Sint c;
    Eterm key;
    int dir;
    TreeDbTerm *p1, *p2, *p;

    key = GETKEY(tb, tuple_val(obj));

    reset_static_stack(tb);

    dstack[dpos++] = DIR_END;
    for (;;)
	if (!*this) { /* Found our place */
	    state = 1;
	    if (erts_smp_atomic_inc_read_nob(&tb->common.nitems) >= TREE_MAX_ELEMENTS) {
		erts_smp_atomic_dec_nob(&tb->common.nitems);
		return DB_ERROR_SYSRES;
	    }
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

static int db_get_tree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    Eterm copy;
    Eterm *hp, *hend;
    TreeDbTerm *this;

    /*
     * This is always a set, so we know exactly how large
     * the data is when we have found it.
     * The list created around it is purely for interface conformance.
     */
    
    this = find_node(tb,key);
    if (this == NULL) {
	*ret = NIL;
    } else {
	hp = HAlloc(p, this->dbterm.size + 2);
	hend = hp + this->dbterm.size + 2;
	copy = db_copy_object_from_ets(&tb->common, &this->dbterm, &hp, &MSO(p));
	*ret = CONS(hp, copy, NIL);
	hp += 2;
	HRelease(p,hend,hp);
    }
    return DB_ERROR_NONE;
}

static int db_member_tree(DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;

    *ret = (find_node(tb,key) == NULL) ? am_false : am_true;
    return DB_ERROR_NONE;
}

static int db_get_element_tree(Process *p, DbTable *tbl,
			       Eterm key, int ndex, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
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
    
    this = find_node(tb,key);
    if (this == NULL) {
	return DB_ERROR_BADKEY;
    } else {
	if (ndex > arityval(this->dbterm.tpl[0])) {
	    return DB_ERROR_BADPARAM;
	}
	*ret = db_copy_element_from_ets(&tb->common, p, &this->dbterm, ndex, &hp, 0);
    }
    return DB_ERROR_NONE;
}

static int db_erase_tree(DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    TreeDbTerm *res;

    *ret = am_true;

    if ((res = linkout_tree(tb, key)) != NULL) {
	free_term(tb, res);
    }
    return DB_ERROR_NONE;
}

static int db_erase_object_tree(DbTable *tbl, Eterm object, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    TreeDbTerm *res;

    *ret = am_true;

    if ((res = linkout_object_tree(tb, object)) != NULL) {
	free_term(tb, res);
    }
    return DB_ERROR_NONE;
}


static int db_slot_tree(Process *p, DbTable *tbl, 
			Eterm slot_term, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    Sint slot;
    TreeDbTerm *st;
    Eterm *hp, *hend;
    Eterm copy;

    /*
     * The notion of a "slot" is not natural in a tree, but we try to
     * simulate it by giving the n'th node in the tree instead.
     * Traversing a tree in this way is not very convenient, but by
     * using the saved stack we at least sometimes will get acceptable 
     * performance.
     */

    if (is_not_small(slot_term) ||
	((slot = signed_val(slot_term)) < 0) ||
	(slot > NITEMS(tb)))
	return DB_ERROR_BADPARAM;

    if (slot == NITEMS(tb)) {
	*ret = am_EOT;
	return DB_ERROR_NONE;
    }

    /* 
     * We use the slot position and search from there, slot positions 
     * are counted from 1 and up.
     */
    ++slot;
    st = slot_search(p, tb, slot); 
    if (st == NULL) {
	*ret = am_false;
	return DB_ERROR_UNSPEC;
    }
    hp = HAlloc(p, st->dbterm.size + 2);
    hend = hp + st->dbterm.size + 2;
    copy = db_copy_object_from_ets(&tb->common, &st->dbterm, &hp, &MSO(p));
    *ret = CONS(hp, copy, NIL);
    hp += 2;
    HRelease(p,hend,hp);
    return DB_ERROR_NONE;
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
    
/*
** This is called either when the select bif traps or when ets:select/1 
** is called. It does mostly the same as db_select_tree and may in either case
** trap to itself again (via the ets:select/1 bif).
** Note that this is common for db_select_tree and db_select_chunk_tree.
*/
static int db_select_continue_tree(Process *p, 
				   DbTable *tbl,
				   Eterm continuation,
				   Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
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
    sc.keypos = tb->common.keypos;
    sc.all_objects = mp->intern.flags & BIN_FLAG_ALL_OBJECTS;
    sc.chunk_size = chunk_size;
    reverse = unsigned_val(tptr[7]);
    sc.got = signed_val(tptr[8]);

    stack = get_any_stack(tb);
    if (chunk_size) {
	if (reverse) {
	    traverse_backwards(tb, stack, lastkey, &doit_select_chunk, &sc);
	} else {
	    traverse_forward(tb, stack, lastkey, &doit_select_chunk, &sc);
	}
    } else {
	if (reverse) {
	    traverse_forward(tb, stack, lastkey, &doit_select, &sc);
	} else {
	    traverse_backwards(tb, stack, lastkey, &doit_select, &sc);
	}
    }
    release_stack(tb,stack);

    BUMP_REDS(p, 1000 - sc.max);

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


static int db_select_tree(Process *p, DbTable *tbl, Eterm tid,
			  Eterm pattern, int reverse, Eterm *ret)
{
    /* Strategy: Traverse backwards to build resulting list from tail to head */
    DbTableTree *tb = &tbl->tree;
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

    if ((errcode = analyze_pattern(tb, pattern, NULL, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(NIL,errcode);
    }

    if (!mpi.something_can_match) {
	RET_TO_BIF(NIL,DB_ERROR_NONE);  
	/* can't possibly match anything */
    }

    sc.mp = mpi.mp;
    sc.all_objects = mpi.all_objects;

    if (!mpi.got_partial && mpi.some_limitation && 
	CMP_EQ(mpi.least,mpi.most)) {
	doit_select(tb,*(mpi.save_term),&sc,0 /* direction doesn't matter */);
	RET_TO_BIF(sc.accum,DB_ERROR_NONE);
    }

    stack = get_any_stack(tb);
    if (reverse) {
	if (mpi.some_limitation) {
	    if ((this = find_prev_from_pb_key(tb, stack, mpi.least)) != NULL) {
		lastkey = GETKEY(tb, this->dbterm.tpl);
	    }
	    sc.end_condition = mpi.most;
	}
	traverse_forward(tb, stack, lastkey, &doit_select, &sc);
    } else {
	if (mpi.some_limitation) {
	    if ((this = find_next_from_pb_key(tb, stack, mpi.most)) != NULL) {
		lastkey = GETKEY(tb, this->dbterm.tpl);
	    }
	    sc.end_condition = mpi.least;
	}
	traverse_backwards(tb, stack, lastkey, &doit_select, &sc);
    }
    release_stack(tb,stack);
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
    if (mpi.all_objects)
	(mpi.mp)->intern.flags |= BIN_FLAG_ALL_OBJECTS;
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

    
/*
** This is called either when the select_count bif traps.
*/
static int db_select_count_continue_tree(Process *p, 
					 DbTable *tbl,
					 Eterm continuation,
					 Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
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

    stack = get_any_stack(tb);
    traverse_backwards(tb, stack, lastkey, &doit_select_count, &sc);
    release_stack(tb,stack);

    BUMP_REDS(p, 1000 - sc.max);

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


static int db_select_count_tree(Process *p, DbTable *tbl, Eterm tid,
				Eterm pattern, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
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

    if ((errcode = analyze_pattern(tb, pattern, NULL, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(NIL,errcode);
    }

    if (!mpi.something_can_match) {
	RET_TO_BIF(make_small(0),DB_ERROR_NONE);  
	/* can't possibly match anything */
    }

    sc.mp = mpi.mp;
    sc.all_objects = mpi.all_objects;

    if (!mpi.got_partial && mpi.some_limitation && 
	CMP_EQ(mpi.least,mpi.most)) {
	doit_select_count(tb,*(mpi.save_term),&sc,0 /* dummy */);
	RET_TO_BIF(erts_make_integer(sc.got,p),DB_ERROR_NONE);
    }

    stack = get_any_stack(tb);
    if (mpi.some_limitation) {
	if ((this = find_next_from_pb_key(tb, stack, mpi.most)) != NULL) {
	    lastkey = GETKEY(tb, this->dbterm.tpl);
	}
	sc.end_condition = mpi.least;
    }
    
    traverse_backwards(tb, stack, lastkey, &doit_select_count, &sc);
    release_stack(tb,stack);
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
    if (mpi.all_objects)
	(mpi.mp)->intern.flags |= BIN_FLAG_ALL_OBJECTS;
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

static int db_select_chunk_tree(Process *p, DbTable *tbl, Eterm tid,
				Eterm pattern, Sint chunk_size,
				int reverse,
				Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
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

    if ((errcode = analyze_pattern(tb, pattern, NULL, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(NIL,errcode);
    }

    if (!mpi.something_can_match) {
	RET_TO_BIF(am_EOT,DB_ERROR_NONE);
	/* can't possibly match anything */
    }

    sc.mp = mpi.mp;
    sc.all_objects = mpi.all_objects;

    if (!mpi.got_partial && mpi.some_limitation && 
	CMP_EQ(mpi.least,mpi.most)) {
	doit_select(tb,*(mpi.save_term),&sc, 0 /* direction doesn't matter */);
	if (sc.accum != NIL) {
	    hp=HAlloc(p, 3);
	    RET_TO_BIF(TUPLE2(hp,sc.accum,am_EOT),DB_ERROR_NONE);
	} else {
	    RET_TO_BIF(am_EOT,DB_ERROR_NONE);
	}
    }

    stack = get_any_stack(tb);
    if (reverse) {
	if (mpi.some_limitation) {
	    if ((this = find_next_from_pb_key(tb, stack, mpi.most)) != NULL) {
		lastkey = GETKEY(tb, this->dbterm.tpl);
	    }
	    sc.end_condition = mpi.least;
	}
	traverse_backwards(tb, stack, lastkey, &doit_select_chunk, &sc);
    } else {
	if (mpi.some_limitation) {
	    if ((this = find_prev_from_pb_key(tb, stack, mpi.least)) != NULL) {
		lastkey = GETKEY(tb, this->dbterm.tpl);
	    }
	    sc.end_condition = mpi.most;
	}
	traverse_forward(tb, stack, lastkey, &doit_select_chunk, &sc);
    }
    release_stack(tb,stack);

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
	if (mpi.all_objects)
	    (mpi.mp)->intern.flags |= BIN_FLAG_ALL_OBJECTS;
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

    if (mpi.all_objects)
	(mpi.mp)->intern.flags |= BIN_FLAG_ALL_OBJECTS;
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

/*
** This is called when select_delete traps
*/
static int db_select_delete_continue_tree(Process *p, 
					  DbTable *tbl,
					  Eterm continuation,
					  Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
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
	    free_term(tb, sc.lastterm);		\
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
    sc.tb = tb;
    if (is_big(tptr[5])) {
	sc.accum = big_to_uint32(tptr[5]);
    } else {
	sc.accum = unsigned_val(tptr[5]);
    }
    sc.mp = mp;
    sc.end_condition = NIL;
    sc.max = 1000;
    sc.keypos = tb->common.keypos;

    ASSERT(!erts_smp_atomic_read_nob(&tb->is_stack_busy));
    traverse_backwards(tb, &tb->static_stack, lastkey, &doit_select_delete, &sc);

    BUMP_REDS(p, 1000 - sc.max);

    if (sc.max > 0) {
	RET_TO_BIF(erts_make_integer(sc.accum, p), DB_ERROR_NONE);
    }	
    key = GETKEY(tb, (sc.lastterm)->dbterm.tpl);
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

static int db_select_delete_tree(Process *p, DbTable *tbl, Eterm tid,
				 Eterm pattern, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
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
	    free_term(tb, sc.lastterm);         \
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
    sc.keypos = tb->common.keypos;
    sc.tb = tb;
    
    if ((errcode = analyze_pattern(tb, pattern, NULL, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(0,errcode);
    }

    if (!mpi.something_can_match) {
	RET_TO_BIF(make_small(0),DB_ERROR_NONE);  
	/* can't possibly match anything */
    }

    sc.mp = mpi.mp;

    if (!mpi.got_partial && mpi.some_limitation && 
	CMP_EQ(mpi.least,mpi.most)) {
	doit_select_delete(tb,*(mpi.save_term),&sc, 0 /* direction doesn't
						      matter */);
	RET_TO_BIF(erts_make_integer(sc.accum,p),DB_ERROR_NONE);
    }

    if (mpi.some_limitation) {
	if ((this = find_next_from_pb_key(tb, &tb->static_stack, mpi.most)) != NULL) {
	    lastkey = GETKEY(tb, this->dbterm.tpl);
	}
	sc.end_condition = mpi.least;
    }

    traverse_backwards(tb, &tb->static_stack, lastkey, &doit_select_delete, &sc);
    BUMP_REDS(p, 1000 - sc.max);

    if (sc.max > 0) {
	RET_TO_BIF(erts_make_integer(sc.accum,p), DB_ERROR_NONE);
    }

    key = GETKEY(tb, (sc.lastterm)->dbterm.tpl);
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
	free_term(tb, sc.lastterm);
    }
    *ret = bif_trap1(&ets_select_delete_continue_exp, p, continuation); 
    return DB_ERROR_NONE;

#undef RET_TO_BIF

}

static int db_select_replace_continue_tree(Process *p,
                                           DbTable *tbl,
                                           Eterm continuation,
                                           Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
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
    sc.keypos = tb->common.keypos;
    if (is_big(tptr[5])) {
        sc.replaced = big_to_uint32(tptr[5]);
    } else {
        sc.replaced = unsigned_val(tptr[5]);
    }
    prev_replaced = sc.replaced;

    stack = get_any_stack(tb);
    traverse_update_backwards(tb, stack, lastkey, &doit_select_replace, &sc);
    release_stack(tb,stack);

    // the more objects we've replaced, the more reductions we've consumed
    BUMP_REDS(p, MIN(2000, (1000 - sc.max) + (sc.replaced - prev_replaced)));

    if (sc.max > 0) {
        RET_TO_BIF(erts_make_integer(sc.replaced,p), DB_ERROR_NONE);
    }
    key = GETKEY(tb, sc.lastobj);
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

static int db_select_replace_tree(Process *p, DbTable *tbl, Eterm tid,
                                  Eterm pattern, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
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
    sc.tb = tb;
    sc.max = 1000;
    sc.end_condition = NIL;
    sc.keypos = tb->common.keypos;
    sc.replaced = 0;

    if ((errcode = analyze_pattern(tb, pattern, db_match_keeps_key, &mpi)) != DB_ERROR_NONE) {
        RET_TO_BIF(NIL,errcode);
    }

    if (!mpi.something_can_match) {
        RET_TO_BIF(make_small(0),DB_ERROR_NONE);
        /* can't possibly match anything */
    }

    sc.mp = mpi.mp;
    sc.all_objects = mpi.all_objects;

    stack = get_static_stack(tb);
    if (!mpi.got_partial && mpi.some_limitation &&
            CMP_EQ(mpi.least,mpi.most)) {
        TreeDbTerm* term = *(mpi.save_term);
        doit_select_replace(tb,mpi.save_term,&sc,0 /* dummy */);
        if (stack != NULL) {
            if (TOP_NODE(stack) == term)
                // throw away potentially invalid reference
                REPLACE_TOP_NODE(stack, *(mpi.save_term));
            release_stack(tb, stack);
        }
        RET_TO_BIF(erts_make_integer(sc.replaced,p),DB_ERROR_NONE);
    }

    if (stack == NULL)
        stack = get_any_stack(tb);

    if (mpi.some_limitation) {
        if ((this = find_next_from_pb_key(tb, stack, mpi.most)) != NULL) {
            lastkey = GETKEY(tb, this->dbterm.tpl);
        }
        sc.end_condition = mpi.least;
    }

    traverse_update_backwards(tb, stack, lastkey, &doit_select_replace, &sc);
    release_stack(tb,stack);
    // the more objects we've replaced, the more reductions we've consumed
    BUMP_REDS(p, MIN(2000, (1000 - sc.max) + sc.replaced));
    if (sc.max > 0) {
        RET_TO_BIF(erts_make_integer(sc.replaced,p),DB_ERROR_NONE);
    }

    key = GETKEY(tb, sc.lastobj);
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
    if (mpi.all_objects)
        (mpi.mp)->intern.flags |= BIN_FLAG_ALL_OBJECTS;
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

static int db_take_tree(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableTree *tb = &tbl->tree;
    TreeDbTerm *this;

    *ret = NIL;
    this = linkout_tree(tb, key);
    if (this) {
        Eterm copy, *hp, *hend;

        hp = HAlloc(p, this->dbterm.size + 2);
        hend = hp + this->dbterm.size + 2;
        copy = db_copy_object_from_ets(&tb->common,
                                       &this->dbterm, &hp, &MSO(p));
        *ret = CONS(hp, copy, NIL);
        hp += 2;
        HRelease(p, hend, hp);
        free_term(tb, this);
    }
    return DB_ERROR_NONE;
}

/*
** Other interface routines (not directly coupled to one bif)
*/


/* Display tree contents (for dump) */
static void db_print_tree(fmtfn_t to, void *to_arg,
			  int show,
			  DbTable *tbl)
{
    DbTableTree *tb = &tbl->tree;
#ifdef TREE_DEBUG
    if (show)
	erts_print(to, to_arg, "\nTree data dump:\n"
		   "------------------------------------------------\n");
    do_dump_tree2(&tbl->tree, to, to_arg, show, tb->root, 0);
    if (show)
	erts_print(to, to_arg, "\n"
		   "------------------------------------------------\n");
#else
    erts_print(to, to_arg, "Ordered set (AVL tree), Elements: %d\n", NITEMS(tb));
#endif
}

/* release all memory occupied by a single table */
static int db_free_table_tree(DbTable *tbl)
{
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
	ASSERT(erts_smp_atomic_read_nob(&tb->common.memory_size)
	       == sizeof(DbTable));
    }
    return reds;
}

static int db_delete_all_objects_tree(Process* p, DbTable* tbl)
{
    db_free_table_tree(tbl);
    db_create_tree(p, tbl);
    erts_smp_atomic_set_nob(&tbl->tree.common.nitems, 0);
    return 0;
}

static void do_db_tree_foreach_offheap(TreeDbTerm *,
				       void (*)(ErlOffHeap *, void *),
				       void *);

static void db_foreach_offheap_tree(DbTable *tbl,
				    void (*func)(ErlOffHeap *, void *),
				    void * arg)
{
    do_db_tree_foreach_offheap(tbl->tree.root, func, arg);
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

static TreeDbTerm *linkout_tree(DbTableTree *tb, Eterm key) {
    TreeDbTerm **tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 0;
    int state = 0;
    TreeDbTerm **this = &tb->root;
    Sint c;
    int dir;
    TreeDbTerm *q = NULL;

    /*
     * Somewhat complicated, deletion in an AVL tree,
     * The two helpers balance_left and balance_right are used to
     * keep the balance. As in insert, we do the stacking ourselves.
     */

    reset_static_stack(tb);
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
	    erts_smp_atomic_dec_nob(&tb->common.nitems);
	    break;
	}
    }
    while (state && ( dir = dstack[--dpos] ) != DIR_END) {
	this = tstack[--tpos];
	if (dir == DIR_LEFT) {
	    state = balance_left(this);
	} else {
	    state = balance_right(this);
	}
    }
    return q;
}

static TreeDbTerm *linkout_object_tree(DbTableTree *tb, 
				       Eterm object)
{
    TreeDbTerm **tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 0;
    int state = 0;
    TreeDbTerm **this = &tb->root;
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

    reset_static_stack(tb);
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
	    if (!db_eq(&tb->common,object,&(*this)->dbterm)) {
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
	    erts_smp_atomic_dec_nob(&tb->common.nitems);
	    break;
	}
    }
    while (state && ( dir = dstack[--dpos] ) != DIR_END) {
	this = tstack[--tpos];
	if (dir == DIR_LEFT) {
	    state = balance_left(this);
	} else {
	    state = balance_right(this);
	}
    }
    return q;
}

/*
** For the select functions, analyzes the pattern and determines which
** part of the tree should be searched. Also compiles the match program
*/
static int analyze_pattern(DbTableTree *tb, Eterm pattern,
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
    Eterm key;
    Eterm partly_bound;
    int res;
    Eterm least = 0;
    Eterm most = 0;

    mpi->some_limitation = 1;
    mpi->got_partial = 0;
    mpi->something_can_match = 0;
    mpi->mp = NULL;
    mpi->all_objects = 1;
    mpi->save_term = NULL;

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

        if(extra_validator != NULL && !extra_validator(tb->common.keypos, match, guard, body)) {
	    if (buff != sbuff) {
		erts_free(ERTS_ALC_T_DB_TMP, buff);
	    }
            return DB_ERROR_BADPARAM;
        }

	if (!is_list(body) || CDR(list_val(body)) != NIL ||
	    CAR(list_val(body)) != am_DollarUnderscore) {
	    mpi->all_objects = 0;
	}
	++i;

	partly_bound = NIL;
	res = key_given(tb, tpl, &(mpi->save_term), &partly_bound);
	if ( res >= 0 ) {   /* Can match something */
	    key = 0;
	    mpi->something_can_match = 1;
	    if (res > 0) {
		key = GETKEY(tb,tuple_val(tpl)); 
	    } else if (partly_bound != NIL) {
		mpi->got_partial = 1;
		key = partly_bound;
	    } else {
		mpi->some_limitation = 0;
	    }
	    if (key != 0) {
		if (least == 0 || 
		    partly_bound_can_match_lesser(key,least)) {
		    least = key;
		}
		if (most == 0 || 
		    partly_bound_can_match_greater(key,most)) {
		    most = key;
		}
	    }
	}
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
		free_term(tb, root);
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
static int balance_left(TreeDbTerm **this) 
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

static int balance_right(TreeDbTerm **this) 
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
	h = balance_right(r);
    }
    return h;
}

/*
 * Helper for db_slot
 */

static TreeDbTerm *slot_search(Process *p, DbTableTree *tb, Sint slot)
{
    TreeDbTerm *this;
    TreeDbTerm *tmp;
    DbTreeStack* stack = get_any_stack(tb);
    ASSERT(stack != NULL);

    if (slot == 1) { /* Don't search from where we are if we are 
			looking for the first slot */
	stack->slot = 0;
    }

    if (stack->slot == 0) { /* clear stack if slot positions 
				are not recorded */
	stack->pos = 0;
    }
    if (EMPTY_NODE(stack)) {
	this = tb->root;
	if (this == NULL)
	    goto done;
	while (this->left != NULL){
	    PUSH_NODE(stack, this);
	    this = this->left;
	}
	PUSH_NODE(stack, this);
	stack->slot = 1;
    }
    this = TOP_NODE(stack);
    while (stack->slot != slot && this != NULL) {
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
		    if (this == NULL || this->left == tmp)
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
		    if (this == NULL || this->right == tmp)
			break;
		}
	    }		
	    --(stack->slot);
	}
    }
done:
    release_stack(tb,stack);
    return this;
}

/*
 * Find next and previous in sort order
 */

static TreeDbTerm *find_next(DbTableTree *tb, DbTreeStack* stack, Eterm key) {
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
	if (( this = tb->root ) == NULL)
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
		    return this;
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
	if (stack->slot > 0) 
	    ++(stack->slot);
    } else {
	do {
	    tmp = POP_NODE(stack);
	    if (( this = TOP_NODE(stack)) == NULL) {
		stack->slot = 0;
		return NULL;
	    }
	} while (this->right == tmp);
	if (stack->slot > 0) 
	    ++(stack->slot);
    }
    return this;
}

static TreeDbTerm *find_prev(DbTableTree *tb, DbTreeStack* stack, Eterm key) {
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
	if (( this = tb->root ) == NULL)
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
		    return this;
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
	if (stack->slot > 0) 
	    --(stack->slot);
    } else {
	do {
	    tmp = POP_NODE(stack);
	    if (( this = TOP_NODE(stack)) == NULL) {
		stack->slot = 0;
		return NULL;
	    }
	} while (this->left == tmp);
	if (stack->slot > 0) 
	    --(stack->slot);
    }
    return this;
}

static TreeDbTerm *find_next_from_pb_key(DbTableTree *tb, DbTreeStack* stack,
					 Eterm key)
{
    TreeDbTerm *this;
    TreeDbTerm *tmp;
    Sint c;

    /* spool the stack, we have to "re-search" */
    stack->pos = stack->slot = 0;
    if (( this = tb->root ) == NULL)
	return NULL;
    for (;;) {
	PUSH_NODE(stack, this);
	if (( c = cmp_partly_bound(key,GETKEY(tb, this->dbterm.tpl))) >= 0) {
	    if (this->right == NULL) {
		do {
		    tmp = POP_NODE(stack);
		    if (( this = TOP_NODE(stack)) == NULL) {
			return NULL;
		    }
		} while (this->right == tmp);
		return this;
	    } else
		this = this->right;
	} else /*if (c < 0)*/ {
	    if (this->left == NULL) /* Done */
		return this;
	    else
		this = this->left;
	} 
    }
}

static TreeDbTerm *find_prev_from_pb_key(DbTableTree *tb, DbTreeStack* stack,
					 Eterm key)
{
    TreeDbTerm *this;
    TreeDbTerm *tmp;
    Sint c;

    /* spool the stack, we have to "re-search" */
    stack->pos = stack->slot = 0;
    if (( this = tb->root ) == NULL)
	return NULL;
    for (;;) {
	PUSH_NODE(stack, this);
	if (( c = cmp_partly_bound(key,GETKEY(tb, this->dbterm.tpl))) <= 0) {
	    if (this->left == NULL) {
		do {
		    tmp = POP_NODE(stack);
		    if (( this = TOP_NODE(stack)) == NULL) {
			return NULL;
		    }
		} while (this->left == tmp);
		return this;
	    } else
		this = this->left;
	} else /*if (c < 0)*/ {
	    if (this->right == NULL) /* Done */
		return this;
	    else
		this = this->right;
	} 
    }
}


/*
 * Just lookup a node
 */
static TreeDbTerm *find_node(DbTableTree *tb, Eterm key)
{
    TreeDbTerm *this;
    Sint res;
    DbTreeStack* stack = get_static_stack(tb);

    if(!stack || EMPTY_NODE(stack)
       || !cmp_key_eq(tb, key, (this=TOP_NODE(stack)))) {

	this = tb->root;
	while (this != NULL && (res = cmp_key(tb,key,this)) != 0) {
	    if (res < 0)
		this = this->left;
	    else
		this = this->right;
	}
    }
    if (stack) {
	release_stack(tb,stack);
    }
    return this;
}

/*
 * Lookup a node and return the address of the node pointer in the tree
 */
static TreeDbTerm **find_node2(DbTableTree *tb, Eterm key)
{
    TreeDbTerm **this;
    Sint res;

    this = &tb->root;
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

static TreeDbTerm **find_ptr(DbTableTree *tb, DbTreeStack *stack, TreeDbTerm *this) {
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
	if (( tmp = tb->root ) == NULL)
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
        return ((this != tb->root) ? NULL : &(tb->root));
    if (parent->left == this)
        return &(parent->left);
    if (parent->right == this)
        return &(parent->right);
    return NULL;
}

static int
db_lookup_dbterm_tree(Process *p, DbTable *tbl, Eterm key, Eterm obj,
                      DbUpdateHandle* handle)
{
    DbTableTree *tb = &tbl->tree;
    TreeDbTerm **pp = find_node2(tb, key);
    int flags = 0;

    if (pp == NULL) {
        if (obj == THE_NON_VALUE) {
            return 0;
        } else {
            Eterm *objp = tuple_val(obj);
            int arity = arityval(*objp);
            Eterm *htop, *hend;

            ASSERT(arity >= tb->common.keypos);
            htop = HAlloc(p, arity + 1);
            hend = htop + arity + 1;
            sys_memcpy(htop, objp, sizeof(Eterm) * (arity + 1));
            htop[tb->common.keypos] = key;
            obj = make_tuple(htop);

            if (db_put_tree(tbl, obj, 1) != DB_ERROR_NONE) {
                return 0;
            }

            pp = find_node2(tb, key);
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

static void
db_finalize_dbterm_tree(int cret, DbUpdateHandle *handle)
{
    DbTable *tbl = handle->tb;
    DbTableTree *tb = &tbl->tree;
    TreeDbTerm *bp = (TreeDbTerm *) *handle->bp;

    if (handle->flags & DB_NEW_OBJECT && cret != DB_ERROR_NONE) {
        Eterm ret;
        db_erase_tree(tbl, GETKEY(tb, bp->dbterm.tpl), &ret);
    } else if (handle->flags & DB_MUST_RESIZE) {
	db_finalize_resize(handle, offsetof(TreeDbTerm,dbterm));
        reset_static_stack(tb);

        free_term(tb, bp);
    }
#ifdef DEBUG
    handle->dbterm = 0;
#endif
    return;
}   

/*
 * Traverse the tree with a callback function, used by db_match_xxx
 */
static void traverse_backwards(DbTableTree *tb,
			       DbTreeStack* stack,
			       Eterm lastkey,
			       int (*doit)(DbTableTree *,
					   TreeDbTerm *,
					   void *,
					   int),
			       void *context) 
{
    TreeDbTerm *this, *next;

    if (lastkey == THE_NON_VALUE) {
	stack->pos = stack->slot = 0;
	if (( this = tb->root ) == NULL) {
	    return;
	}
	while (this != NULL) {
	    PUSH_NODE(stack, this);
	    this = this->right;
	}
	this = TOP_NODE(stack);
	next = find_prev(tb, stack, GETKEY(tb, this->dbterm.tpl));
	if (!((*doit)(tb, this, context, 0)))
	    return;
    } else {
	next = find_prev(tb, stack, lastkey);
    }

    while ((this = next) != NULL) {
	next = find_prev(tb, stack, GETKEY(tb, this->dbterm.tpl));
	if (!((*doit)(tb, this, context, 0)))
	    return;
    }
}

/*
 * Traverse the tree with a callback function, used by db_match_xxx
 */
static void traverse_forward(DbTableTree *tb,
			     DbTreeStack* stack,
			     Eterm lastkey,
			     int (*doit)(DbTableTree *,
					 TreeDbTerm *,
					 void *,
					 int),
			     void *context) 
{
    TreeDbTerm *this, *next;

    if (lastkey == THE_NON_VALUE) {
	stack->pos = stack->slot = 0;
	if (( this = tb->root ) == NULL) {
	    return;
	}
	while (this != NULL) {
	    PUSH_NODE(stack, this);
	    this = this->left;
	}
	this = TOP_NODE(stack);
	next = find_next(tb, stack, GETKEY(tb, this->dbterm.tpl));
	if (!((*doit)(tb, this, context, 1)))
	    return;
    } else {
	next = find_next(tb, stack, lastkey);
    }

    while ((this = next) != NULL) {
	next = find_next(tb, stack, GETKEY(tb, this->dbterm.tpl));
	if (!((*doit)(tb, this, context, 1)))
	    return;
    }
}

/*
 * Traverse the tree with an update callback function, used by db_select_replace
 */
static void traverse_update_backwards(DbTableTree *tb,
                                      DbTreeStack* stack,
                                      Eterm lastkey,
                                      int (*doit)(DbTableTree*,
                                                  TreeDbTerm**,
                                                  void*,
                                                  int),
                                      void* context)
{
    int res;
    TreeDbTerm *this, *next, **this_ptr;

    if (lastkey == THE_NON_VALUE) {
        stack->pos = stack->slot = 0;
        if (( this = tb->root ) == NULL) {
            return;
        }
        while (this != NULL) {
            PUSH_NODE(stack, this);
            this = this->right;
        }
        this = TOP_NODE(stack);
        this_ptr = find_ptr(tb, stack, this);
        ASSERT(this_ptr != NULL);
        res = (*doit)(tb, this_ptr, context, 0);
        REPLACE_TOP_NODE(stack, *this_ptr);
        next = find_prev(tb, stack, GETKEY(tb, (*this_ptr)->dbterm.tpl));
        if (!res)
            return;
    } else {
        next = find_prev(tb, stack, lastkey);
    }

    while ((this = next) != NULL) {
        this_ptr = find_ptr(tb, stack, this);
        ASSERT(this_ptr != NULL);
        res = (*doit)(tb, this_ptr, context, 0);
        REPLACE_TOP_NODE(stack, *this_ptr);
        next = find_prev(tb, stack, GETKEY(tb, (*this_ptr)->dbterm.tpl));
        if (!res)
            return;
    }
}

/*
 * Returns 0 if not given 1 if given and -1 on no possible match
 * if key is given; *ret is set to point to the object concerned.
 */
static int key_given(DbTableTree *tb, Eterm pattern, TreeDbTerm ***ret,
		     Eterm *partly_bound)
{
    TreeDbTerm **this;
    Eterm key;

    ASSERT(ret != NULL);
    if (pattern == am_Underscore || db_is_variable(pattern) != -1)
	return 0;
    key = db_getkey(tb->common.keypos, pattern);
    if (is_non_value(key))
	return -1;  /* can't possibly match anything */
    if (!db_has_variable(key)) {   /* Bound key */
	if (( this = find_node2(tb, key) ) == NULL) {
	    return -1;
	}
	*ret = this;
	return 1;
    } else if (partly_bound != NULL && key != am_Underscore && 
	       db_is_variable(key) < 0 && !db_has_map(key))
	*partly_bound = key;
	
    return 0;
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

static Sint cmp_partly_bound(Eterm partly_bound_key, Eterm bound_key) {
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
	erts_fprintf(stderr," can not match lesser than ");
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
	erts_fprintf(stderr," can not match greater than ");
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

static int doit_select(DbTableTree *tb, TreeDbTerm *this, void *ptr,
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
    ret = db_match_dbterm(&tb->common,sc->p,sc->mp,sc->all_objects,
			  &this->dbterm, &hp, 2);
    if (is_value(ret)) {
	sc->accum = CONS(hp, ret, sc->accum);
    }
    if (MBUF(sc->p)) {
	/*
	 * Force a trap and GC if a heap fragment was created. Many heap fragments
	 * make the GC slow.
	 */
	sc->max = 0;
    }
    if (--(sc->max) <= 0) {
	return 0;
    }
    return 1;
}

static int doit_select_count(DbTableTree *tb, TreeDbTerm *this, void *ptr,
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
    ret = db_match_dbterm(&tb->common, sc->p, sc->mp, 0,
			  &this->dbterm, NULL, 0);
    if (ret == am_true) {
	++(sc->got);
    }
    if (--(sc->max) <= 0) {
	return 0;
    }
    return 1;
}

static int doit_select_chunk(DbTableTree *tb, TreeDbTerm *this, void *ptr,
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

    ret = db_match_dbterm(&tb->common, sc->p, sc->mp, sc->all_objects,
			  &this->dbterm, &hp, 2);
    if (is_value(ret)) {
	++(sc->got);
	sc->accum = CONS(hp, ret, sc->accum);
    }
    if (MBUF(sc->p)) {
	/*
	 * Force a trap and GC if a heap fragment was created. Many heap fragments
	 * make the GC slow.
	 */
	sc->max = 0;
    }
    if (--(sc->max) <= 0 || sc->got == sc->chunk_size) {
	return 0;
    }
    return 1;
}


static int doit_select_delete(DbTableTree *tb, TreeDbTerm *this, void *ptr,
			      int forward)
{
    struct select_delete_context *sc = (struct select_delete_context *) ptr;
    Eterm ret;
    Eterm key;

    if (sc->erase_lastterm)
	free_term(tb, sc->lastterm);
    sc->erase_lastterm = 0;
    sc->lastterm = this;
    
    if (sc->end_condition != NIL && 
	cmp_partly_bound(sc->end_condition, 
			 GETKEY_WITH_POS(sc->keypos, this->dbterm.tpl)) > 0)
	return 0;
    ret = db_match_dbterm(&tb->common, sc->p, sc->mp, 0,
			  &this->dbterm, NULL, 0);
    if (ret == am_true) {
	key = GETKEY(sc->tb, this->dbterm.tpl);
	linkout_tree(sc->tb, key);
	sc->erase_lastterm = 1;
	++sc->accum;
    }
    if (--(sc->max) <= 0) {
	return 0;
    }
    return 1;
}

static int doit_select_replace(DbTableTree *tb, TreeDbTerm **this, void *ptr,
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
    ret = db_match_dbterm(&tb->common, sc->p, sc->mp, 0,
			  &(*this)->dbterm, NULL, 0);

    if (is_value(ret)) {
        TreeDbTerm* new;
        TreeDbTerm* old = *this;
#ifdef DEBUG
        Eterm key = db_getkey(tb->common.keypos, ret);
        ASSERT(is_value(key));
        ASSERT(cmp_key(tb, key, old) == 0);
#endif
        new = new_dbterm(tb, ret);
        new->left = old->left;
        new->right = old->right;
        new->balance = old->balance;
        sc->lastobj = new->dbterm.tpl;
        *this = new;
        free_term(tb, old);
        ++(sc->replaced);
    }
    if (--(sc->max) <= 0) {
	return 0;
    }
    return 1;
}

#ifdef TREE_DEBUG
static void do_dump_tree2(DbTableTree* tb, int to, void *to_arg, int show,
			  TreeDbTerm *t, int offset)
{
    if (t == NULL)
	return;
    do_dump_tree2(tb, to, to_arg, show, t->right, offset + 4);
    if (show) {
	const char* prefix;
	Eterm term;
	if (tb->common.compress) {
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
	do_dump_tree2(tb, ERTS_PRINT_STDERR, NULL, 1, tb->root, 0);
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
	 do_dump_tree2(tb, ERTS_PRINT_STDERR, NULL, 1, tb->root, 0);
	 return;
     }
     while (n < stack->pos) {
	 if (t == NULL) {
	     erts_fprintf(stderr, "NULL pointer in tree when stack not empty,"
			" stack depth is %d\n", n);
	     do_dump_tree2(tb, ERTS_PRINT_STDERR, NULL, 1, tb->root, 0);
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
		 do_dump_tree2(tb, ERTS_PRINT_STDERR, NULL, 1, tb->root, 0);
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
	do_dump_tree2(tb, ERTS_PRINT_STDERR, NULL, 1, t, 0);
	erts_fprintf(stderr,"\n---------------------------------\n");
    }
    return ((rh > lh) ? rh : lh) + 1;
}
	
#endif
