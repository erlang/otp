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

#ifndef _DB_TREE_UTIL_H
#define _DB_TREE_UTIL_H

/*
** Internal functions and macros used by both the CA tree and the AVL tree
*/


#if defined(ARCH_32)
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
#elif defined(ARCH_64)
/*
** A stack of this size is enough for an AVL tree with more than
** 2^61 elements. 
** The Maximal height of an AVL tree is calculated as above.
*/
#define STACK_NEED 90
#else
#error "Unsported architecture"
#endif



#define PUSH_NODE(Dtt, Tdt)                     \
    ((Dtt)->array[(Dtt)->pos++] = Tdt)

#define POP_NODE(Dtt)			\
     (((Dtt)->pos) ? 			\
      (Dtt)->array[--((Dtt)->pos)] : NULL)

#define TOP_NODE(Dtt)                   \
     ((Dtt->pos) ? 			\
      (Dtt)->array[(Dtt)->pos - 1] : NULL)

#define EMPTY_NODE(Dtt) (TOP_NODE(Dtt) == NULL)

#define DEC_NITEMS(DB)                                                  \
    erts_flxctr_dec(&(DB)->common.counters, ERTS_DB_TABLE_NITEMS_COUNTER_ID)

static ERTS_INLINE void free_term(DbTable *tb, TreeDbTerm* p)
{
    db_free_term(tb, p, offsetof(TreeDbTerm, dbterm));
}

/*
** Some macros for "direction stacks"
*/
#define DIR_LEFT 0
#define DIR_RIGHT 1
#define DIR_END 2 

static ERTS_INLINE Sint cmp_key(DbTableCommon* tb, Eterm key, TreeDbTerm* obj) {
    return CMP(key, GETKEY(tb,obj->dbterm.tpl));
}

int tree_balance_left(TreeDbTerm **this);
int tree_balance_right(TreeDbTerm **this);

int db_first_tree_common(Process *p, DbTable *tbl, TreeDbTerm *root,
                         Eterm *ret, DbTableTree *stack_container);
int db_next_tree_common(Process *p, DbTable *tbl,
                        TreeDbTerm *root, Eterm key,
                        Eterm *ret, DbTreeStack* stack);
int db_last_tree_common(Process *p, DbTable *tbl, TreeDbTerm *root,
                        Eterm *ret, DbTableTree *stack_container);
int db_prev_tree_common(Process *p, DbTable *tbl, TreeDbTerm *root, Eterm key,
                        Eterm *ret, DbTreeStack* stack);
int db_put_tree_common(DbTableCommon *tb, TreeDbTerm **root, Eterm obj,
                       int key_clash_fail, DbTableTree *stack_container);
int db_get_tree_common(Process *p, DbTableCommon *tb, TreeDbTerm *root, Eterm key,
                       Eterm *ret, DbTableTree *stack_container);
int db_get_element_tree_common(Process *p, DbTableCommon *tb, TreeDbTerm *root, Eterm key,
                               int ndex, Eterm *ret, DbTableTree *stack_container);
int db_member_tree_common(DbTableCommon *tb, TreeDbTerm *root, Eterm key, Eterm *ret,
                          DbTableTree *stack_container);
int db_erase_tree_common(DbTable *tbl, TreeDbTerm **root, Eterm key, Eterm *ret,
                         DbTreeStack *stack /* NULL if no static stack */);
int db_erase_object_tree_common(DbTable *tbl, TreeDbTerm **root, Eterm object,
                                Eterm *ret, DbTableTree *stack_container);
int db_slot_tree_common(Process *p, DbTable *tbl, TreeDbTerm *root,
                        Eterm slot_term, Eterm *ret,
                        DbTableTree *stack_container,
                        CATreeRootIterator*);
int db_select_chunk_tree_common(Process *p, DbTable *tb,
                                Eterm tid, Eterm pattern, Sint chunk_size,
                                int reverse, Eterm *ret,
                                DbTableTree *stack_container,
                                CATreeRootIterator*);
int db_select_tree_common(Process *p, DbTable *tb,
                          Eterm tid, Eterm pattern, int reverse, Eterm *ret,
                          DbTableTree *stack_container,
                          CATreeRootIterator*);
int db_select_delete_tree_common(Process *p, DbTable *tbl,
                                 Eterm tid, Eterm pattern,
                                 Eterm *ret,
                                 DbTreeStack* stack,
                                 CATreeRootIterator* iter);
int db_select_continue_tree_common(Process *p, 
                                   DbTableCommon *tb,
                                   Eterm continuation,
                                   Eterm *ret,
                                   DbTableTree *stack_container,
                                   CATreeRootIterator* iter);
int db_select_delete_continue_tree_common(Process *p, 
                                          DbTable *tbl,
                                          Eterm continuation,
                                          Eterm *ret,
                                          DbTreeStack* stack,
                                          CATreeRootIterator* iter);
int db_select_count_tree_common(Process *p, DbTable *tb,
                                Eterm tid, Eterm pattern, Eterm *ret,
                                DbTableTree *stack_container,
                                CATreeRootIterator* iter);
int db_select_count_continue_tree_common(Process *p,
                                         DbTable *tb,
                                         Eterm continuation,
                                         Eterm *ret,
                                         DbTableTree *stack_container,
                                         CATreeRootIterator* iter);
int db_select_replace_tree_common(Process *p, DbTable*,
                                  Eterm tid, Eterm pattern, Eterm *ret,
                                  DbTableTree *stack_container,
                                  CATreeRootIterator* iter);
int db_select_replace_continue_tree_common(Process *p,
                                           DbTable*,
                                           Eterm continuation,
                                           Eterm *ret,
                                           DbTableTree *stack_container,
                                           CATreeRootIterator* iter);
int db_take_tree_common(Process *p, DbTable *tbl, TreeDbTerm **root,
                        Eterm key, Eterm *ret,
                        DbTreeStack *stack /* NULL if no static stack */);
void db_print_tree_common(fmtfn_t to, void *to_arg,
                          int show, TreeDbTerm *root, DbTable *tbl);
void db_foreach_offheap_tree_common(TreeDbTerm *root,
                                    void (*func)(ErlOffHeap *, void *),
                                    void * arg);
int db_lookup_dbterm_tree_common(Process *p, DbTable *tbl, TreeDbTerm **root,
                                 Eterm key, Eterm obj, DbUpdateHandle* handle,
                                 DbTableTree *stack_container);
void db_finalize_dbterm_tree_common(int cret,
                                    DbUpdateHandle *handle,
                                    TreeDbTerm **root,
                                    DbTableTree *stack_container);
Sint cmp_partly_bound(Eterm partly_bound_key, Eterm bound_key);

TreeDbTerm *db_find_tree_node_common(DbTableCommon*, TreeDbTerm *root,
                                     Eterm key);
Eterm db_binary_info_tree_common(Process*, TreeDbTerm*);

#endif /* _DB_TREE_UTIL_H */
