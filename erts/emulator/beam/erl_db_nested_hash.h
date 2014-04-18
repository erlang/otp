/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2013. All Rights Reserved.
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

#ifndef _DB_NESTED_HASH_H
#define _DB_NESTED_HASH_H


#include "erl_db_util.h" /* DbTerm & DbTableCommon */


#ifdef ERTS_DB_HASH_LOCK_CNT
#   define DB_NESTED_HASH_LOCK_CNT ERTS_DB_HASH_LOCK_CNT
#else
#   define DB_NESTED_HASH_LOCK_CNT 64
#endif


typedef struct linear_hash_table {
    erts_smp_atomic_t segtab; /* The segment table (struct segment**) */
    erts_smp_atomic_t szm;    /* current size mask. */

    /* SMP: nslots and nsegs are protected by is_resizing or table write lock */
    int nslots; /* Total number of slots */
    int nsegs;  /* Size of segment table */

    erts_smp_atomic_t nactive; /* Number of "active" slots */
} LinearHashTable;

/* !!! REMOVE !!! */
typedef struct nested_hash_db_term {
    struct nested_hash_db_term *next;      /* next term (different key) */
    struct nested_hash_db_term *knext;     /* next term (same key) */
    struct nested_hash_db_term *kprevious; /* previous term (same key) */

    /* length of knext/kprevious chain (includes INVALID_HASH terms) */
    int nkitems;

    LinearHashTable *lht;
    HashValue hvalue; /* hash value of the key */
    DbTerm dbterm;     /* The actual term */
} NestedHashDbTerm;

struct branch_db_term;

typedef struct trunk_db_term {
    struct trunk_db_term *next;    /* next term (different key) */
    struct branch_db_term *branch; /* next term (same key) */

    /* length of branch chain (includes INVALID_HASH terms) */
    int nkitems;

    HashValue hvalue; /* hash value of the key */
    LinearHashTable *lht;

    DbTerm dbterm; /* The actual term */
} TrunkDbTerm;

typedef union {
    struct trunk_db_term *trunk;
    struct branch_db_term *branch;
} TrunkOrBranchDbTerm;

typedef struct branch_db_term {
    TrunkOrBranchDbTerm previous; /* previous term (same key) */
    struct branch_db_term *next;  /* next term (same key) */

    DbTerm dbterm; /* The actual term */
} BranchDbTerm;

typedef struct nested_db_term {
    struct nested_db_term* next; /* next nested term */
    HashValue ohvalue;           /* hash value of the whole dbterm */
    TrunkOrBranchDbTerm hdbterm;
} NestedDbTerm;

typedef struct nested_fixed_deletion {
    int slot;
    struct nested_fixed_deletion *next;
} NestedFixedDeletion;

typedef struct db_table_nested_hash_fine_locks {
    union {
	erts_smp_rwmtx_t lck;
	byte _cache_line_alignment[64];
    } lck_vec[DB_NESTED_HASH_LOCK_CNT];
} DbTableNestedHashFineLocks;

typedef struct db_table_nested_hash {
    DbTableCommon common;
    LinearHashTable linearht;
    erts_smp_atomic_t is_resizing; /* grow/shrink in progress */

    /* List of slots where elements have been deleted while table was fixed */
    erts_smp_atomic_t fixdel; /* (NestedFixedDeletion *) */
#ifdef ERTS_SMP
    DbTableNestedHashFineLocks* locks;
#endif
} DbTableNestedHash;

typedef struct {
    float avg_chain_len;
    float std_dev_chain_len;
    float std_dev_expected;
    int max_chain_len;
    int min_chain_len;
} DbNestedHashStats;


/*
 * Function prototypes, looks the same (except the suffix) for all
 * table types. The process is always an [in out] parameter.
 */
void db_initialize_nhash(void);

void db_unfix_table_nhash(DbTableNestedHash *tb);

Uint db_kept_items_nhash(DbTableNestedHash *tb);

/* Interface for meta pid table */
int db_create_nhash(Process *p, DbTable *tbl);

int db_put_nhash(DbTable *tbl, Eterm obj, int key_clash_fail);

int db_get_nhash(Process *p, DbTable *tbl, Eterm key, Eterm *ret);

int db_erase_nhash(DbTable *tbl, Eterm key, Eterm *ret);

int db_get_element_array(DbTable *tbl, Eterm key, int ndex,
                         Eterm *ret, int *num_ret);

int db_erase_bag_exact2(DbTable *tbl, Eterm key, Eterm value);

/* not yet in method table */
int db_mark_all_deleted_nhash(DbTable *tbl);

void db_calc_stats_nhash(DbTableNestedHash *tb, DbNestedHashStats *stats);


#endif /* _DB_NESTED_HASH_H */
