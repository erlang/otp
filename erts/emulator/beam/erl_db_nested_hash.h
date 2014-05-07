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
    erts_smp_atomic_t segtab; /* the segment table (struct segment **) */
    erts_smp_atomic_t szm;    /* current size mask. */

    /* SMP: nslots and nsegs are protected by is_resizing or table write lock */
    int nslots; /* total number of slots */
    int nsegs;  /* size of segment table */

    erts_smp_atomic_t nactive; /* number of active slots */
} LinearHashTable;

typedef struct trunk_db_term {
    /*
     * The np field of the first TrunkDbTerm contains nkitems, the np
     * field of the other TrunkDbTerms contains the previous pointer.
     */
    union {
        int nkitems;                    /* length of trunk chain */
        struct trunk_db_term *previous; /* previous term (same key) */
    } np;

    struct trunk_db_term *next; /* next term (same key) */

    DbTerm dbterm; /* the actual term */
} TrunkDbTerm;

typedef struct root_db_term {
    struct root_db_term *next; /* next root term */

    /*
     * The first trunk term of the chain (it's never NULL).
     * Its lsb is used as a flag:
     * o if zero, this RootDbTerm is truncated at the lht field
     * o if set, this RootDbTerm includes the lht field
     * Use GET_TRUNK() and SET_TRUNK() to access this field.
     */
    TrunkDbTerm *trunk;

    HashValue hvalue;    /* hash value of the key */
    LinearHashTable lht;
} RootDbTerm;

typedef struct nested_db_term {
    struct nested_db_term *next; /* next nested term */
    HashValue ohvalue;           /* hash value of the whole dbterm */
    TrunkDbTerm *hdbterm;
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
    DbTableNestedHashFineLocks *locks;
#endif
} DbTableNestedHash;

typedef struct {
    float avg_chain_len;
    float std_dev_chain_len;
    float std_dev_expected;
    int max_chain_len;
    int min_chain_len;
} DbNestedHashStats;

/* Interface for meta pid table */
int
db_create_nhash(Process *p, DbTable *tbl);

int
db_put_nhash(DbTable *tbl, Eterm obj, int key_clash_fail);

int
db_erase_nhash(DbTable *tbl, Eterm key, Eterm *ret);

#ifdef HARDDEBUG
void
db_check_table_nhash(DbTable *tbl);
#endif

/*
 * Function prototypes, looks the same (except the suffix) for all
 * table types. The process is always an [in out] parameter.
 */
void
db_initialize_nhash(void);

void
db_unfix_table_nhash(DbTableNestedHash *tb);

Uint
db_kept_items_nhash(DbTableNestedHash *tb);


int
db_get_element_array(DbTable *tbl, Eterm key, int ndex,
                     Eterm *ret, int *num_ret);

int
db_erase_bag_exact2(DbTable *tbl, Eterm key, Eterm value);

void
db_calc_stats_nhash(DbTableNestedHash *tb, DbNestedHashStats *stats);


#endif /* _DB_NESTED_HASH_H */
