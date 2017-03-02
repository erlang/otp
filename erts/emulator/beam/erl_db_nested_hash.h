/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2015. All Rights Reserved.
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

#ifndef _DB_NESTED_HASH_H
#define _DB_NESTED_HASH_H


#include "erl_db_util.h" /* DbTerm & DbTableCommon */


#ifdef ERTS_DB_HASH_LOCK_CNT
#   define DB_NESTED_HASH_LOCK_CNT ERTS_DB_HASH_LOCK_CNT
#else
#   define DB_NESTED_HASH_LOCK_CNT 64
#endif


typedef struct trunk_db_term {
    /*
     * The following three fields are allocated
     * if the RootDbTerm contains a nested LHT.
     */

    struct trunk_db_term *onext; /* next term (nested tree) */
    HashValue ohvalue;           /* hash value of the object */

    /*
     * The sp field of the first TrunkDbTerm contains
     * the segtab, the sp field of the other TrunkDbTerms
     * contains the pointer to the previous TrunkDbTerm.
     */
    union {
        struct segment **segtab;
        struct trunk_db_term *previous; /* previous term (same key) */
    } sp;

    /* The following fields are always allocated. */

    struct trunk_db_term *next; /* next term (same key) */

    DbTerm dbterm; /* the actual term */
} TrunkDbTerm;

typedef struct root_db_term {
    struct root_db_term *next; /* next root term (different key) */

    /*
     * The first trunk term of the chain (it's never NULL).
     * Its lsb is used as a flag:
     * o if zero, this RootDbTerm is truncated at the szm field
     * o if set, this RootDbTerm includes szm and successive fields
     * Use GET_TRUNK() and SET_TRUNK() to access this field.
     */
    TrunkDbTerm *trunk;

    HashValue hvalue; /* hash value of the key */
    int nkitems;      /* length of trunk chain */

    /*
     * The following fields are allocated if a nested LHT is required.
     */

    HashValue szm; /* current size mask. */

    int nslots;  /* total number of slots */
    int nsegs;   /* size of segment table */
    int nactive; /* number of active slots */
} RootDbTerm;

typedef struct nested_fixed_deletion {
    int slot;
    struct nested_fixed_deletion *next;
} NestedFixedDeletion;

typedef struct db_table_nested_hash_fine_locks {
    union {
	erts_smp_rwmtx_t lck;
	byte _cache_line_alignment[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(erts_smp_rwmtx_t))];
    } lck_vec[DB_NESTED_HASH_LOCK_CNT];
} DbTableNestedHashFineLocks;

typedef struct db_table_nested_hash {
    DbTableCommon common;
    erts_smp_atomic_t nkeys; /* Number of different keys in table */

    erts_smp_atomic_t segtab; /* The segment table (struct segment **) */
    erts_smp_atomic_t szm;    /* current size mask. */

    /* SMP: nslots and nsegs are protected by is_resizing or table write lock */
    int nslots; /* Total number of slots */
    int nsegs;  /* Size of segment table */

    /* List of slots where elements have been deleted while table was fixed */
    erts_smp_atomic_t fixdel; /* (NestedFixedDeletion *) */
    erts_smp_atomic_t nactive; /* number of active slots */
    erts_smp_atomic_t is_resizing; /* grow/shrink in progress */
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
    int kept_items;
} DbNestedHashStats;

/* Interface for meta pid table */
int
db_create_nhash(Process *p, DbTable *tbl);

int
db_put_nhash(DbTable *tbl, Eterm obj, int key_clash_fail);

int
db_erase_nhash(DbTable *tbl, Eterm key, Eterm *ret);

/*
 * Function prototypes, looks the same (except the suffix) for all
 * table types. The process is always an [in out] parameter.
 */
void
db_initialize_nhash(void);

void
db_unfix_table_nhash(DbTableNestedHash *tb);


int
db_get_element_array(DbTable *tbl, Eterm key, int ndex,
                     Eterm *ret, int *num_ret);

int
db_erase_bag_exact2(DbTable *tbl, Eterm key, Eterm value);

void
db_calc_stats_nhash(DbTableNestedHash *tb, DbNestedHashStats *stats);


#endif /* _DB_NESTED_HASH_H */
