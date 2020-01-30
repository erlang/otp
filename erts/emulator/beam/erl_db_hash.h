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

#ifndef _DB_HASH_H
#define _DB_HASH_H

#include "erl_db_util.h" /* DbTerm & DbTableCommon */

typedef struct fixed_deletion {
    UWord slot : sizeof(UWord)*8 - 2;
    UWord all : 1;
    UWord trap : 1;
    struct fixed_deletion *next;
} FixedDeletion;


typedef Uint32 HashVal;

typedef struct hash_db_term {
    struct  hash_db_term* next;  /* next bucket */
#if SIZEOF_VOID_P == 4
    Uint32 hvalue : 31;     /* stored hash value */
    Uint32 pseudo_deleted : 1;
# define MAX_HASH_MASK (((Uint32)1 << 31)-1)
#elif SIZEOF_VOID_P == 8
    Uint32 hvalue;
    Uint32 pseudo_deleted;
# define MAX_HASH_MASK ((Uint32)(Sint32)-1)
#endif
    DbTerm dbterm;         /* The actual term */
} HashDbTerm;

#ifdef ERTS_DB_HASH_LOCK_CNT
#define DB_HASH_LOCK_CNT ERTS_DB_HASH_LOCK_CNT
#else
#define DB_HASH_LOCK_CNT 64
#endif

typedef struct DbTableHashLockAndCounter {
    Sint nitems;
    erts_rwmtx_t lck;
} DbTableHashLockAndCounter;

typedef struct db_table_hash_fine_locks {
    union {
	DbTableHashLockAndCounter lck_ctr;
	byte _cache_line_alignment[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(erts_rwmtx_t))];
    }lck_vec[DB_HASH_LOCK_CNT];
} DbTableHashFineLocks;

typedef struct db_table_hash {
    DbTableCommon common;

    /* szm, nactive, shrink_limit are write-protected by is_resizing or table write lock */
    erts_atomic_t szm;     /* current size mask. */
    erts_atomic_t nactive; /* Number of "active" slots */
    erts_atomic_t shrink_limit; /* Shrink table when fewer objects than this */

    erts_atomic_t segtab;  /* The segment table (struct segment**) */
    struct segment* first_segtab[1];

    /* SMP: nslots and nsegs are protected by is_resizing or table write lock */
    int nslots;       /* Total number of slots */
    int nsegs;        /* Size of segment table */

    /* List of slots where elements have been deleted while table was fixed */
    erts_atomic_t fixdel;  /* (FixedDeletion*) */
    erts_atomic_t is_resizing; /* grow/shrink in progress */
    DbTableHashFineLocks* locks;
} DbTableHash;


/*
** Function prototypes, looks the same (except the suffix) for all 
** table types. The process is always an [in out] parameter.
*/
void db_initialize_hash(void);
SWord db_unfix_table_hash(DbTableHash *tb);
Uint db_kept_items_hash(DbTableHash *tb);

/* Interface for meta pid table */
int db_create_hash(Process *p, 
		   DbTable *tbl /* [in out] */);

int db_put_hash(DbTable *tbl, Eterm obj, int key_clash_fail, SWord* consumed_reds_p);

int db_get_hash(Process *p, DbTable *tbl, Eterm key, Eterm *ret);

int db_erase_hash(DbTable *tbl, Eterm key, Eterm *ret);

typedef struct {
    float avg_chain_len;
    float std_dev_chain_len;
    float std_dev_expected;
    int max_chain_len;
    int min_chain_len;
    int kept_items;
}DbHashStats;

void db_calc_stats_hash(DbTableHash* tb, DbHashStats*);
Eterm erts_ets_hash_sizeof_ext_segtab(void);
void
erts_db_foreach_thr_prgr_offheap_hash(void (*func)(ErlOffHeap *, void *),
                                      void *arg);

#ifdef ERTS_ENABLE_LOCK_COUNT
void erts_lcnt_enable_db_hash_lock_count(DbTableHash *tb, int enable);
#endif

#endif /* _DB_HASH_H */
