/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2009. All Rights Reserved.
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

#ifndef _DB_HASH_H
#define _DB_HASH_H

#include "erl_db_util.h" /* DbTerm & DbTableCommon */

typedef struct fixed_deletion {
    int slot;
    struct fixed_deletion *next;
} FixedDeletion;

typedef struct hash_db_term {
    struct  hash_db_term* next;  /* next bucket */
    HashValue  hvalue;        /* stored hash value */
    DbTerm dbterm;         /* The actual term */
} HashDbTerm;

#define DB_HASH_LOCK_CNT 16
typedef struct db_table_hash_fine_locks {
    union {
	erts_smp_rwmtx_t lck;
	byte _cache_line_alignment[64];
    }lck_vec[DB_HASH_LOCK_CNT];
} DbTableHashFineLocks;

typedef struct db_table_hash {
    DbTableCommon common;

    erts_smp_atomic_t segtab;  /* The segment table (struct segment**) */
    erts_smp_atomic_t szm;     /* current size mask. */
    
    /* SMP: nslots and nsegs are protected by is_resizing or table write lock */
    int nslots;       /* Total number of slots */
    int nsegs;        /* Size of segment table */

    /* List of slots where elements have been deleted while table was fixed */
    erts_smp_atomic_t fixdel;  /* (FixedDeletion*) */	
    erts_smp_atomic_t nactive; /* Number of "active" slots */
    erts_smp_atomic_t is_resizing; /* grow/shrink in progress */
#ifdef ERTS_SMP
    DbTableHashFineLocks* locks;
#endif
} DbTableHash;


/*
** Function prototypes, looks the same (except the suffix) for all 
** table types. The process is always an [in out] parameter.
*/
void db_initialize_hash(void);
void db_unfix_table_hash(DbTableHash *tb /* [in out] */);
Uint db_kept_items_hash(DbTableHash *tb);

/* Interface for meta pid table */
int db_create_hash(Process *p, 
		   DbTable *tbl /* [in out] */);

int db_put_hash(DbTable *tbl, Eterm obj, int key_clash_fail);

int db_get_hash(Process *p, DbTable *tbl, Eterm key, Eterm *ret);

int db_erase_hash(DbTable *tbl, Eterm key, Eterm *ret);

int db_get_element_array(DbTable *tbl, 
			 Eterm key,
			 int ndex, 
			 Eterm *ret,
			 int *num_ret); 

int db_erase_bag_exact2(DbTable *tbl, Eterm key, Eterm value);

/* not yet in method table */
int db_mark_all_deleted_hash(DbTable *tbl);

typedef struct {
    float avg_chain_len;
    float std_dev_chain_len;
    float std_dev_expected;
    int max_chain_len;
    int min_chain_len;
}DbHashStats;

void db_calc_stats_hash(DbTableHash* tb, DbHashStats*);

#endif /* _DB_HASH_H */
