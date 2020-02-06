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
** Implementation of unordered ETS tables.
** The tables are implemented as linear dynamic hash tables.
** https://en.wikipedia.org/wiki/Linear_hashing
*/

/* SMP:
** The hash table supports two different locking "modes",
** coarse grained and fine grained locking.
**
** Coarse grained locking relies entirely on the caller (erl_db.c) to obtain
** the right kind of lock on the entire table depending on operation (reading
** or writing). No further locking is then done by the table itself.
**
** Fine grained locking is supported by this code to allow concurrent updates
** (and reading) to different parts of the table. This works by keeping one
** rw-mtx for every N'th bucket. Even dynamic growing and shrinking by
** rehashing buckets can be done without exclusive table lock.
**
** A table will support fine grained locking if it is created with flag
** DB_FINE_LOCKED set. The table variable is_thread_safe will then indicate
** if operations need to obtain fine grained locks or not. Some operations
** will for example always use exclusive table lock to guarantee
** a higher level of atomicity.
*/

/* FIXATION:
** Fixating the table, by ets:safe_fixtable or as done by select-operations,
** guarantees two things in current implementation.
** (1) Keys will not *totaly* disappear from the table. A key can thus be used
**     as an iterator to find the next key in iteration sequence. Note however
**     that this does not mean that (pointers to) table objects are guaranteed
**     to be maintained while the table is fixated. A BAG or DBAG may actually
**     remove objects as long as there is at least one object left in the table
**     with the same key (alive or pseudo-deleted). 
** (2) Objects will not be moved between buckets due to table grow/shrink.
**     This will guarantee that iterations do not miss keys or get double-hits.
**
** With fine grained locking, a concurrent thread can fixate the table at any
** time. A "dangerous" operation (delete or move) therefore needs to check
** if the table is fixated while write-locking the bucket.
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
#include "export.h"
#include "erl_binary.h"

#include "erl_db_hash.h"

#define IS_DECENTRALIZED_CTRS(DB) ((DB)->common.counters.is_decentralized)

#define NITEMS_ESTIMATE_FROM_LCK_CTR(LCK_CTR_P)   \
    (LCK_CTR_P->nitems <= 0 ? 1: LCK_CTR_P->nitems)

#define NITEMS_ESTIMATE(DB, LCK_CTR, HASH)                              \
    (IS_DECENTRALIZED_CTRS(DB) ?                                        \
     (DB_HASH_LOCK_CNT *                                                \
      (LCK_CTR != NULL ?                                                \
       NITEMS_ESTIMATE_FROM_LCK_CTR(LCK_CTR) :                          \
       NITEMS_ESTIMATE_FROM_LCK_CTR(GET_LOCK_AND_CTR(DB, HASH)))) :     \
     erts_flxctr_read_centralized(&(DB)->common.counters,               \
                                  ERTS_DB_TABLE_NITEMS_COUNTER_ID))

#define ADD_NITEMS(DB, LCK_CTR, HASH, TO_ADD)                           \
    do {                                                                \
        if (IS_DECENTRALIZED_CTRS(DB)) {                                \
            if (LCK_CTR != NULL) {                                      \
                LCK_CTR->nitems += TO_ADD;                              \
            } else {                                                    \
                GET_LOCK_AND_CTR(DB,HASH)->nitems += TO_ADD;            \
            }                                                           \
        }                                                               \
        erts_flxctr_add(&(DB)->common.counters, ERTS_DB_TABLE_NITEMS_COUNTER_ID, TO_ADD); \
    } while(0)
#define INC_NITEMS(DB, LCK_CTR, HASH)                                   \
    do {                                                                \
        if (IS_DECENTRALIZED_CTRS(DB)) {                                \
            if (LCK_CTR != NULL) {                                      \
                LCK_CTR->nitems++;                                      \
            } else {                                                    \
                GET_LOCK_AND_CTR(DB,HASH)->nitems++;                    \
            }                                                           \
        }                                                               \
        erts_flxctr_inc(&(DB)->common.counters, ERTS_DB_TABLE_NITEMS_COUNTER_ID); \
    } while(0)
#define DEC_NITEMS(DB, LCK_CTR, HASH)                                   \
    do {                                                                \
        if (IS_DECENTRALIZED_CTRS(DB)) {                                \
            if (LCK_CTR != NULL) {                                      \
                LCK_CTR->nitems--;                                      \
            } else {                                                    \
                GET_LOCK_AND_CTR(DB,HASH)->nitems--;                    \
            }                                                           \
        }                                                               \
        erts_flxctr_dec(&(DB)->common.counters, ERTS_DB_TABLE_NITEMS_COUNTER_ID); \
    } while(0)
#define RESET_NITEMS(DB)                                                \
    erts_flxctr_reset(&(DB)->common.counters, ERTS_DB_TABLE_NITEMS_COUNTER_ID)

#define GROW_LIMIT(NACTIVE) ((NACTIVE)*1)
#define SHRINK_LIMIT(TB) erts_atomic_read_nob(&(TB)->shrink_limit)

/*
** We want the first mandatory segment to be small (to reduce minimal footprint)
** and larger extra segments (to reduce number of alloc/free calls).
*/

/* Number of slots in first segment */
#define FIRST_SEGSZ_EXP  8
#define FIRST_SEGSZ      (1 << FIRST_SEGSZ_EXP)
#define FIRST_SEGSZ_MASK (FIRST_SEGSZ - 1)

/* Number of slots per extra segment */
#define EXT_SEGSZ_EXP  11
#define EXT_SEGSZ   (1 << EXT_SEGSZ_EXP)
#define EXT_SEGSZ_MASK (EXT_SEGSZ-1)

#define NSEG_1   (ErtsSizeofMember(DbTableHash,first_segtab) / sizeof(struct segment*))
#define NSEG_2   256 /* Size of second segment table */
#define NSEG_INC 128 /* Number of segments to grow after that */

#  define DB_USING_FINE_LOCKING(TB) (((TB))->common.type & DB_FINE_LOCKED)

#ifdef ETHR_ORDERED_READ_DEPEND
#define SEGTAB(tb) ((struct segment**) erts_atomic_read_nob(&(tb)->segtab))
#else
#define SEGTAB(tb)							\
    (DB_USING_FINE_LOCKING(tb)						\
     ? ((struct segment**) erts_atomic_read_ddrb(&(tb)->segtab))	\
     : ((struct segment**) erts_atomic_read_nob(&(tb)->segtab)))
#endif
#define NACTIVE(tb) ((int)erts_atomic_read_nob(&(tb)->nactive))

#define SLOT_IX_TO_SEG_IX(i) (((i)+(EXT_SEGSZ-FIRST_SEGSZ)) >> EXT_SEGSZ_EXP)

#define BUCKET(tb, i) SEGTAB(tb)[SLOT_IX_TO_SEG_IX(i)]->buckets[(i) & EXT_SEGSZ_MASK]

#ifdef DEBUG
#  define DBG_BUCKET_INACTIVE ((HashDbTerm*)0xdead5107)
#endif


/*
 * When deleting a table, the number of records to delete.
 * Approximate number, because we must delete entire buckets.
 */
#define DELETE_RECORD_LIMIT 10000

/* Calculate slot index from hash value.
** RLOCK_HASH or WLOCK_HASH must be done before.
*/
static ERTS_INLINE Uint hash_to_ix(DbTableHash* tb, HashValue hval)
{
    Uint mask = (DB_USING_FINE_LOCKING(tb)
		 ? erts_atomic_read_acqb(&tb->szm)
		 : erts_atomic_read_nob(&tb->szm));
    Uint ix = hval & mask; 
    if (ix >= erts_atomic_read_nob(&tb->nactive)) {
	ix &= mask>>1;
	ASSERT(ix < erts_atomic_read_nob(&tb->nactive));
    }
    return ix;
}


static ERTS_INLINE FixedDeletion* alloc_fixdel(DbTableHash* tb)
{
    FixedDeletion* fixd = (FixedDeletion*) erts_db_alloc(ERTS_ALC_T_DB_FIX_DEL,
                                                         (DbTable *) tb,
                                                         sizeof(FixedDeletion));
    ERTS_ETS_MISC_MEM_ADD(sizeof(FixedDeletion));
    return fixd;
}

static ERTS_INLINE void free_fixdel(DbTableHash* tb, FixedDeletion* fixd)
{
    erts_db_free(ERTS_ALC_T_DB_FIX_DEL, (DbTable*)tb,
                 fixd, sizeof(FixedDeletion));
    ERTS_ETS_MISC_MEM_ADD(-sizeof(FixedDeletion));
}

static ERTS_INLINE int link_fixdel(DbTableHash* tb,
                                   FixedDeletion* fixd,
                                   erts_aint_t fixated_by_me)
{
    erts_aint_t was_next;
    erts_aint_t exp_next;

    was_next = erts_atomic_read_acqb(&tb->fixdel);
    do { /* Lockless atomic insertion in linked list: */
        if (NFIXED(tb) <= fixated_by_me) {
            free_fixdel(tb, fixd);
            return 0; /* raced by unfixer */
        }
        exp_next = was_next;
	fixd->next = (FixedDeletion*) exp_next;
	was_next = erts_atomic_cmpxchg_mb(&tb->fixdel,
                                              (erts_aint_t) fixd,
                                              exp_next);
    }while (was_next != exp_next);
    return 1;
}

/* Remember a slot containing a pseudo-deleted item
 * Return false if we got raced by unfixing thread
 * and the object should be deleted for real.
 */
static int add_fixed_deletion(DbTableHash* tb, int ix,
                              erts_aint_t fixated_by_me)
{
    FixedDeletion* fixd = alloc_fixdel(tb);
    fixd->slot = ix;
    fixd->all = 0;
    return link_fixdel(tb, fixd, fixated_by_me);
}


static ERTS_INLINE int is_pseudo_deleted(HashDbTerm* p)
{
    return p->pseudo_deleted;
}


/* optimised version of make_hash (normal case? atomic key) */
#define MAKE_HASH(term) \
    ((is_atom(term) ? (atom_tab(atom_val(term))->slot.bucket.hvalue) : \
      make_internal_hash(term, 0)) & MAX_HASH_MASK)

#  define DB_HASH_LOCK_MASK (DB_HASH_LOCK_CNT-1)
#  define GET_LOCK(tb,hval) (&(tb)->locks->lck_vec[(hval) & DB_HASH_LOCK_MASK].lck_ctr.lck)
#  define GET_LOCK_AND_CTR(tb,hval) (&(tb)->locks->lck_vec[(hval) & DB_HASH_LOCK_MASK].lck_ctr)
#  define GET_LOCK_MAYBE(tb,hval) ((tb)->common.is_thread_safe ? NULL : GET_LOCK(tb,hval))

/* Fine grained read lock */
static ERTS_INLINE erts_rwmtx_t* RLOCK_HASH(DbTableHash* tb, HashValue hval)
{
    if (tb->common.is_thread_safe) {
	return NULL;
    } else {
	erts_rwmtx_t* lck = GET_LOCK(tb,hval);
	ASSERT(tb->common.type & DB_FINE_LOCKED);
	erts_rwmtx_rlock(lck);
	return lck;
    }
}
/* Fine grained write lock */
static ERTS_INLINE erts_rwmtx_t* WLOCK_HASH(DbTableHash* tb, HashValue hval)
{
    if (tb->common.is_thread_safe) {
	return NULL;
    } else {
	erts_rwmtx_t* lck = GET_LOCK(tb,hval);
	ASSERT(tb->common.type & DB_FINE_LOCKED);
	erts_rwmtx_rwlock(lck);
	return lck;
    }
}

/* Fine grained write lock */
static ERTS_INLINE
DbTableHashLockAndCounter* WLOCK_HASH_GET_LCK_AND_CTR(DbTableHash* tb, HashValue hval)
{
    if (tb->common.is_thread_safe) {
	return NULL;
    } else {
        DbTableHashLockAndCounter* lck_ctr = GET_LOCK_AND_CTR(tb,hval);
	ASSERT(tb->common.type & DB_FINE_LOCKED);
	erts_rwmtx_rwlock(&lck_ctr->lck);
	return lck_ctr;
    }
}

static ERTS_INLINE void RUNLOCK_HASH(erts_rwmtx_t* lck)
{
    if (lck != NULL) {
	erts_rwmtx_runlock(lck);
    }
}

static ERTS_INLINE void WUNLOCK_HASH(erts_rwmtx_t* lck)
{
    if (lck != NULL) {
	erts_rwmtx_rwunlock(lck);
    }
}

static ERTS_INLINE void WUNLOCK_HASH_LCK_CTR(DbTableHashLockAndCounter* lck_ctr)
{
    if (lck_ctr != NULL) {
	erts_rwmtx_rwunlock(&lck_ctr->lck);
    }
}


#ifdef ERTS_ENABLE_LOCK_CHECK
#  define IFN_EXCL(tb,cmd) (((tb)->common.is_thread_safe) || (cmd))
#  define IS_HASH_RLOCKED(tb,hval) IFN_EXCL(tb,erts_lc_rwmtx_is_rlocked(GET_LOCK(tb,hval)))
#  define IS_HASH_WLOCKED(tb,lck) IFN_EXCL(tb,erts_lc_rwmtx_is_rwlocked(lck))
#  define IS_TAB_WLOCKED(tb) (DB_LOCK_FREE(tb) || erts_lc_rwmtx_is_rwlocked(&(tb)->common.rwlock))
#else
#  define IS_HASH_RLOCKED(tb,hval) (1)
#  define IS_HASH_WLOCKED(tb,hval) (1)
#  define IS_TAB_WLOCKED(tb) (1)
#endif


/* Iteration helper
** Returns "next" slot index or 0 if EOT reached.
** Slot READ locks updated accordingly, unlocked if EOT.
*/
static ERTS_INLINE Sint next_slot(DbTableHash* tb, Uint ix,
				  erts_rwmtx_t** lck_ptr)
{
    ix += DB_HASH_LOCK_CNT;
    if (ix < NACTIVE(tb)) return ix;
    RUNLOCK_HASH(*lck_ptr);
    ix = (ix + 1) & DB_HASH_LOCK_MASK;
    if (ix != 0) *lck_ptr = RLOCK_HASH(tb,ix);
    return ix;
}
/* Same as next_slot but with WRITE locking */
static ERTS_INLINE Sint next_slot_w(DbTableHash* tb, Uint ix,
				    erts_rwmtx_t** lck_ptr)
{
    ix += DB_HASH_LOCK_CNT;
    if (ix < NACTIVE(tb)) return ix;
    WUNLOCK_HASH(*lck_ptr);
    ix = (ix + 1) & DB_HASH_LOCK_MASK;
    if (ix != 0) *lck_ptr = WLOCK_HASH(tb,ix);
    return ix;
}



static ERTS_INLINE void free_term(DbTableHash *tb, HashDbTerm* p)
{
    db_free_term((DbTable*)tb, p, offsetof(HashDbTerm, dbterm));
}

static ERTS_INLINE void free_term_list(DbTableHash *tb, HashDbTerm* p)
{
    while (p) {
        HashDbTerm* next = p->next;
        free_term(tb, p);
        p = next;
    }
}


/*
 * Local types 
 */
struct mp_prefound {
    HashDbTerm** bucket;
    int ix;
};

struct mp_info {
    int something_can_match;	/* The match_spec is not "impossible" */
    int key_given;
    struct mp_prefound dlists[10];  /* Default list of "pre-found" buckets */
    struct mp_prefound* lists;   /* Buckets to search if keys are given, 
				  * = dlists initially */
    unsigned num_lists;         /* Number of elements in "lists",
				 * = 0 initially */
    Binary *mp;                 /* The compiled match program */
};

/* A table segment */
struct segment {
    HashDbTerm* buckets[1];
};
#define SIZEOF_SEGMENT(N) \
    (offsetof(struct segment,buckets) + sizeof(HashDbTerm*)*(N))

/* An extended segment table */
struct ext_segtab {
    ErtsThrPrgrLaterOp lop;
    struct segment** prev_segtab;  /* Used when table is shrinking */
    int prev_nsegs;                /* Size of prev_segtab */
    int nsegs;                     /* Size of this segtab */
    struct segment* segtab[1];     /* The segment table */
};
#define SIZEOF_EXT_SEGTAB(NSEGS) \
    (offsetof(struct ext_segtab,segtab) + sizeof(struct segment*)*(NSEGS))


static ERTS_INLINE void SET_SEGTAB(DbTableHash* tb,
				   struct segment** segtab)
{
    if (DB_USING_FINE_LOCKING(tb))
	erts_atomic_set_wb(&tb->segtab, (erts_aint_t) segtab);
    else
	erts_atomic_set_nob(&tb->segtab, (erts_aint_t) segtab);
}

/* Used by select_replace on analyze_pattern */
typedef int (*extra_match_validator_t)(int keypos, Eterm match, Eterm guard, Eterm body);

/*
** Forward decl's (static functions)
*/
static struct ext_segtab* alloc_ext_segtab(DbTableHash* tb, unsigned seg_ix);
static void alloc_seg(DbTableHash *tb);
static int free_seg(DbTableHash *tb);
static HashDbTerm* next_live(DbTableHash *tb, Uint *iptr, erts_rwmtx_t** lck_ptr,
			     HashDbTerm *list);
static HashDbTerm* search_list(DbTableHash* tb, Eterm key, 
			       HashValue hval, HashDbTerm *list);
static void shrink(DbTableHash* tb, int nitems);
static void grow(DbTableHash* tb, int nitems);
static Eterm build_term_list(Process* p, HashDbTerm* ptr1, HashDbTerm* ptr2,
			   Uint sz, DbTableHash*);
static int analyze_pattern(DbTableHash *tb, Eterm pattern,
                           extra_match_validator_t extra_validator, /* Optional callback */
                           struct mp_info *mpi);

/*
 *  Method interface functions
 */
static int db_first_hash(Process *p, 
			 DbTable *tbl, 
			 Eterm *ret);

static int db_next_hash(Process *p, 
			DbTable *tbl, 
			Eterm key,
			Eterm *ret);

static int db_member_hash(DbTable *tbl, Eterm key, Eterm *ret);

static int db_get_element_hash(Process *p, DbTable *tbl, 
			       Eterm key, int ndex, Eterm *ret);

static int db_erase_object_hash(DbTable *tbl, Eterm object,Eterm *ret);

static int db_slot_hash(Process *p, DbTable *tbl, 
			Eterm slot_term, Eterm *ret);

static int db_select_chunk_hash(Process *p, DbTable *tbl, Eterm tid,
				Eterm pattern, Sint chunk_size,
				int reverse, Eterm *ret, enum DbIterSafety);
static int db_select_hash(Process *p, DbTable *tbl, Eterm tid,
			  Eterm pattern, int reverse, Eterm *ret,
                          enum DbIterSafety);
static int db_select_continue_hash(Process *p, DbTable *tbl,
				   Eterm continuation, Eterm *ret,
                                   enum DbIterSafety*);

static int db_select_count_hash(Process *p, DbTable *tbl, Eterm tid,
				Eterm pattern, Eterm *ret, enum DbIterSafety);
static int db_select_count_continue_hash(Process *p, DbTable *tbl,
					 Eterm continuation, Eterm *ret,
                                         enum DbIterSafety*);
static int db_select_delete_hash(Process *p, DbTable *tbl, Eterm tid,
				 Eterm pattern, Eterm *ret,
                                 enum DbIterSafety);
static int db_select_delete_continue_hash(Process *p, DbTable *tbl,
					  Eterm continuation, Eterm *ret,
                                          enum DbIterSafety*);

static int db_select_replace_hash(Process *p, DbTable *tbl, Eterm tid,
                                  Eterm pattern, Eterm *ret, enum DbIterSafety);
static int db_select_replace_continue_hash(Process *p, DbTable *tbl,
                                           Eterm continuation, Eterm *ret,
                                           enum DbIterSafety*);

static int db_take_hash(Process *, DbTable *, Eterm, Eterm *);
static void db_print_hash(fmtfn_t to,
			  void *to_arg,
			  int show,
			  DbTable *tbl);
static int db_free_empty_table_hash(DbTable *tbl);

static SWord db_free_table_continue_hash(DbTable *tbl, SWord reds);


static void db_foreach_offheap_hash(DbTable *,
				    void (*)(ErlOffHeap *, void *),
				    void *);

static SWord db_delete_all_objects_hash(Process* p,
                                        DbTable* tbl,
                                        SWord reds,
                                        Eterm* nitems_holder_wb);
static Eterm db_delete_all_objects_get_nitems_from_holder_hash(Process* p,
                                                               Eterm nitems_holder);
#ifdef HARDDEBUG
static void db_check_table_hash(DbTableHash *tb);
#endif
static int
db_lookup_dbterm_hash(Process *p, DbTable *tbl, Eterm key, Eterm obj,
                      DbUpdateHandle* handle);
static void
db_finalize_dbterm_hash(int cret, DbUpdateHandle* handle);
static void* db_eterm_to_dbterm_hash(int compress, int keypos, Eterm obj);
static void* db_dbterm_list_prepend_hash(void* list, void* db_term);
static void* db_dbterm_list_remove_first_hash(void** list);
static int db_put_dbterm_hash(DbTable* tb,
                              void* obj,
                              int key_clash_fail,
                              SWord *consumed_reds_p);
static void db_free_dbterm_hash(int compressed, void* obj);
static Eterm db_get_dbterm_key_hash(DbTable* tb, void* db_term);

static int
db_get_binary_info_hash(Process *p, DbTable *tbl, Eterm key, Eterm *ret);
static int db_raw_first_hash(Process* p, DbTable *tbl, Eterm *ret);
static int db_raw_next_hash(Process* p, DbTable *tbl, Eterm key, Eterm *ret);

static ERTS_INLINE void try_shrink(DbTableHash* tb, Sint nitems)
{
    if (nitems < SHRINK_LIMIT(tb) && !IS_FIXED(tb)) {
	shrink(tb, nitems);
    }
}	

/* Is this a live object (not pseodo-deleted) with the specified key? 
*/
static ERTS_INLINE int has_live_key(DbTableHash* tb, HashDbTerm* b,
				    Eterm key, HashValue hval)
{
    if (b->hvalue != hval || is_pseudo_deleted(b))
        return 0;
    else {
	Eterm itemKey = GETKEY(tb, b->dbterm.tpl);
	ASSERT(!is_header(itemKey));
	return EQ(key, itemKey);
    }
}

/* Has this object the specified key? Can be pseudo-deleted.
*/
static ERTS_INLINE int has_key(DbTableHash* tb, HashDbTerm* b,
			       Eterm key, HashValue hval)
{
    if (b->hvalue != hval)
        return 0;
    else {
	Eterm itemKey = GETKEY(tb, b->dbterm.tpl);
	ASSERT(!is_header(itemKey));
	return EQ(key, itemKey);
    }
}

static ERTS_INLINE HashDbTerm* new_dbterm_hash(DbTableCommon* tb, Eterm obj)
{
    HashDbTerm* p;
    if (tb->compress) {
	p = db_store_term_comp(tb, tb->keypos, NULL, offsetof(HashDbTerm,dbterm), obj);
    }
    else {
	p = db_store_term(tb, NULL, offsetof(HashDbTerm,dbterm), obj);
    }
    return p;
}

/*
 * This function only differ from new_dbterm_hash in that it does not
 * adjust the memory size of a given table.
 */
static ERTS_INLINE HashDbTerm* new_dbterm_hash_no_tab(int compress, int keypos, Eterm obj)
{
    HashDbTerm* p;
    if (compress) {
	p = db_store_term_comp(NULL, keypos, NULL, offsetof(HashDbTerm,dbterm), obj);
    } else {
	p = db_store_term(NULL, NULL, offsetof(HashDbTerm,dbterm), obj);
    }
    return p;
}

static ERTS_INLINE HashDbTerm* new_dbterm(DbTableHash* tb, Eterm obj)
{
    return new_dbterm_hash(&tb->common, obj);
}

static ERTS_INLINE HashDbTerm* replace_dbterm(DbTableHash* tb, HashDbTerm* old,
					      Eterm obj)
{
    HashDbTerm* ret;
    ASSERT(old != NULL);
    if (tb->common.compress) {
	ret = db_store_term_comp(&tb->common,
                                 tb->common.keypos,
                                 &(old->dbterm),
                                 offsetof(HashDbTerm,dbterm),
                                 obj);
    }
    else {
	ret = db_store_term(&tb->common,
                            &(old->dbterm),
                            offsetof(HashDbTerm,dbterm),
                            obj);
    }
    return ret;
}



/*
** External interface 
*/
DbTableMethod db_hash =
{
    db_create_hash,
    db_first_hash,
    db_next_hash,
    db_first_hash,   /* last == first  */
    db_next_hash,    /* prev == next   */
    db_put_hash,
    db_get_hash,
    db_get_element_hash,
    db_member_hash,
    db_erase_hash,
    db_erase_object_hash,
    db_slot_hash,
    db_select_chunk_hash,
    db_select_hash,
    db_select_delete_hash,
    db_select_continue_hash,
    db_select_delete_continue_hash,
    db_select_count_hash,
    db_select_count_continue_hash,
    db_select_replace_hash,
    db_select_replace_continue_hash,
    db_take_hash,
    db_delete_all_objects_hash,
    db_delete_all_objects_get_nitems_from_holder_hash,
    db_free_empty_table_hash,
    db_free_table_continue_hash,
    db_print_hash,
    db_foreach_offheap_hash,
    db_lookup_dbterm_hash,
    db_finalize_dbterm_hash,
    db_eterm_to_dbterm_hash,
    db_dbterm_list_prepend_hash,
    db_dbterm_list_remove_first_hash,
    db_put_dbterm_hash,
    db_free_dbterm_hash,
    db_get_dbterm_key_hash,
    db_get_binary_info_hash,
    db_raw_first_hash,
    db_raw_next_hash
};

#ifdef DEBUG
/* Wait a while to provoke race and get code coverage */
static void DEBUG_WAIT(void)
{
    unsigned long spin = 1UL << 20;
    while (--spin);
}
#else
#  define DEBUG_WAIT()
#endif

/* Rare case of restoring the rest of the fixdel list
   when "unfixer" gets interrupted by "fixer" */ 
static void restore_fixdel(DbTableHash* tb, FixedDeletion* fixdel)
{
    /*int tries = 0;*/
    DEBUG_WAIT();
    if (erts_atomic_cmpxchg_relb(&tb->fixdel,
				     (erts_aint_t) fixdel,
				     (erts_aint_t) NULL) != (erts_aint_t) NULL) {
	/* Oboy, must join lists */    
	FixedDeletion* last = fixdel;
	erts_aint_t was_tail;
	erts_aint_t exp_tail;

	while (last->next != NULL) last = last->next;
	was_tail = erts_atomic_read_acqb(&tb->fixdel);
	do { /* Lockless atomic list insertion */
	    exp_tail = was_tail;
	    last->next = (FixedDeletion*) exp_tail;
	    /*++tries;*/
	    DEBUG_WAIT();
	    was_tail = erts_atomic_cmpxchg_relb(&tb->fixdel,
						    (erts_aint_t) fixdel,
						    exp_tail);
	}while (was_tail != exp_tail);
    }
    /*erts_fprintf(stderr,"erl_db_hash: restore_fixdel tries=%d\r\n", tries);*/
}
/*
** Table interface routines ie what's called by the bif's 
*/

SWord db_unfix_table_hash(DbTableHash *tb)
{
    FixedDeletion* fixdel;
    SWord work = 0;

    ERTS_LC_ASSERT(IS_TAB_WLOCKED(tb)
                   || (erts_lc_rwmtx_is_rlocked(&tb->common.rwlock)
                       && !tb->common.is_thread_safe));
restart:
    fixdel = (FixedDeletion*) erts_atomic_xchg_mb(&tb->fixdel,
                                                  (erts_aint_t) NULL);
    while (fixdel) {
        FixedDeletion *free_me;

        do {
            HashDbTerm **bp;
            HashDbTerm *b;
            HashDbTerm *free_us = NULL;
            erts_rwmtx_t* lck;

            lck = WLOCK_HASH(tb, fixdel->slot);

            if (IS_FIXED(tb)) { /* interrupted by fixer */
                WUNLOCK_HASH(lck);
                restore_fixdel(tb,fixdel);
                if (!IS_FIXED(tb)) {
                    goto restart; /* unfixed again! */
                }
                return work;
            }
            if (fixdel->slot < NACTIVE(tb)) {
                bp = &BUCKET(tb, fixdel->slot);
                b = *bp;

                while (b != NULL) {
                    if (is_pseudo_deleted(b)) {
                        HashDbTerm* nxt = b->next;
                        b->next = free_us;
                        free_us = b;
                        work++;
                        b = *bp = nxt;
                    } else {
                        bp = &b->next;
                        b = b->next;
                    }
                }
            }
            /* else slot has been joined and purged by shrink() */
            WUNLOCK_HASH(lck);
            free_term_list(tb, free_us);

        }while (fixdel->all && fixdel->slot-- > 0);

        free_me = fixdel;
        fixdel = fixdel->next;
        free_fixdel(tb, free_me);
        work++;
    }

    /* ToDo: Maybe try grow/shrink the table as well */

    return work;
}

int db_create_hash(Process *p, DbTable *tbl)
{
    DbTableHash *tb = &tbl->hash;

    erts_atomic_init_nob(&tb->szm, FIRST_SEGSZ_MASK);
    erts_atomic_init_nob(&tb->nactive, FIRST_SEGSZ);
    erts_atomic_init_nob(&tb->shrink_limit, 0);
    erts_atomic_init_nob(&tb->fixdel, (erts_aint_t)NULL);
    erts_atomic_init_nob(&tb->segtab, (erts_aint_t)NULL);
    SET_SEGTAB(tb, tb->first_segtab);
    tb->nsegs = NSEG_1;
    tb->nslots = FIRST_SEGSZ;
    tb->first_segtab[0] = (struct segment*) erts_db_alloc(ERTS_ALC_T_DB_SEG,
                                                          (DbTable *) tb,
                                                          SIZEOF_SEGMENT(FIRST_SEGSZ));
    sys_memset(tb->first_segtab[0], 0, SIZEOF_SEGMENT(FIRST_SEGSZ));

    erts_atomic_init_nob(&tb->is_resizing, 0);
    if (tb->common.type & DB_FINE_LOCKED) {
	erts_rwmtx_opt_t rwmtx_opt = ERTS_RWMTX_OPT_DEFAULT_INITER;
	int i;
	if (tb->common.type & DB_FREQ_READ)
	    rwmtx_opt.type = ERTS_RWMTX_TYPE_FREQUENT_READ;
	if (erts_ets_rwmtx_spin_count >= 0)
	    rwmtx_opt.main_spincount = erts_ets_rwmtx_spin_count;
	tb->locks = (DbTableHashFineLocks*) erts_db_alloc(ERTS_ALC_T_DB_SEG, /* Other type maybe? */
                                                          (DbTable *) tb,
                                                          sizeof(DbTableHashFineLocks));
	for (i=0; i<DB_HASH_LOCK_CNT; ++i) {
            erts_rwmtx_init_opt(&tb->locks->lck_vec[i].lck_ctr.lck, &rwmtx_opt,
                "db_hash_slot", tb->common.the_name, ERTS_LOCK_FLAGS_CATEGORY_DB);
            tb->locks->lck_vec[i].lck_ctr.nitems = 0;
	}
	/* This important property is needed to guarantee the two buckets
    	 * involved in a grow/shrink operation it protected by the same lock:
	 */
	ASSERT(erts_atomic_read_nob(&tb->nactive) % DB_HASH_LOCK_CNT == 0);
    }
    else { /* coarse locking */
	tb->locks = NULL;
    }
    ERTS_THR_MEMORY_BARRIER;
    return DB_ERROR_NONE;
}

static int db_first_hash(Process *p, DbTable *tbl, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    Uint ix = 0;
    erts_rwmtx_t* lck = RLOCK_HASH(tb,ix);
    HashDbTerm* list;

    list = BUCKET(tb,ix);
    list = next_live(tb, &ix, &lck, list);

    if (list != NULL) {
	*ret = db_copy_key(p, tbl, &list->dbterm);
	RUNLOCK_HASH(lck);
    }
    else {
	*ret = am_EOT;
    }
    return DB_ERROR_NONE;
}


static int db_next_hash(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    Uint ix;
    HashDbTerm* b;
    erts_rwmtx_t* lck;

    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb,hval);
    ix = hash_to_ix(tb, hval);
    b = BUCKET(tb, ix);    

    for (;;) {
	if (b == NULL) {
	    RUNLOCK_HASH(lck);
	    return DB_ERROR_BADKEY;    
	}
	if (has_key(tb, b, key, hval)) {
	    break;
	}
	b = b->next;
    }
    /* Key found */

    b = next_live(tb, &ix, &lck, b->next);
    if (tb->common.status & (DB_BAG | DB_DUPLICATE_BAG)) {
	while (b != 0) {
	    if (!has_key(tb, b, key, hval)) {
		break;
	    }
	    b = next_live(tb, &ix, &lck, b->next);
	}
    }
    if (b == NULL) {
	*ret = am_EOT;
    }
    else {
        ASSERT(!is_pseudo_deleted(b));
	*ret = db_copy_key(p, tbl, &b->dbterm);
	RUNLOCK_HASH(lck);
    }    
    return DB_ERROR_NONE;
}    

static int db_eq_terms_comp(DbTableCommon* tb, DbTerm* a, DbTerm* b)
{
    ErlOffHeap tmp_offheap_a;
    Eterm* allocp_a;
    Eterm* hp_a;
    Eterm tmp_a;
    ErlOffHeap tmp_offheap_b;
    Eterm* allocp_b;
    Eterm* hp_b;
    Eterm tmp_b;
    int is_eq;

    ASSERT(tb->compress);
    hp_a = allocp_a = erts_alloc(ERTS_ALC_T_TMP, b->size*sizeof(Eterm));
    tmp_offheap_a.first = NULL;
    tmp_a = db_copy_from_comp(tb, a, &hp_a, &tmp_offheap_a);

    hp_b = allocp_b = erts_alloc(ERTS_ALC_T_TMP, b->size*sizeof(Eterm));
    tmp_offheap_b.first = NULL;
    tmp_b = db_copy_from_comp(tb, b, &hp_b, &tmp_offheap_b);

    is_eq = eq(tmp_a,tmp_b);
    erts_cleanup_offheap(&tmp_offheap_a);
    erts_free(ERTS_ALC_T_TMP, allocp_a);
    erts_cleanup_offheap(&tmp_offheap_b);
    erts_free(ERTS_ALC_T_TMP, allocp_b);
    return is_eq;
}

static ERTS_INLINE int db_terms_eq(DbTableCommon* tb, DbTerm* a, DbTerm* b)
{
    if (!tb->compress) {
	return EQ(make_tuple(a->tpl), make_tuple(b->tpl));
    }
    else {
	return db_eq_terms_comp(tb, a, b);
    }
}

static int db_put_dbterm_hash(DbTable* tbl,
                              void* ob,
                              int key_clash_fail,
                              SWord *consumed_reds_p)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    Eterm key;
    HashDbTerm** bp;
    HashDbTerm* b;
    HashDbTerm* q;
    DbTableHashLockAndCounter* lck_ctr;
    int nitems;
    int ret = DB_ERROR_NONE;
    HashDbTerm *value_to_insert = ob;
    Uint size_to_insert = db_term_size(tbl, value_to_insert, offsetof(HashDbTerm, dbterm));
    ERTS_DB_ALC_MEM_UPDATE_(tbl, 0, size_to_insert);
    key = GETKEY(tb, value_to_insert->dbterm.tpl);
    hval = MAKE_HASH(key);
    value_to_insert->hvalue = hval;
    lck_ctr = WLOCK_HASH_GET_LCK_AND_CTR(tb, hval);
    ix = hash_to_ix(tb, hval);
    bp = &BUCKET(tb, ix);
    b = *bp;

    for (;;) {
	if (b == NULL) {
	    goto Lnew;
	}
	if (has_key(tb,b,key,hval)) {
	    break;
	}
	bp = &b->next;
	b = b->next;
    }
    /* Key found
    */
    if (tb->common.status & DB_SET) {
	HashDbTerm* bnext = b->next;
	if (is_pseudo_deleted(b)) {
            INC_NITEMS(tb, lck_ctr, hval);
            b->pseudo_deleted = 0;
	}
	else if (key_clash_fail) {
	    ret = DB_ERROR_BADKEY;
	    goto Ldone;
	}
        value_to_insert->pseudo_deleted = b->pseudo_deleted;
        free_term(tb, b);
        q = value_to_insert;
	q->next = bnext;
	ASSERT(q->hvalue == hval);
	*bp = q;
	goto Ldone;
    }
    else if (key_clash_fail) { /* && (DB_BAG || DB_DUPLICATE_BAG) */
	q = b;
	do {
	    if (!is_pseudo_deleted(q)) {
		ret = DB_ERROR_BADKEY;
		goto Ldone;
	    }
	    q = q->next;
	}while (q != NULL && has_key(tb,q,key,hval));
    }
    else if (tb->common.status & DB_BAG) {
	HashDbTerm** qp = bp;
	q = b;
	do {
	    if (db_terms_eq(&tb->common,
                            &value_to_insert->dbterm,
                            &q->dbterm)) {
		if (is_pseudo_deleted(q)) {
                    INC_NITEMS(tb, lck_ctr, hval);
                    q->pseudo_deleted = 0;
		    ASSERT(q->hvalue == hval);
		    if (q != b) { /* must move to preserve key insertion order */
			*qp = q->next;
			q->next = b;
			*bp = q;
		    }
		}
                free_term(tb, value_to_insert);
		goto Ldone;
	    }
	    qp = &q->next;
	    q = *qp;
            (*consumed_reds_p)++;
	}while (q != NULL && has_key(tb,q,key,hval));
    }
    /*else DB_DUPLICATE_BAG */

Lnew:
    q = value_to_insert;
    q->hvalue = hval;
    q->pseudo_deleted = 0;
    q->next = b;
    *bp = q;
    INC_NITEMS(tb, lck_ctr, hval);
    nitems = NITEMS_ESTIMATE(tb, lck_ctr, hval);
    WUNLOCK_HASH_LCK_CTR(lck_ctr);
    {
	int nactive = NACTIVE(tb);
	if (nitems > GROW_LIMIT(nactive) && !IS_FIXED(tb)) {
	    grow(tb, nitems);
	}
    }
    return DB_ERROR_NONE;

Ldone:
    WUNLOCK_HASH_LCK_CTR(lck_ctr);
    return ret;
}

int db_put_hash(DbTable *tbl, Eterm obj, int key_clash_fail,
                SWord *consumed_reds_p)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    Eterm key;
    HashDbTerm** bp;
    HashDbTerm* b;
    HashDbTerm* q;
    DbTableHashLockAndCounter* lck_ctr;
    Sint nitems;
    int ret = DB_ERROR_NONE;

    key = GETKEY(tb, tuple_val(obj));
    hval = MAKE_HASH(key);
    lck_ctr = WLOCK_HASH_GET_LCK_AND_CTR(tb, hval);
    ix = hash_to_ix(tb, hval);
    bp = &BUCKET(tb, ix);
    b = *bp;

    for (;;) {
	if (b == NULL) {
	    goto Lnew;
	}
	if (has_key(tb,b,key,hval)) {
	    break;
	}
	bp = &b->next;
	b = b->next;
    }
    /* Key found
    */
    if (tb->common.status & DB_SET) {
	HashDbTerm* bnext = b->next;
	if (is_pseudo_deleted(b)) {
            INC_NITEMS(tb, lck_ctr, hval);
            b->pseudo_deleted = 0;
	}
	else if (key_clash_fail) {
	    ret = DB_ERROR_BADKEY;
	    goto Ldone;
	}
	q = replace_dbterm(tb, b, obj);
	q->next = bnext;
	ASSERT(q->hvalue == hval);
	*bp = q;
	goto Ldone;
    }
    else if (key_clash_fail) { /* && (DB_BAG || DB_DUPLICATE_BAG) */
	q = b;
	do {
	    if (!is_pseudo_deleted(q)) {
		ret = DB_ERROR_BADKEY;
		goto Ldone;
	    }
	    q = q->next;
	}while (q != NULL && has_key(tb,q,key,hval)); 	
    }
    else if (tb->common.status & DB_BAG) {
	HashDbTerm** qp = bp;
	q = b;
	do {
	    if (db_eq(&tb->common,obj,&q->dbterm)) {
		if (is_pseudo_deleted(q)) {
		    INC_NITEMS(tb, lck_ctr, hval);
                    q->pseudo_deleted = 0;
		    ASSERT(q->hvalue == hval);
		    if (q != b) { /* must move to preserve key insertion order */
			*qp = q->next;
			q->next = b;
			*bp = q;
		    }
		}
		goto Ldone;
	    }
	    qp = &q->next;
            q = *qp;
            (*consumed_reds_p)++;
        }while (q != NULL && has_key(tb,q,key,hval));

    }
    /*else DB_DUPLICATE_BAG */

Lnew:
    q = new_dbterm(tb, obj);
    q->hvalue = hval;
    q->pseudo_deleted = 0;
    q->next = b;
    *bp = q;
    INC_NITEMS(tb, lck_ctr, hval);
    nitems = NITEMS_ESTIMATE(tb, lck_ctr, hval);
    WUNLOCK_HASH_LCK_CTR(lck_ctr);
    {
	int nactive = NACTIVE(tb);
	if (nitems > GROW_LIMIT(nactive) && !IS_FIXED(tb)) {
	    grow(tb, nitems);
	}
    }
    return DB_ERROR_NONE;

Ldone:
    WUNLOCK_HASH_LCK_CTR(lck_ctr);
    return ret;
}

static Eterm
get_term_list(Process *p, DbTableHash *tb, Eterm key, HashValue hval,
              HashDbTerm *b1, HashDbTerm **bend)
{
    HashDbTerm* b2 = b1->next;
    Eterm copy;
    Uint sz = b1->dbterm.size + 2;

    if (tb->common.status & (DB_BAG | DB_DUPLICATE_BAG)) {
        while (b2 && has_key(tb, b2, key, hval)) {
	    if (!is_pseudo_deleted(b2))
		sz += b2->dbterm.size + 2;

            b2 = b2->next;
        }
    }
    copy = build_term_list(p, b1, b2, sz, tb);
    if (bend) {
        *bend = b2;
    }
    return copy;
}

int db_get_hash(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    HashDbTerm* b;
    erts_rwmtx_t* lck;

    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb,hval);
    ix = hash_to_ix(tb, hval);
    b = BUCKET(tb, ix);

    while(b != 0) {
        if (has_live_key(tb, b, key, hval)) {
            *ret = get_term_list(p, tb, key, hval, b, NULL);
	    goto done;
	}
        b = b->next;
    }
    *ret = NIL;
done:
    RUNLOCK_HASH(lck);
    return DB_ERROR_NONE;
}
    
static int db_member_hash(DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    HashDbTerm* b1;
    erts_rwmtx_t* lck;

    hval = MAKE_HASH(key);
    ix = hash_to_ix(tb, hval);
    lck = RLOCK_HASH(tb, hval);
    b1 = BUCKET(tb, ix);

    while(b1 != 0) {
	if (has_live_key(tb,b1,key,hval)) {
	    *ret = am_true;
	    goto done;
	}
	b1 = b1->next;
    }
    *ret = am_false;
done:
    RUNLOCK_HASH(lck);
    return DB_ERROR_NONE;
}
    
static int db_get_element_hash(Process *p, DbTable *tbl, 
			       Eterm key,
			       int ndex, 
			       Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    HashDbTerm* b1;
    erts_rwmtx_t* lck;
    int retval;
    
    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, hval);
    b1 = BUCKET(tb, ix);


    while(b1 != 0) {
	if (has_live_key(tb,b1,key,hval)) {
	    if (ndex > arityval(b1->dbterm.tpl[0])) {
		retval = DB_ERROR_BADITEM;
		goto done;
	    }
	    if (tb->common.status & (DB_BAG | DB_DUPLICATE_BAG)) {
		HashDbTerm* b;
		HashDbTerm* b2 = b1->next;
		Eterm elem_list = NIL;

		while(b2 != NULL && has_key(tb,b2,key,hval)) {
		    if (ndex > arityval(b2->dbterm.tpl[0])
			&& !is_pseudo_deleted(b2)) {
			retval = DB_ERROR_BADITEM;
			goto done;
		    }
		    b2 = b2->next;
		}
		b = b1;
		while(b != b2) {
		    if (!is_pseudo_deleted(b)) {
			Eterm *hp;
			Eterm copy = db_copy_element_from_ets(&tb->common, p,
							      &b->dbterm, ndex, &hp, 2);
			elem_list = CONS(hp, copy, elem_list);
		    }
		    b = b->next;
		}
		*ret = elem_list;
	    }
	    else {
		Eterm* hp;
		*ret = db_copy_element_from_ets(&tb->common, p, &b1->dbterm, ndex, &hp, 0);
	    }
	    retval = DB_ERROR_NONE;
	    goto done;
	}
	b1 = b1->next;
    }
    retval = DB_ERROR_BADKEY;
done:
    RUNLOCK_HASH(lck);
    return retval;
}

/*
** NB, this is for the db_erase/2 bif.
*/
int db_erase_hash(DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    HashDbTerm** bp;
    HashDbTerm* b;
    HashDbTerm* free_us = NULL;
    DbTableHashLockAndCounter* lck_ctr;
    int nitems_diff = 0;
    Sint nitems;
    hval = MAKE_HASH(key);
    lck_ctr = WLOCK_HASH_GET_LCK_AND_CTR(tb,hval);
    ix = hash_to_ix(tb, hval);
    bp = &BUCKET(tb, ix);
    b = *bp;

    while(b != 0) {
	if (has_live_key(tb,b,key,hval)) {
	    --nitems_diff;
	    if (nitems_diff == -1 && IS_FIXED(tb)
                && add_fixed_deletion(tb, ix, 0)) {
		/* Pseudo remove (no need to keep several of same key) */
		b->pseudo_deleted = 1;
	    } else {
		HashDbTerm* next = b->next;
                b->next = free_us;
		free_us = b;
		b = *bp = next;
		continue;
	    }
	}
	else {
	    if (nitems_diff && !is_pseudo_deleted(b))
		break;
	}
	bp = &b->next;
	b = b->next;
    }
    if (nitems_diff) {
        ADD_NITEMS(tb, lck_ctr, hval, nitems_diff);
        nitems = NITEMS_ESTIMATE(tb, lck_ctr, hval);
    }
    WUNLOCK_HASH_LCK_CTR(lck_ctr);
    if (nitems_diff) {
	try_shrink(tb, nitems);
    }
    free_term_list(tb, free_us);
    *ret = am_true;
    return DB_ERROR_NONE;
}    

/*
** This is for the ets:delete_object BIF
*/
static int db_erase_object_hash(DbTable *tbl, Eterm object, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    HashDbTerm** bp;
    HashDbTerm* b;
    HashDbTerm* free_us = NULL;
    DbTableHashLockAndCounter* lck_ctr;
    int nitems_diff = 0;
    Sint nitems;
    int nkeys = 0;
    Eterm key;

    key = GETKEY(tb, tuple_val(object));
    hval = MAKE_HASH(key);
    lck_ctr = WLOCK_HASH_GET_LCK_AND_CTR(tb,hval);
    ix = hash_to_ix(tb, hval);
    bp = &BUCKET(tb, ix);
    b = *bp;

    while(b != 0) {
	if (has_live_key(tb,b,key,hval)) {
	    ++nkeys;
	    if (db_eq(&tb->common,object, &b->dbterm)) {
		--nitems_diff;
		if (nkeys==1 && IS_FIXED(tb) && add_fixed_deletion(tb,ix,0)) {
		    b->pseudo_deleted = 1;
		    bp = &b->next;
		    b = b->next;
		} else {
                    HashDbTerm* next = b->next;
		    b->next = free_us;
                    free_us = b;
		    b = *bp = next;
		}
		if (tb->common.status & (DB_DUPLICATE_BAG)) {
		    continue;
		} else {
		    break;
		}
	    }
	}
	else if (nitems_diff && !is_pseudo_deleted(b)) {
	    break;
	}
	bp = &b->next;
	b = b->next;
    }
    if (nitems_diff) {
        ADD_NITEMS(tb, lck_ctr, hval, nitems_diff);
        nitems = NITEMS_ESTIMATE(tb, lck_ctr, hval);
    }
    WUNLOCK_HASH_LCK_CTR(lck_ctr);
    if (nitems_diff) {
	try_shrink(tb, nitems);
    }
    free_term_list(tb, free_us);
    *ret = am_true;
    return DB_ERROR_NONE;
}    


static int db_slot_hash(Process *p, DbTable *tbl, Eterm slot_term, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    erts_rwmtx_t* lck;
    Sint slot;
    int retval;
    int nactive;

    if (is_not_small(slot_term) || ((slot = signed_val(slot_term)) < 0)) {
	return DB_ERROR_BADPARAM;
    }
    lck = RLOCK_HASH(tb, slot);
    nactive = NACTIVE(tb);
    if (slot < nactive) {
	*ret = build_term_list(p, BUCKET(tb, slot), NULL, 0, tb);
	retval = DB_ERROR_NONE;
    }
    else if (slot == nactive) {
	*ret = am_EOT;
	retval = DB_ERROR_NONE;
    }    
    else {
	retval = DB_ERROR_BADPARAM;		
    }    
    RUNLOCK_HASH(lck);
    return retval;
}


/*
 * Match traversal callbacks
 */


typedef struct traverse_context_t_ traverse_context_t;
struct traverse_context_t_
{
/* Called when no match is possible.
 *      context_ptr: Pointer to context
 *      ret: Pointer to traversal function term return.
 *
 * Both the direct return value and 'ret' are used as the traversal function return values.
 */
    int (*on_nothing_can_match)(traverse_context_t* ctx, Eterm* ret);

/* Called for each match result.
 *      context_ptr: Pointer to context
 *      slot_ix: Current slot index
 *      current_ptr_ptr: Triple pointer to either the bucket or the 'next' pointer in the previous element;
 *                       can be (carefully) used to adjust iteration when deleting or replacing elements.
 *      match_res: The result of running the match program against the current term.
 *
 * Should return 1 for successful match, 0 otherwise.
 */
    int (*on_match_res)(traverse_context_t* ctx, Sint slot_ix,
                        HashDbTerm*** current_ptr_ptr, Eterm match_res);

/* Called when either we've matched enough elements in this cycle or EOT was reached.
 *      context_ptr: Pointer to context
 *      slot_ix: Current slot index
 *      got: How many elements have been matched so far
 *      iterations_left: Number of intended iterations (down from an initial max.) left in this traversal cycle
 *      mpp: Double pointer to the compiled match program
 *      ret: Pointer to traversal function term return.
 *
 * Both the direct return value and 'ret' are used as the traversal function return values.
 * If *mpp is set to NULL, it won't be deallocated (useful for trapping.)
 */
    int (*on_loop_ended)(traverse_context_t* ctx, Sint slot_ix, Sint got,
                         Sint iterations_left, Binary** mpp, Eterm* ret);

/* Called when it's time to trap
 *      context_ptr: Pointer to context
 *      slot_ix: Current slot index
 *      got: How many elements have been matched so far
 *      mpp: Double pointer to the compiled match program
 *      ret: Pointer to traversal function term return.
 *
 * Both the direct return value and 'ret' are used as the traversal function return values.
 * If *mpp is set to NULL, it won't be deallocated (useful for trapping.)
 */
    int (*on_trap)(traverse_context_t* ctx, Sint slot_ix, Sint got, Binary** mpp,
                   Eterm* ret);

    Process* p;
    DbTableHash* tb;
    Eterm tid;
    Eterm* prev_continuation_tptr;
    enum DbIterSafety safety;
};


/*
 * Begin hash table match traversal
 */
static int match_traverse(traverse_context_t* ctx,
                          Eterm pattern,
                          extra_match_validator_t extra_match_validator, /* Optional */
                          Sint chunk_size,      /* If 0, no chunking */
                          Sint iterations_left, /* Nr. of iterations left */
                          Eterm** hpp,          /* Heap */
                          int lock_for_write,   /* Set to 1 if we're going to delete or
                                                   modify existing terms */
                          Eterm* ret)
{
    DbTableHash* tb = ctx->tb;
    Sint slot_ix;                  /* Slot index */
    HashDbTerm** current_ptr;      /* Refers to either the bucket pointer or
                                    * the 'next' pointer in the previous term
                                    */
    HashDbTerm* saved_current;     /* Helper to avoid double skip on match */
    struct mp_info mpi;
    unsigned current_list_pos = 0; /* Prefound buckets list index */
    Eterm match_res;
    Sint got = 0;                  /* Matched terms counter */
    erts_rwmtx_t* lck;         /* Slot lock */
    int ret_value;
    erts_rwmtx_t* (*lock_hash_function)(DbTableHash*, HashValue)
        = (lock_for_write ? WLOCK_HASH : RLOCK_HASH);
    void (*unlock_hash_function)(erts_rwmtx_t*)
        = (lock_for_write ? WUNLOCK_HASH : RUNLOCK_HASH);
    Sint (*next_slot_function)(DbTableHash*, Uint, erts_rwmtx_t**)
        = (lock_for_write ? next_slot_w : next_slot);

    if ((ret_value = analyze_pattern(tb, pattern, extra_match_validator, &mpi))
            != DB_ERROR_NONE)
    {
        *ret = NIL;
        goto done;
    }

    if (!mpi.something_can_match) {
        /* Can't possibly match anything */
        ret_value = ctx->on_nothing_can_match(ctx, ret);
        goto done;
    }

    /*
     * Look for initial slot / bucket
     */
    if (!mpi.key_given) {
        /* Run this code if pattern is variable or GETKEY(pattern)  */
        /* is a variable                                            */
        slot_ix = 0;
        lck = lock_hash_function(tb,slot_ix);
        for (;;) {
            ASSERT(slot_ix < NACTIVE(tb));
            if (*(current_ptr = &BUCKET(tb,slot_ix)) != NULL) {
                break;
            }
            slot_ix = next_slot_function(tb,slot_ix,&lck);
            if (slot_ix == 0) {
                ret_value = ctx->on_loop_ended(ctx, slot_ix, got, iterations_left,
                                               &mpi.mp, ret);
                goto done;
            }
        }
    } else {
        /* We have at least one */
        slot_ix = mpi.lists[current_list_pos].ix;
        lck = lock_hash_function(tb, slot_ix);
        current_ptr = mpi.lists[current_list_pos].bucket;
        ASSERT(*current_ptr == BUCKET(tb,slot_ix));
        ++current_list_pos;
    }

    /*
     * Execute traversal cycle
     */
    for(;;) {
        if (*current_ptr != NULL) {
            if (!is_pseudo_deleted(*current_ptr)) {
                match_res = db_match_dbterm(&tb->common, ctx->p, mpi.mp,
                                            &(*current_ptr)->dbterm, hpp, 2);
                saved_current = *current_ptr;
                if (ctx->on_match_res(ctx, slot_ix, &current_ptr, match_res)) {
                    ++got;
                }
                --iterations_left;
                if (*current_ptr != saved_current) {
                    /* Don't advance to next, the callback did it already */
                    continue;
                }
            }
            current_ptr = &((*current_ptr)->next);
        }
        else if (mpi.key_given) {  /* Key is bound */
            unlock_hash_function(lck);
            if (current_list_pos == mpi.num_lists) {
                ret_value = ctx->on_loop_ended(ctx, -1, got, iterations_left, &mpi.mp, ret);
                goto done;
            } else {
                slot_ix = mpi.lists[current_list_pos].ix;
                lck = lock_hash_function(tb, slot_ix);
                current_ptr = mpi.lists[current_list_pos].bucket;
                ASSERT(mpi.lists[current_list_pos].bucket == &BUCKET(tb,slot_ix));
                ++current_list_pos;
            }
        }
        else { /* Key is variable */
            if ((slot_ix = next_slot_function(tb,slot_ix,&lck)) == 0) {
                slot_ix = -1;
                break;
            }
            if (chunk_size && got >= chunk_size) {
                unlock_hash_function(lck);
                break;
            }
            if (iterations_left <= 0) {
                unlock_hash_function(lck);
                ret_value = ctx->on_trap(ctx, slot_ix, got, &mpi.mp, ret);
                goto done;
            }
            current_ptr = &BUCKET(tb,slot_ix);
        }
    }

    ret_value = ctx->on_loop_ended(ctx, slot_ix, got, iterations_left, &mpi.mp, ret);

done:
    /* We should only jump directly to this label if
     * we've already called ctx->nothing_can_match / loop_ended / trap
     */
    if (mpi.mp != NULL) {
        erts_bin_free(mpi.mp);
    }
    if (mpi.lists != mpi.dlists) {
        erts_free(ERTS_ALC_T_DB_SEL_LIST,
                (void *) mpi.lists);
    }
    return ret_value;

}

/*
 * Continue hash table match traversal
 */
static int match_traverse_continue(traverse_context_t* ctx,
                                   Sint chunk_size,      /* If 0, no chunking */
                                   Sint iterations_left, /* Nr. of iterations left */
                                   Eterm** hpp,          /* Heap */
                                   Sint slot_ix,         /* Slot index to resume traversal from */
                                   Sint got,             /* Matched terms counter */
                                   Binary** mpp,         /* Existing match program */
                                   int lock_for_write,   /* Set to 1 if we're going to delete or
                                                            modify existing terms */
                                   Eterm* ret)
{
    DbTableHash* tb = ctx->tb;
    HashDbTerm** current_ptr;  /* Refers to either the bucket pointer or
                                       * the 'next' pointer in the previous term
                                       */
    HashDbTerm* saved_current; /* Helper to avoid double skip on match */
    Eterm match_res;
    erts_rwmtx_t* lck;
    int ret_value;
    erts_rwmtx_t* (*lock_hash_function)(DbTableHash*, HashValue)
        = (lock_for_write ? WLOCK_HASH : RLOCK_HASH);
    void (*unlock_hash_function)(erts_rwmtx_t*)
        = (lock_for_write ? WUNLOCK_HASH : RUNLOCK_HASH);
    Sint (*next_slot_function)(DbTableHash* tb, Uint ix, erts_rwmtx_t** lck_ptr)
        = (lock_for_write ? next_slot_w : next_slot);

    if (got < 0) {
        *ret = NIL;
        return DB_ERROR_BADPARAM;
    }

    if (slot_ix < 0 /* EOT */
       || (chunk_size && got >= chunk_size))
    {
        /* Already got all or enough in the match_list */
        ret_value = ctx->on_loop_ended(ctx, slot_ix, got, iterations_left, mpp, ret);
        goto done;
    }

    lck = lock_hash_function(tb, slot_ix);
    if (slot_ix >= NACTIVE(tb)) { /* Is this possible? */
        unlock_hash_function(lck);
        *ret = NIL;
        ret_value = DB_ERROR_BADPARAM;
        goto done;
    }

    /*
     * Resume traversal cycle from where we left
     */
    current_ptr = &BUCKET(tb,slot_ix);
    for(;;) {
        if (*current_ptr != NULL) {
            if (!is_pseudo_deleted(*current_ptr)) {
                match_res = db_match_dbterm(&tb->common, ctx->p, *mpp,
                                            &(*current_ptr)->dbterm, hpp, 2);
                saved_current = *current_ptr;
                if (ctx->on_match_res(ctx, slot_ix, &current_ptr, match_res)) {
                    ++got;
                }
                --iterations_left;
                if (*current_ptr != saved_current) {
                    /* Don't advance to next, the callback did it already */
                    continue;
                }
            }
            current_ptr = &((*current_ptr)->next);
        }
        else {
            if ((slot_ix=next_slot_function(tb,slot_ix,&lck)) == 0) {
                slot_ix = -1;
                break;
            }
            if (chunk_size && got >= chunk_size) {
                unlock_hash_function(lck);
                break;
            }
            if (iterations_left <= 0) {
                unlock_hash_function(lck);
                ret_value = ctx->on_trap(ctx, slot_ix, got, mpp, ret);
                goto done;
            }
            current_ptr = &BUCKET(tb,slot_ix);
        }
    }

    ret_value = ctx->on_loop_ended(ctx, slot_ix, got, iterations_left, mpp, ret);

done:
    /* We should only jump directly to this label if
     * we've already called on_loop_ended / on_trap
     */
    return ret_value;

}


/*
 * Common traversal trapping/continuation code;
 * used by select_count, select_delete and select_replace,
 * as well as their continuation-handling counterparts.
 */

static ERTS_INLINE int on_simple_trap(Export* trap_function,
                                      traverse_context_t* ctx,
                                      Sint slot_ix,
                                      Sint got,
                                      Binary** mpp,
                                      Eterm* ret)
{
    Eterm* hp;
    Eterm egot;
    Eterm mpb;
    Eterm continuation;
    int is_first_trap = (ctx->prev_continuation_tptr == NULL);
    size_t base_halloc_sz = (is_first_trap ? ERTS_MAGIC_REF_THING_SIZE : 0);

    BUMP_ALL_REDS(ctx->p);
    if (IS_USMALL(0, got)) {
	hp = HAllocX(ctx->p,  base_halloc_sz + 6, ERTS_MAGIC_REF_THING_SIZE);
	egot = make_small(got);
    }
    else {
	hp = HAllocX(ctx->p, base_halloc_sz + BIG_UINT_HEAP_SIZE + 6,
                     ERTS_MAGIC_REF_THING_SIZE);
	egot = uint_to_big(got, hp);
	hp += BIG_UINT_HEAP_SIZE;
    }

    if (is_first_trap) {
        if (is_atom(ctx->tid))
            ctx->tid = erts_db_make_tid(ctx->p, &ctx->tb->common);
        mpb = erts_db_make_match_prog_ref(ctx->p, *mpp, &hp);
        *mpp = NULL; /* otherwise the caller will destroy it */
    }
    else {
        ASSERT(!is_atom(ctx->tid));
        mpb = ctx->prev_continuation_tptr[3];
    }

    continuation = TUPLE5(
            hp,
            ctx->tid,
            make_small(slot_ix),
            mpb,
            egot,
            make_small(ctx->safety));
    ERTS_BIF_PREP_TRAP1(*ret, trap_function, ctx->p, continuation);
    return DB_ERROR_NONE;
}

static ERTS_INLINE int unpack_simple_continuation(Eterm continuation,
                                                  Eterm** tptr_ptr,
                                                  Eterm* tid_ptr,
                                                  Sint* slot_ix_p,
                                                  Binary** mpp,
                                                  Sint* got_p,
                                                  enum DbIterSafety* safety_p)
{
    Eterm* tptr;
    ASSERT(is_tuple(continuation));
    tptr = tuple_val(continuation);
    if (*tptr != make_arityval(5))
        return 1;

    if (!is_small(tptr[2]) || !(is_big(tptr[4]) || is_small(tptr[4]))
        || !is_small(tptr[5]))
        return 1;

    *tptr_ptr = tptr;
    *tid_ptr = tptr[1];
    *slot_ix_p = unsigned_val(tptr[2]);
    *mpp = erts_db_get_match_prog_binary_unchecked(tptr[3]);
    if (is_big(tptr[4])) {
        *got_p = big_to_uint32(tptr[4]);
    }
    else {
        *got_p = unsigned_val(tptr[4]);
    }
    *safety_p = signed_val(tptr[5]);
    return 0;
}


/*
 *
 * select / select_chunk match traversal
 *
 */

#define MAX_SELECT_CHUNK_ITERATIONS 1000

typedef struct {
    traverse_context_t base;
    Eterm* hp;
    Sint chunk_size;
    Eterm match_list;
} select_chunk_context_t;

static int select_chunk_on_nothing_can_match(traverse_context_t* ctx_base, Eterm* ret)
{
    select_chunk_context_t* ctx = (select_chunk_context_t*) ctx_base;
    *ret = (ctx->chunk_size > 0 ? am_EOT : NIL);
    return DB_ERROR_NONE;
}

static int select_chunk_on_match_res(traverse_context_t* ctx_base, Sint slot_ix,
                                     HashDbTerm*** current_ptr_ptr,
                                     Eterm match_res)
{
    select_chunk_context_t* ctx = (select_chunk_context_t*) ctx_base;
    if (is_value(match_res)) {
        ctx->match_list = CONS(ctx->hp, match_res, ctx->match_list);
        return 1;
    }
    return 0;
}

static int select_chunk_on_loop_ended(traverse_context_t* ctx_base,
                                      Sint slot_ix, Sint got,
                                      Sint iterations_left, Binary** mpp,
                                      Eterm* ret)
{
    select_chunk_context_t* ctx = (select_chunk_context_t*) ctx_base;
    Eterm mpb;

    if (iterations_left == MAX_SELECT_CHUNK_ITERATIONS) {
        /* We didn't get to iterate a single time, which means EOT */
        ASSERT(ctx->match_list == NIL);
        *ret = (ctx->chunk_size > 0 ? am_EOT : NIL);
        return DB_ERROR_NONE;
    }
    else {
        ASSERT(iterations_left < MAX_SELECT_CHUNK_ITERATIONS);
        BUMP_REDS(ctx->base.p, MAX_SELECT_CHUNK_ITERATIONS - iterations_left);
        if (ctx->chunk_size) {
            Eterm continuation;
            Eterm rest = NIL;
            Sint rest_size = 0;

            if (got > ctx->chunk_size) { /* Split list in return value and 'rest' */
                Eterm tmp = ctx->match_list;
                rest = ctx->match_list;
                while (got-- > ctx->chunk_size + 1) {
                    tmp = CDR(list_val(tmp));
                    ++rest_size;
                }
                ++rest_size;
                ctx->match_list = CDR(list_val(tmp));
                CDR(list_val(tmp)) = NIL; /* Destructive, the list has never
                                             been in 'user space' */
            }
            if (rest != NIL || slot_ix >= 0) { /* Need more calls */
                Eterm tid = ctx->base.tid;
                ctx->hp = HAllocX(ctx->base.p,
                                  3 + 7 + ERTS_MAGIC_REF_THING_SIZE,
                                  ERTS_MAGIC_REF_THING_SIZE);
                mpb = erts_db_make_match_prog_ref(ctx->base.p, *mpp, &ctx->hp);
                if (is_atom(tid))
                    tid = erts_db_make_tid(ctx->base.p,
                                           &ctx->base.tb->common);
                continuation = TUPLE6(
                        ctx->hp,
                        tid,
                        make_small(slot_ix),
                        make_small(ctx->chunk_size),
                        mpb, rest,
                        make_small(rest_size));
                *mpp = NULL; /* Otherwise the caller will destroy it */
                ctx->hp += 7;
                *ret = TUPLE2(ctx->hp, ctx->match_list, continuation);
                return DB_ERROR_NONE;
            } else { /* All data is exhausted */
                if (ctx->match_list != NIL) { /* No more data to search but still a
                                                            result to return to the caller */
                    ctx->hp = HAlloc(ctx->base.p, 3);
                    *ret = TUPLE2(ctx->hp, ctx->match_list, am_EOT);
                    return DB_ERROR_NONE;
                } else { /* Reached the end of the ttable with no data to return */
                    *ret = am_EOT;
                    return DB_ERROR_NONE;
                }
            }
        }
        *ret = ctx->match_list;
        return DB_ERROR_NONE;
    }
}

static int select_chunk_on_trap(traverse_context_t* ctx_base,
                                Sint slot_ix, Sint got,
                                Binary** mpp, Eterm* ret)
{
    select_chunk_context_t* ctx = (select_chunk_context_t*) ctx_base;
    Eterm mpb;
    Eterm continuation;
    Eterm* hp;

    BUMP_ALL_REDS(ctx->base.p);

    if (ctx->base.prev_continuation_tptr == NULL) {
        Eterm tid = ctx->base.tid;
        /* First time we're trapping */
        hp = HAllocX(ctx->base.p, 8 + ERTS_MAGIC_REF_THING_SIZE,
                     ERTS_MAGIC_REF_THING_SIZE);
        if (is_atom(tid))
            tid = erts_db_make_tid(ctx->base.p, &ctx->base.tb->common);
        mpb = erts_db_make_match_prog_ref(ctx->base.p, *mpp, &hp);
        continuation = TUPLE7(
                hp,
                tid,
                make_small(slot_ix),
                make_small(ctx->chunk_size),
                mpb,
                ctx->match_list,
                make_small(got),
                make_small(ctx->base.safety));
        *mpp = NULL; /* otherwise the caller will destroy it */
    }
    else {
        /* Not the first time we're trapping; reuse continuation terms */
        hp = HAlloc(ctx->base.p, 8);
        continuation = TUPLE7(
                hp,
                ctx->base.prev_continuation_tptr[1],
                make_small(slot_ix),
                ctx->base.prev_continuation_tptr[3],
                ctx->base.prev_continuation_tptr[4],
                ctx->match_list,
                make_small(got),
                make_small(ctx->base.safety));
    }
    ERTS_BIF_PREP_TRAP1(*ret, &ets_select_continue_exp, ctx->base.p,
                        continuation);
    return DB_ERROR_NONE;
}

static int db_select_hash(Process *p, DbTable *tbl, Eterm tid, Eterm pattern,
                          int reverse, Eterm *ret, enum DbIterSafety safety)
{
    return db_select_chunk_hash(p, tbl, tid, pattern, 0, reverse, ret, safety);
}

static int db_select_chunk_hash(Process *p, DbTable *tbl, Eterm tid,
                                Eterm pattern, Sint chunk_size,
                                int reverse, Eterm *ret, enum DbIterSafety safety)
{
    select_chunk_context_t ctx;

    ctx.base.on_nothing_can_match = select_chunk_on_nothing_can_match;
    ctx.base.on_match_res         = select_chunk_on_match_res;
    ctx.base.on_loop_ended        = select_chunk_on_loop_ended;
    ctx.base.on_trap              = select_chunk_on_trap;
    ctx.base.p = p;
    ctx.base.tb = &tbl->hash;
    ctx.base.tid = tid;
    ctx.base.prev_continuation_tptr = NULL;
    ctx.base.safety = safety;
    ctx.hp = NULL;
    ctx.chunk_size = chunk_size;
    ctx.match_list = NIL;

    return match_traverse(
            &ctx.base,
            pattern, NULL,
            ctx.chunk_size,
            MAX_SELECT_CHUNK_ITERATIONS,
            &ctx.hp, 0,
            ret);
}

/*
 *
 * select_continue match traversal
 *
 */

static
int select_chunk_continue_on_loop_ended(traverse_context_t* ctx_base,
                                        Sint slot_ix, Sint got,
                                        Sint iterations_left, Binary** mpp,
                                        Eterm* ret)
{
    select_chunk_context_t* ctx = (select_chunk_context_t*) ctx_base;
    Eterm continuation;
    Eterm rest = NIL;
    Eterm* hp;

    ASSERT(iterations_left <= MAX_SELECT_CHUNK_ITERATIONS);
    BUMP_REDS(ctx->base.p, MAX_SELECT_CHUNK_ITERATIONS - iterations_left);
    if (ctx->chunk_size) {
        Sint rest_size = 0;
        if (got > ctx->chunk_size) {
            /* Cannot write destructively here,
               the list may have
               been in user space */
            hp = HAlloc(ctx->base.p, (got - ctx->chunk_size) * 2);
            while (got-- > ctx->chunk_size) {
                rest = CONS(hp, CAR(list_val(ctx->match_list)), rest);
                hp += 2;
                ctx->match_list = CDR(list_val(ctx->match_list));
                ++rest_size;
            }
        }
        if (rest != NIL || slot_ix >= 0) {
            hp = HAlloc(ctx->base.p, 3 + 7);
            continuation = TUPLE6(
                    hp,
                    ctx->base.prev_continuation_tptr[1],
                    make_small(slot_ix),
                    ctx->base.prev_continuation_tptr[3],
                    ctx->base.prev_continuation_tptr[4],
                    rest,
                    make_small(rest_size));
            hp += 7;
            *ret = TUPLE2(hp, ctx->match_list, continuation);
            return DB_ERROR_NONE;
        } else {
            if (ctx->match_list != NIL) {
                hp = HAlloc(ctx->base.p, 3);
                *ret = TUPLE2(hp, ctx->match_list, am_EOT);
                return DB_ERROR_NONE;
            } else {
                *ret = am_EOT;
                return DB_ERROR_NONE;
            }
        }
    }
    *ret = ctx->match_list;
    return DB_ERROR_NONE;
}

/*
 * This is called when ets:select/1/2/3 traps
 * and for ets:select/1 with user continuation term.
 */
static int db_select_continue_hash(Process* p, DbTable* tbl, Eterm continuation,
                                   Eterm* ret, enum DbIterSafety* safety_p)
{
    select_chunk_context_t ctx;
    Eterm* tptr;
    Eterm tid;
    Binary* mp;
    Sint got;
    Sint slot_ix;
    Sint chunk_size;
    Eterm match_list;
    Sint iterations_left = MAX_SELECT_CHUNK_ITERATIONS;

    /* Decode continuation. We know it's a tuple but not the arity or anything else */
    ASSERT(is_tuple(continuation));
    tptr = tuple_val(continuation);

    /*
     * 6-tuple is select/1 user continuation term
     * 7-tuple is select trap continuation
     */
    if (*tptr == make_arityval(7) && is_small(tptr[7]))
        *safety_p = signed_val(tptr[7]);
    else if (*tptr != make_arityval(6))
        goto badparam;

    if (!is_small(tptr[2]) || !is_small(tptr[3]) ||
            !(is_list(tptr[5]) || tptr[5] == NIL) || !is_small(tptr[6]))
        goto badparam;
    if ((chunk_size = signed_val(tptr[3])) < 0)
        goto badparam;

    mp = erts_db_get_match_prog_binary(tptr[4]);
    if (mp == NULL)
        goto badparam;

    if ((got = signed_val(tptr[6])) < 0)
        goto badparam;

    tid = tptr[1];
    slot_ix = signed_val(tptr[2]);
    match_list = tptr[5];

    /* Proceed */
    ctx.base.on_match_res  = select_chunk_on_match_res;
    ctx.base.on_loop_ended = select_chunk_continue_on_loop_ended;
    ctx.base.on_trap       = select_chunk_on_trap;
    ctx.base.p = p;
    ctx.base.tb = &tbl->hash;
    ctx.base.tid = tid;
    ctx.base.prev_continuation_tptr = tptr;
    ctx.base.safety = *safety_p;
    ctx.hp = NULL;
    ctx.chunk_size = chunk_size;
    ctx.match_list = match_list;

    return match_traverse_continue(
        &ctx.base, ctx.chunk_size,
        iterations_left, &ctx.hp, slot_ix, got, &mp, 0,
        ret);

badparam:
    *ret = NIL;
    return DB_ERROR_BADPARAM;
}

#undef MAX_SELECT_CHUNK_ITERATIONS


/*
 *
 * select_count match traversal
 *
 */

#define MAX_SELECT_COUNT_ITERATIONS 1000

static int select_count_on_nothing_can_match(traverse_context_t* ctx_base,
                                             Eterm* ret)
{
    *ret = make_small(0);
    return DB_ERROR_NONE;
}

static int select_count_on_match_res(traverse_context_t* ctx_base, Sint slot_ix,
                                     HashDbTerm*** current_ptr_ptr,
                                     Eterm match_res)
{
    return (match_res == am_true);
}

static int select_count_on_loop_ended(traverse_context_t* ctx,
                                      Sint slot_ix, Sint got,
                                      Sint iterations_left, Binary** mpp,
                                      Eterm* ret)
{
    ASSERT(iterations_left <= MAX_SELECT_COUNT_ITERATIONS);
    BUMP_REDS(ctx->p, MAX_SELECT_COUNT_ITERATIONS - iterations_left);
    *ret = erts_make_integer(got, ctx->p);
    return DB_ERROR_NONE;
}

static int select_count_on_trap(traverse_context_t* ctx,
                                Sint slot_ix, Sint got,
                                Binary** mpp, Eterm* ret)
{
    return on_simple_trap(
            &ets_select_count_continue_exp, ctx,
            slot_ix, got, mpp, ret);
}

static int db_select_count_hash(Process *p, DbTable *tbl, Eterm tid,
                                Eterm pattern, Eterm *ret,
                                enum DbIterSafety safety)
{
    traverse_context_t ctx;
    Sint iterations_left = MAX_SELECT_COUNT_ITERATIONS;
    Sint chunk_size = 0;

    ctx.on_nothing_can_match = select_count_on_nothing_can_match;
    ctx.on_match_res         = select_count_on_match_res;
    ctx.on_loop_ended        = select_count_on_loop_ended;
    ctx.on_trap              = select_count_on_trap;
    ctx.p = p;
    ctx.tb = &tbl->hash;
    ctx.tid = tid;
    ctx.prev_continuation_tptr = NULL;
    ctx.safety = safety;

    return match_traverse(
            &ctx,
            pattern, NULL,
            chunk_size, iterations_left, NULL, 0,
            ret);
}

/*
 * This is called when select_count traps
 */
static int db_select_count_continue_hash(Process* p, DbTable* tbl,
                                         Eterm continuation, Eterm* ret,
                                         enum DbIterSafety* safety_p)
{
    traverse_context_t ctx;
    Eterm* tptr;
    Eterm tid;
    Binary* mp;
    Sint got;
    Sint slot_ix;
    Sint chunk_size = 0;
    *ret = NIL;

    if (unpack_simple_continuation(continuation, &tptr, &tid, &slot_ix, &mp,
                                   &got, safety_p)) {
        *ret = NIL;
        return DB_ERROR_BADPARAM;
    }

    ctx.on_match_res  = select_count_on_match_res;
    ctx.on_loop_ended = select_count_on_loop_ended;
    ctx.on_trap       = select_count_on_trap;
    ctx.p = p;
    ctx.tb = &tbl->hash;
    ctx.tid = tid;
    ctx.prev_continuation_tptr = tptr;
    ctx.safety = *safety_p;

    return match_traverse_continue(
            &ctx, chunk_size,
            MAX_SELECT_COUNT_ITERATIONS,
            NULL, slot_ix, got, &mp, 0,
            ret);
}

#undef MAX_SELECT_COUNT_ITERATIONS


/*
 *
 * select_delete match traversal
 *
 */

#define MAX_SELECT_DELETE_ITERATIONS 1000

typedef struct {
    traverse_context_t base;
    erts_aint_t fixated_by_me;
    Uint last_pseudo_delete;
    HashDbTerm* free_us;
} select_delete_context_t;

static int select_delete_on_nothing_can_match(traverse_context_t* ctx_base,
                                              Eterm* ret)
{
    *ret = make_small(0);
    return DB_ERROR_NONE;
}

static int select_delete_on_match_res(traverse_context_t* ctx_base, Sint slot_ix,
                                      HashDbTerm*** current_ptr_ptr,
                                      Eterm match_res)
{
    HashDbTerm** current_ptr = *current_ptr_ptr;
    select_delete_context_t* ctx = (select_delete_context_t*) ctx_base;
    HashDbTerm* del;
    DbTableHashLockAndCounter* lck_ctr;
    Uint32 hval;
    if (match_res != am_true)
        return 0;
    hval = (*current_ptr)->hvalue;
    if (NFIXED(ctx->base.tb) > ctx->fixated_by_me) { /* fixated by others? */
        if (slot_ix != ctx->last_pseudo_delete) {
            if (!add_fixed_deletion(ctx->base.tb, slot_ix, ctx->fixated_by_me))
                goto do_erase;
            ctx->last_pseudo_delete = slot_ix;
        }
        (*current_ptr)->pseudo_deleted = 1;
    }
    else {
    do_erase:
        del = *current_ptr;
        *current_ptr = (*current_ptr)->next; // replace pointer to term using next
        del->next = ctx->free_us;
        ctx->free_us = del;
    }
    lck_ctr = GET_LOCK_AND_CTR(ctx->base.tb,slot_ix);
    DEC_NITEMS(ctx->base.tb, lck_ctr, hval);

    return 1;
}

/* This function is only safe to call while the table lock is held in
   write mode */
static Sint get_nitems_from_locks_or_counter(DbTableHash* tb)
{
    if (IS_DECENTRALIZED_CTRS(tb)) {
        int i;
        Sint total = 0;
        for (i=0; i < DB_HASH_LOCK_CNT; ++i) {
            total += tb->locks->lck_vec[i].lck_ctr.nitems;
        }
        return total;
    } else {
        return erts_flxctr_read_centralized(&tb->common.counters,
                                            ERTS_DB_TABLE_NITEMS_COUNTER_ID);
    }
}

static int select_delete_on_loop_ended(traverse_context_t* ctx_base,
                                       Sint slot_ix, Sint got,
                                       Sint iterations_left, Binary** mpp,
                                       Eterm* ret)
{
    select_delete_context_t* ctx = (select_delete_context_t*) ctx_base;
    DbTableHash* tb = ctx->base.tb;
    free_term_list(tb, ctx->free_us);
    ctx->free_us = NULL;
    ASSERT(iterations_left <= MAX_SELECT_DELETE_ITERATIONS);
    BUMP_REDS(ctx->base.p, MAX_SELECT_DELETE_ITERATIONS - iterations_left);
    if (got) {
        Sint nitems;
        if (IS_DECENTRALIZED_CTRS(tb)) {
            /* Get a random hash value so we can get an nitems
               estimate from a random lock */
            HashValue hval =
                (HashValue)&ctx +
                (HashValue)iterations_left +
                (HashValue)erts_get_scheduler_data()->reductions;
            erts_rwmtx_t* lck = RLOCK_HASH(tb, hval);
            DbTableHashLockAndCounter* lck_ctr = GET_LOCK_AND_CTR(tb, hval);
            nitems = NITEMS_ESTIMATE(tb, lck_ctr, hval);
            RUNLOCK_HASH(lck);
        } else {
            nitems = erts_flxctr_read_centralized(&tb->common.counters,
                                                  ERTS_DB_TABLE_NITEMS_COUNTER_ID);
        }
	try_shrink(tb, nitems);
    }
    *ret = erts_make_integer(got, ctx->base.p);
    return DB_ERROR_NONE;
}

static int select_delete_on_trap(traverse_context_t* ctx_base,
                                 Sint slot_ix, Sint got,
                                 Binary** mpp, Eterm* ret)
{
    select_delete_context_t* ctx = (select_delete_context_t*) ctx_base;
    free_term_list(ctx->base.tb, ctx->free_us);
    ctx->free_us = NULL;
    return on_simple_trap(
            &ets_select_delete_continue_exp, &ctx->base,
            slot_ix, got, mpp, ret);
}

static int db_select_delete_hash(Process *p, DbTable *tbl, Eterm tid,
                                 Eterm pattern, Eterm *ret,
                                 enum DbIterSafety safety)
{
    select_delete_context_t ctx;
    Sint chunk_size = 0;

    ctx.base.on_nothing_can_match = select_delete_on_nothing_can_match;
    ctx.base.on_match_res         = select_delete_on_match_res;
    ctx.base.on_loop_ended        = select_delete_on_loop_ended;
    ctx.base.on_trap              = select_delete_on_trap;
    ctx.base.p = p;
    ctx.base.tb = &tbl->hash;
    ctx.base.tid = tid;
    ctx.base.prev_continuation_tptr = NULL;
    ctx.base.safety = safety;
    ctx.fixated_by_me = ctx.base.tb->common.is_thread_safe ? 0 : 1;
    ctx.last_pseudo_delete = (Uint) -1;
    ctx.free_us = NULL;

    return match_traverse(
            &ctx.base,
            pattern, NULL,
            chunk_size,
            MAX_SELECT_DELETE_ITERATIONS, NULL, 1,
            ret);
}

/*
 * This is called when select_delete traps
 */
static int db_select_delete_continue_hash(Process* p, DbTable* tbl,
                                          Eterm continuation, Eterm* ret,
                                          enum DbIterSafety* safety_p)
{
    select_delete_context_t ctx;
    Eterm* tptr;
    Eterm tid;
    Binary* mp;
    Sint got;
    Sint slot_ix;
    Sint chunk_size = 0;

    if (unpack_simple_continuation(continuation, &tptr, &tid, &slot_ix, &mp,
                                   &got, safety_p)) {
        *ret = NIL;
        return DB_ERROR_BADPARAM;
    }

    ctx.base.on_match_res  = select_delete_on_match_res;
    ctx.base.on_loop_ended = select_delete_on_loop_ended;
    ctx.base.on_trap       = select_delete_on_trap;
    ctx.base.p = p;
    ctx.base.tb = &tbl->hash;
    ctx.base.tid = tid;
    ctx.base.prev_continuation_tptr = tptr;
    ctx.base.safety = *safety_p;
    ctx.fixated_by_me = ONLY_WRITER(p, ctx.base.tb) ? 0 : 1;
    ctx.last_pseudo_delete = (Uint) -1;
    ctx.free_us = NULL;

    return match_traverse_continue(
            &ctx.base, chunk_size,
            MAX_SELECT_DELETE_ITERATIONS,
            NULL, slot_ix, got, &mp, 1,
            ret);
}

#undef MAX_SELECT_DELETE_ITERATIONS


/*
 *
 * select_replace match traversal
 *
 */

#define MAX_SELECT_REPLACE_ITERATIONS 1000

static int select_replace_on_nothing_can_match(traverse_context_t* ctx_base,
                                               Eterm* ret)
{
    *ret = make_small(0);
    return DB_ERROR_NONE;
}

static int select_replace_on_match_res(traverse_context_t* ctx, Sint slot_ix,
                                       HashDbTerm*** current_ptr_ptr,
                                       Eterm match_res)
{
    DbTableHash* tb = ctx->tb;
    HashDbTerm* new;
    HashDbTerm* next;
    HashValue hval;

    if (is_value(match_res)) {
#ifdef DEBUG
        Eterm key = db_getkey(tb->common.keypos, match_res);
        ASSERT(is_value(key));
        ASSERT(eq(key, GETKEY(tb, (**current_ptr_ptr)->dbterm.tpl)));
#endif
        next = (**current_ptr_ptr)->next;
        hval = (**current_ptr_ptr)->hvalue;
        new = new_dbterm(tb, match_res);
        new->next = next;
        new->hvalue = hval;
        new->pseudo_deleted = 0;
        free_term(tb, **current_ptr_ptr);
        **current_ptr_ptr = new; /* replace 'next' pointer in previous object */
        *current_ptr_ptr = &((**current_ptr_ptr)->next); /* advance to next object */
        return 1;
    }
    return 0;
}

static int select_replace_on_loop_ended(traverse_context_t* ctx, Sint slot_ix,
                                        Sint got, Sint iterations_left,
                                        Binary** mpp, Eterm* ret)
{
    ASSERT(iterations_left <= MAX_SELECT_REPLACE_ITERATIONS);
    /* the more objects we've replaced, the more reductions we've consumed */
    BUMP_REDS(ctx->p,
              MIN(MAX_SELECT_REPLACE_ITERATIONS * 2,
                  (MAX_SELECT_REPLACE_ITERATIONS - iterations_left) + (int)got));
    *ret = erts_make_integer(got, ctx->p);
    return DB_ERROR_NONE;
}

static int select_replace_on_trap(traverse_context_t* ctx,
                                  Sint slot_ix, Sint got,
                                  Binary** mpp, Eterm* ret)
{
    return on_simple_trap(
            &ets_select_replace_continue_exp, ctx,
            slot_ix, got, mpp, ret);
}

static int db_select_replace_hash(Process *p, DbTable *tbl, Eterm tid,
                                  Eterm pattern, Eterm *ret,
                                  enum DbIterSafety safety)
{
    traverse_context_t ctx;
    Sint chunk_size = 0;

    /* Bag implementation presented both semantic consistency and performance issues,
     * unsupported for now
     */
    ASSERT(!(tbl->hash.common.status & DB_BAG));

    ctx.on_nothing_can_match = select_replace_on_nothing_can_match;
    ctx.on_match_res         = select_replace_on_match_res;
    ctx.on_loop_ended        = select_replace_on_loop_ended;
    ctx.on_trap              = select_replace_on_trap;
    ctx.p = p;
    ctx.tb = &tbl->hash;
    ctx.tid = tid;
    ctx.prev_continuation_tptr = NULL;
    ctx.safety = safety;

    return match_traverse(
            &ctx,
            pattern, db_match_keeps_key,
            chunk_size,
            MAX_SELECT_REPLACE_ITERATIONS, NULL, 1,
            ret);
}

/*
 * This is called when select_replace traps
 */
static int db_select_replace_continue_hash(Process* p, DbTable* tbl,
                                           Eterm continuation, Eterm* ret,
                                           enum DbIterSafety* safety_p)
{
    traverse_context_t ctx;
    Eterm* tptr;
    Eterm tid ;
    Binary* mp;
    Sint got;
    Sint slot_ix;
    Sint chunk_size = 0;
    *ret = NIL;

    if (unpack_simple_continuation(continuation, &tptr, &tid, &slot_ix, &mp,
                                   &got, safety_p)) {
        *ret = NIL;
        return DB_ERROR_BADPARAM;
    }

    /* Proceed */
    ctx.on_match_res  = select_replace_on_match_res;
    ctx.on_loop_ended = select_replace_on_loop_ended;
    ctx.on_trap       = select_replace_on_trap;
    ctx.p = p;
    ctx.tb = &tbl->hash;
    ctx.tid = tid;
    ctx.prev_continuation_tptr = tptr;
    ctx.safety = *safety_p;

    return match_traverse_continue(
            &ctx, chunk_size,
            MAX_SELECT_REPLACE_ITERATIONS,
            NULL, slot_ix, got, &mp, 1,
            ret);
}


static int db_take_hash(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashDbTerm **bp, *b;
    HashDbTerm *free_us = NULL;
    HashValue hval = MAKE_HASH(key);
    DbTableHashLockAndCounter *lck_ctr = WLOCK_HASH_GET_LCK_AND_CTR(tb, hval);
    int ix = hash_to_ix(tb, hval);
    int nitems_diff = 0;
    Sint nitems;

    *ret = NIL;
    for (bp = &BUCKET(tb, ix), b = *bp; b; bp = &b->next, b = b->next) {
        if (has_live_key(tb, b, key, hval)) {
            HashDbTerm *bend;

            *ret = get_term_list(p, tb, key, hval, b, &bend);
            while (b != bend) {
                --nitems_diff;
                if (nitems_diff == -1 && IS_FIXED(tb)
                    && add_fixed_deletion(tb, ix, 0)) {
                    /* Pseudo remove (no need to keep several of same key) */
                    bp = &b->next;
                    b->pseudo_deleted = 1;
                    b = b->next;
                } else {
                    HashDbTerm* next = b->next;
                    b->next = free_us;
                    free_us = b;
                    b = *bp = next;
                }
            }
            break;
        }
    }
    if (nitems_diff) {
        ADD_NITEMS(tb, lck_ctr, hval, nitems_diff);
        nitems = NITEMS_ESTIMATE(tb, lck_ctr, hval);
    }
    WUNLOCK_HASH_LCK_CTR(lck_ctr);
    if (nitems_diff) {
        try_shrink(tb, nitems);
    }
    free_term_list(tb, free_us);
    return DB_ERROR_NONE;
}


/*
** Other interface routines (not directly coupled to one bif)
*/

void db_initialize_hash(void)
{
}


static SWord db_mark_all_deleted_hash(DbTable *tbl, SWord reds)
{
    const int LOOPS_PER_REDUCTION  = 8;
    DbTableHash *tb = &tbl->hash;
    FixedDeletion* fixdel;
    SWord loops = reds * LOOPS_PER_REDUCTION;
    int i;

    ERTS_LC_ASSERT(IS_TAB_WLOCKED(tb));

    fixdel = (FixedDeletion*) erts_atomic_read_nob(&tb->fixdel);
    if (fixdel && fixdel->trap) {
        /* Continue after trap */
        ASSERT(fixdel->all);
        ASSERT(fixdel->slot < NACTIVE(tb));
        i = fixdel->slot;
    }
    else {
        /* First call */
        int ok;
        fixdel = alloc_fixdel(tb);
        ok = link_fixdel(tb, fixdel, 0);
        ASSERT(ok); (void)ok;
        i = 0;
    }

    do {
        HashDbTerm* b;
	for (b = BUCKET(tb,i); b; b = b->next)
            b->pseudo_deleted = 1;
    } while (++i < NACTIVE(tb) && --loops > 0);

    if (i < NACTIVE(tb)) {
         /* Yield */
        fixdel->slot = i;
        fixdel->all = 0;
        fixdel->trap = 1;
        return -1;
    }

    fixdel->slot = NACTIVE(tb) - 1;
    fixdel->all = 1;
    fixdel->trap = 0;
    RESET_NITEMS(tb);
    return loops < 0 ? 0 : loops / LOOPS_PER_REDUCTION;
}


/* Display hash table contents (for dump) */
static void db_print_hash(fmtfn_t to, void *to_arg, int show, DbTable *tbl)
{
    DbTableHash *tb = &tbl->hash;
    DbHashStats stats;
    int i;

    erts_print(to, to_arg, "Buckets: %d\n", NACTIVE(tb));

    i = tbl->common.is_thread_safe;
    /* If crash dumping we set table to thread safe in order to
       avoid taking any locks */
    if (ERTS_IS_CRASH_DUMPING)
        tbl->common.is_thread_safe = 1;

    db_calc_stats_hash(&tbl->hash, &stats);

    tbl->common.is_thread_safe = i;

    erts_print(to, to_arg, "Chain Length Avg: %f\n", stats.avg_chain_len);
    erts_print(to, to_arg, "Chain Length Max: %d\n", stats.max_chain_len);
    erts_print(to, to_arg, "Chain Length Min: %d\n", stats.min_chain_len);
    erts_print(to, to_arg, "Chain Length Std Dev: %f\n",
               stats.std_dev_chain_len);
    erts_print(to, to_arg, "Chain Length Expected Std Dev: %f\n",
               stats.std_dev_expected);

    if (IS_FIXED(tb))
        erts_print(to, to_arg, "Fixed: %d\n", stats.kept_items);
    else
        erts_print(to, to_arg, "Fixed: false\n");

    if (show) {
	for (i = 0; i < NACTIVE(tb); i++) {
	    HashDbTerm* list = BUCKET(tb,i);
	    if (list == NULL)
		continue;
	    erts_print(to, to_arg, "%d: [", i);
	    while(list != 0) {
		if (is_pseudo_deleted(list))
		    erts_print(to, to_arg, "*");
		if (tb->common.compress) {
		    Eterm key = GETKEY(tb, list->dbterm.tpl);
		    erts_print(to, to_arg, "key=%T", key);
		}
		else {
		    Eterm obj = make_tuple(list->dbterm.tpl);
		    erts_print(to, to_arg, "%T", obj);
		}
		if (list->next != 0)
		    erts_print(to, to_arg, ",");
		list = list->next;
	    }
	    erts_print(to, to_arg, "]\n");
	}
    }
}

static int db_free_empty_table_hash(DbTable *tbl)
{
    ASSERT(get_nitems_from_locks_or_counter(&tbl->hash) == 0);
    while (db_free_table_continue_hash(tbl, ERTS_SWORD_MAX) < 0)
	;
    return 0;
}

static SWord db_free_table_continue_hash(DbTable *tbl, SWord reds)
{
    DbTableHash *tb = &tbl->hash;
    FixedDeletion* fixdel = (FixedDeletion*) erts_atomic_read_acqb(&tb->fixdel);
    ERTS_LC_ASSERT(IS_TAB_WLOCKED(tb) || (tb->common.status & DB_DELETE));

    while (fixdel != NULL) {
	FixedDeletion *fx = fixdel;

	fixdel = fx->next;
        free_fixdel(tb, fx);
	if (--reds < 0) {
	    erts_atomic_set_relb(&tb->fixdel, (erts_aint_t)fixdel);
	    return reds;		/* Not done */
	}
    }
    erts_atomic_set_relb(&tb->fixdel, (erts_aint_t)NULL);

    while(tb->nslots != 0) {
	reds -= EXT_SEGSZ/64 + free_seg(tb);

	/*
	 * If we have done enough work, get out here.
	 */
	if (reds < 0) {
	    return reds;	/* Not done */
	}
    }
    if (tb->locks != NULL) {
	int i;
	for (i=0; i<DB_HASH_LOCK_CNT; ++i) {
	    erts_rwmtx_destroy(GET_LOCK(tb,i)); 
	}
	erts_db_free(ERTS_ALC_T_DB_SEG, (DbTable *)tb,
		     (void*)tb->locks, sizeof(DbTableHashFineLocks));
	tb->locks = NULL;
    }
    ASSERT(erts_flxctr_is_snapshot_ongoing(&tb->common.counters) ||
           ((sizeof(DbTable) +
             erts_flxctr_nr_of_allocated_bytes(&tb->common.counters)) ==
            erts_flxctr_read_approx(&tb->common.counters,
                                    ERTS_DB_TABLE_MEM_COUNTER_ID)));
    return reds;			/* Done */
}



/*
** Utility routines. (static)
*/
/*
** For the select functions, analyzes the pattern and determines which
** slots should be searched. Also compiles the match program
*/
static int analyze_pattern(DbTableHash *tb, Eterm pattern, 
                           extra_match_validator_t extra_validator, /* Optional callback */
                           struct mp_info *mpi)
{
    Eterm *ptpl;
    Eterm lst, tpl, ttpl;
    Eterm *matches,*guards, *bodies;
    Eterm sbuff[30];
    Eterm *buff = sbuff;
    Eterm key = NIL;	       
    HashValue hval = NIL;      
    int num_heads = 0;
    int i;

    mpi->lists = mpi->dlists;
    mpi->num_lists = 0;
    mpi->key_given = 1;
    mpi->something_can_match = 0;
    mpi->mp = NULL;

    for (lst = pattern; is_list(lst); lst = CDR(list_val(lst)))
	++num_heads;

    if (lst != NIL) {/* proper list... */
	return DB_ERROR_BADPARAM;
    }

    if (num_heads > 10) {
	buff = erts_alloc(ERTS_ALC_T_DB_TMP, sizeof(Eterm) * num_heads * 3);
	mpi->lists = erts_alloc(ERTS_ALC_T_DB_SEL_LIST,
				sizeof(*(mpi->lists)) * num_heads);	
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
	}
	++i;
	if (!(mpi->key_given)) {
	    continue;
	}
	if (tpl == am_Underscore || db_is_variable(tpl) != -1) {
	    (mpi->key_given) = 0;
	    (mpi->something_can_match) = 1;
	} else {
	    key = db_getkey(tb->common.keypos, tpl);
	    if (is_value(key)) {
		if (!db_has_variable(key)) {   /* Bound key */
		    int ix, search_slot;
		    HashDbTerm** bp;
		    erts_rwmtx_t* lck;
		    hval = MAKE_HASH(key);
		    lck = RLOCK_HASH(tb,hval);
		    ix = hash_to_ix(tb, hval);
		    bp = &BUCKET(tb,ix);
		    if (lck == NULL) {
			search_slot = search_list(tb,key,hval,*bp) != NULL;
		    } else {
			/* No point to verify if key exist now as there may be
			   concurrent inserters/deleters anyway */
			RUNLOCK_HASH(lck);
			search_slot = 1;
		    }
		    if (search_slot) {
			int j;
			for (j=0; ; ++j) {
			    if (j == mpi->num_lists) {
				mpi->lists[mpi->num_lists].bucket = bp;
				mpi->lists[mpi->num_lists].ix = ix;
				++mpi->num_lists;
				break;
			    }
			    if (mpi->lists[j].bucket == bp) {
				ASSERT(mpi->lists[j].ix == ix);
				break;
			    }
			    ASSERT(mpi->lists[j].ix != ix);
			}
			mpi->something_can_match = 1;
		    }
		} else {
		    mpi->key_given = 0;
		    mpi->something_can_match = 1;
		}
	    }
	}
    }

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

static struct ext_segtab* alloc_ext_segtab(DbTableHash* tb, unsigned seg_ix)
{
    struct segment** old_segtab = SEGTAB(tb);
    int nsegs = 0;
    struct ext_segtab* est;
    
    ASSERT(seg_ix >= NSEG_1);
    switch (seg_ix) {
    case NSEG_1: nsegs = NSEG_2; break;
    default:     nsegs = seg_ix + NSEG_INC; break;
    }
    ASSERT(nsegs > tb->nsegs);
    est = (struct ext_segtab*) erts_db_alloc(ERTS_ALC_T_DB_SEG,
                                             (DbTable *) tb,
                                             SIZEOF_EXT_SEGTAB(nsegs));
    est->nsegs = nsegs;
    est->prev_segtab = old_segtab;
    est->prev_nsegs = tb->nsegs;
    sys_memcpy(est->segtab, old_segtab, tb->nsegs*sizeof(struct segment*));
#ifdef DEBUG
    sys_memset(&est->segtab[seg_ix], 0, (nsegs-seg_ix)*sizeof(struct segment*));
#endif
    return est;
}

static void calc_shrink_limit(DbTableHash* tb)
{
    erts_aint_t shrink_limit;
    int sample_size_is_enough = 1;

    if (IS_DECENTRALIZED_CTRS(tb)) {
        /*
           Cochran’s Sample Size Formula indicates that we will get
           good estimates if we have 100 buckets or more per lock (see
           calculations below)
        */
        /* square of z-score 95% confidence */
        /* const double z2 = 1.96*1.96; */
        /* Estimated propotion used buckets */
        /* const double p = 0.5; */
        /* margin of error */
        /* const double moe = 0.1; */
        /* const double moe2 = moe*moe; */
        /* Cochran’s Sample Size Formula x=96.040 */
        /* const double x = (z2 * p * (1-p)) / moe2; */
        /* Modification for smaller populations */
        /* for(int n = 10; n < 1000; n = n + 100){ */
        /*   const double d = n*x / (x + n - 1) + 1; */
        /*   printf("Cochran_formula=%f size=%d mod_with_size=%f\n", x, n, d); */
        /* } */
        const int needed_slots = 100 * DB_HASH_LOCK_CNT;
        if (tb->nslots < needed_slots) {
            sample_size_is_enough = 0;
        }
    }

    if (sample_size_is_enough && tb->nslots >= (FIRST_SEGSZ + 2*EXT_SEGSZ)) {
        /*
         * Start shrink when the sample size is big enough for
         * decentralized counters if decentralized counters are used
         * and when we can remove one extra segment and still remain
         * below 50% load.
         */
        shrink_limit = (tb->nslots - EXT_SEGSZ) / 2;
    }
    else {
        /*
         * But don't shrink below two segments.
         * Why? In order to have chance of getting rid of the last extra segment,
         * and rehash it into the first small segment, we either have to start
         * early and do speculative joining of buckets or we have to join a lot
         * of buckets during each delete-op.
         *
         * Instead keep segment #2 once allocated. I also think it's a good bet
         * a shrinking large table will grow large again.
         */
        shrink_limit = 0;
    }
    erts_atomic_set_nob(&tb->shrink_limit, shrink_limit);
}


/* Extend table with one new segment
*/
static void alloc_seg(DbTableHash *tb)
{    
    int seg_ix = SLOT_IX_TO_SEG_IX(tb->nslots);
    struct segment** segtab;

    ASSERT(seg_ix > 0);
    if (seg_ix == tb->nsegs) { /* New segtab needed */
	struct ext_segtab* est = alloc_ext_segtab(tb, seg_ix);
        SET_SEGTAB(tb, est->segtab);
        tb->nsegs = est->nsegs;
    }
    ASSERT(seg_ix < tb->nsegs);
    segtab = SEGTAB(tb);
    segtab[seg_ix] = (struct segment*) erts_db_alloc(ERTS_ALC_T_DB_SEG,
                                                     (DbTable *) tb,
                                                     SIZEOF_SEGMENT(EXT_SEGSZ));
#ifdef DEBUG
    {
        int i;
        for (i = 0; i < EXT_SEGSZ; i++) {
            segtab[seg_ix]->buckets[i] = DBG_BUCKET_INACTIVE;
        }
    }
#endif
    tb->nslots += EXT_SEGSZ;

    calc_shrink_limit(tb);
}

static void dealloc_ext_segtab(void* lop_data)
{
    struct ext_segtab* est = (struct ext_segtab*) lop_data;

    erts_free(ERTS_ALC_T_DB_SEG, est);
}

struct dealloc_seg_ops {
    struct segment* segp;
    Uint seg_sz;

    struct ext_segtab* est;
};

/* Shrink table by removing the top segment
** free_records: 1=free any records in segment, 0=assume segment is empty 
** ds_ops: (out) Instructions for dealloc_seg().
*/
static int remove_seg(DbTableHash *tb, int free_records,
                      struct dealloc_seg_ops *ds_ops)
{
    const int seg_ix = SLOT_IX_TO_SEG_IX(tb->nslots) - 1;
    struct segment** const segtab = SEGTAB(tb);
    struct segment* const segp = segtab[seg_ix];
    Uint seg_sz;
    int nrecords = 0;

    ERTS_LC_ASSERT(IS_TAB_WLOCKED(tb) || tb->common.status & DB_DELETE
                   || erts_atomic_read_nob(&tb->is_resizing));

    ASSERT(segp != NULL);
    if (free_records) {
        int ix, n;
        if (seg_ix == 0) {
            /* First segment (always fully active) */
            n = FIRST_SEGSZ;
            ix = FIRST_SEGSZ-1;
        }
        else if (NACTIVE(tb) < tb->nslots) {
            /* Last extended segment partially active */
            n = (NACTIVE(tb) - FIRST_SEGSZ) & EXT_SEGSZ_MASK;
            ix = (NACTIVE(tb)-1) & EXT_SEGSZ_MASK;
        }
        else {
            /* Full extended segment */
            n = EXT_SEGSZ;
            ix = EXT_SEGSZ - 1;
        }
        for ( ; n > 0; n--, ix--) {
	    HashDbTerm* p = segp->buckets[ix & EXT_SEGSZ_MASK];
	    while(p != 0) {		
		HashDbTerm* nxt = p->next;
		free_term(tb, p);
		p = nxt;
		++nrecords;
	    }
	}
    }
#ifdef DEBUG
    else {
        int ix = (seg_ix == 0) ? FIRST_SEGSZ-1 : EXT_SEGSZ-1;
        for ( ; ix >= 0; ix--) {
            ASSERT(segp->buckets[ix] == DBG_BUCKET_INACTIVE);
        }
    }
#endif

    ds_ops->est = NULL;
    if (seg_ix >= NSEG_1) {
        struct ext_segtab* est = ErtsContainerStruct_(segtab,struct ext_segtab,segtab);

        if (seg_ix == est->prev_nsegs) { /* Dealloc extended segtab */
            ASSERT(est->prev_segtab != NULL);
            SET_SEGTAB(tb, est->prev_segtab);
            tb->nsegs = est->prev_nsegs;

            ds_ops->est = est;
        }
    }

    seg_sz = (seg_ix == 0) ? FIRST_SEGSZ : EXT_SEGSZ;
    tb->nslots -= seg_sz;
    ASSERT(tb->nslots >= 0);

    ds_ops->segp = segp;
    ds_ops->seg_sz = seg_sz;
    
#ifdef DEBUG
    if (seg_ix < tb->nsegs)
        SEGTAB(tb)[seg_ix] = NULL;
#endif
    calc_shrink_limit(tb);
    return nrecords;
}

/*
 * Deallocate segment removed by remove_seg()
 */
static void dealloc_seg(DbTableHash *tb, struct dealloc_seg_ops* ds_ops)
{
    struct ext_segtab* est = ds_ops->est;

    if (est) {
        if (!tb->common.is_thread_safe) {
            /*
             * Table is doing a graceful shrink operation and we must avoid
             * deallocating this segtab while it may still be read by other
             * threads. Schedule deallocation with thread progress to make
             * sure no lingering threads are still hanging in BUCKET macro
             * with an old segtab pointer.
             */
            erts_schedule_db_free(&tb->common, dealloc_ext_segtab,
                                  est, &est->lop,
                                  SIZEOF_EXT_SEGTAB(est->nsegs));
        }
        else
            erts_db_free(ERTS_ALC_T_DB_SEG, (DbTable*)tb, est,
                         SIZEOF_EXT_SEGTAB(est->nsegs));
    }

    erts_db_free(ERTS_ALC_T_DB_SEG, (DbTable *)tb,
                 ds_ops->segp, SIZEOF_SEGMENT(ds_ops->seg_sz));
}

/* Remove and deallocate top segment and all its contained objects */
static int free_seg(DbTableHash *tb)
{
    struct dealloc_seg_ops ds_ops;
    int reds;

    reds = remove_seg(tb, 1, &ds_ops);
    dealloc_seg(tb, &ds_ops);
    return reds;
}

/*
** Copy terms from ptr1 until ptr2
** works for ptr1 == ptr2 == 0  => []
** or ptr2 == 0
** sz is either precalculated heap size or 0 if not known
*/
static Eterm build_term_list(Process* p, HashDbTerm* ptr1, HashDbTerm* ptr2,
			     Uint sz, DbTableHash* tb)
{
    HashDbTerm* ptr;
    Eterm list = NIL;
    Eterm copy;
    Eterm *hp, *hend;

    if (!sz) {
	ptr = ptr1;
	while(ptr != ptr2) {
	    if (!is_pseudo_deleted(ptr))
		sz += ptr->dbterm.size + 2;
	    ptr = ptr->next;
	}
    }

    hp = HAlloc(p, sz);
    hend = hp + sz;

    ptr = ptr1;
    while(ptr != ptr2) {
	if (!is_pseudo_deleted(ptr)) {
	    copy = db_copy_object_from_ets(&tb->common, &ptr->dbterm, &hp, &MSO(p));
	    list = CONS(hp, copy, list);
	    hp  += 2;
	}
	ptr = ptr->next;
    }
    HRelease(p,hend,hp);

    return list;
}

static ERTS_INLINE int
begin_resizing(DbTableHash* tb)
{
    if (DB_USING_FINE_LOCKING(tb)) {
	return
            !erts_atomic_read_acqb(&tb->is_resizing) &&
            !erts_atomic_xchg_acqb(&tb->is_resizing, 1);
    } else
        ERTS_LC_ASSERT(IS_TAB_WLOCKED(tb));
    return 1;
}

static ERTS_INLINE void
done_resizing(DbTableHash* tb)
{
    if (DB_USING_FINE_LOCKING(tb))
	erts_atomic_set_relb(&tb->is_resizing, 0);
}

/* Grow table with one or more new buckets.
** Allocate new segment if needed.
*/
static void grow(DbTableHash* tb, int nitems)
{
    HashDbTerm** pnext;
    HashDbTerm** to_pnext;
    HashDbTerm* p;
    erts_rwmtx_t* lck;
    int nactive;
    int from_ix, to_ix;
    int szm;
    int loop_limit = 5;

    do {
        if (!begin_resizing(tb))
            return; /* already in progress */
        nactive = NACTIVE(tb);
        if (nitems <= GROW_LIMIT(nactive)) {
            goto abort; /* already done (race) */
        }

        /* Ensure that the slot nactive exists */
        if (nactive == tb->nslots) {
            /* Time to get a new segment */
            ASSERT(((nactive-FIRST_SEGSZ) & EXT_SEGSZ_MASK) == 0);
            alloc_seg(tb);
        }
        ASSERT(nactive < tb->nslots);

        szm = erts_atomic_read_nob(&tb->szm);
        if (nactive <= szm) {
            from_ix = nactive & (szm >> 1);
        } else {
            ASSERT(nactive == szm+1);
            from_ix = 0;
            szm = (szm<<1) | 1;
        }
        to_ix = nactive;

        lck = WLOCK_HASH(tb, from_ix);
        ERTS_ASSERT(lck == GET_LOCK_MAYBE(tb,to_ix));
        /* Now a final double check (with the from_ix lock held)
         * that we did not get raced by a table fixer.
         */
        if (IS_FIXED(tb)) {
            WUNLOCK_HASH(lck);
            goto abort;
        }
        erts_atomic_set_nob(&tb->nactive, ++nactive);
        if (from_ix == 0) {
            if (DB_USING_FINE_LOCKING(tb))
                erts_atomic_set_relb(&tb->szm, szm);
            else
                erts_atomic_set_nob(&tb->szm, szm);
        }
        done_resizing(tb);

        /* Finally, let's split the bucket. We try to do it in a smart way
           to keep link order and avoid unnecessary updates of next-pointers */
        pnext = &BUCKET(tb, from_ix);
        p = *pnext;
        to_pnext = &BUCKET(tb, to_ix);
        ASSERT(*to_pnext == DBG_BUCKET_INACTIVE);
        while (p != NULL) {
            if (is_pseudo_deleted(p)) { /* rare but possible with fine locking */
                *pnext = p->next;
                free_term(tb, p);
                p = *pnext;
            }
            else {
                int ix = p->hvalue & szm;
                if (ix != from_ix) {
                    ASSERT(ix == (from_ix ^ ((szm+1)>>1)));
                    *to_pnext = p;
                    /* Swap "from" and "to": */
                    from_ix = ix;
                    to_pnext = pnext;
                }
                pnext = &p->next;
                p = *pnext;
            }
        }
        *to_pnext = NULL;
        WUNLOCK_HASH(lck);

    }while (--loop_limit && nitems > GROW_LIMIT(nactive));

    return;
   
abort:
    done_resizing(tb);
}


/* Shrink table by joining top bucket.
** Remove top segment if it gets empty.
*/
static void shrink(DbTableHash* tb, int nitems)
{
    struct dealloc_seg_ops ds_ops;
    HashDbTerm* src;
    HashDbTerm* tail;
    HashDbTerm** bp;
    erts_rwmtx_t* lck;
    int src_ix, dst_ix, low_szm;
    int nactive;
    int loop_limit = 5;

    ds_ops.segp = NULL;
    do {
        if (!begin_resizing(tb))
            return; /* already in progress */
        nactive = NACTIVE(tb);
        if (!(nitems < SHRINK_LIMIT(tb))) {
            goto abort; /* already done (race) */
        }
        src_ix = nactive - 1;
        low_szm = erts_atomic_read_nob(&tb->szm) >> 1;
        dst_ix = src_ix & low_szm;

        ASSERT(dst_ix < src_ix);
        ASSERT(nactive > FIRST_SEGSZ);
        lck = WLOCK_HASH(tb, dst_ix);
        ERTS_ASSERT(lck == GET_LOCK_MAYBE(tb,src_ix));
        /* Double check for racing table fixers */
        if (IS_FIXED(tb)) {
            WUNLOCK_HASH(lck);
            goto abort;
        }

        src = BUCKET(tb, src_ix);
#ifdef DEBUG
        BUCKET(tb, src_ix) = DBG_BUCKET_INACTIVE;
#endif
        nactive = src_ix;
        erts_atomic_set_nob(&tb->nactive, nactive);
        if (dst_ix == 0) {
            erts_atomic_set_relb(&tb->szm, low_szm);
        }
        if (tb->nslots - src_ix >= EXT_SEGSZ) {
            remove_seg(tb, 0, &ds_ops);
        }
        done_resizing(tb);

        if (src) {
            /*
             * We join buckets by appending "dst" list at the end of "src" list
             * as we must step through "src" anyway to purge pseudo deleted.
             */
            bp = &BUCKET(tb, dst_ix);
            tail = *bp;
            *bp = src;

            while(*bp != NULL) {
                if (is_pseudo_deleted(*bp)) {
                    HashDbTerm* deleted = *bp;
                    *bp = deleted->next;
                    free_term(tb, deleted);
                } else {
                    bp = &(*bp)->next;
                }
            }
            *bp = tail;
        }

        WUNLOCK_HASH(lck);

        if (ds_ops.segp) {
            dealloc_seg(tb, &ds_ops);
            ds_ops.segp = NULL;
        }

    } while (--loop_limit && nitems < SHRINK_LIMIT(tb));
    return;

abort:
    done_resizing(tb);
}

/* Search a list of tuples for a matching key */

static HashDbTerm* search_list(DbTableHash* tb, Eterm key, 
			       HashValue hval, HashDbTerm *list)
{
    while (list != 0) {
	if (has_live_key(tb,list,key,hval))
	    return list;
	list = list->next;
    }
    return 0;
}


/* This function is called by the next AND the select BIF */
/* It return the next live object in a table, NULL if no more */
/* In-bucket: RLOCKED */
/* Out-bucket: RLOCKED unless NULL */
static HashDbTerm* next_live(DbTableHash *tb, Uint *iptr, erts_rwmtx_t** lck_ptr,
			     HashDbTerm *list)
{
    int i;

    ERTS_LC_ASSERT(IS_HASH_RLOCKED(tb,*iptr));

    for ( ; list != NULL; list = list->next) {
	if (!is_pseudo_deleted(list))
	    return list;        
    }

    i = *iptr;
    while ((i=next_slot(tb, i, lck_ptr)) != 0) {

	list = BUCKET(tb,i);
	while (list != NULL) {
	    if (!is_pseudo_deleted(list)) {
		*iptr = i;
		return list;        
	    }
	    list = list->next;
	}
    }
    /* *iptr = ??? */
    return NULL;
}

static int
db_lookup_dbterm_hash(Process *p, DbTable *tbl, Eterm key, Eterm obj,
                      DbUpdateHandle* handle)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    HashDbTerm **bp, *b;
    DbTableHashLockAndCounter* lck_ctr;
    int flags = 0;

    ASSERT(tb->common.status & DB_SET);

    hval = MAKE_HASH(key);
    lck_ctr = WLOCK_HASH_GET_LCK_AND_CTR(tb, hval);
    bp = &BUCKET(tb, hash_to_ix(tb, hval));
    b = *bp;

    for (;;) {
        if (b == NULL) {
            break;
        }
        if (has_key(tb, b, key, hval)) {
            if (!is_pseudo_deleted(b)) {
                goto Ldone;
            }
            break;
        }
        bp = &b->next;
        b = *bp;
    }

    if (obj == THE_NON_VALUE) {
        WUNLOCK_HASH_LCK_CTR(lck_ctr);
        return 0;
    }

    {
        Eterm *objp = tuple_val(obj);
        int arity = arityval(*objp);
        Eterm *htop, *hend;

        ASSERT(arity >= tb->common.keypos);
        htop = HAlloc(p, arity + 1);
        hend = htop + arity + 1;
        sys_memcpy(htop, objp, sizeof(Eterm) * (arity + 1));
        htop[tb->common.keypos] = key;
        obj = make_tuple(htop);

        if (b == NULL) {
            HashDbTerm *q = new_dbterm(tb, obj);

            q->hvalue = hval;
            q->pseudo_deleted = 0;
            q->next = NULL;
            *bp = b = q;
            flags |= DB_INC_TRY_GROW;
        } else {
            HashDbTerm *q, *next = b->next;

            ASSERT(is_pseudo_deleted(b));
            q = replace_dbterm(tb, b, obj);
            q->next = next;
            ASSERT(q->hvalue == hval);
            q->pseudo_deleted = 0;
            *bp = b = q;
            INC_NITEMS(tb, lck_ctr, hval);
        }

        HRelease(p, hend, htop);
        flags |= DB_NEW_OBJECT;
    }

Ldone:
    handle->tb = tbl;
    handle->bp = (void **)bp;
    handle->dbterm = &b->dbterm;
    handle->flags = flags;
    handle->new_size = b->dbterm.size;
    handle->u.hash.lck_ctr = lck_ctr;
    return 1;
}

/* Must be called after call to db_lookup_dbterm
*/
static void
db_finalize_dbterm_hash(int cret, DbUpdateHandle* handle)
{
    DbTable* tbl = handle->tb;
    DbTableHash *tb = &tbl->hash;
    HashDbTerm **bp = (HashDbTerm **) handle->bp;
    HashDbTerm *b = *bp;
    Uint32 hval = b->hvalue;
    DbTableHashLockAndCounter* lck_ctr = handle->u.hash.lck_ctr;
    HashDbTerm* free_me = NULL;
    Sint nitems;

    ERTS_LC_ASSERT(IS_HASH_WLOCKED(tb, &lck_ctr->lck));  /* locked by db_lookup_dbterm_hash */

    ASSERT((&b->dbterm == handle->dbterm) == !(tb->common.compress && handle->flags & DB_MUST_RESIZE));

    if (handle->flags & DB_NEW_OBJECT && cret != DB_ERROR_NONE) {
        if (IS_FIXED(tb) && add_fixed_deletion(tb, hash_to_ix(tb, b->hvalue),
                                               0)) {
            b->pseudo_deleted = 1;
        } else {
            *bp = b->next;
            free_me = b;
        }
        if (!(handle->flags & DB_INC_TRY_GROW))
            DEC_NITEMS(tb, lck_ctr, hval);
        nitems = NITEMS_ESTIMATE(tb, lck_ctr, hval);
        WUNLOCK_HASH_LCK_CTR(lck_ctr);
        try_shrink(tb, nitems);
    } else {
        if (handle->flags & DB_MUST_RESIZE) {
            ASSERT(cret == DB_ERROR_NONE);
            db_finalize_resize(handle, offsetof(HashDbTerm,dbterm));
            free_me = b;
        }
        if (handle->flags & DB_INC_TRY_GROW) {
            int nactive;
            int nitems;
            ASSERT(cret == DB_ERROR_NONE);
            INC_NITEMS(tb, lck_ctr, hval);
            nitems = NITEMS_ESTIMATE(tb, lck_ctr, hval);
            WUNLOCK_HASH_LCK_CTR(lck_ctr);
            nactive = NACTIVE(tb);

            if (nitems > GROW_LIMIT(nactive) && !IS_FIXED(tb)) {
                grow(tb, nitems);
            }
        } else {
            WUNLOCK_HASH_LCK_CTR(lck_ctr);
        }
    }

    if (free_me)
        free_term(tb, free_me);

#ifdef DEBUG
    handle->dbterm = 0;
#endif
    return;
}

static SWord db_delete_all_objects_hash(Process* p,
                                        DbTable* tbl,
                                        SWord reds,
                                        Eterm* nitems_holder_wb)
{
    if (nitems_holder_wb != NULL) {
        Uint nr_of_items = get_nitems_from_locks_or_counter(&tbl->hash);
        *nitems_holder_wb = erts_make_integer(nr_of_items, p);
    }
    if (IS_FIXED(tbl)) {
	reds = db_mark_all_deleted_hash(tbl, reds);
    } else {
        reds = db_free_table_continue_hash(tbl, reds);
        if (reds < 0)
            return reds;

	db_create_hash(p, tbl);
        RESET_NITEMS(tbl);
    }
    return reds;
}

static Eterm db_delete_all_objects_get_nitems_from_holder_hash(Process* p,
                                                               Eterm nitems_holder){
    return nitems_holder;
}

void db_foreach_offheap_hash(DbTable *tbl,
			     void (*func)(ErlOffHeap *, void *),
			     void * arg)
{
    DbTableHash *tb = &tbl->hash;
    HashDbTerm* list;
    int i;
    int nactive = NACTIVE(tb);
    
    if (nactive > tb->nslots) {
        /* Table is being emptied by delete/1 or delete_all_objects/1 */
        ASSERT(!(tb->common.status & (DB_PRIVATE|DB_PROTECTED|DB_PUBLIC)));
        nactive = tb->nslots;
    }

    for (i = 0; i < nactive; i++) {
	list = BUCKET(tb,i);
	while(list != 0) {
	    ErlOffHeap tmp_offheap;
	    tmp_offheap.first = list->dbterm.first_oh;
	    tmp_offheap.overhead = 0;
	    (*func)(&tmp_offheap, arg);
	    list->dbterm.first_oh = tmp_offheap.first;
	    list = list->next;
	}
    }
}

void db_calc_stats_hash(DbTableHash* tb, DbHashStats* stats)
{
    HashDbTerm* b;
    erts_rwmtx_t* lck;
    int sum = 0;
    int sq_sum = 0;
    int kept_items = 0;
    int ix;
    int len;
    
    stats->min_chain_len = INT_MAX;
    stats->max_chain_len = 0;
    ix = 0;
    lck = RLOCK_HASH(tb,ix);
    do {
	len = 0;
	for (b = BUCKET(tb,ix); b!=NULL; b=b->next) {
	    len++;
            if (is_pseudo_deleted(b))
                ++kept_items;
	}
	sum += len;
	sq_sum += len*len;
	if (len < stats->min_chain_len) stats->min_chain_len = len;
	if (len > stats->max_chain_len) stats->max_chain_len = len;
	ix = next_slot(tb,ix,&lck);
    }while (ix);
    stats->avg_chain_len = (float)sum / NACTIVE(tb);	
    stats->std_dev_chain_len = sqrt((sq_sum - stats->avg_chain_len*sum) / NACTIVE(tb));
    /* Expected	standard deviation from a good uniform hash function, 
       ie binomial distribution (not taking the linear hashing into acount) */
    stats->std_dev_expected = sqrt(stats->avg_chain_len * (1 - 1.0/NACTIVE(tb)));
    stats->kept_items = kept_items;
}


/*
 * erts_internal:ets_lookup_binary_info/2
 */
static int db_get_binary_info_hash(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    HashDbTerm *b, *first, *end;
    erts_rwmtx_t* lck;
    Eterm *hp, *hp_end;
    Uint hsz;
    Eterm list;

    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb,hval);
    ix = hash_to_ix(tb, hval);
    b = BUCKET(tb, ix);

    while(b != 0) {
        if (has_key(tb, b, key, hval)) {
            goto found_key;
	}
        b = b->next;
    }
    RUNLOCK_HASH(lck);
    *ret = NIL;
    return DB_ERROR_NONE;

found_key:

    first = b;
    hsz = 0;
    do {
        ErlOffHeap oh;
        oh.first = b->dbterm.first_oh;
        erts_bld_bin_list(NULL, &hsz, &oh, NIL);
        b = b->next;
    } while (b && has_key(tb, b, key, hval));    
    end = b;

    hp = HAlloc(p, hsz);
    hp_end = hp + hsz;
    list = NIL; 
    for (b = first; b != end; b = b->next) {
        ErlOffHeap oh;
        oh.first = b->dbterm.first_oh;
        list = erts_bld_bin_list(&hp, NULL, &oh, list);
    }
    ASSERT(hp == hp_end); (void)hp_end;
    
    RUNLOCK_HASH(lck);
    *ret = list;
    return DB_ERROR_NONE;
}

static int raw_find_next(Process *p, DbTable* tbl, Uint ix,
                         erts_rwmtx_t* lck, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashDbTerm *b;

    do {
        b = BUCKET(tb,ix);
        if (b) {
            *ret = db_copy_key(p, tbl, &b->dbterm);
            RUNLOCK_HASH(lck);
            return DB_ERROR_NONE;
        }
        ix = next_slot(tb, ix, &lck);
    } while (ix);

    *ret = am_EOT;
    return DB_ERROR_NONE;
}

static int db_raw_first_hash(Process *p, DbTable *tbl, Eterm *ret)
{
    const Uint ix = 0;
    return raw_find_next(p, tbl, ix, RLOCK_HASH(&tbl->hash, ix), ret);
}

static int db_raw_next_hash(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    Uint ix;
    HashDbTerm* b;
    erts_rwmtx_t* lck;

    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb,hval);
    ix = hash_to_ix(tb, hval);
    b = BUCKET(tb, ix);

    for (;;) {
	if (b == NULL) {
	    RUNLOCK_HASH(lck);
	    return DB_ERROR_BADKEY;
	}
	if (has_key(tb, b, key, hval)) {
	    break;
	}
	b = b->next;
    }
    /* Key found */

    for (b = b->next; b; b = b->next) {
        if (!has_key(tb, b, key, hval)) {
            *ret = db_copy_key(p, tbl, &b->dbterm);
            RUNLOCK_HASH(lck);
            return DB_ERROR_NONE;
        }
    }

    ix = next_slot(tb, ix, &lck);
    if (ix)
        return raw_find_next(p, tbl, ix, lck, ret);

    *ret = am_EOT;
    return DB_ERROR_NONE;
}

static void* db_eterm_to_dbterm_hash(int compress, int keypos, Eterm obj)
{
    HashDbTerm* term = new_dbterm_hash_no_tab(compress, keypos, obj);
    term->next = NULL;
    return term;
}

static void* db_dbterm_list_prepend_hash(void* list, void* db_term)
{
    HashDbTerm* l = list;
    HashDbTerm* t = db_term;
    t->next = l;
    return t;
}

static void* db_dbterm_list_remove_first_hash(void** list)
{
    if (*list == NULL) {
        return NULL;
    } else {
        HashDbTerm* t = (*list);
        HashDbTerm* l = t->next;
        *list = l;
        return t;
    }
}

/*
 * Frees a HashDbTerm without updating the memory footprint of the
 * table.
 */
static void db_free_dbterm_hash(int compressed, void* obj)
{
    HashDbTerm* p = obj;
    db_free_term_no_tab(compressed, p, offsetof(HashDbTerm, dbterm));
}

static Eterm db_get_dbterm_key_hash(DbTable* tb, void* db_term)
{
    HashDbTerm *value_to_insert = db_term;
    return GETKEY(tb, value_to_insert->dbterm.tpl);
}

/* For testing only */
Eterm erts_ets_hash_sizeof_ext_segtab(void)
{
    return make_small(((SIZEOF_EXT_SEGTAB(0)-1) / sizeof(UWord)) + 1);
}

void
erts_db_foreach_thr_prgr_offheap_hash(void (*func)(ErlOffHeap *, void *),
                                      void *arg)
{
}

#ifdef ERTS_ENABLE_LOCK_COUNT
void erts_lcnt_enable_db_hash_lock_count(DbTableHash *tb, int enable) {
    int i;

    if(tb->locks == NULL) {
        return;
    }

    for(i = 0; i < DB_HASH_LOCK_CNT; i++) {
        erts_lcnt_ref_t *ref = &tb->locks->lck_vec[i].lck_ctr.lck.lcnt;

        if(enable) {
            erts_lcnt_install_new_lock_info(ref, "db_hash_slot", tb->common.the_name,
                ERTS_LOCK_TYPE_RWMUTEX | ERTS_LOCK_FLAGS_CATEGORY_DB);
        } else {
            erts_lcnt_uninstall(ref);
        }
    }
}
#endif /* ERTS_ENABLE_LOCK_COUNT */
