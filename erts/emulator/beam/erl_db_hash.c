/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2017. All Rights Reserved.
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

/* 
 * The following symbols can be manipulated to "tune" the linear hash array 
 */
#define GROW_LIMIT(NACTIVE) ((NACTIVE)*1)
#define SHRINK_LIMIT(NACTIVE) ((NACTIVE) / 2)

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
#define NITEMS(tb) ((int)erts_atomic_read_nob(&(tb)->common.nitems))

#define SLOT_IX_TO_SEG_IX(i) (((i)+(EXT_SEGSZ-FIRST_SEGSZ)) >> EXT_SEGSZ_EXP)

#define BUCKET(tb, i) SEGTAB(tb)[SLOT_IX_TO_SEG_IX(i)]->buckets[(i) & EXT_SEGSZ_MASK]

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

/* Remember a slot containing a pseudo-deleted item (INVALID_HASH)
 * Return false if we got raced by unfixing thread
 * and the object should be deleted for real.
 */
static ERTS_INLINE int add_fixed_deletion(DbTableHash* tb, int ix,
                                          erts_aint_t fixated_by_me)
{
    erts_aint_t was_next;
    erts_aint_t exp_next;
    FixedDeletion* fixd = (FixedDeletion*) erts_db_alloc(ERTS_ALC_T_DB_FIX_DEL,
							 (DbTable *) tb,
							 sizeof(FixedDeletion));
    ERTS_ETS_MISC_MEM_ADD(sizeof(FixedDeletion));
    fixd->slot = ix;
    was_next = erts_atomic_read_acqb(&tb->fixdel);
    do { /* Lockless atomic insertion in linked list: */
        if (NFIXED(tb) <= fixated_by_me) {
            erts_db_free(ERTS_ALC_T_DB_FIX_DEL, (DbTable*)tb,
                         fixd, sizeof(FixedDeletion));
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


#define MAX_HASH 0xEFFFFFFFUL
#define INVALID_HASH 0xFFFFFFFFUL

/* optimised version of make_hash (normal case? atomic key) */
#define MAKE_HASH(term) \
    ((is_atom(term) ? (atom_tab(atom_val(term))->slot.bucket.hvalue) : \
      make_internal_hash(term, 0)) % MAX_HASH)

#  define DB_HASH_LOCK_MASK (DB_HASH_LOCK_CNT-1)
#  define GET_LOCK(tb,hval) (&(tb)->locks->lck_vec[(hval) & DB_HASH_LOCK_MASK].lck)
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


#ifdef ERTS_ENABLE_LOCK_CHECK
#  define IFN_EXCL(tb,cmd) (((tb)->common.is_thread_safe) || (cmd))
#  define IS_HASH_RLOCKED(tb,hval) IFN_EXCL(tb,erts_lc_rwmtx_is_rlocked(GET_LOCK(tb,hval)))
#  define IS_HASH_WLOCKED(tb,lck) IFN_EXCL(tb,erts_lc_rwmtx_is_rwlocked(lck))
#  define IS_TAB_WLOCKED(tb) erts_lc_rwmtx_is_rwlocked(&(tb)->common.rwlock)
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


/*
 * Some special binary flags
 */
#define BIN_FLAG_ALL_OBJECTS         BIN_FLAG_USR1


static ERTS_INLINE void free_term(DbTableHash *tb, HashDbTerm* p)
{
    db_free_term((DbTable*)tb, p, offsetof(HashDbTerm, dbterm));
}

/*
 * Local types 
 */
struct mp_prefound {
    HashDbTerm** bucket;
    int ix;
};

struct mp_info {
    int all_objects;		/* True if complete objects are always
				 * returned from the match_spec (can use 
				 * copy_shallow on the return value) */
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
static int free_seg(DbTableHash *tb, int free_records);
static HashDbTerm* next(DbTableHash *tb, Uint *iptr, erts_rwmtx_t** lck_ptr,
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
				int reverse, Eterm *ret);
static int db_select_hash(Process *p, DbTable *tbl, Eterm tid,
			  Eterm pattern, int reverse, Eterm *ret);
static int db_select_continue_hash(Process *p, DbTable *tbl,
				   Eterm continuation, Eterm *ret);

static int db_select_count_hash(Process *p, DbTable *tbl, Eterm tid,
				Eterm pattern, Eterm *ret);
static int db_select_count_continue_hash(Process *p, DbTable *tbl,
					 Eterm continuation, Eterm *ret);

static int db_select_delete_hash(Process *p, DbTable *tbl, Eterm tid,
				 Eterm pattern, Eterm *ret);
static int db_select_delete_continue_hash(Process *p, DbTable *tbl,
					  Eterm continuation, Eterm *ret);

static int db_select_replace_hash(Process *p, DbTable *tbl, Eterm tid,
                                  Eterm pattern, Eterm *ret);
static int db_select_replace_continue_hash(Process *p, DbTable *tbl,
                                           Eterm continuation, Eterm *ret);

static int db_take_hash(Process *, DbTable *, Eterm, Eterm *);
static void db_print_hash(fmtfn_t to,
			  void *to_arg,
			  int show,
			  DbTable *tbl);
static int db_free_table_hash(DbTable *tbl);

static SWord db_free_table_continue_hash(DbTable *tbl, SWord reds);


static void db_foreach_offheap_hash(DbTable *,
				    void (*)(ErlOffHeap *, void *),
				    void *);

static int db_delete_all_objects_hash(Process* p, DbTable* tbl);
#ifdef HARDDEBUG
static void db_check_table_hash(DbTableHash *tb);
#endif
static int
db_lookup_dbterm_hash(Process *p, DbTable *tbl, Eterm key, Eterm obj,
                      DbUpdateHandle* handle);
static void
db_finalize_dbterm_hash(int cret, DbUpdateHandle* handle);

static ERTS_INLINE void try_shrink(DbTableHash* tb)
{
    int nactive = NACTIVE(tb);
    int nitems = NITEMS(tb);
    if (nactive > FIRST_SEGSZ && nitems < SHRINK_LIMIT(nactive)
	&& !IS_FIXED(tb)) {
	shrink(tb, nitems);
    }
}	

/* Is this a live object (not pseodo-deleted) with the specified key? 
*/
static ERTS_INLINE int has_live_key(DbTableHash* tb, HashDbTerm* b,
				    Eterm key, HashValue hval)
{
    if (b->hvalue != hval) return 0;
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
    if (b->hvalue != hval && b->hvalue != INVALID_HASH) return 0;
    else {
	Eterm itemKey = GETKEY(tb, b->dbterm.tpl);
	ASSERT(!is_header(itemKey));
	return EQ(key, itemKey);
    }
}

static ERTS_INLINE HashDbTerm* new_dbterm(DbTableHash* tb, Eterm obj)
{
    HashDbTerm* p;
    if (tb->common.compress) {
	p = db_store_term_comp(&tb->common, NULL, offsetof(HashDbTerm,dbterm), obj);
    }
    else {
	p = db_store_term(&tb->common, NULL, offsetof(HashDbTerm,dbterm), obj);
    }
    return p;
}

static ERTS_INLINE HashDbTerm* replace_dbterm(DbTableHash* tb, HashDbTerm* old,
					      Eterm obj)
{
    HashDbTerm* ret;
    ASSERT(old != NULL);
    if (tb->common.compress) {
	ret = db_store_term_comp(&tb->common, &(old->dbterm), offsetof(HashDbTerm,dbterm), obj);
    }
    else {
	ret = db_store_term(&tb->common, &(old->dbterm), offsetof(HashDbTerm,dbterm), obj);
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
    db_select_continue_hash, /* hmm continue_hash? */
    db_select_delete_continue_hash,
    db_select_count_hash,
    db_select_count_continue_hash,
    db_select_replace_hash,
    db_select_replace_continue_hash,
    db_take_hash,
    db_delete_all_objects_hash,
    db_free_table_hash,
    db_free_table_continue_hash,
    db_print_hash,
    db_foreach_offheap_hash,
    db_lookup_dbterm_hash,
    db_finalize_dbterm_hash
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

    ERTS_LC_ASSERT(erts_lc_rwmtx_is_rwlocked(&tb->common.rwlock)
		       || (erts_lc_rwmtx_is_rlocked(&tb->common.rwlock)
			   && !tb->common.is_thread_safe));
restart:
    fixdel = (FixedDeletion*) erts_atomic_xchg_mb(&tb->fixdel,
                                                      (erts_aint_t) NULL);
    while (fixdel != NULL) {
	FixedDeletion *fx = fixdel;
	int ix = fx->slot;
	HashDbTerm **bp;
	HashDbTerm *b;
	erts_rwmtx_t* lck = WLOCK_HASH(tb,ix);

	if (IS_FIXED(tb)) { /* interrupted by fixer */
	    WUNLOCK_HASH(lck);
	    restore_fixdel(tb,fixdel);
	    if (!IS_FIXED(tb)) {
		goto restart; /* unfixed again! */
	    }
	    return work;
	}
	if (ix < NACTIVE(tb)) {
	    bp = &BUCKET(tb, ix);
	    b = *bp;
	    
	    while (b != NULL) {
		if (b->hvalue == INVALID_HASH) {
		    *bp = b->next;
		    free_term(tb, b);
		    work++;
		    b = *bp;
		} else {
		    bp = &b->next;
		    b = b->next;
		}
	    }
	}
	/* else slot has been joined and purged by shrink() */
	WUNLOCK_HASH(lck);
	fixdel = fx->next;
	erts_db_free(ERTS_ALC_T_DB_FIX_DEL,
		     (DbTable *) tb,
		     (void *) fx,
		     sizeof(FixedDeletion));
	ERTS_ETS_MISC_MEM_ADD(-sizeof(FixedDeletion));
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
	tb->locks = (DbTableHashFineLocks*) erts_db_alloc_fnf(ERTS_ALC_T_DB_SEG, /* Other type maybe? */ 
							      (DbTable *) tb,
							      sizeof(DbTableHashFineLocks));	    	    
	for (i=0; i<DB_HASH_LOCK_CNT; ++i) {
            erts_rwmtx_init_opt(&tb->locks->lck_vec[i].lck, &rwmtx_opt,
                "db_hash_slot", tb->common.the_name, ERTS_LOCK_FLAGS_CATEGORY_DB);
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

    for (;;) {
	list = BUCKET(tb,ix);
	if (list != NULL) {
	    if (list->hvalue == INVALID_HASH) {
		list = next(tb,&ix,&lck,list);
	    }
	    break;
	}
	if ((ix=next_slot(tb,ix,&lck)) == 0) {
	    list = NULL;
	    break;
	}
    }
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

    b = next(tb, &ix, &lck, b);
    if (tb->common.status & (DB_BAG | DB_DUPLICATE_BAG)) {
	while (b != 0) {
	    if (!has_live_key(tb, b, key, hval)) {
		break;
	    }
	    b = next(tb, &ix, &lck, b);
	}
    }
    if (b == NULL) {
	*ret = am_EOT;
    }
    else {
	*ret = db_copy_key(p, tbl, &b->dbterm);
	RUNLOCK_HASH(lck);
    }    
    return DB_ERROR_NONE;
}    

int db_put_hash(DbTable *tbl, Eterm obj, int key_clash_fail)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    Eterm key;
    HashDbTerm** bp;
    HashDbTerm* b;
    HashDbTerm* q;
    erts_rwmtx_t* lck;
    int nitems;
    int ret = DB_ERROR_NONE;

    key = GETKEY(tb, tuple_val(obj));
    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb, hval);
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
	if (b->hvalue == INVALID_HASH) {
	    erts_atomic_inc_nob(&tb->common.nitems);
	}
	else if (key_clash_fail) {
	    ret = DB_ERROR_BADKEY;
	    goto Ldone;
	}
	q = replace_dbterm(tb, b, obj);
	q->next = bnext;
	q->hvalue = hval; /* In case of INVALID_HASH */
	*bp = q;
	goto Ldone;
    }
    else if (key_clash_fail) { /* && (DB_BAG || DB_DUPLICATE_BAG) */
	q = b;
	do {
	    if (q->hvalue != INVALID_HASH) {
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
		if (q->hvalue == INVALID_HASH) {
		    erts_atomic_inc_nob(&tb->common.nitems);
		    q->hvalue = hval;
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
	}while (q != NULL && has_key(tb,q,key,hval)); 
    }
    /*else DB_DUPLICATE_BAG */

Lnew:
    q = new_dbterm(tb, obj);
    q->hvalue = hval;
    q->next = b;
    *bp = q;
    nitems = erts_atomic_inc_read_nob(&tb->common.nitems);
    WUNLOCK_HASH(lck);
    {
	int nactive = NACTIVE(tb);       
	if (nitems > GROW_LIMIT(nactive) && !IS_FIXED(tb)) {
	    grow(tb, nitems);
	}
    }
    return DB_ERROR_NONE;

Ldone:
    WUNLOCK_HASH(lck);	
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
	    if (b2->hvalue != INVALID_HASH)
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
			&& b2->hvalue != INVALID_HASH) {
			retval = DB_ERROR_BADITEM;
			goto done;
		    }
		    b2 = b2->next;
		}
		b = b1;
		while(b != b2) {
		    if (b->hvalue != INVALID_HASH) {
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
    erts_rwmtx_t* lck;
    int nitems_diff = 0;

    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb,hval);
    ix = hash_to_ix(tb, hval);
    bp = &BUCKET(tb, ix);
    b = *bp;

    while(b != 0) {
	if (has_live_key(tb,b,key,hval)) {
	    --nitems_diff;
	    if (nitems_diff == -1 && IS_FIXED(tb)
                && add_fixed_deletion(tb, ix, 0)) {
		/* Pseudo remove (no need to keep several of same key) */
		b->hvalue = INVALID_HASH;
	    } else {
		*bp = b->next;
		free_term(tb, b);
		b = *bp;
		continue;
	    }
	}
	else {
	    if (nitems_diff && b->hvalue != INVALID_HASH)
		break;
	}
	bp = &b->next;
	b = b->next;
    }
    WUNLOCK_HASH(lck);
    if (nitems_diff) {
	erts_atomic_add_nob(&tb->common.nitems, nitems_diff);
	try_shrink(tb);
    }
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
    erts_rwmtx_t* lck;
    int nitems_diff = 0;
    int nkeys = 0;
    Eterm key;

    key = GETKEY(tb, tuple_val(object));
    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb,hval);
    ix = hash_to_ix(tb, hval);
    bp = &BUCKET(tb, ix);
    b = *bp;

    while(b != 0) {
	if (has_live_key(tb,b,key,hval)) {
	    ++nkeys;
	    if (db_eq(&tb->common,object, &b->dbterm)) {
		--nitems_diff;
		if (nkeys==1 && IS_FIXED(tb) && add_fixed_deletion(tb,ix,0)) {
		    b->hvalue = INVALID_HASH;        /* Pseudo remove */
		    bp = &b->next;
		    b = b->next;
		} else {
		    *bp = b->next;
		    free_term(tb, b);
		    b = *bp;
		}
		if (tb->common.status & (DB_DUPLICATE_BAG)) {
		    continue;
		} else {
		    break;
		}
	    }
	}
	else if (nitems_diff && b->hvalue != INVALID_HASH) {
	    break;
	}
	bp = &b->next;
	b = b->next;
    }
    WUNLOCK_HASH(lck);
    if (nitems_diff) {
	erts_atomic_add_nob(&tb->common.nitems, nitems_diff);
	try_shrink(tb);
    }
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
 * This is just here so I can take care of the return value 
 * that is to be sent during a trap (the BIF_TRAP macros explicitly returns)
 */
static BIF_RETTYPE bif_trap1(Export *bif,
			      Process *p, 
			      Eterm p1) 
{
    BIF_TRAP1(bif, p, p1);
}


/*
 * Match traversal callbacks
 */

/* Called when no match is possible.
 *      context_ptr: Pointer to context
 *      ret: Pointer to traversal function term return.
 *
 * Both the direct return value and 'ret' are used as the traversal function return values.
 */
typedef int (*mtraversal_on_nothing_can_match_t)(void* context_ptr, Eterm* ret);

/* Called for each match result.
 *      context_ptr: Pointer to context
 *      slot_ix: Current slot index
 *      current_ptr_ptr: Triple pointer to either the bucket or the 'next' pointer in the previous element;
 *                       can be (carefully) used to adjust iteration when deleting or replacing elements.
 *      match_res: The result of running the match program against the current term.
 *
 * Should return 1 for successful match, 0 otherwise.
 */
typedef int (*mtraversal_on_match_res_t)(void* context_ptr, Sint slot_ix, HashDbTerm*** current_ptr_ptr,
                                         Eterm match_res);

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
typedef int (*mtraversal_on_loop_ended_t)(void* context_ptr, Sint slot_ix, Sint got,
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
typedef int (*mtraversal_on_trap_t)(void* context_ptr, Sint slot_ix, Sint got, Binary** mpp, Eterm* ret);

/*
 * Begin hash table match traversal
 */
static int match_traverse(Process* p, DbTableHash* tb,
                          Eterm pattern,
                          extra_match_validator_t extra_match_validator, /* Optional */
                          Sint chunk_size,      /* If 0, no chunking */
                          Sint iterations_left, /* Nr. of iterations left */
                          Eterm** hpp,          /* Heap */
                          int lock_for_write,   /* Set to 1 if we're going to delete or
                                                   modify existing terms */
                          mtraversal_on_nothing_can_match_t on_nothing_can_match,
                          mtraversal_on_match_res_t on_match_res,
                          mtraversal_on_loop_ended_t on_loop_ended,
                          mtraversal_on_trap_t on_trap,
                          void* context_ptr, /* State for callbacks above */
                          Eterm* ret)
{
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
        ret_value = on_nothing_can_match(context_ptr, ret);
        goto done;
    }

    if (mpi.all_objects) {
        mpi.mp->intern.flags |= BIN_FLAG_ALL_OBJECTS;
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
                ret_value = on_loop_ended(context_ptr, slot_ix, got, iterations_left, &mpi.mp, ret);
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
            if ((*current_ptr)->hvalue != INVALID_HASH) {
                match_res = db_match_dbterm(&tb->common, p, mpi.mp, 0,
                                            &(*current_ptr)->dbterm, hpp, 2);
                saved_current = *current_ptr;
                if (on_match_res(context_ptr, slot_ix, &current_ptr, match_res)) {
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
                ret_value = on_loop_ended(context_ptr, -1, got, iterations_left, &mpi.mp, ret);
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
            if (iterations_left <= 0 || MBUF(p)) {
                /*
                 * We have either reached our limit, or just created some heap fragments.
                 * Since many heap fragments will make the GC slower, trap and GC now.
                 */
                unlock_hash_function(lck);
                ret_value = on_trap(context_ptr, slot_ix, got, &mpi.mp, ret);
                goto done;
            }
            current_ptr = &BUCKET(tb,slot_ix);
        }
    }

    ret_value = on_loop_ended(context_ptr, slot_ix, got, iterations_left, &mpi.mp, ret);

done:
    /* We should only jump directly to this label if
     * we've already called on_nothing_can_match / on_loop_ended / on_trap
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
static int match_traverse_continue(Process* p, DbTableHash* tb,
                                   Sint chunk_size,      /* If 0, no chunking */
                                   Sint iterations_left, /* Nr. of iterations left */
                                   Eterm** hpp,          /* Heap */
                                   Sint slot_ix,         /* Slot index to resume traversal from */
                                   Sint got,             /* Matched terms counter */
                                   Binary** mpp,         /* Existing match program */
                                   int lock_for_write,   /* Set to 1 if we're going to delete or
                                                            modify existing terms */
                                   mtraversal_on_match_res_t on_match_res,
                                   mtraversal_on_loop_ended_t on_loop_ended,
                                   mtraversal_on_trap_t on_trap,
                                   void* context_ptr, /* For callbacks */
                                   Eterm* ret)
{
    int all_objects = (*mpp)->intern.flags & BIN_FLAG_ALL_OBJECTS;
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
        ret_value = on_loop_ended(context_ptr, slot_ix, got, iterations_left, mpp, ret);
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
            if ((*current_ptr)->hvalue != INVALID_HASH) {
                match_res = db_match_dbterm(&tb->common, p, *mpp, all_objects,
                                            &(*current_ptr)->dbterm, hpp, 2);
                saved_current = *current_ptr;
                if (on_match_res(context_ptr, slot_ix, &current_ptr, match_res)) {
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
            if (iterations_left <= 0 || MBUF(p)) {
                /*
                 * We have either reached our limit, or just created some heap fragments.
                 * Since many heap fragments will make the GC slower, trap and GC now.
                 */
                unlock_hash_function(lck);
                ret_value = on_trap(context_ptr, slot_ix, got, mpp, ret);
                goto done;
            }
            current_ptr = &BUCKET(tb,slot_ix);
        }
    }

    ret_value = on_loop_ended(context_ptr, slot_ix, got, iterations_left, mpp, ret);

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

static ERTS_INLINE int on_mtraversal_simple_trap(Export* trap_function,
                                                 Process* p,
                                                 DbTableHash* tb,
                                                 Eterm tid,
                                                 Eterm* prev_continuation_tptr,
                                                 Sint slot_ix,
                                                 Sint got,
                                                 Binary** mpp,
                                                 Eterm* ret)
{
    Eterm* hp;
    Eterm egot;
    Eterm mpb;
    Eterm continuation;
    int is_first_trap = (prev_continuation_tptr == NULL);
    size_t base_halloc_sz = (is_first_trap ? ERTS_MAGIC_REF_THING_SIZE : 0);

    BUMP_ALL_REDS(p);
    if (IS_USMALL(0, got)) {
	hp = HAlloc(p,  base_halloc_sz + 5);
	egot = make_small(got);
    }
    else {
	hp = HAlloc(p, base_halloc_sz + BIG_UINT_HEAP_SIZE + 5);
	egot = uint_to_big(got, hp);
	hp += BIG_UINT_HEAP_SIZE;
    }

    if (is_first_trap) {
        mpb = erts_db_make_match_prog_ref(p, *mpp, &hp);
        *mpp = NULL; /* otherwise the caller will destroy it */
    }
    else {
        mpb = prev_continuation_tptr[3];
    }

    continuation = TUPLE4(
            hp,
            tid,
            make_small(slot_ix),
            mpb,
            egot);
    *ret = bif_trap1(trap_function, p, continuation);
    return DB_ERROR_NONE;
}

static ERTS_INLINE int unpack_simple_mtraversal_continuation(Eterm continuation,
                                                             Eterm** tptr_ptr,
                                                             Eterm* tid_ptr,
                                                             Sint* slot_ix_p,
                                                             Binary** mpp,
                                                             Sint* got_p)
{
    Eterm* tptr;
    ASSERT(is_tuple(continuation));
    tptr = tuple_val(continuation);
    if (arityval(*tptr) != 4)
        return 1;

    if (! is_small(tptr[2]) || !(is_big(tptr[4]) || is_small(tptr[4]))) {
        return 1;
    }

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
    return 0;
}


/*
 *
 * select / select_chunk match traversal
 *
 */

#define MAX_SELECT_CHUNK_ITERATIONS 1000

typedef struct {
    Process* p;
    DbTableHash* tb;
    Eterm tid;
    Eterm* hp;
    Sint chunk_size;
    Eterm match_list;
    Eterm* prev_continuation_tptr;
} mtraversal_select_chunk_context_t;

static int mtraversal_select_chunk_on_nothing_can_match(void* context_ptr, Eterm* ret) {
    mtraversal_select_chunk_context_t* sc_context_ptr = (mtraversal_select_chunk_context_t*) context_ptr;
    *ret = (sc_context_ptr->chunk_size > 0 ? am_EOT : NIL);
    return DB_ERROR_NONE;
}

static int mtraversal_select_chunk_on_match_res(void* context_ptr, Sint slot_ix,
                                                HashDbTerm*** current_ptr_ptr,
                                                Eterm match_res)
{
    mtraversal_select_chunk_context_t* sc_context_ptr = (mtraversal_select_chunk_context_t*) context_ptr;
    if (is_value(match_res)) {
        sc_context_ptr->match_list = CONS(sc_context_ptr->hp, match_res, sc_context_ptr->match_list);
        return 1;
    }
    return 0;
}

static int mtraversal_select_chunk_on_loop_ended(void* context_ptr, Sint slot_ix, Sint got,
                                                 Sint iterations_left, Binary** mpp, Eterm* ret)
{
    mtraversal_select_chunk_context_t* sc_context_ptr = (mtraversal_select_chunk_context_t*) context_ptr;
    Eterm mpb;

    if (iterations_left == MAX_SELECT_CHUNK_ITERATIONS) {
        /* We didn't get to iterate a single time, which means EOT */
        ASSERT(sc_context_ptr->match_list == NIL);
        *ret = (sc_context_ptr->chunk_size > 0 ? am_EOT : NIL);
        return DB_ERROR_NONE;
    }
    else {
        ASSERT(iterations_left < MAX_SELECT_CHUNK_ITERATIONS);
        BUMP_REDS(sc_context_ptr->p, MAX_SELECT_CHUNK_ITERATIONS - iterations_left);
        if (sc_context_ptr->chunk_size) {
            Eterm continuation;
            Eterm rest = NIL;
            Sint rest_size = 0;

            if (got > sc_context_ptr->chunk_size) { /* Split list in return value and 'rest' */
                Eterm tmp = sc_context_ptr->match_list;
                rest = sc_context_ptr->match_list;
                while (got-- > sc_context_ptr->chunk_size + 1) {
                    tmp = CDR(list_val(tmp));
                    ++rest_size;
                }
                ++rest_size;
                sc_context_ptr->match_list = CDR(list_val(tmp));
                CDR(list_val(tmp)) = NIL; /* Destructive, the list has never
                                             been in 'user space' */
            }
            if (rest != NIL || slot_ix >= 0) { /* Need more calls */
                sc_context_ptr->hp = HAlloc(sc_context_ptr->p, 3 + 7 + ERTS_MAGIC_REF_THING_SIZE);
                mpb = erts_db_make_match_prog_ref(sc_context_ptr->p, *mpp, &sc_context_ptr->hp);
                continuation = TUPLE6(
                        sc_context_ptr->hp,
                        sc_context_ptr->tid,
                        make_small(slot_ix),
                        make_small(sc_context_ptr->chunk_size),
                        mpb, rest,
                        make_small(rest_size));
                *mpp = NULL; /* Otherwise the caller will destroy it */
                sc_context_ptr->hp += 7;
                *ret = TUPLE2(sc_context_ptr->hp, sc_context_ptr->match_list, continuation);
                return DB_ERROR_NONE;
            } else { /* All data is exhausted */
                if (sc_context_ptr->match_list != NIL) { /* No more data to search but still a
                                                            result to return to the caller */
                    sc_context_ptr->hp = HAlloc(sc_context_ptr->p, 3);
                    *ret = TUPLE2(sc_context_ptr->hp, sc_context_ptr->match_list, am_EOT);
                    return DB_ERROR_NONE;
                } else { /* Reached the end of the ttable with no data to return */
                    *ret = am_EOT;
                    return DB_ERROR_NONE;
                }
            }
        }
        *ret = sc_context_ptr->match_list;
        return DB_ERROR_NONE;
    }
}

static int mtraversal_select_chunk_on_trap(void* context_ptr, Sint slot_ix, Sint got,
                                           Binary** mpp, Eterm* ret)
{
    mtraversal_select_chunk_context_t* sc_context_ptr = (mtraversal_select_chunk_context_t*) context_ptr;
    Eterm mpb;
    Eterm continuation;
    Eterm* hp;

    BUMP_ALL_REDS(sc_context_ptr->p);

    if (sc_context_ptr->prev_continuation_tptr == NULL) {
        /* First time we're trapping */
        hp = HAlloc(sc_context_ptr->p, 7 + ERTS_MAGIC_REF_THING_SIZE);
        mpb = erts_db_make_match_prog_ref(sc_context_ptr->p, *mpp, &hp);
        continuation = TUPLE6(
                hp,
                sc_context_ptr->tid,
                make_small(slot_ix),
                make_small(sc_context_ptr->chunk_size),
                mpb,
                sc_context_ptr->match_list,
                make_small(got));
        *mpp = NULL; /* otherwise the caller will destroy it */
    }
    else {
        /* Not the first time we're trapping; reuse continuation terms */
        hp = HAlloc(sc_context_ptr->p, 7);
        continuation = TUPLE6(
                hp,
                sc_context_ptr->prev_continuation_tptr[1],
                make_small(slot_ix),
                sc_context_ptr->prev_continuation_tptr[3],
                sc_context_ptr->prev_continuation_tptr[4],
                sc_context_ptr->match_list,
                make_small(got));
    }
    *ret = bif_trap1(&ets_select_continue_exp, sc_context_ptr->p, continuation);
    return DB_ERROR_NONE;
}

static int db_select_hash(Process *p, DbTable *tbl, Eterm tid, Eterm pattern, int reverse, Eterm *ret) {
    return db_select_chunk_hash(p, tbl, tid, pattern, 0, reverse, ret);
}

static int db_select_chunk_hash(Process *p, DbTable *tbl, Eterm tid, Eterm pattern, Sint chunk_size,
                                int reverse, Eterm *ret)
{
    mtraversal_select_chunk_context_t sc_context;
    sc_context.p = p;
    sc_context.tb = &tbl->hash;
    sc_context.tid = tid;
    sc_context.hp = NULL;
    sc_context.chunk_size = chunk_size;
    sc_context.match_list = NIL;
    sc_context.prev_continuation_tptr = NULL;

    return match_traverse(
            sc_context.p, sc_context.tb,
            pattern, NULL,
            sc_context.chunk_size,
            MAX_SELECT_CHUNK_ITERATIONS,
            &sc_context.hp, 0,
            mtraversal_select_chunk_on_nothing_can_match,
            mtraversal_select_chunk_on_match_res,
            mtraversal_select_chunk_on_loop_ended,
            mtraversal_select_chunk_on_trap,
            &sc_context, ret);
}

/*
 *
 * select_continue match traversal
 *
 */

static int mtraversal_select_chunk_continue_on_loop_ended(void* context_ptr, Sint slot_ix, Sint got,
                                                          Sint iterations_left, Binary** mpp, Eterm* ret)
{
    mtraversal_select_chunk_context_t* sc_context_ptr = (mtraversal_select_chunk_context_t*) context_ptr;
    Eterm continuation;
    Eterm rest = NIL;
    Eterm* hp;

    ASSERT(iterations_left <= MAX_SELECT_CHUNK_ITERATIONS);
    BUMP_REDS(sc_context_ptr->p, MAX_SELECT_CHUNK_ITERATIONS - iterations_left);
    if (sc_context_ptr->chunk_size) {
        Sint rest_size = 0;
        if (got > sc_context_ptr->chunk_size) {
            /* Cannot write destructively here,
               the list may have
               been in user space */
            hp = HAlloc(sc_context_ptr->p, (got - sc_context_ptr->chunk_size) * 2);
            while (got-- > sc_context_ptr->chunk_size) {
                rest = CONS(hp, CAR(list_val(sc_context_ptr->match_list)), rest);
                hp += 2;
                sc_context_ptr->match_list = CDR(list_val(sc_context_ptr->match_list));
                ++rest_size;
            }
        }
        if (rest != NIL || slot_ix >= 0) {
            hp = HAlloc(sc_context_ptr->p, 3 + 7);
            continuation = TUPLE6(
                    hp,
                    sc_context_ptr->prev_continuation_tptr[1],
                    make_small(slot_ix),
                    sc_context_ptr->prev_continuation_tptr[3],
                    sc_context_ptr->prev_continuation_tptr[4],
                    rest,
                    make_small(rest_size));
            hp += 7;
            *ret = TUPLE2(hp, sc_context_ptr->match_list, continuation);
            return DB_ERROR_NONE;
        } else {
            if (sc_context_ptr->match_list != NIL) {
                hp = HAlloc(sc_context_ptr->p, 3);
                *ret = TUPLE2(hp, sc_context_ptr->match_list, am_EOT);
                return DB_ERROR_NONE;
            } else {
                *ret = am_EOT;
                return DB_ERROR_NONE;
            }
        }
    }
    *ret = sc_context_ptr->match_list;
    return DB_ERROR_NONE;
}

/*
 * This is called when select traps
 */
static int db_select_continue_hash(Process* p, DbTable* tbl, Eterm continuation, Eterm* ret) {
    mtraversal_select_chunk_context_t sc_context = {0};
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

    if (arityval(*tptr) != 6)
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
    sc_context.p = p;
    sc_context.tb = &tbl->hash;
    sc_context.tid = tid;
    sc_context.hp = NULL;
    sc_context.chunk_size = chunk_size;
    sc_context.match_list = match_list;
    sc_context.prev_continuation_tptr = tptr;

    return match_traverse_continue(
            sc_context.p, sc_context.tb, sc_context.chunk_size,
            iterations_left, &sc_context.hp, slot_ix, got, &mp, 0,
            mtraversal_select_chunk_on_match_res, /* Reuse callback */
            mtraversal_select_chunk_continue_on_loop_ended,
            mtraversal_select_chunk_on_trap,      /* Reuse callback */
            &sc_context, ret);

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

typedef struct {
    Process* p;
    DbTableHash* tb;
    Eterm tid;
    Eterm* hp;
    Eterm* prev_continuation_tptr;
} mtraversal_select_count_context_t;

static int mtraversal_select_count_on_nothing_can_match(void* context_ptr, Eterm* ret) {
    *ret = make_small(0);
    return DB_ERROR_NONE;
}

static int mtraversal_select_count_on_match_res(void* context_ptr, Sint slot_ix,
                                                HashDbTerm*** current_ptr_ptr,
                                                Eterm match_res)
{
    return (match_res == am_true);
}

static int mtraversal_select_count_on_loop_ended(void* context_ptr, Sint slot_ix, Sint got,
                                                 Sint iterations_left, Binary** mpp, Eterm* ret)
{
    mtraversal_select_count_context_t* scnt_context_ptr = (mtraversal_select_count_context_t*) context_ptr;
    ASSERT(iterations_left <= MAX_SELECT_COUNT_ITERATIONS);
    BUMP_REDS(scnt_context_ptr->p, MAX_SELECT_COUNT_ITERATIONS - iterations_left);
    *ret = erts_make_integer(got, scnt_context_ptr->p);
    return DB_ERROR_NONE;
}

static int mtraversal_select_count_on_trap(void* context_ptr, Sint slot_ix, Sint got,
                                           Binary** mpp, Eterm* ret)
{
    mtraversal_select_count_context_t* scnt_context_ptr = (mtraversal_select_count_context_t*) context_ptr;
    return on_mtraversal_simple_trap(
            &ets_select_count_continue_exp,
            scnt_context_ptr->p,
            scnt_context_ptr->tb,
            scnt_context_ptr->tid,
            scnt_context_ptr->prev_continuation_tptr,
            slot_ix, got, mpp, ret);
}

static int db_select_count_hash(Process *p, DbTable *tbl, Eterm tid, Eterm pattern, Eterm *ret) {
    mtraversal_select_count_context_t scnt_context = {0};
    Sint iterations_left = MAX_SELECT_COUNT_ITERATIONS;
    Sint chunk_size = 0;

    scnt_context.p = p;
    scnt_context.tb = &tbl->hash;
    scnt_context.tid = tid;
    scnt_context.hp = NULL;
    scnt_context.prev_continuation_tptr = NULL;

    return match_traverse(
            scnt_context.p, scnt_context.tb,
            pattern, NULL,
            chunk_size, iterations_left, NULL, 0,
            mtraversal_select_count_on_nothing_can_match,
            mtraversal_select_count_on_match_res,
            mtraversal_select_count_on_loop_ended,
            mtraversal_select_count_on_trap,
            &scnt_context, ret);
}

/*
 * This is called when select_count traps
 */
static int db_select_count_continue_hash(Process* p, DbTable* tbl, Eterm continuation, Eterm* ret) {
    mtraversal_select_count_context_t scnt_context = {0};
    Eterm* tptr;
    Eterm tid;
    Binary* mp;
    Sint got;
    Sint slot_ix;
    Sint chunk_size = 0;
    *ret = NIL;

    if (unpack_simple_mtraversal_continuation(continuation, &tptr, &tid, &slot_ix, &mp, &got)) {
        *ret = NIL;
        return DB_ERROR_BADPARAM;
    }

    scnt_context.p = p;
    scnt_context.tb = &tbl->hash;
    scnt_context.tid = tid;
    scnt_context.hp = NULL;
    scnt_context.prev_continuation_tptr = tptr;

    return match_traverse_continue(
            scnt_context.p, scnt_context.tb, chunk_size,
            MAX_SELECT_COUNT_ITERATIONS,
            NULL, slot_ix, got, &mp, 0,
            mtraversal_select_count_on_match_res,  /* Reuse callback */
            mtraversal_select_count_on_loop_ended, /* Reuse callback */
            mtraversal_select_count_on_trap,       /* Reuse callback */
            &scnt_context, ret);
}

#undef MAX_SELECT_COUNT_ITERATIONS


/*
 *
 * select_delete match traversal
 *
 */

#define MAX_SELECT_DELETE_ITERATIONS 1000

typedef struct {
    Process* p;
    DbTableHash* tb;
    Eterm tid;
    Eterm* hp;
    Eterm* prev_continuation_tptr;
    erts_aint_t fixated_by_me;
    Uint last_pseudo_delete;
} mtraversal_select_delete_context_t;

static int mtraversal_select_delete_on_nothing_can_match(void* context_ptr, Eterm* ret) {
    *ret = make_small(0);
    return DB_ERROR_NONE;
}

static int mtraversal_select_delete_on_match_res(void* context_ptr, Sint slot_ix,
                                                 HashDbTerm*** current_ptr_ptr,
                                                 Eterm match_res)
{
    HashDbTerm** current_ptr = *current_ptr_ptr;
    mtraversal_select_delete_context_t* sd_context_ptr = (mtraversal_select_delete_context_t*) context_ptr;
    HashDbTerm* del;
    if (match_res != am_true)
        return 0;

    if (NFIXED(sd_context_ptr->tb) > sd_context_ptr->fixated_by_me) { /* fixated by others? */
        if (slot_ix != sd_context_ptr->last_pseudo_delete) {
            if (!add_fixed_deletion(sd_context_ptr->tb, slot_ix, sd_context_ptr->fixated_by_me))
                goto do_erase;
            sd_context_ptr->last_pseudo_delete = slot_ix;
        }
        (*current_ptr)->hvalue = INVALID_HASH;
    }
    else {
    do_erase:
        del = *current_ptr;
        *current_ptr = (*current_ptr)->next; // replace pointer to term using next
        free_term(sd_context_ptr->tb, del);
    }
    erts_atomic_dec_nob(&sd_context_ptr->tb->common.nitems);

    return 1;
}

static int mtraversal_select_delete_on_loop_ended(void* context_ptr, Sint slot_ix, Sint got,
                                                  Sint iterations_left, Binary** mpp, Eterm* ret)
{
    mtraversal_select_delete_context_t* sd_context_ptr = (mtraversal_select_delete_context_t*) context_ptr;
    ASSERT(iterations_left <= MAX_SELECT_DELETE_ITERATIONS);
    BUMP_REDS(sd_context_ptr->p, MAX_SELECT_DELETE_ITERATIONS - iterations_left);
    if (got) {
	try_shrink(sd_context_ptr->tb);
    }
    *ret = erts_make_integer(got, sd_context_ptr->p);
    return DB_ERROR_NONE;
}

static int mtraversal_select_delete_on_trap(void* context_ptr, Sint slot_ix, Sint got,
                                            Binary** mpp, Eterm* ret)
{
    mtraversal_select_delete_context_t* sd_context_ptr = (mtraversal_select_delete_context_t*) context_ptr;
    return on_mtraversal_simple_trap(
            &ets_select_delete_continue_exp,
            sd_context_ptr->p,
            sd_context_ptr->tb,
            sd_context_ptr->tid,
            sd_context_ptr->prev_continuation_tptr,
            slot_ix, got, mpp, ret);
}

static int db_select_delete_hash(Process *p, DbTable *tbl, Eterm tid, Eterm pattern, Eterm *ret) {
    mtraversal_select_delete_context_t sd_context = {0};
    Sint chunk_size = 0;

    sd_context.p = p;
    sd_context.tb = &tbl->hash;
    sd_context.tid = tid;
    sd_context.hp = NULL;
    sd_context.prev_continuation_tptr = NULL;
    sd_context.fixated_by_me = sd_context.tb->common.is_thread_safe ? 0 : 1; /* TODO: something nicer */
    sd_context.last_pseudo_delete = (Uint) -1;

    return match_traverse(
            sd_context.p, sd_context.tb,
            pattern, NULL,
            chunk_size,
            MAX_SELECT_DELETE_ITERATIONS, NULL, 1,
            mtraversal_select_delete_on_nothing_can_match,
            mtraversal_select_delete_on_match_res,
            mtraversal_select_delete_on_loop_ended,
            mtraversal_select_delete_on_trap,
            &sd_context, ret);
}

/*
 * This is called when select_delete traps
 */
static int db_select_delete_continue_hash(Process* p, DbTable* tbl, Eterm continuation, Eterm* ret) {
    mtraversal_select_delete_context_t sd_context = {0};
    Eterm* tptr;
    Eterm tid;
    Binary* mp;
    Sint got;
    Sint slot_ix;
    Sint chunk_size = 0;

    if (unpack_simple_mtraversal_continuation(continuation, &tptr, &tid, &slot_ix, &mp, &got)) {
        *ret = NIL;
        return DB_ERROR_BADPARAM;
    }

    sd_context.p = p;
    sd_context.tb = &tbl->hash;
    sd_context.tid = tid;
    sd_context.hp = NULL;
    sd_context.prev_continuation_tptr = tptr;
    sd_context.fixated_by_me = ONLY_WRITER(p, sd_context.tb) ? 0 : 1; /* TODO: something nicer */
    sd_context.last_pseudo_delete = (Uint) -1;

    return match_traverse_continue(
            sd_context.p, sd_context.tb, chunk_size,
            MAX_SELECT_DELETE_ITERATIONS,
            NULL, slot_ix, got, &mp, 1,
            mtraversal_select_delete_on_match_res,  /* Reuse callback */
            mtraversal_select_delete_on_loop_ended, /* Reuse callback */
            mtraversal_select_delete_on_trap,       /* Reuse callback */
            &sd_context, ret);
}

#undef MAX_SELECT_DELETE_ITERATIONS


/*
 *
 * select_replace match traversal
 *
 */

#define MAX_SELECT_REPLACE_ITERATIONS 1000

typedef struct {
    Process* p;
    DbTableHash* tb;
    Eterm tid;
    Eterm* hp;
    Eterm* prev_continuation_tptr;
} mtraversal_select_replace_context_t;

static int mtraversal_select_replace_on_nothing_can_match(void* context_ptr, Eterm* ret) {
    *ret = make_small(0);
    return DB_ERROR_NONE;
}

static int mtraversal_select_replace_on_match_res(void* context_ptr, Sint slot_ix,
                                                  HashDbTerm*** current_ptr_ptr,
                                                  Eterm match_res)
{
    mtraversal_select_replace_context_t* sr_context_ptr = (mtraversal_select_replace_context_t*) context_ptr;
    DbTableHash* tb = sr_context_ptr->tb;
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
        free_term(tb, **current_ptr_ptr);
        **current_ptr_ptr = new; /* replace 'next' pointer in previous object */
        *current_ptr_ptr = &((**current_ptr_ptr)->next); /* advance to next object */
        return 1;
    }
    return 0;
}

static int mtraversal_select_replace_on_loop_ended(void* context_ptr, Sint slot_ix, Sint got,
                                                   Sint iterations_left, Binary** mpp, Eterm* ret)
{
    mtraversal_select_replace_context_t* sr_context_ptr = (mtraversal_select_replace_context_t*) context_ptr;
    ASSERT(iterations_left <= MAX_SELECT_REPLACE_ITERATIONS);
    /* the more objects we've replaced, the more reductions we've consumed */
    BUMP_REDS(sr_context_ptr->p,
              MIN(MAX_SELECT_REPLACE_ITERATIONS * 2,
                  (MAX_SELECT_REPLACE_ITERATIONS - iterations_left) + (int)got));
    *ret = erts_make_integer(got, sr_context_ptr->p);
    return DB_ERROR_NONE;
}

static int mtraversal_select_replace_on_trap(void* context_ptr, Sint slot_ix, Sint got,
                                             Binary** mpp, Eterm* ret)
{
    mtraversal_select_replace_context_t* sr_context_ptr = (mtraversal_select_replace_context_t*) context_ptr;
    return on_mtraversal_simple_trap(
            &ets_select_replace_continue_exp,
            sr_context_ptr->p,
            sr_context_ptr->tb,
            sr_context_ptr->tid,
            sr_context_ptr->prev_continuation_tptr,
            slot_ix, got, mpp, ret);
}

static int db_select_replace_hash(Process *p, DbTable *tbl, Eterm tid, Eterm pattern, Eterm *ret)
{
    mtraversal_select_replace_context_t sr_context = {0};
    Sint chunk_size = 0;

    /* Bag implementation presented both semantic consistency and performance issues,
     * unsupported for now
     */
    ASSERT(!(tbl->hash.common.status & DB_BAG));

    sr_context.p = p;
    sr_context.tb = &tbl->hash;
    sr_context.tid = tid;
    sr_context.hp = NULL;
    sr_context.prev_continuation_tptr = NULL;

    return match_traverse(
            sr_context.p, sr_context.tb,
            pattern, db_match_keeps_key,
            chunk_size,
            MAX_SELECT_REPLACE_ITERATIONS, NULL, 1,
            mtraversal_select_replace_on_nothing_can_match,
            mtraversal_select_replace_on_match_res,
            mtraversal_select_replace_on_loop_ended,
            mtraversal_select_replace_on_trap,
            &sr_context, ret);
}

/*
 * This is called when select_replace traps
 */
static int db_select_replace_continue_hash(Process* p, DbTable* tbl, Eterm continuation, Eterm* ret)
{
    mtraversal_select_replace_context_t sr_context = {0};
    Eterm* tptr;
    Eterm tid ;
    Binary* mp;
    Sint got;
    Sint slot_ix;
    Sint chunk_size = 0;
    *ret = NIL;

    if (unpack_simple_mtraversal_continuation(continuation, &tptr, &tid, &slot_ix, &mp, &got)) {
        *ret = NIL;
        return DB_ERROR_BADPARAM;
    }

    /* Proceed */
    sr_context.p = p;
    sr_context.tb = &tbl->hash;
    sr_context.tid = tid;
    sr_context.hp = NULL;
    sr_context.prev_continuation_tptr = tptr;

    return match_traverse_continue(
            sr_context.p, sr_context.tb, chunk_size,
            MAX_SELECT_REPLACE_ITERATIONS,
            NULL, slot_ix, got, &mp, 1,
            mtraversal_select_replace_on_match_res,  /* Reuse callback */
            mtraversal_select_replace_on_loop_ended, /* Reuse callback */
            mtraversal_select_replace_on_trap,       /* Reuse callback */
            &sr_context, ret);
}


static int db_take_hash(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashDbTerm **bp, *b;
    HashValue hval = MAKE_HASH(key);
    erts_rwmtx_t *lck = WLOCK_HASH(tb, hval);
    int ix = hash_to_ix(tb, hval);
    int nitems_diff = 0;

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
                    b->hvalue = INVALID_HASH;
                    b = b->next;
                } else {
                    *bp = b->next;
                    free_term(tb, b);
                    b = *bp;
                }
            }
            break;
        }
    }
    WUNLOCK_HASH(lck);
    if (nitems_diff) {
        erts_atomic_add_nob(&tb->common.nitems, nitems_diff);
        try_shrink(tb);
    }
    return DB_ERROR_NONE;
}


/*
** Other interface routines (not directly coupled to one bif)
*/

void db_initialize_hash(void)
{
}


int db_mark_all_deleted_hash(DbTable *tbl)
{
    DbTableHash *tb = &tbl->hash;
    HashDbTerm* list;
    int i;

    ERTS_LC_ASSERT(IS_TAB_WLOCKED(tb));

    for (i = 0; i < NACTIVE(tb); i++) {
	if ((list = BUCKET(tb,i)) != NULL) {
	    add_fixed_deletion(tb, i, 0);
	    do {
		list->hvalue = INVALID_HASH;
		list = list->next;
	    }while(list != NULL);
	}
    }
    erts_atomic_set_nob(&tb->common.nitems, 0);
    return DB_ERROR_NONE;
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
		if (list->hvalue == INVALID_HASH)
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

/* release all memory occupied by a single table */
static int db_free_table_hash(DbTable *tbl)
{
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
	erts_db_free(ERTS_ALC_T_DB_FIX_DEL,
		     (DbTable *) tb,
		     (void *) fx,
		     sizeof(FixedDeletion));
	ERTS_ETS_MISC_MEM_ADD(-sizeof(FixedDeletion));
	if (--reds < 0) {
	    erts_atomic_set_relb(&tb->fixdel, (erts_aint_t)fixdel);
	    return reds;		/* Not done */
	}
    }
    erts_atomic_set_relb(&tb->fixdel, (erts_aint_t)NULL);

    while(tb->nslots != 0) {
	reds -= EXT_SEGSZ/64 + free_seg(tb, 1);

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
    ASSERT(erts_atomic_read_nob(&tb->common.memory_size) == sizeof(DbTable));
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
    mpi->all_objects = 1;
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
	    mpi->all_objects = 0;
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
    sys_memset(segtab[seg_ix], 0, SIZEOF_SEGMENT(EXT_SEGSZ));
    tb->nslots += EXT_SEGSZ;
}

static void dealloc_ext_segtab(void* lop_data)
{
    struct ext_segtab* est = (struct ext_segtab*) lop_data;

    erts_free(ERTS_ALC_T_DB_SEG, est);
}

/* Shrink table by freeing the top segment
** free_records: 1=free any records in segment, 0=assume segment is empty 
*/
static int free_seg(DbTableHash *tb, int free_records)
{
    const int seg_ix = SLOT_IX_TO_SEG_IX(tb->nslots) - 1;
    struct segment** const segtab = SEGTAB(tb);
    struct segment* const segp = segtab[seg_ix];
    Uint seg_sz;
    int nrecords = 0;

    ASSERT(segp != NULL);
#ifndef DEBUG
    if (free_records)
#endif
    {	
	int i = (seg_ix == 0) ? FIRST_SEGSZ : EXT_SEGSZ;
	while (i--) {
	    HashDbTerm* p = segp->buckets[i];
	    while(p != 0) {		
		HashDbTerm* nxt = p->next;
		ASSERT(free_records); /* segment not empty as assumed? */
		free_term(tb, p);
		p = nxt;
		++nrecords;
	    }
	}
    }
    
    if (seg_ix >= NSEG_1) {
        struct ext_segtab* est = ErtsContainerStruct_(segtab,struct ext_segtab,segtab);

        if (seg_ix == est->prev_nsegs) { /* Dealloc extended segtab */
            ASSERT(est->prev_segtab != NULL);
            SET_SEGTAB(tb, est->prev_segtab);
            tb->nsegs = est->prev_nsegs;

            if (!tb->common.is_thread_safe) {
                /*
                 * Table is doing a graceful shrink operation and we must avoid
                 * deallocating this segtab while it may still be read by other
                 * threads. Schedule deallocation with thread progress to make
                 * sure no lingering threads are still hanging in BUCKET macro
                 * with an old segtab pointer.
                 */
                Uint sz = SIZEOF_EXT_SEGTAB(est->nsegs);
                ASSERT(sz == ERTS_ALC_DBG_BLK_SZ(est));
                ERTS_DB_ALC_MEM_UPDATE_(tb, sz, 0);
                erts_schedule_thr_prgr_later_cleanup_op(dealloc_ext_segtab,
                                                        est,
                                                        &est->lop,
                                                        sz);
            }
            else
                erts_db_free(ERTS_ALC_T_DB_SEG, (DbTable*)tb, est,
                             SIZEOF_EXT_SEGTAB(est->nsegs));
        }
    }
    seg_sz = (seg_ix == 0) ? FIRST_SEGSZ : EXT_SEGSZ;
    erts_db_free(ERTS_ALC_T_DB_SEG, (DbTable *)tb, segp, SIZEOF_SEGMENT(seg_sz));
    
#ifdef DEBUG
    if (seg_ix < tb->nsegs)
        SEGTAB(tb)[seg_ix] = NULL;
#endif
    tb->nslots -= seg_sz;
    ASSERT(tb->nslots >= 0);
    return nrecords;
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
	    if (ptr->hvalue != INVALID_HASH)
		sz += ptr->dbterm.size + 2;
	    ptr = ptr->next;
	}
    }

    hp = HAlloc(p, sz);
    hend = hp + sz;

    ptr = ptr1;
    while(ptr != ptr2) {
	if (ptr->hvalue != INVALID_HASH) {
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
    if (DB_USING_FINE_LOCKING(tb))
	return !erts_atomic_xchg_acqb(&tb->is_resizing, 1);
    else
        ERTS_LC_ASSERT(erts_lc_rwmtx_is_rwlocked(&tb->common.rwlock));
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
        while (p != NULL) {
            if (p->hvalue == INVALID_HASH) { /* rare but possible with fine locking */
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
    HashDbTerm** src_bp;
    HashDbTerm** dst_bp;
    HashDbTerm** bp;
    erts_rwmtx_t* lck;
    int src_ix, dst_ix, low_szm;
    int nactive;
    int loop_limit = 5;

    do {
        if (!begin_resizing(tb))
            return; /* already in progress */
        nactive = NACTIVE(tb);
        if (!(nactive > FIRST_SEGSZ && nitems < SHRINK_LIMIT(nactive))) {
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

        src_bp = &BUCKET(tb, src_ix);
        dst_bp = &BUCKET(tb, dst_ix);
        bp = src_bp;

        /*
         * We join lists by appending "dst" at the end of "src"
         * as we must step through "src" anyway to purge pseudo deleted.
         */
        while(*bp != NULL) {
            if ((*bp)->hvalue == INVALID_HASH) {
                HashDbTerm* deleted = *bp;
                *bp = deleted->next;
                free_term(tb, deleted);
            } else {
                bp = &(*bp)->next;
            }
        }
        *bp = *dst_bp;
        *dst_bp = *src_bp;
        *src_bp = NULL;

        nactive = src_ix;
        erts_atomic_set_nob(&tb->nactive, nactive);
        if (dst_ix == 0) {
            erts_atomic_set_relb(&tb->szm, low_szm);
        }
        WUNLOCK_HASH(lck);

        if (tb->nslots - src_ix >= EXT_SEGSZ) {
            free_seg(tb, 0);
        }
        done_resizing(tb);

    } while (--loop_limit
             && nactive > FIRST_SEGSZ && nitems < SHRINK_LIMIT(nactive));
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
static HashDbTerm* next(DbTableHash *tb, Uint *iptr, erts_rwmtx_t** lck_ptr,
			HashDbTerm *list)
{
    int i;

    ERTS_LC_ASSERT(IS_HASH_RLOCKED(tb,*iptr));

    for (list = list->next; list != NULL; list = list->next) {
	if (list->hvalue != INVALID_HASH)
	    return list;        
    }

    i = *iptr;
    while ((i=next_slot(tb, i, lck_ptr)) != 0) {

	list = BUCKET(tb,i);
	while (list != NULL) {
	    if (list->hvalue != INVALID_HASH) {
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
    erts_rwmtx_t* lck;
    int flags = 0;

    ASSERT(tb->common.status & DB_SET);

    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb, hval);
    bp = &BUCKET(tb, hash_to_ix(tb, hval));
    b = *bp;

    for (;;) {
        if (b == NULL) {
            break;
        }
        if (has_key(tb, b, key, hval)) {
            if (b->hvalue != INVALID_HASH) {
                goto Ldone;
            }
            break;
        }
        bp = &b->next;
        b = *bp;
    }

    if (obj == THE_NON_VALUE) {
        WUNLOCK_HASH(lck);
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
            q->next = NULL;
            *bp = b = q;
            flags |= DB_INC_TRY_GROW;
        } else {
            HashDbTerm *q, *next = b->next;

            ASSERT(b->hvalue == INVALID_HASH);
            q = replace_dbterm(tb, b, obj);
            q->next = next;
            q->hvalue = hval;
            *bp = b = q;
            erts_atomic_inc_nob(&tb->common.nitems);
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
    handle->lck = lck;
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
    erts_rwmtx_t* lck = (erts_rwmtx_t*) handle->lck;
    HashDbTerm* free_me = NULL;

    ERTS_LC_ASSERT(IS_HASH_WLOCKED(tb, lck));  /* locked by db_lookup_dbterm_hash */

    ASSERT((&b->dbterm == handle->dbterm) == !(tb->common.compress && handle->flags & DB_MUST_RESIZE));

    if (handle->flags & DB_NEW_OBJECT && cret != DB_ERROR_NONE) {
        if (IS_FIXED(tb) && add_fixed_deletion(tb, hash_to_ix(tb, b->hvalue),
                                               0)) {
            b->hvalue = INVALID_HASH;
        } else {
            *bp = b->next;
            free_me = b;
        }

        WUNLOCK_HASH(lck);
        erts_atomic_dec_nob(&tb->common.nitems);
        try_shrink(tb);
    } else {
        if (handle->flags & DB_MUST_RESIZE) {
            db_finalize_resize(handle, offsetof(HashDbTerm,dbterm));
            free_me = b;
        }
        if (handle->flags & DB_INC_TRY_GROW) {
            int nactive;
            int nitems = erts_atomic_inc_read_nob(&tb->common.nitems);
            WUNLOCK_HASH(lck);
            nactive = NACTIVE(tb);

            if (nitems > GROW_LIMIT(nactive) && !IS_FIXED(tb)) {
                grow(tb, nitems);
            }
        } else {
            WUNLOCK_HASH(lck);
        }
    }

    if (free_me)
        free_term(tb, free_me);

#ifdef DEBUG
    handle->dbterm = 0;
#endif
    return;
}

static int db_delete_all_objects_hash(Process* p, DbTable* tbl)
{
    if (IS_FIXED(tbl)) {
	db_mark_all_deleted_hash(tbl);
    } else {
	db_free_table_hash(tbl);
	db_create_hash(p, tbl);
	erts_atomic_set_nob(&tbl->hash.common.nitems, 0);
    }
    return 0;
}

void db_foreach_offheap_hash(DbTable *tbl,
			     void (*func)(ErlOffHeap *, void *),
			     void * arg)
{
    DbTableHash *tb = &tbl->hash;
    HashDbTerm* list;
    int i;
    int nactive = NACTIVE(tb);
    
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
            if (b->hvalue == INVALID_HASH)
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

/* For testing only */
Eterm erts_ets_hash_sizeof_ext_segtab(void)
{
    return make_small(((SIZEOF_EXT_SEGTAB(0)-1) / sizeof(UWord)) + 1);
}

#ifdef ERTS_ENABLE_LOCK_COUNT
void erts_lcnt_enable_db_hash_lock_count(DbTableHash *tb, int enable) {
    int i;

    if(tb->locks == NULL) {
        return;
    }

    for(i = 0; i < DB_HASH_LOCK_CNT; i++) {
        erts_lcnt_ref_t *ref = &tb->locks->lck_vec[i].lck.lcnt;

        if(enable) {
            erts_lcnt_install_new_lock_info(ref, "db_hash_slot", tb->common.the_name,
                ERTS_LOCK_TYPE_RWMUTEX | ERTS_LOCK_FLAGS_CATEGORY_DB);
        } else {
            erts_lcnt_uninstall(ref);
        }
    }
}
#endif /* ERTS_ENABLE_LOCK_COUNT */
