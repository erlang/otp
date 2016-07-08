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
** a higher level of atomicy.
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

#ifdef MYDEBUG /* Will fail test case ets_SUITE:memory */
#  define IF_DEBUG(x) x
#  define MY_ASSERT(x) ASSERT(x)
#else
#  define IF_DEBUG(x)
#  define MY_ASSERT(x)
#endif

/* 
 * The following symbols can be manipulated to "tune" the linear hash array 
 */
#define CHAIN_LEN 6                 /* Medium bucket chain len      */

/* Number of slots per segment */
#define SEGSZ_EXP  8
#define SEGSZ   (1 << SEGSZ_EXP)
#define SEGSZ_MASK (SEGSZ-1)

#define NSEG_1     2 /* Size of first segment table (must be at least 2) */ 
#define NSEG_2   256 /* Size of second segment table */
#define NSEG_INC 128 /* Number of segments to grow after that */

#ifdef ERTS_SMP
#  define DB_USING_FINE_LOCKING(TB) (((TB))->common.type & DB_FINE_LOCKED)
#else
#  define DB_USING_FINE_LOCKING(TB) 0
#endif

#ifdef ETHR_ORDERED_READ_DEPEND
#define SEGTAB(tb) ((struct segment**) erts_smp_atomic_read_nob(&(tb)->segtab))
#else
#define SEGTAB(tb)							\
    (DB_USING_FINE_LOCKING(tb)						\
     ? ((struct segment**) erts_smp_atomic_read_ddrb(&(tb)->segtab))	\
     : ((struct segment**) erts_smp_atomic_read_nob(&(tb)->segtab)))
#endif
#define NACTIVE(tb) ((int)erts_smp_atomic_read_nob(&(tb)->nactive))
#define NITEMS(tb) ((int)erts_smp_atomic_read_nob(&(tb)->common.nitems))

#define BUCKET(tb, i) SEGTAB(tb)[(i) >> SEGSZ_EXP]->buckets[(i) & SEGSZ_MASK]

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
		 ? erts_smp_atomic_read_acqb(&tb->szm)
		 : erts_smp_atomic_read_nob(&tb->szm));
    Uint ix = hval & mask; 
    if (ix >= erts_smp_atomic_read_nob(&tb->nactive)) {
	ix &= mask>>1;
	ASSERT(ix < erts_smp_atomic_read_nob(&tb->nactive));
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
    was_next = erts_smp_atomic_read_acqb(&tb->fixdel);
    do { /* Lockless atomic insertion in linked list: */
        if (NFIXED(tb) <= fixated_by_me) {
            erts_db_free(ERTS_ALC_T_DB_FIX_DEL, (DbTable*)tb,
                         fixd, sizeof(FixedDeletion));
            return 0; /* raced by unfixer */
        }
        exp_next = was_next;
	fixd->next = (FixedDeletion*) exp_next;
	was_next = erts_smp_atomic_cmpxchg_mb(&tb->fixdel,
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
      make_internal_hash(term)) % MAX_HASH)

#ifdef ERTS_SMP
#  define DB_HASH_LOCK_MASK (DB_HASH_LOCK_CNT-1)
#  define GET_LOCK(tb,hval) (&(tb)->locks->lck_vec[(hval) & DB_HASH_LOCK_MASK].lck)

/* Fine grained read lock */
static ERTS_INLINE erts_smp_rwmtx_t* RLOCK_HASH(DbTableHash* tb, HashValue hval)
{
    if (tb->common.is_thread_safe) {
	return NULL;
    } else {
	erts_smp_rwmtx_t* lck = GET_LOCK(tb,hval);
	ASSERT(tb->common.type & DB_FINE_LOCKED);
	erts_smp_rwmtx_rlock(lck);
	return lck;
    }
}
/* Fine grained write lock */
static ERTS_INLINE erts_smp_rwmtx_t* WLOCK_HASH(DbTableHash* tb, HashValue hval)
{
    if (tb->common.is_thread_safe) {
	return NULL;
    } else {
	erts_smp_rwmtx_t* lck = GET_LOCK(tb,hval);
	ASSERT(tb->common.type & DB_FINE_LOCKED);
	erts_smp_rwmtx_rwlock(lck);
	return lck;
    }
}

static ERTS_INLINE void RUNLOCK_HASH(erts_smp_rwmtx_t* lck)
{
    if (lck != NULL) {
	erts_smp_rwmtx_runlock(lck);
    }
}

static ERTS_INLINE void WUNLOCK_HASH(erts_smp_rwmtx_t* lck)
{
    if (lck != NULL) {
	erts_smp_rwmtx_rwunlock(lck);
    }
}
#else /* ERTS_SMP */
# define RLOCK_HASH(tb,hval) NULL 
# define WLOCK_HASH(tb,hval) NULL
# define RUNLOCK_HASH(lck) ((void)lck) 
# define WUNLOCK_HASH(lck) ((void)lck)
#endif /* ERTS_SMP */


#ifdef ERTS_ENABLE_LOCK_CHECK
#  define IFN_EXCL(tb,cmd) (((tb)->common.is_thread_safe) || (cmd))
#  define IS_HASH_RLOCKED(tb,hval) IFN_EXCL(tb,erts_smp_lc_rwmtx_is_rlocked(GET_LOCK(tb,hval)))
#  define IS_HASH_WLOCKED(tb,lck) IFN_EXCL(tb,erts_smp_lc_rwmtx_is_rwlocked(lck))
#  define IS_TAB_WLOCKED(tb) erts_smp_lc_rwmtx_is_rwlocked(&(tb)->common.rwlock)
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
				  erts_smp_rwmtx_t** lck_ptr)
{
#ifdef ERTS_SMP
    ix += DB_HASH_LOCK_CNT;
    if (ix < NACTIVE(tb)) return ix;
    RUNLOCK_HASH(*lck_ptr);
    ix = (ix + 1) & DB_HASH_LOCK_MASK;
    if (ix != 0) *lck_ptr = RLOCK_HASH(tb,ix);
    return ix;
#else
    return (++ix < NACTIVE(tb)) ? ix : 0;
#endif
}
/* Same as next_slot but with WRITE locking */
static ERTS_INLINE Sint next_slot_w(DbTableHash* tb, Uint ix,
				    erts_smp_rwmtx_t** lck_ptr)
{
#ifdef ERTS_SMP
    ix += DB_HASH_LOCK_CNT;
    if (ix < NACTIVE(tb)) return ix;
    WUNLOCK_HASH(*lck_ptr);
    ix = (ix + 1) & DB_HASH_LOCK_MASK;
    if (ix != 0) *lck_ptr = WLOCK_HASH(tb,ix);
    return ix;
#else
    return next_slot(tb,ix,lck_ptr);
#endif
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
    HashDbTerm* buckets[SEGSZ];
#ifdef MYDEBUG
    int is_ext_segment;
#endif
};

/* A segment that also contains a segment table */
struct ext_segment {    
    struct segment s; /* The segment itself. Must be first */
	
    struct segment** prev_segtab;  /* Used when table is shrinking */
    int nsegs;                     /* Size of segtab */
    struct segment* segtab[1];     /* The segment table */
};
#define SIZEOF_EXTSEG(NSEGS) \
    (offsetof(struct ext_segment,segtab) + sizeof(struct segment*)*(NSEGS))

#if defined(DEBUG) || defined(VALGRIND)
#  define EXTSEG(SEGTAB_PTR) \
    ((struct ext_segment*) (((char*)(SEGTAB_PTR)) - offsetof(struct ext_segment,segtab)))
#endif


static ERTS_INLINE void SET_SEGTAB(DbTableHash* tb,
				   struct segment** segtab)
{
    if (DB_USING_FINE_LOCKING(tb))
	erts_smp_atomic_set_wb(&tb->segtab, (erts_aint_t) segtab);
    else
	erts_smp_atomic_set_nob(&tb->segtab, (erts_aint_t) segtab);
#ifdef VALGRIND
    tb->top_ptr_to_segment_with_active_segtab = EXTSEG(segtab);
#endif
}


/* How the table segments relate to each other:

    ext_segment:                      ext_segment:              "plain" segment
   #=================#                #================#        #=============#
   | bucket[0]       |<--+   +------->| bucket[256]    |     +->| bucket[512] |
   | bucket[1]       |   |   |        |       [257]    |     |  |       [513] |
   :                 :   |   |        :                :     |  :             :
   | bucket[255]     |   |   |        |       [511]    |     |  |       [767] |
   |-----------------|   |   |        |----------------|     |  #=============#
   | prev_segtab=NULL|   |   |   +--<---prev_segtab    |     |
   | nsegs = 2       |   |   |   |    | nsegs = 256    |     |
+->| segtab[0] -->-------+---|---|--<---segtab[0]      |<-+  |
|  | segtab[1] -->-----------+---|--<---segtab[1]      |  |  |
|  #=================#           |    | segtab[2] -->-----|--+    ext_segment:         
|                                |    :                :  |      #================#
+----------------<---------------+    | segtab[255] ->----|----->| bucket[255*256]| 
                                      #================#  |      |                | 
                                                          |      :                :
                                                          |      |----------------| 
                                                          +----<---prev_segtab    | 
                                                                 :                :
*/


/*
** Forward decl's (static functions)
*/
static struct ext_segment* alloc_ext_seg(DbTableHash* tb, unsigned seg_ix,
					 struct segment** old_segtab);
static int alloc_seg(DbTableHash *tb);
static int free_seg(DbTableHash *tb, int free_records);
static HashDbTerm* next(DbTableHash *tb, Uint *iptr, erts_smp_rwmtx_t** lck_ptr,
			HashDbTerm *list);
static HashDbTerm* search_list(DbTableHash* tb, Eterm key, 
			       HashValue hval, HashDbTerm *list);
static void shrink(DbTableHash* tb, int nactive);
static void grow(DbTableHash* tb, int nactive);
static Eterm build_term_list(Process* p, HashDbTerm* ptr1, HashDbTerm* ptr2,
			   Uint sz, DbTableHash*);
static int analyze_pattern(DbTableHash *tb, Eterm pattern, 
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

static int db_select_chunk_hash(Process *p, DbTable *tbl, 
				Eterm pattern, Sint chunk_size,
				int reverse, Eterm *ret);
static int db_select_hash(Process *p, DbTable *tbl, 
			  Eterm pattern, int reverse, Eterm *ret);
static int db_select_count_hash(Process *p, DbTable *tbl, 
				Eterm pattern, Eterm *ret);
static int db_select_delete_hash(Process *p, DbTable *tbl, 
				 Eterm pattern, Eterm *ret);

static int db_select_continue_hash(Process *p, DbTable *tbl, 
				   Eterm continuation, Eterm *ret);

static int db_select_count_continue_hash(Process *p, DbTable *tbl, 
					 Eterm continuation, Eterm *ret);

static int db_select_delete_continue_hash(Process *p, DbTable *tbl,
					  Eterm continuation, Eterm *ret);
static int db_take_hash(Process *, DbTable *, Eterm, Eterm *);
static void db_print_hash(int to,
			  void *to_arg,
			  int show,
			  DbTable *tbl);
static int db_free_table_hash(DbTable *tbl);

static int db_free_table_continue_hash(DbTable *tbl);


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
    if (nactive > SEGSZ && NITEMS(tb) < (nactive * CHAIN_LEN)
	&& !IS_FIXED(tb)) {
	shrink(tb, nactive);
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
    db_take_hash,
    db_delete_all_objects_hash,
    db_free_table_hash,
    db_free_table_continue_hash,
    db_print_hash,
    db_foreach_offheap_hash,
#ifdef HARDDEBUG
    db_check_table_hash,
#else
    NULL,
#endif
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
    if (erts_smp_atomic_cmpxchg_relb(&tb->fixdel,
				     (erts_aint_t) fixdel,
				     (erts_aint_t) NULL) != (erts_aint_t) NULL) {
	/* Oboy, must join lists */    
	FixedDeletion* last = fixdel;
	erts_aint_t was_tail;
	erts_aint_t exp_tail;

	while (last->next != NULL) last = last->next;
	was_tail = erts_smp_atomic_read_acqb(&tb->fixdel);
	do { /* Lockless atomic list insertion */
	    exp_tail = was_tail;
	    last->next = (FixedDeletion*) exp_tail;
	    /*++tries;*/
	    DEBUG_WAIT();
	    was_tail = erts_smp_atomic_cmpxchg_relb(&tb->fixdel,
						    (erts_aint_t) fixdel,
						    exp_tail);
	}while (was_tail != exp_tail);
    }
    /*erts_fprintf(stderr,"erl_db_hash: restore_fixdel tries=%d\r\n", tries);*/
}
/*
** Table interface routines ie what's called by the bif's 
*/

void db_unfix_table_hash(DbTableHash *tb)
{
    FixedDeletion* fixdel;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_rwmtx_is_rwlocked(&tb->common.rwlock)
		       || (erts_smp_lc_rwmtx_is_rlocked(&tb->common.rwlock)
			   && !tb->common.is_thread_safe));
restart:
    fixdel = (FixedDeletion*) erts_smp_atomic_xchg_mb(&tb->fixdel,
                                                      (erts_aint_t) NULL);
    while (fixdel != NULL) {
	FixedDeletion *fx = fixdel;
	int ix = fx->slot;
	HashDbTerm **bp;
	HashDbTerm *b;
	erts_smp_rwmtx_t* lck = WLOCK_HASH(tb,ix);

	if (IS_FIXED(tb)) { /* interrupted by fixer */
	    WUNLOCK_HASH(lck);
	    restore_fixdel(tb,fixdel);
	    if (!IS_FIXED(tb)) {
		goto restart; /* unfixed again! */
	    }
	    return;
	}
	if (ix < NACTIVE(tb)) {
	    bp = &BUCKET(tb, ix);
	    b = *bp;
	    
	    while (b != NULL) {
		if (b->hvalue == INVALID_HASH) {
		    *bp = b->next;
		    free_term(tb, b);
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
    }

    /* ToDo: Maybe try grow/shrink the table as well */
}

int db_create_hash(Process *p, DbTable *tbl)
{
    DbTableHash *tb = &tbl->hash;

    erts_smp_atomic_init_nob(&tb->szm, SEGSZ_MASK);
    erts_smp_atomic_init_nob(&tb->nactive, SEGSZ);
    erts_smp_atomic_init_nob(&tb->fixdel, (erts_aint_t)NULL);
    erts_smp_atomic_init_nob(&tb->segtab, (erts_aint_t)NULL);
    SET_SEGTAB(tb, alloc_ext_seg(tb,0,NULL)->segtab);
    tb->nsegs = NSEG_1;
    tb->nslots = SEGSZ;

    erts_smp_atomic_init_nob(&tb->is_resizing, 0);
#ifdef ERTS_SMP
    if (tb->common.type & DB_FINE_LOCKED) {
	erts_smp_rwmtx_opt_t rwmtx_opt = ERTS_SMP_RWMTX_OPT_DEFAULT_INITER;
	int i;
	if (tb->common.type & DB_FREQ_READ)
	    rwmtx_opt.type = ERTS_SMP_RWMTX_TYPE_FREQUENT_READ;
	if (erts_ets_rwmtx_spin_count >= 0)
	    rwmtx_opt.main_spincount = erts_ets_rwmtx_spin_count;
	tb->locks = (DbTableHashFineLocks*) erts_db_alloc_fnf(ERTS_ALC_T_DB_SEG, /* Other type maybe? */ 
							      (DbTable *) tb,
							      sizeof(DbTableHashFineLocks));	    	    
	for (i=0; i<DB_HASH_LOCK_CNT; ++i) {
	    erts_smp_rwmtx_init_opt_x(&tb->locks->lck_vec[i].lck, &rwmtx_opt,
				      "db_hash_slot", make_small(i));
	}
	/* This important property is needed to guarantee that the buckets
    	 * involved in a grow/shrink operation it protected by the same lock:
	 */
	ASSERT(erts_smp_atomic_read_nob(&tb->nactive) % DB_HASH_LOCK_CNT == 0);
    }
    else { /* coarse locking */
	tb->locks = NULL;
    }
    ERTS_THR_MEMORY_BARRIER;
#endif /* ERST_SMP */
    return DB_ERROR_NONE;
}

static int db_first_hash(Process *p, DbTable *tbl, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    Uint ix = 0;
    erts_smp_rwmtx_t* lck = RLOCK_HASH(tb,ix);
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
    erts_smp_rwmtx_t* lck;

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
    erts_smp_rwmtx_t* lck;
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
	    erts_smp_atomic_inc_nob(&tb->common.nitems);
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
		    erts_smp_atomic_inc_nob(&tb->common.nitems);
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
    nitems = erts_smp_atomic_inc_read_nob(&tb->common.nitems);
    WUNLOCK_HASH(lck);
    {
	int nactive = NACTIVE(tb);       
	if (nitems > nactive * (CHAIN_LEN+1) && !IS_FIXED(tb)) {
	    grow(tb, nactive);
	}
    }
    CHECK_TABLES();
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
    CHECK_TABLES();
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
    erts_smp_rwmtx_t* lck;

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

int db_get_element_array(DbTable *tbl, 
			 Eterm key,
			 int ndex, 
			 Eterm *ret,
			 int *num_ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    HashDbTerm* b1;
    int num = 0;
    int retval;
    erts_smp_rwmtx_t* lck;

    ASSERT(!IS_FIXED(tbl)); /* no support for fixed tables here */

    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, hval);
    b1 = BUCKET(tb, ix);

    while(b1 != 0) {
	if (has_live_key(tb,b1,key,hval)) {
	    if (tb->common.status & (DB_BAG | DB_DUPLICATE_BAG)) {
		HashDbTerm* b;
		HashDbTerm* b2 = b1->next;

		while(b2 != NULL && has_live_key(tb,b2,key,hval)) {
		    if (ndex > arityval(b2->dbterm.tpl[0])) {
			retval = DB_ERROR_BADITEM;
			goto done;
		    }
		    b2 = b2->next;
		}

		b = b1;
		while(b != b2) {
		    if (num < *num_ret) {
			ret[num++] = b->dbterm.tpl[ndex];
		    } else {
			retval = DB_ERROR_NONE;
			goto done;
		    }
		    b = b->next;
		}
		*num_ret = num;
	    }
	    else {
		ASSERT(*num_ret > 0);
		ret[0] = b1->dbterm.tpl[ndex];
		*num_ret = 1;
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
    
    
static int db_member_hash(DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    HashDbTerm* b1;
    erts_smp_rwmtx_t* lck;

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
    erts_smp_rwmtx_t* lck;
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
 * Very internal interface, removes elements of arity two from 
 * BAG. Used for the PID meta table
 */
int db_erase_bag_exact2(DbTable *tbl, Eterm key, Eterm value)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    HashDbTerm** bp;
    HashDbTerm* b;
    erts_smp_rwmtx_t* lck;
    int found = 0;

    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb,hval);
    ix = hash_to_ix(tb, hval);
    bp = &BUCKET(tb, ix);
    b = *bp;

    ASSERT(!IS_FIXED(tb));
    ASSERT((tb->common.status & DB_BAG));
    ASSERT(!tb->common.compress);

    while(b != 0) {
	if (has_live_key(tb,b,key,hval)) {
	    found = 1;
	    if ((arityval(b->dbterm.tpl[0]) == 2) && 
		EQ(value, b->dbterm.tpl[2])) {
		*bp = b->next;
		free_term(tb, b);
		erts_smp_atomic_dec_nob(&tb->common.nitems);
		b = *bp;
		break;
	    }
	} else if (found) {
		break;
	}
	bp = &b->next;
	b = b->next;
    }
    WUNLOCK_HASH(lck);
    if (found) {
	try_shrink(tb);
    }
    return DB_ERROR_NONE;
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
    erts_smp_rwmtx_t* lck;
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
	erts_smp_atomic_add_nob(&tb->common.nitems, nitems_diff);
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
    erts_smp_rwmtx_t* lck;
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
	erts_smp_atomic_add_nob(&tb->common.nitems, nitems_diff);
	try_shrink(tb);
    }
    *ret = am_true;
    return DB_ERROR_NONE;
}    


static int db_slot_hash(Process *p, DbTable *tbl, Eterm slot_term, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    erts_smp_rwmtx_t* lck;
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
 * Continue collecting select matches, this may happen either due to a trap
 * or when the user calls ets:select/1
 */
static int db_select_continue_hash(Process *p, 
				   DbTable *tbl, 
				   Eterm continuation, 
				   Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    Sint slot_ix; 
    Sint save_slot_ix;
    Sint chunk_size;
    int all_objects;
    Binary *mp;
    int num_left = 1000;
    HashDbTerm *current = 0;
    Eterm match_list;
    Eterm *hp;
    Eterm match_res;
    Sint got;
    Eterm *tptr;
    erts_smp_rwmtx_t* lck;

#define RET_TO_BIF(Term, State) do { *ret = (Term); return State; } while(0);

    /* Decode continuation. We know it's a tuple but not the arity or anything else */

    tptr = tuple_val(continuation);

    if (arityval(*tptr) != 6)
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    
    if (!is_small(tptr[2]) || !is_small(tptr[3]) || !is_binary(tptr[4]) || 
	!(is_list(tptr[5]) || tptr[5] == NIL) || !is_small(tptr[6]))
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    if ((chunk_size = signed_val(tptr[3])) < 0)
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    if (!(thing_subtag(*binary_val(tptr[4])) == REFC_BINARY_SUBTAG))
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    mp = ((ProcBin *) binary_val(tptr[4]))->val;
    if (!IsMatchProgBinary(mp))
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    all_objects = mp->flags & BIN_FLAG_ALL_OBJECTS;
    match_list = tptr[5];
    if ((got = signed_val(tptr[6])) < 0)
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);

    slot_ix = signed_val(tptr[2]);
    if (slot_ix < 0 /* EOT */ 
	|| (chunk_size && got >= chunk_size)) {       
	goto done; /* Already got all or enough in the match_list */
    }

    lck = RLOCK_HASH(tb,slot_ix);
    if (slot_ix >= NACTIVE(tb)) {
	RUNLOCK_HASH(lck);	
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    }

    while ((current = BUCKET(tb,slot_ix)) == NULL) {
	slot_ix = next_slot(tb, slot_ix, &lck);
	if (slot_ix == 0) {
	    slot_ix = -1; /* EOT */
	    goto done;	   
	}
    }	  
    for(;;) {
	if (current->hvalue != INVALID_HASH && 
	    (match_res = db_match_dbterm(&tb->common, p, mp, all_objects,
					 &current->dbterm, &hp, 2),
	     is_value(match_res))) {

	    match_list = CONS(hp, match_res, match_list);
	    ++got;
	}

	--num_left;
	save_slot_ix = slot_ix;
	if ((current = next(tb, (Uint*)&slot_ix, &lck, current)) == NULL) {
	    slot_ix = -1; /* EOT */
	    break;
	}
	if (slot_ix != save_slot_ix) { 
	    if (chunk_size && got >= chunk_size) {
		RUNLOCK_HASH(lck);
		break;
	    }    
	    if (num_left <= 0 || MBUF(p)) {
		/*
		 * We have either reached our limit, or just created some heap fragments.
		 * Since many heap fragments will make the GC slower, trap and GC now.
		 */
		RUNLOCK_HASH(lck);
		goto trap;
	    }
	}
    }
done:
    BUMP_REDS(p, 1000 - num_left);
    if (chunk_size) {
	Eterm continuation;
	Eterm rest = NIL;
	Sint rest_size = 0;

	if (got > chunk_size) { /* Cannot write destructively here, 
				   the list may have 
				   been in user space */
	    rest = NIL;
	    hp = HAlloc(p, (got - chunk_size) * 2); 
	    while (got-- > chunk_size) {
		rest = CONS(hp, CAR(list_val(match_list)), rest);
		hp += 2;
		match_list = CDR(list_val(match_list));
		++rest_size;
	    }
	}
	if (rest != NIL || slot_ix >= 0) {
	    hp = HAlloc(p,3+7);
	    continuation = TUPLE6(hp, tptr[1], make_small(slot_ix), 
				  tptr[3], tptr[4], rest, 
				  make_small(rest_size));
	    hp += 7;
	    RET_TO_BIF(TUPLE2(hp, match_list, continuation),DB_ERROR_NONE);
	} else {
	    if (match_list != NIL) {
		hp = HAlloc(p, 3);
		RET_TO_BIF(TUPLE2(hp, match_list, am_EOT),DB_ERROR_NONE);
	    } else {
		RET_TO_BIF(am_EOT, DB_ERROR_NONE);
	    }
	}
    }
    RET_TO_BIF(match_list,DB_ERROR_NONE);

trap:
    BUMP_ALL_REDS(p);

    hp = HAlloc(p,7);
    continuation = TUPLE6(hp, tptr[1], make_small(slot_ix), tptr[3],
			  tptr[4], match_list, make_small(got));
    RET_TO_BIF(bif_trap1(&ets_select_continue_exp, p, 
			 continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF

}

static int db_select_hash(Process *p, DbTable *tbl, 
			  Eterm pattern, int reverse,
			  Eterm *ret)
{
    return db_select_chunk_hash(p, tbl, pattern, 0, reverse, ret);
}

static int db_select_chunk_hash(Process *p, DbTable *tbl, 
				Eterm pattern, Sint chunk_size, 
				int reverse, /* not used */
				Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    struct mp_info mpi;
    Sint slot_ix;
    HashDbTerm *current = 0;
    unsigned current_list_pos = 0;
    Eterm match_list;
    Eterm match_res;
    Eterm *hp;
    int num_left = 1000;
    Uint got = 0;
    Eterm continuation;
    int errcode;
    Eterm mpb;
    erts_smp_rwmtx_t* lck;


#define RET_TO_BIF(Term,RetVal) do {		\
	if (mpi.mp != NULL) {			\
	    erts_bin_free(mpi.mp);		\
	}					\
	if (mpi.lists != mpi.dlists) {		\
	    erts_free(ERTS_ALC_T_DB_SEL_LIST,	\
		      (void *) mpi.lists);	\
	}					\
	*ret = (Term);				\
	return RetVal;				\
    } while(0)


    if ((errcode = analyze_pattern(tb, pattern, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(NIL,errcode);
    }

    if (!mpi.something_can_match) {
	if (chunk_size) {
	    RET_TO_BIF(am_EOT, DB_ERROR_NONE); /* We're done */
	}  
	RET_TO_BIF(NIL, DB_ERROR_NONE);
	/* can't possibly match anything */
    }

    if (!mpi.key_given) {
    /* Run this code if pattern is variable or GETKEY(pattern)  */
    /* is a variable                                            */
	slot_ix = 0;
	lck = RLOCK_HASH(tb,slot_ix);
	for (;;) { 
	    ASSERT(slot_ix < NACTIVE(tb));
	    if ((current = BUCKET(tb,slot_ix)) != NULL) {
		break;
	    }
	    slot_ix = next_slot(tb,slot_ix,&lck);
	    if (slot_ix == 0) {
		if (chunk_size) {
		    RET_TO_BIF(am_EOT, DB_ERROR_NONE); /* We're done */
		}  
		RET_TO_BIF(NIL,DB_ERROR_NONE); 
	    }
	}
    } else {
	/* We have at least one */
	slot_ix = mpi.lists[current_list_pos].ix;
	lck = RLOCK_HASH(tb, slot_ix);
	current = *(mpi.lists[current_list_pos].bucket);
	ASSERT(current == BUCKET(tb,slot_ix));
	++current_list_pos;
    }

    match_list = NIL;

    for(;;) {
	if (current != NULL) {
	    if (current->hvalue != INVALID_HASH) {
		match_res = db_match_dbterm(&tb->common, p, mpi.mp, 0,
					    &current->dbterm, &hp, 2);
		if (is_value(match_res)) {
		    match_list = CONS(hp, match_res, match_list);
		    ++got;
		}
	    }
	    current = current->next;
	}	
	else if (mpi.key_given) {  /* Key is bound */
	    RUNLOCK_HASH(lck);
	    if (current_list_pos == mpi.num_lists) {
		slot_ix = -1; /* EOT */
		goto done;
	    } else {
		slot_ix = mpi.lists[current_list_pos].ix;
		lck = RLOCK_HASH(tb, slot_ix);
		current = *(mpi.lists[current_list_pos].bucket);
		ASSERT(mpi.lists[current_list_pos].bucket == &BUCKET(tb,slot_ix));
		++current_list_pos;		       
	    }
	}
	else { /* Key is variable */
	    --num_left;

	    if ((slot_ix=next_slot(tb,slot_ix,&lck)) == 0) {
		slot_ix = -1;
		break;
	    }
	    if (chunk_size && got >= chunk_size) {
		RUNLOCK_HASH(lck);
		break;
	    }    
	    if (num_left <= 0 || MBUF(p)) {
		/*
		 * We have either reached our limit, or just created some heap fragments.
		 * Since many heap fragments will make the GC slower, trap and GC now.
		 */
		RUNLOCK_HASH(lck);
		goto trap;
	    }
	    current = BUCKET(tb,slot_ix);
        }
    }
done:
    BUMP_REDS(p, 1000 - num_left);
    if (chunk_size) {
	Eterm continuation;
	Eterm rest = NIL;
	Sint rest_size = 0;

	if (mpi.all_objects)
	    (mpi.mp)->flags |= BIN_FLAG_ALL_OBJECTS;
	if (got > chunk_size) { /* Split list in return value and 'rest' */
	    Eterm tmp = match_list;
	    rest = match_list;
	    while (got-- > chunk_size + 1) { 
		tmp = CDR(list_val(tmp));
		++rest_size;
	    }
	    ++rest_size;
	    match_list = CDR(list_val(tmp));
	    CDR(list_val(tmp)) = NIL; /* Destructive, the list has never 
					 been in 'user space' */ 
	}
	if (rest != NIL || slot_ix >= 0) { /* Need more calls */
	    hp = HAlloc(p,3+7+PROC_BIN_SIZE);
	    mpb =db_make_mp_binary(p,(mpi.mp),&hp);
	    if (mpi.all_objects)
		(mpi.mp)->flags |= BIN_FLAG_ALL_OBJECTS;
	    continuation = TUPLE6(hp, tb->common.id,make_small(slot_ix), 
				  make_small(chunk_size),  
				  mpb, rest, 
				  make_small(rest_size));
	    mpi.mp = NULL; /*otherwise the return macro will destroy it */
	    hp += 7;
	    RET_TO_BIF(TUPLE2(hp, match_list, continuation),DB_ERROR_NONE);
	} else { /* All data is exhausted */
	    if (match_list != NIL) { /* No more data to search but still a
					result to return to the caller */
		hp = HAlloc(p, 3);
		RET_TO_BIF(TUPLE2(hp, match_list, am_EOT),DB_ERROR_NONE);
	    } else { /* Reached the end of the ttable with no data to return */
		RET_TO_BIF(am_EOT, DB_ERROR_NONE);
	    }
	}
    }
    RET_TO_BIF(match_list,DB_ERROR_NONE);
trap:
    BUMP_ALL_REDS(p);
    if (mpi.all_objects)
	(mpi.mp)->flags |= BIN_FLAG_ALL_OBJECTS;
    hp = HAlloc(p,7+PROC_BIN_SIZE);
    mpb =db_make_mp_binary(p,(mpi.mp),&hp);
    continuation = TUPLE6(hp, tb->common.id, make_small(slot_ix), 
			  make_small(chunk_size), 
			  mpb, match_list, 
			  make_small(got));
    mpi.mp = NULL; /*otherwise the return macro will destroy it */
    RET_TO_BIF(bif_trap1(&ets_select_continue_exp, p, 
			 continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF

}

static int db_select_count_hash(Process *p, 
				DbTable *tbl, 
				Eterm pattern,
				Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    struct mp_info mpi;
    Uint slot_ix = 0;
    HashDbTerm* current = NULL;
    unsigned current_list_pos = 0;
    Eterm *hp;
    int num_left = 1000;
    Uint got = 0;
    Eterm continuation;
    int errcode;
    Eterm egot;
    Eterm mpb;
    erts_smp_rwmtx_t* lck;

#define RET_TO_BIF(Term,RetVal) do {		\
	if (mpi.mp != NULL) {			\
	    erts_bin_free(mpi.mp);		\
	}					\
	if (mpi.lists != mpi.dlists) {		\
	    erts_free(ERTS_ALC_T_DB_SEL_LIST,	\
		      (void *) mpi.lists);	\
	}					\
	*ret = (Term);				\
	return RetVal;				\
    } while(0)


    if ((errcode = analyze_pattern(tb, pattern, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(NIL,errcode);
    }

    if (!mpi.something_can_match) {
	RET_TO_BIF(make_small(0), DB_ERROR_NONE);
	/* can't possibly match anything */
    }

    if (!mpi.key_given) {
    /* Run this code if pattern is variable or GETKEY(pattern)  */
    /* is a variable                                            */      
	slot_ix = 0;
	lck = RLOCK_HASH(tb,slot_ix);
	current = BUCKET(tb,slot_ix);
    } else {
	/* We have at least one */
	slot_ix = mpi.lists[current_list_pos].ix;
	lck = RLOCK_HASH(tb, slot_ix);
	current = *(mpi.lists[current_list_pos].bucket);
	ASSERT(current == BUCKET(tb,slot_ix));
	++current_list_pos;
    }

    for(;;) {
	if (current != NULL) {
	    if (current->hvalue != INVALID_HASH) {
		if (db_match_dbterm(&tb->common, p, mpi.mp, 0,
				    &current->dbterm, NULL,0) == am_true) {
		    ++got;
		}
		--num_left;
	    }
	    current = current->next;
	}
	else { /* next bucket */
	    if (mpi.key_given) {  /* Key is bound */
		RUNLOCK_HASH(lck);
		if (current_list_pos == mpi.num_lists) {
		    goto done;
		} else {
		    slot_ix = mpi.lists[current_list_pos].ix;
		    lck = RLOCK_HASH(tb, slot_ix);
		    current = *(mpi.lists[current_list_pos].bucket);
		    ASSERT(mpi.lists[current_list_pos].bucket == &BUCKET(tb,slot_ix));
		    ++current_list_pos;		       
		}
	    }
	    else {
		if ((slot_ix=next_slot(tb,slot_ix,&lck)) == 0) {
		    goto done;
		}
		if (num_left <= 0) {
		    RUNLOCK_HASH(lck);
		    goto trap;
		}		
		current = BUCKET(tb,slot_ix);
	    }
	}
    }
done:
    BUMP_REDS(p, 1000 - num_left);
    RET_TO_BIF(erts_make_integer(got,p),DB_ERROR_NONE);
trap:
    BUMP_ALL_REDS(p);
    if (IS_USMALL(0, got)) {
	hp = HAlloc(p,  PROC_BIN_SIZE + 5);
	egot = make_small(got);
    }
    else {
	hp = HAlloc(p, BIG_UINT_HEAP_SIZE + PROC_BIN_SIZE + 5);
	egot = uint_to_big(got, hp);
	hp += BIG_UINT_HEAP_SIZE;
    }
    mpb = db_make_mp_binary(p,mpi.mp,&hp);
    continuation = TUPLE4(hp, tb->common.id, make_small(slot_ix), 
			  mpb, 
			  egot);
    mpi.mp = NULL; /*otherwise the return macro will destroy it */
    RET_TO_BIF(bif_trap1(&ets_select_count_continue_exp, p, 
			 continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF
}

static int db_select_delete_hash(Process *p,
				 DbTable *tbl,
				 Eterm pattern,
				 Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    struct mp_info mpi;
    Uint slot_ix = 0;
    HashDbTerm **current = NULL;
    unsigned current_list_pos = 0;
    Eterm *hp;
    int num_left = 1000;
    Uint got = 0;
    Eterm continuation;
    int errcode;
    Uint last_pseudo_delete = (Uint)-1;
    Eterm mpb;
    Eterm egot;
#ifdef ERTS_SMP
    erts_aint_t fixated_by_me = tb->common.is_thread_safe ? 0 : 1; /* ToDo: something nicer */
#else
    erts_aint_t fixated_by_me = 0;
#endif
    erts_smp_rwmtx_t* lck;

#define RET_TO_BIF(Term,RetVal) do {		\
	if (mpi.mp != NULL) {			\
	    erts_bin_free(mpi.mp);		\
	}					\
	if (mpi.lists != mpi.dlists) {		\
	    erts_free(ERTS_ALC_T_DB_SEL_LIST,	\
		      (void *) mpi.lists);	\
	}					\
	*ret = (Term);				\
	return RetVal;				\
    } while(0)


    if ((errcode = analyze_pattern(tb, pattern, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(NIL,errcode);
    }

    if (!mpi.something_can_match) {
	RET_TO_BIF(make_small(0), DB_ERROR_NONE);
	/* can't possibly match anything */
    }

    if (!mpi.key_given) {
	/* Run this code if pattern is variable or GETKEY(pattern)  */
	/* is a variable                                            */
	lck = WLOCK_HASH(tb,slot_ix);
	current = &BUCKET(tb,slot_ix);
    } else {
	/* We have at least one */
	slot_ix = mpi.lists[current_list_pos].ix;
	lck = WLOCK_HASH(tb, slot_ix);
	current = mpi.lists[current_list_pos++].bucket; 
	ASSERT(*current == BUCKET(tb,slot_ix));
    }


    for(;;) {
	if ((*current) == NULL) {
	    if (mpi.key_given) {  /* Key is bound */
		WUNLOCK_HASH(lck);
		if (current_list_pos == mpi.num_lists) {
		    goto done;
		} else {
		    slot_ix = mpi.lists[current_list_pos].ix;
		    lck = WLOCK_HASH(tb, slot_ix);
		    current = mpi.lists[current_list_pos].bucket;
		    ASSERT(mpi.lists[current_list_pos].bucket == &BUCKET(tb,slot_ix));
		    ++current_list_pos;
		}
	    } else {
		if ((slot_ix=next_slot_w(tb,slot_ix,&lck)) == 0) {
		    goto done;
		}
		if (num_left <= 0) {
		    WUNLOCK_HASH(lck);
		    goto trap;
		}		
		current = &BUCKET(tb,slot_ix);
	    } 
	}
	else if ((*current)->hvalue == INVALID_HASH) {
	    current = &((*current)->next);
	} 
	else {
	    int did_erase = 0;
	    if (db_match_dbterm(&tb->common, p, mpi.mp, 0,
				&(*current)->dbterm, NULL, 0) == am_true) {
                HashDbTerm *del;
		if (NFIXED(tb) > fixated_by_me) { /* fixated by others? */
		    if (slot_ix != last_pseudo_delete) {
                        if (!add_fixed_deletion(tb, slot_ix, fixated_by_me))
                            goto do_erase;
                        last_pseudo_delete = slot_ix;
		    }
		    (*current)->hvalue = INVALID_HASH;
		} else {
                do_erase:
		    del = *current;
		    *current = (*current)->next;
		    free_term(tb, del);
		    did_erase = 1;
		}
		erts_smp_atomic_dec_nob(&tb->common.nitems);
		++got;
	    }	    
	    --num_left;
	    if (!did_erase) {
		current = &((*current)->next);
	    }
	}
    }
done:
    BUMP_REDS(p, 1000 - num_left);
    if (got) {
	try_shrink(tb);
    }
    RET_TO_BIF(erts_make_integer(got,p),DB_ERROR_NONE);
trap:
    BUMP_ALL_REDS(p);
    if (IS_USMALL(0, got)) {
	hp = HAlloc(p,  PROC_BIN_SIZE + 5);
	egot = make_small(got);
    }
    else {
	hp = HAlloc(p, BIG_UINT_HEAP_SIZE + PROC_BIN_SIZE + 5);
	egot = uint_to_big(got, hp);
	hp += BIG_UINT_HEAP_SIZE;
    }
    mpb = db_make_mp_binary(p,mpi.mp,&hp);
    continuation = TUPLE4(hp, tb->common.id, make_small(slot_ix), 
			  mpb, 
			  egot);
    mpi.mp = NULL; /*otherwise the return macro will destroy it */
    RET_TO_BIF(bif_trap1(&ets_select_delete_continue_exp, p, 
			 continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF

}
/*
** This is called when select_delete traps
*/
static int db_select_delete_continue_hash(Process *p, 
					  DbTable *tbl,
					  Eterm continuation,
					  Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    Uint slot_ix;
    Uint last_pseudo_delete = (Uint)-1;
    HashDbTerm **current = NULL;
    Eterm *hp;
    int num_left = 1000;
    Uint got;
    Eterm *tptr;
    Binary *mp;
    Eterm egot;
    int fixated_by_me = ONLY_WRITER(p,tb) ? 0 : 1; /* ToDo: something nicer */
    erts_smp_rwmtx_t* lck;

#define RET_TO_BIF(Term,RetVal) do {		\
	*ret = (Term);				\
	return RetVal;				\
    } while(0)

    
    tptr = tuple_val(continuation);
    slot_ix = unsigned_val(tptr[2]);
    mp = ((ProcBin *) binary_val(tptr[3]))->val;
    if (is_big(tptr[4])) {
	got = big_to_uint32(tptr[4]);
    } else {
	got = unsigned_val(tptr[4]);
    }
    
    lck = WLOCK_HASH(tb,slot_ix);
    if (slot_ix >= NACTIVE(tb)) {
	WUNLOCK_HASH(lck);
	goto done;
    } 
    current = &BUCKET(tb,slot_ix);

    for(;;) {
	if ((*current) == NULL) {
	    if ((slot_ix=next_slot_w(tb,slot_ix,&lck)) == 0) {
		goto done;
	    }
	    if (num_left <= 0) {
		WUNLOCK_HASH(lck);
		goto trap;
	    }		
	    current = &BUCKET(tb,slot_ix);
	}
	else if ((*current)->hvalue == INVALID_HASH) {
	    current = &((*current)->next);
	} 
	else {
	    int did_erase = 0;
	    if (db_match_dbterm(&tb->common, p, mp, 0,
				&(*current)->dbterm, NULL, 0) == am_true) {
                HashDbTerm *del;
		if (NFIXED(tb) > fixated_by_me) { /* fixated by others? */
		    if (slot_ix != last_pseudo_delete) {
                        if (!add_fixed_deletion(tb, slot_ix, fixated_by_me))
                            goto do_erase;
			last_pseudo_delete = slot_ix;
		    }
		    (*current)->hvalue = INVALID_HASH;
		} else {
                do_erase:
		    del = *current;
		    *current = (*current)->next;
		    free_term(tb, del);
		    did_erase = 1;
		}
		erts_smp_atomic_dec_nob(&tb->common.nitems);
		++got;
	    }
	    
	    --num_left;
	    if (!did_erase) {
		current = &((*current)->next);
	    }
	}
    }
done:
    BUMP_REDS(p, 1000 - num_left);
    if (got) {
	try_shrink(tb);
    }
    RET_TO_BIF(erts_make_integer(got,p),DB_ERROR_NONE);
trap:
    BUMP_ALL_REDS(p);
    if (IS_USMALL(0, got)) {
	hp = HAlloc(p,  5);
	egot = make_small(got);
    }
    else {
	hp = HAlloc(p, BIG_UINT_HEAP_SIZE + 5);
	egot = uint_to_big(got, hp);
	hp += BIG_UINT_HEAP_SIZE;
    }
    continuation = TUPLE4(hp, tb->common.id, make_small(slot_ix), 
			  tptr[3], 
			  egot);
    RET_TO_BIF(bif_trap1(&ets_select_delete_continue_exp, p, 
			 continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF

}
    
/*
** This is called when select_count traps
*/
static int db_select_count_continue_hash(Process *p, 
					 DbTable *tbl,
					 Eterm continuation,
					 Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    Uint slot_ix;
    HashDbTerm* current;
    Eterm *hp;
    int num_left = 1000;
    Uint got;
    Eterm *tptr;
    Binary *mp;
    Eterm egot;
    erts_smp_rwmtx_t* lck;

#define RET_TO_BIF(Term,RetVal) do {		\
	*ret = (Term);				\
	return RetVal;				\
    } while(0)

    
    tptr = tuple_val(continuation);
    slot_ix = unsigned_val(tptr[2]);
    mp = ((ProcBin *) binary_val(tptr[3]))->val;
    if (is_big(tptr[4])) {
	got = big_to_uint32(tptr[4]);
    } else {
	got = unsigned_val(tptr[4]);
    }
    

    lck = RLOCK_HASH(tb, slot_ix);
    if (slot_ix >= NACTIVE(tb)) { /* Is this posible? */
	RUNLOCK_HASH(lck);
	goto done;
    }     
    current = BUCKET(tb,slot_ix);
    
    for(;;) {
	if (current != NULL) {
	    if (current->hvalue == INVALID_HASH) {
		current = current->next;
		continue;
	    }
	    if (db_match_dbterm(&tb->common, p, mp, 0, &current->dbterm,
				NULL, 0) == am_true) {
		++got;
	    }
	    --num_left;
	    current = current->next;
	}
	else { /* next bucket */
            if ((slot_ix = next_slot(tb,slot_ix,&lck)) == 0) {
		goto done;
	    }
	    if (num_left <= 0) {
		RUNLOCK_HASH(lck);
		goto trap;
	    }
	    current = BUCKET(tb,slot_ix);
	}
    }
done:
    BUMP_REDS(p, 1000 - num_left);
    RET_TO_BIF(erts_make_integer(got,p),DB_ERROR_NONE);
trap:
    BUMP_ALL_REDS(p);
    if (IS_USMALL(0, got)) {
	hp = HAlloc(p, 5);
	egot = make_small(got);
    }
    else {
	hp = HAlloc(p, BIG_UINT_HEAP_SIZE + 5);
	egot = uint_to_big(got, hp);
	hp += BIG_UINT_HEAP_SIZE;
    }
    continuation = TUPLE4(hp, tb->common.id, make_small(slot_ix), 
			  tptr[3], 
			  egot);
    RET_TO_BIF(bif_trap1(&ets_select_count_continue_exp, p, 
			 continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF

}
    
static int db_take_hash(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashDbTerm **bp, *b;
    HashValue hval = MAKE_HASH(key);
    erts_smp_rwmtx_t *lck = WLOCK_HASH(tb, hval);
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
        erts_smp_atomic_add_nob(&tb->common.nitems, nitems_diff);
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

    ERTS_SMP_LC_ASSERT(IS_TAB_WLOCKED(tb));

    for (i = 0; i < NACTIVE(tb); i++) {
	if ((list = BUCKET(tb,i)) != NULL) {
	    add_fixed_deletion(tb, i, 0);
	    do {
		list->hvalue = INVALID_HASH;
		list = list->next;
	    }while(list != NULL);
	}
    }
    erts_smp_atomic_set_nob(&tb->common.nitems, 0);    
    return DB_ERROR_NONE;
}


/* Display hash table contents (for dump) */
static void db_print_hash(int to, void *to_arg, int show, DbTable *tbl)
{
    DbTableHash *tb = &tbl->hash;
    DbHashStats stats;
    int i;

    erts_print(to, to_arg, "Buckets: %d\n", NACTIVE(tb));

#ifdef ERTS_SMP
    i = tbl->common.is_thread_safe;
    /* If crash dumping we set table to thread safe in order to
       avoid taking any locks */
    if (ERTS_IS_CRASH_DUMPING)
        tbl->common.is_thread_safe = 1;

    db_calc_stats_hash(&tbl->hash, &stats);

    tbl->common.is_thread_safe = i;
#else
    db_calc_stats_hash(&tbl->hash, &stats);
#endif

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
    while (!db_free_table_continue_hash(tbl))
	;
    return 0;
}

static int db_free_table_continue_hash(DbTable *tbl)
{
    DbTableHash *tb = &tbl->hash;
    int done;
    FixedDeletion* fixdel = (FixedDeletion*) erts_smp_atomic_read_acqb(&tb->fixdel);
    ERTS_SMP_LC_ASSERT(IS_TAB_WLOCKED(tb));

    done = 0;
    while (fixdel != NULL) {
	FixedDeletion *fx = fixdel;

	fixdel = fx->next;
	erts_db_free(ERTS_ALC_T_DB_FIX_DEL,
		     (DbTable *) tb,
		     (void *) fx,
		     sizeof(FixedDeletion));
	ERTS_ETS_MISC_MEM_ADD(-sizeof(FixedDeletion));
	if (++done >= 2*DELETE_RECORD_LIMIT) {
	    erts_smp_atomic_set_relb(&tb->fixdel, (erts_aint_t)fixdel);
	    return 0;		/* Not done */
	}
    }
    erts_smp_atomic_set_relb(&tb->fixdel, (erts_aint_t)NULL);

    done /= 2;
    while(tb->nslots != 0) {
	free_seg(tb, 1);

	/*
	 * If we have done enough work, get out here.
	 */
	if (++done >= (DELETE_RECORD_LIMIT / CHAIN_LEN / SEGSZ)) {
	    return 0;	/* Not done */
	}
    }
#ifdef ERTS_SMP
    if (tb->locks != NULL) {
	int i;
	for (i=0; i<DB_HASH_LOCK_CNT; ++i) {
	    erts_rwmtx_destroy(GET_LOCK(tb,i)); 
	}
	erts_db_free(ERTS_ALC_T_DB_SEG, (DbTable *)tb,
		     (void*)tb->locks, sizeof(DbTableHashFineLocks));
	tb->locks = NULL;
    }
#endif    
    ASSERT(erts_smp_atomic_read_nob(&tb->common.memory_size) == sizeof(DbTable));
    return 1;			/* Done */
}



/*
** Utility routines. (static)
*/
/*
** For the select functions, analyzes the pattern and determines which
** slots should be searched. Also compiles the match program
*/
static int analyze_pattern(DbTableHash *tb, Eterm pattern, 
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
	matches[i] = tpl = ptpl[1];
	guards[i] = ptpl[2];
	bodies[i] = body = ptpl[3];
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
		    erts_smp_rwmtx_t* lck;
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

static struct ext_segment* alloc_ext_seg(DbTableHash* tb, unsigned seg_ix,
					 struct segment** old_segtab)
{
    int nsegs;
    struct ext_segment* eseg;
    
    switch (seg_ix) {
    case 0: nsegs = NSEG_1; break;
    case 1: nsegs = NSEG_2; break; 
    default: nsegs = seg_ix + NSEG_INC; break;
    }    
    eseg = (struct ext_segment*) erts_db_alloc_fnf(ERTS_ALC_T_DB_SEG,
						   (DbTable *) tb,
						   SIZEOF_EXTSEG(nsegs));
    ASSERT(eseg != NULL);	
    sys_memset(&eseg->s, 0, sizeof(struct segment));
    IF_DEBUG(eseg->s.is_ext_segment = 1);
    eseg->prev_segtab = old_segtab;
    eseg->nsegs = nsegs;
    if (old_segtab) {
	ASSERT(nsegs > tb->nsegs);
	sys_memcpy(eseg->segtab, old_segtab, tb->nsegs*sizeof(struct segment*));
    }
#ifdef DEBUG
    sys_memset(&eseg->segtab[seg_ix], 0, (nsegs-seg_ix)*sizeof(struct segment*));
#endif
    eseg->segtab[seg_ix] = &eseg->s;
    return eseg;
}

/* Extend table with one new segment
*/
static int alloc_seg(DbTableHash *tb)
{    
    int seg_ix = tb->nslots >> SEGSZ_EXP;

    if (seg_ix+1 == tb->nsegs) { /* New segtab needed (extended segment) */
	struct segment** segtab = SEGTAB(tb);
	struct ext_segment* seg = alloc_ext_seg(tb, seg_ix, segtab);
    	if (seg == NULL) return 0;
	segtab[seg_ix] = &seg->s;
	/* We don't use the new segtab until next call (see "shrink race") */
    }
    else { /* Just a new plain segment */
	struct segment** segtab;
	if (seg_ix == tb->nsegs) { /* Time to start use segtab from last call */
	    struct ext_segment* eseg;
	    eseg = (struct ext_segment*) SEGTAB(tb)[seg_ix-1];
	    MY_ASSERT(eseg!=NULL && eseg->s.is_ext_segment);
	    SET_SEGTAB(tb, eseg->segtab);	    
	    tb->nsegs = eseg->nsegs;
	}
	ASSERT(seg_ix < tb->nsegs);
	segtab = SEGTAB(tb);
	ASSERT(segtab[seg_ix] == NULL);
	segtab[seg_ix] = (struct segment*) erts_db_alloc_fnf(ERTS_ALC_T_DB_SEG,
							     (DbTable *) tb,
							     sizeof(struct segment));
	if (segtab[seg_ix] == NULL) return 0;
	sys_memset(segtab[seg_ix], 0, sizeof(struct segment));
    }
    tb->nslots += SEGSZ;
    return 1;
}

/* Shrink table by freeing the top segment
** free_records: 1=free any records in segment, 0=assume segment is empty 
*/
static int free_seg(DbTableHash *tb, int free_records)
{
    const int seg_ix = (tb->nslots >> SEGSZ_EXP) - 1;
    struct segment** const segtab = SEGTAB(tb);
    struct ext_segment* const top = (struct ext_segment*) segtab[seg_ix];
    int bytes;
    int nrecords = 0;

    ASSERT(top != NULL); 
#ifndef DEBUG
    if (free_records)
#endif
    {	
	int i;
	for (i=0; i<SEGSZ; ++i) {
	    HashDbTerm* p = top->s.buckets[i];	    
	    while(p != 0) {		
		HashDbTerm* nxt = p->next;
		ASSERT(free_records); /* segment not empty as assumed? */
		free_term(tb, p);
		p = nxt;
		++nrecords;
	    }
	}
    }

    /* The "shrink race":
     * We must avoid deallocating an extended segment while its segtab may
     * still be used by other threads.
     * The trick is to stop use a segtab one call earlier. That is, stop use
     * a segtab when the segment above it is deallocated. When the segtab is
     * later deallocated, it has not been used for a very long time.
     * It is even theoretically safe as we have by then rehashed the entire
     * segment, seizing *all* locks, so there cannot exist any retarded threads
     * still hanging in BUCKET macro with an old segtab pointer.
     * For this to work, we must of course allocate a new segtab one call
     * earlier in alloc_seg() as well. And this is also the reason why
     * the minimum size of the first segtab is 2 and not 1 (NSEG_1).
     */
    
    if (seg_ix == tb->nsegs-1 || seg_ix==0) { /* Dealloc extended segment */
	MY_ASSERT(top->s.is_ext_segment);   
    	ASSERT(segtab != top->segtab || seg_ix==0);    
	bytes = SIZEOF_EXTSEG(top->nsegs);
    }
    else { /* Dealloc plain segment */
	struct ext_segment* newtop = (struct ext_segment*) segtab[seg_ix-1];
	MY_ASSERT(!top->s.is_ext_segment);
	
	if (segtab == newtop->segtab) { /* New top segment is extended */
	    MY_ASSERT(newtop->s.is_ext_segment);
	    if (newtop->prev_segtab != NULL) {
		/* Time to use a smaller segtab */
		SET_SEGTAB(tb, newtop->prev_segtab);
		tb->nsegs = seg_ix;
		ASSERT(tb->nsegs == EXTSEG(SEGTAB(tb))->nsegs);
	    }
	    else {
		ASSERT(NSEG_1 > 2 && seg_ix==1);
	    }
	}
	bytes = sizeof(struct segment);
    }
    
    erts_db_free(ERTS_ALC_T_DB_SEG, (DbTable *)tb,
		 (void*)top, bytes);
#ifdef DEBUG
    if (seg_ix > 0) {
        segtab[seg_ix] = NULL;
    } else {
	SET_SEGTAB(tb, NULL);
    }
#endif
    tb->nslots -= SEGSZ;
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
	return !erts_smp_atomic_xchg_acqb(&tb->is_resizing, 1);
    else {
	if (erts_smp_atomic_read_nob(&tb->is_resizing))
	    return 0;
	erts_smp_atomic_set_nob(&tb->is_resizing, 1);
	return 1;
    }
}

static ERTS_INLINE void
done_resizing(DbTableHash* tb)
{
    if (DB_USING_FINE_LOCKING(tb))
	erts_smp_atomic_set_relb(&tb->is_resizing, 0);
    else
	erts_smp_atomic_set_nob(&tb->is_resizing, 0);
}

/* Grow table with one new bucket.
** Allocate new segment if needed.
*/
static void grow(DbTableHash* tb, int nactive)
{
    HashDbTerm** pnext;
    HashDbTerm** to_pnext;
    HashDbTerm* p;
    erts_smp_rwmtx_t* lck;
    int from_ix;
    int szm;

    if (!begin_resizing(tb))
	return; /* already in progress */
    if (NACTIVE(tb) != nactive) {
	goto abort; /* already done (race) */
    }

    /* Ensure that the slot nactive exists */
    if (nactive == tb->nslots) {
	/* Time to get a new segment */    
	ASSERT((nactive & SEGSZ_MASK) == 0);
	if (!alloc_seg(tb)) goto abort;	    
    }
    ASSERT(nactive < tb->nslots);

    szm = erts_smp_atomic_read_nob(&tb->szm);
    if (nactive <= szm) {
	from_ix = nactive & (szm >> 1);
    } else {
	ASSERT(nactive == szm+1);
	from_ix = 0;
	szm = (szm<<1) | 1;
    }

    lck = WLOCK_HASH(tb, from_ix);
    /* Now a final double check (with the from_ix lock held)
     * that we did not get raced by a table fixer.
     */
    if (IS_FIXED(tb)) {
	WUNLOCK_HASH(lck);
	goto abort;
    }
    erts_smp_atomic_inc_nob(&tb->nactive);
    if (from_ix == 0) {
	if (DB_USING_FINE_LOCKING(tb))
	    erts_smp_atomic_set_relb(&tb->szm, szm);
	else
	    erts_smp_atomic_set_nob(&tb->szm, szm);
    }
    done_resizing(tb);

    /* Finally, let's split the bucket. We try to do it in a smart way
       to keep link order and avoid unnecessary updates of next-pointers */
    pnext = &BUCKET(tb, from_ix);
    p = *pnext;
    to_pnext = &BUCKET(tb, nactive);
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
    return;
   
abort:
    done_resizing(tb);
}


/* Shrink table by joining top bucket.
** Remove top segment if it gets empty.
*/
static void shrink(DbTableHash* tb, int nactive)
{     
    if (!begin_resizing(tb))
	return; /* already in progress */
    if (NACTIVE(tb) == nactive) {
	erts_smp_rwmtx_t* lck;
	int src_ix = nactive - 1;
	int low_szm = erts_smp_atomic_read_nob(&tb->szm) >> 1;
	int dst_ix = src_ix & low_szm;

	ASSERT(dst_ix < src_ix);
	ASSERT(nactive > SEGSZ);
	lck = WLOCK_HASH(tb, dst_ix);
	/* Double check for racing table fixers */
	if (!IS_FIXED(tb)) {
	    HashDbTerm** src_bp = &BUCKET(tb, src_ix);
	    HashDbTerm** dst_bp = &BUCKET(tb, dst_ix);
	    HashDbTerm** bp = src_bp;

	    /* Q: Why join lists by appending "dst" at the end of "src"?
	       A: Must step through "src" anyway to purge pseudo deleted. */
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
	    
	    erts_smp_atomic_set_nob(&tb->nactive, src_ix);
	    if (dst_ix == 0) {
		erts_smp_atomic_set_relb(&tb->szm, low_szm);
	    }
	    WUNLOCK_HASH(lck);
	    
	    if (tb->nslots - src_ix >= SEGSZ) {
		free_seg(tb, 0);
	    }
	}
	else {
	    WUNLOCK_HASH(lck);
	}

    }
    /*else already done */
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
static HashDbTerm* next(DbTableHash *tb, Uint *iptr, erts_smp_rwmtx_t** lck_ptr,
			HashDbTerm *list)
{
    int i;

    ERTS_SMP_LC_ASSERT(IS_HASH_RLOCKED(tb,*iptr));

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
    erts_smp_rwmtx_t* lck;
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
            erts_smp_atomic_inc_nob(&tb->common.nitems);
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
    erts_smp_rwmtx_t* lck = (erts_smp_rwmtx_t*) handle->lck;
    HashDbTerm* free_me = NULL;

    ERTS_SMP_LC_ASSERT(IS_HASH_WLOCKED(tb, lck));  /* locked by db_lookup_dbterm_hash */

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
        erts_smp_atomic_dec_nob(&tb->common.nitems);
        try_shrink(tb);
    } else {
        if (handle->flags & DB_MUST_RESIZE) {
            db_finalize_resize(handle, offsetof(HashDbTerm,dbterm));
            free_me = b;
        }
        if (handle->flags & DB_INC_TRY_GROW) {
            int nactive;
            int nitems = erts_smp_atomic_inc_read_nob(&tb->common.nitems);
            WUNLOCK_HASH(lck);
            nactive = NACTIVE(tb);

            if (nitems > nactive * (CHAIN_LEN + 1) && !IS_FIXED(tb)) {
                grow(tb, nactive);
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
	erts_smp_atomic_set_nob(&tbl->hash.common.nitems, 0);
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
    erts_smp_rwmtx_t* lck;
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
#ifdef HARDDEBUG

void db_check_table_hash(DbTable *tbl)
{
    DbTableHash *tb = &tbl->hash;
    HashDbTerm* list;
    int j;
    
    for (j = 0; j < tb->nactive; j++) {
	if ((list = BUCKET(tb,j)) != 0) {
	    while (list != 0) {
		if (!is_tuple(make_tuple(list->dbterm.tpl))) {
		    erts_exit(ERTS_ERROR_EXIT, "Bad term in slot %d of ets table", j);
		}
		list = list->next;
	    }
	}
    }
}

#endif
