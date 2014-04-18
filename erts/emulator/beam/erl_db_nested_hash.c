/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2012. All Rights Reserved.
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

/* !!! FIX !!!
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


#ifdef DEBUG
#define HARDDEBUG 1
#endif

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

#define DB_BAG_LHT_LEN 128

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
#define SEGTAB(tb, lht) ((struct segment**) erts_smp_atomic_read_nob(&(lht)->segtab))
#else
#define SEGTAB(tb, lht)                                                 \
    (DB_USING_FINE_LOCKING(tb)                                          \
     ? ((struct segment**) erts_smp_atomic_read_ddrb(&(lht)->segtab))   \
     : ((struct segment**) erts_smp_atomic_read_nob(&(lht)->segtab)))
#endif
#define NACTIVE(lht) ((int)erts_smp_atomic_read_nob(&(lht)->nactive))
#define NITEMS(tb) ((int)erts_smp_atomic_read_nob(&(tb)->common.nitems))

#define BUCKET(tb, i) SEGTAB(tb, &tb->linearht)[(i) >> SEGSZ_EXP]->buckets[(i) & SEGSZ_MASK].hterm

/* ??? use a different SEGTAB macro ??? */
#define NESTED_BUCKET(tb, lht, i) SEGTAB(tb, lht)[(i) >> SEGSZ_EXP]->buckets[(i) & SEGSZ_MASK].nterm

/*
 * When deleting a table, the number of records to delete.
 * Approximate number, because we must delete entire buckets.
 */
#define DELETE_RECORD_LIMIT 10000

#define MAX_HASH 0xEFFFFFFFUL
#define INVALID_HASH 0xFFFFFFFFUL

/* optimised version of make_hash (normal case? atomic key) */
#define MAKE_HASH(term) \
    ((is_atom(term) ? (atom_tab(atom_val(term))->slot.bucket.hvalue) : \
      make_hash2(term)) % MAX_HASH)

/*
 * Some special binary flags
 */
#define BIN_FLAG_ALL_OBJECTS         BIN_FLAG_USR1

#ifdef ERTS_ENABLE_LOCK_CHECK
#   define IFN_EXCL(tb,cmd) (((tb)->common.is_thread_safe) || (cmd))
#   define IS_HASH_RLOCKED(tb, hval) IFN_EXCL(tb, erts_smp_lc_rwmtx_is_rlocked(GET_LOCK(tb, hval)))
#   define IS_HASH_WLOCKED(tb,lck) IFN_EXCL(tb,erts_smp_lc_rwmtx_is_rwlocked(lck))
#   define IS_TAB_WLOCKED(tb) erts_smp_lc_rwmtx_is_rwlocked(&(tb)->common.rwlock)
#else
#   define IS_HASH_RLOCKED(tb,hval) (1)
#   define IS_HASH_WLOCKED(tb,hval) (1)
#   define IS_TAB_WLOCKED(tb) (1)
#endif


/*
 * Local types
 */
struct mp_prefound {
    TrunkDbTerm **bucket;
    int ix;
};

struct mp_info {
    int all_objects; /* True if complete objects are always
                      * returned from the match_spec (can use
                      * copy_shallow on the return value) */
    int something_can_match;       /* The match_spec is not "impossible" */
    int key_given;
    struct mp_prefound dlists[10]; /* Default list of "pre-found" buckets */
    struct mp_prefound* lists;     /* Buckets to search if keys are given,
                                    * = dlists initially */
    unsigned num_lists; /* Number of elements in "lists",
                         * = 0 initially */
    Binary *mp;         /* The compiled match program */
};

/*
 * How the table segments relate to each other:
 *
 *     ext_segment:                      ext_segment:              "plain" segment
 *    #=================#                #================#        #=============#
 *    | bucket[0]       |<--+   +------->| bucket[256]    |     +->| bucket[512] |
 *    | bucket[1]       |   |   |        |       [257]    |     |  |       [513] |
 *    :                 :   |   |        :                :     |  :             :
 *    | bucket[255]     |   |   |        |       [511]    |     |  |       [767] |
 *    |-----------------|   |   |        |----------------|     |  #=============#
 *    | prev_segtab=NULL|   |   |   +--<---prev_segtab    |     |
 *    | nsegs = 2       |   |   |   |    | nsegs = 256    |     |
 * +->| segtab[0] -->-------+---|---|--<---segtab[0]      |<-+  |
 * |  | segtab[1] -->-----------+---|--<---segtab[1]      |  |  |
 * |  #=================#           |    | segtab[2] -->-----|--+    ext_segment:
 * |                                |    :                :  |      #================#
 * +----------------<---------------+    | segtab[255] ->----|----->| bucket[255*256]|
 *                                       #================#  |      |                |
 *                                                           |      :                :
 *                                                           |      |----------------|
 *                                                           +----<---prev_segtab    |
 *                                                                  :                :
 */

/* A table "plain" segment */
struct segment {
    union {
        TrunkDbTerm *hterm;
        NestedDbTerm *nterm;
    } buckets[SEGSZ];
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

#if defined(DEBUG)
#  define EXTSEG(SEGTAB_PTR) \
    ((struct ext_segment*) (((char*)(SEGTAB_PTR)) - offsetof(struct ext_segment,segtab)))
#endif


/*
 * Forward decl's (static functions)
 */
static struct ext_segment *
alloc_ext_seg(DbTableNestedHash *tb, LinearHashTable *lht,
              unsigned seg_ix, struct segment **old_segtab);

static int
alloc_seg(DbTableNestedHash *tb, LinearHashTable *lht);

static int
free_seg(DbTableNestedHash *tb, LinearHashTable *lht,
         int free_records, int is_nested);

/* !!! */
static NestedHashDbTerm *
search_list(DbTableNestedHash *tb, Eterm key,
            HashValue hval, NestedHashDbTerm *list);

static void
shrink(DbTableNestedHash *tb, LinearHashTable *lht, int nactive);

static void
grow(DbTableNestedHash *tb, LinearHashTable *lht, int nactive);

/* !!! */
static Eterm
build_term_list(Process *p, NestedHashDbTerm *ptr1,
                NestedHashDbTerm *ptr2, DbTableNestedHash *tb);

static int
analyze_pattern(DbTableNestedHash *tb, Eterm pattern, struct mp_info *mpi);


/*
 *  Method interface functions
 */
int
db_create_nhash(Process *p, DbTable *tbl);

static int
db_first_nhash(Process *p, DbTable *tbl, Eterm *ret);

static int
db_next_nhash(Process *p, DbTable *tbl, Eterm key, Eterm *ret);

int
db_put_nhash(DbTable *tbl, Eterm obj, int key_clash_fail);

int
db_get_nhash(Process *p, DbTable *tbl, Eterm key, Eterm *ret);

static int
db_get_element_nhash(Process *p, DbTable *tbl, Eterm key, int ndex, Eterm *ret);

static int
db_member_nhash(DbTable *tbl, Eterm key, Eterm *ret);

int
db_erase_nhash(DbTable *tbl, Eterm key, Eterm *ret);

static int
db_erase_object_nhash(DbTable *tbl, Eterm object,Eterm *ret);

static int
db_slot_nhash(Process *p, DbTable *tbl, Eterm slot_term, Eterm *ret);

static int
db_select_chunk_nhash(Process *p, DbTable *tbl, Eterm pattern,
                      Sint chunk_size, int reverse, Eterm *ret);

static int
db_select_nhash(Process *p, DbTable *tbl, Eterm pattern,
                int reverse, Eterm *ret);

static int
db_select_delete_nhash(Process *p, DbTable *tbl, Eterm pattern, Eterm *ret);

static int
db_select_continue_nhash(Process *p, DbTable *tbl,
                         Eterm continuation, Eterm *ret);

static int
db_select_delete_continue_nhash(Process *p, DbTable *tbl,
                                Eterm continuation, Eterm *ret);

static int
db_select_count_nhash(Process *p, DbTable *tbl, Eterm pattern, Eterm *ret);

static int
db_select_count_continue_nhash(Process *p, DbTable *tbl,
                               Eterm continuation, Eterm *ret);

static int
db_delete_all_objects_nhash(Process *p, DbTable *tbl);

static int
db_free_table_nhash(DbTable *tbl);

static int
db_free_table_continue_nhash(DbTable *tbl);

static void
db_print_nhash(int to, void *to_arg, int show, DbTable *tbl);

static void
db_foreach_offheap_nhash(DbTable *tbl,
                         void (*func)(ErlOffHeap *, void *), void *arg);

#ifdef HARDDEBUG
static void
db_check_table_nhash(DbTable *tbl);
#endif

static int
db_lookup_dbterm_nhash(DbTable *tbl, Eterm key, DbUpdateHandle *handle);

static void
db_finalize_dbterm_nhash(DbUpdateHandle *handle);


/*
 * External interface
 */
DbTableMethod db_nested_hash = {
    db_create_nhash,
    db_first_nhash,
    db_next_nhash,
    db_first_nhash,   /* last == first  */
    db_next_nhash,    /* prev == next   */
    db_put_nhash,
    db_get_nhash,
    db_get_element_nhash,
    db_member_nhash,
    db_erase_nhash,
    db_erase_object_nhash,
    db_slot_nhash,
    db_select_chunk_nhash,
    db_select_nhash,
    db_select_delete_nhash,
    db_select_continue_nhash,
    db_select_delete_continue_nhash,
    db_select_count_nhash,
    db_select_count_continue_nhash,
    db_delete_all_objects_nhash,
    db_free_table_nhash,
    db_free_table_continue_nhash,
    db_print_nhash,
    db_foreach_offheap_nhash,
#ifdef HARDDEBUG
    db_check_table_nhash,
#else
    NULL,
#endif
    db_lookup_dbterm_nhash,
    db_finalize_dbterm_nhash
};


/*
 * Calculate slot index from hash value.
 * RLOCK_HASH or WLOCK_HASH must be done before.
 */
static ERTS_INLINE Uint
hash_to_ix(DbTableNestedHash* tb, LinearHashTable *lht, HashValue hval)
{
    Uint mask = (DB_USING_FINE_LOCKING(tb)
                 ?erts_smp_atomic_read_acqb(&lht->szm)
                 :erts_smp_atomic_read_nob(&lht->szm));
    Uint ix = hval & mask;
    if (ix >= erts_smp_atomic_read_nob(&lht->nactive)) {
        ix &= mask >> 1;
        ASSERT(ix < erts_smp_atomic_read_nob(&lht->nactive));
    }
    return ix;
}

#ifdef ERTS_SMP

#define DB_HASH_LOCK_MASK (DB_NESTED_HASH_LOCK_CNT-1)
#define GET_LOCK(tb, hval) (&(tb)->locks->lck_vec[(hval) & DB_HASH_LOCK_MASK].lck)

/*
 * Fine grained read lock
 */
static ERTS_INLINE erts_smp_rwmtx_t *
RLOCK_HASH(DbTableNestedHash *tb, HashValue hval)
{
    erts_smp_rwmtx_t *lck;
    if (tb->common.is_thread_safe)
        return NULL;
    lck = GET_LOCK(tb, hval);
    ASSERT(tb->common.type & DB_FINE_LOCKED);
    erts_smp_rwmtx_rlock(lck);
    return lck;
}

/*
 * Fine grained write lock
 */
static ERTS_INLINE erts_smp_rwmtx_t *
WLOCK_HASH(DbTableNestedHash *tb, HashValue hval)
{
    erts_smp_rwmtx_t *lck;
    if (tb->common.is_thread_safe)
        return NULL;
    lck = GET_LOCK(tb, hval);
    ASSERT(tb->common.type & DB_FINE_LOCKED);
    erts_smp_rwmtx_rwlock(lck);
    return lck;
}

static ERTS_INLINE void
RUNLOCK_HASH(erts_smp_rwmtx_t *lck)
{
    if (lck != NULL)
        erts_smp_rwmtx_runlock(lck);
}

static ERTS_INLINE void
WUNLOCK_HASH(erts_smp_rwmtx_t *lck)
{
    if (lck != NULL)
        erts_smp_rwmtx_rwunlock(lck);
}

#else /* ERTS_SMP */

#define RLOCK_HASH(tb,hval) NULL
#define WLOCK_HASH(tb,hval) NULL
#define RUNLOCK_HASH(lck) ((void)lck)
#define WUNLOCK_HASH(lck) ((void)lck)

#endif /* ERTS_SMP */

/*
 * 'bpp' and 'kp' are updated such that 'kp' points to the next
 * Trunk/BranchDbTerm in the bucket.
 * NOTE: It is assumed that both *bpp and kp are not NULL
 */
#define NEXT_DBTERM_P(bpp, kp)                  \
    if (((kp).trunk == *(bpp))                  \
        ? ((kp).trunk->branch == NULL)          \
        : ((kp).branch->next == NULL)) {        \
        (bpp) = &(*(bpp))->next;                \
        (kp).trunk = *(bpp);                    \
    } else {                                    \
        (kp).branch = (kp).branch->next;        \
    }

/*
 * 'bp' and 'kp' are updated to point to the next
 * Trunk/BranchDbTerm in the chain.
 * NOTE: It is assumed that both bp and kp are not NULL
 */
#define NEXT_DBTERM(bp, kp)                     \
    if (((kp).trunk == (bp))                    \
        ? ((kp).trunk->branch == NULL)          \
        : ((kp).branch->next == NULL)) {        \
        (kp).trunk = (bp) = (bp)->next;         \
    } else {                                    \
        (kp).branch = (kp).branch->next;        \
    }

/*
 * Iteration helper
 * Returns "next" slot index or 0 if EOT reached.
 * Slot READ locks updated accordingly, unlocked if EOT.
 */
static ERTS_INLINE Sint
next_slot(DbTableNestedHash *tb, Uint ix, erts_smp_rwmtx_t **lck_ptr)
{
#ifdef ERTS_SMP
    ix += DB_NESTED_HASH_LOCK_CNT;
    if (ix < NACTIVE(&tb->linearht))
        return ix;
    RUNLOCK_HASH(*lck_ptr);
    ix = (ix + 1) & DB_HASH_LOCK_MASK;
    if (ix != 0)
        *lck_ptr = RLOCK_HASH(tb, ix);
    return ix;
#else
    return (++ix < NACTIVE(&tb->linearht)) ? ix : 0;
#endif
}

/*
 * Same as next_slot but with WRITE locking
 */
static ERTS_INLINE Sint
next_slot_w(DbTableNestedHash *tb, Uint ix, erts_smp_rwmtx_t **lck_ptr)
{
#ifdef ERTS_SMP
    ix += DB_NESTED_HASH_LOCK_CNT;
    if (ix < NACTIVE(&tb->linearht))
        return ix;
    WUNLOCK_HASH(*lck_ptr);
    ix = (ix + 1) & DB_HASH_LOCK_MASK;
    if (ix != 0)
        *lck_ptr = WLOCK_HASH(tb, ix);
    return ix;
#else
    return next_slot(tb, ix, lck_ptr);
#endif
}

/*
 * This function is called by the next AND the select BIF
 * It return the next live object in a table, NULL if no more
 * In-bucket: RLOCKED
 * Out-bucket: RLOCKED unless NULL
 * N.B.: The name 'next_dbterm' is misleading. It skips DbTerms
 *       until one with a valid key is found -- it will skip none
 *       if *bpp has a valid key.
 */
static TrunkOrBranchDbTerm
next_dbterm(DbTableNestedHash *tb, Uint *iptr, erts_smp_rwmtx_t **lck_ptr,
            TrunkDbTerm **bpp, TrunkOrBranchDbTerm kp)
{
    int i = *iptr;
    ERTS_SMP_LC_ASSERT(IS_HASH_RLOCKED(tb, i));
    for (;;) {
        while (kp.trunk != NULL) {
            /* Only the TrunkDbTerm can be pseudo-deleted */
            if ((kp.trunk != *bpp) || (kp.trunk->hvalue != INVALID_HASH))
                return kp;
            NEXT_DBTERM(*bpp, kp);
        }
        *iptr = i = next_slot(tb, i, lck_ptr);
        if (!i)
            break;
        kp.trunk = *bpp = BUCKET(tb,i);
    }
    return NULL;
}

static ERTS_INLINE void
free_term(DbTableNestedHash *tb, TrunkOrBranchDbTerm p, int is_trunk)
{
    if (is_trunk) {
        db_free_term((DbTable *)tb, p.trunk, offsetof(TrunkDbTerm, dbterm));
    } else {
        db_free_term((DbTable *)tb, p.branch, offsetof(BranchDbTerm, dbterm));
    }
}

static ERTS_INLINE void
SET_SEGTAB(DbTableNestedHash *tb, LinearHashTable *lht, struct segment **segtab)
{
    if (DB_USING_FINE_LOCKING(tb)) {
        erts_smp_atomic_set_wb(&lht->segtab, (erts_aint_t) segtab);
    } else {
        erts_smp_atomic_set_nob(&lht->segtab, (erts_aint_t) segtab);
    }
}


static ERTS_INLINE void
try_shrink(DbTableNestedHash *tb)
{
    int nactive = NACTIVE(&tb->linearht);
    if (nactive > SEGSZ && NITEMS(tb) < (nactive * CHAIN_LEN)
        && !IS_FIXED(tb)) {
        shrink(tb, &tb->linearht, nactive);
    }
}

#define EQ_REL(x, y, y_base)                                            \
    (is_same(x, NULL, y, y_base)                                        \
     || (is_not_both_immed((x),(y)) && eq_rel((x), NULL, (y), y_base)))

/*
 * Has this object the specified key? Can be pseudo-deleted.
 */
static ERTS_INLINE int
has_key(DbTableNestedHash *tb, TrunkDbTerm *bp, Eterm key, HashValue hval)
{
    if ((bp->hvalue != hval) && (bp->hvalue != INVALID_HASH))
        return 0;
    Eterm itemKey = GETKEY(tb, bp->dbterm.tpl);
    ASSERT(!is_header(itemKey));
    return EQ_REL(key, itemKey, bp->dbterm.tpl);
}

static ERTS_INLINE TrunkDbTerm *
new_trunk_dbterm(DbTableNestedHash *tb, Eterm obj)
{
    if (tb->common.compress)
        return db_store_term_comp(&tb->common, NULL,
                                  offsetof(TrunkDbTerm, dbterm), obj);
    return db_store_term(&tb->common, NULL,
                         offsetof(TrunkDbTerm, dbterm), obj);
}

/* !!! REMOVE ??? */
static ERTS_INLINE BranchDbTerm *
new_branch_dbterm(DbTableNestedHash *tb, Eterm obj)
{
    if (tb->common.compress)
        return db_store_term_comp(&tb->common, NULL,
                                  offsetof(BranchDbTerm, dbterm), obj);
    return db_store_term(&tb->common, NULL,
                         offsetof(BranchDbTerm, dbterm), obj);
}

static ERTS_INLINE TrunkDbTerm *
replace_trunk_dbterm(DbTableNestedHash *tb, TrunkDbTerm *old, Eterm obj)
{
    ASSERT(old != NULL);
    if (tb->common.compress)
        return db_store_term_comp(&tb->common, &(old->dbterm),
                                  offsetof(TrunkDbTerm, dbterm), obj);
    return db_store_term(&tb->common, &(old->dbterm),
                         offsetof(TrunkDbTerm, dbterm), obj);
}

/* !!!! Cannot be used if lht != NULL !!!! */
static ERTS_INLINE TrunkDbTerm *
realloc_as_trunk_dbterm(DbTableNestedHash *tb, BranchDbTerm *old,
                        TrunkDbTerm *template)
{
    Uint dbterm_sz;
    TrunkDbTerm *ret;
    dbterm_sz = sizeof(DbTerm) + sizeof(Eterm) * (old->dbterm.size - 1);
    ret = erts_db_alloc(ERTS_ALC_T_DB_TERM, (DbTable *)&tb->common,
                        offsetof(TrunkDbTerm, dbterm) + dbterm_sz);
    if ((ret->branch = old->next) != NULL)
        ret->branch->previous.trunk = ret;
    /* !!! ASSERT(template != NULL); ??? */
    if (template != NULL) {
        ret->next = template->next;
        ret->nkitems = template->nkitems;
        ret->hvalue = template->hvalue;
        ret->lht = template->lht;
    }
    sys_memcpy(&ret->dbterm, &old->dbterm, dbterm_sz);
    erts_db_free(ERTS_ALC_T_DB_TERM, (DbTable *)&tb->common, old,
                 offsetof(BranchDbTerm, dbterm) + dbterm_sz);
    return ret;
}

/* !!!! Cannot be used if lht != NULL !!!! */
/* !!! REMOVE ??? */
static ERTS_INLINE BranchDbTerm *
realloc_as_branch_dbterm(DbTableNestedHash *tb, TrunkDbTerm *old)
{
    Uint dbterm_sz;
    BranchDbTerm *ret;
    dbterm_sz = sizeof(DbTerm) + sizeof(Eterm) * (old->dbterm.size - 1);
    ret = erts_db_alloc(ERTS_ALC_T_DB_TERM, (DbTable *)&tb->common,
                        offsetof(BranchDbTerm, dbterm) + dbterm_sz);
    ret->previous = NULL;
    ret->next = old->branch;
    sys_memcpy(&ret->dbterm, &old->dbterm, dbterm_sz);
    erts_db_free(ERTS_ALC_T_DB_TERM, (DbTable *)&tb->common, old,
                 offsetof(TrunkDbTerm, dbterm) + dbterm_sz);
    return ret;
}

/*
 * It is assumed that the TrunkOrBranchDbTerm looked-up for is present in
 * the nested LinearHashTable.
 */
static NestedDbTerm **
get_nested_dbterm(DbTableNestedHash *tb, LinearHashTable *lht,
                  TrunkOrBranchDbTerm hdb, int is_trunk)
{
    int nix;
    HashValue ohval;
    NestedDbTerm *ntp, **ntpp;
    ohval = (is_trunk)
        ? MAKE_HASH(make_tuple_rel(hdb.trunk->dbterm.tpl,
                                   hdb.trunk->dbterm.tpl))
        : MAKE_HASH(make_tuple_rel(hdb.branch->dbterm.tpl,
                                   hdb.branch->dbterm.tpl));
    /* ??? Use a specialized version of hash_to_ix ??? */
    nix = hash_to_ix(tb, lht, ohval);
    ntpp = &NESTED_BUCKET(tb, lht, nix);
    while ((ntp = *ntpp) != NULL) {
        if (ntp->hdbterm == hdb) {
            ASSERT(ntp->ohvalue == ohval);
            return ntpp;
        }
        ntpp = &ntp->next;
    }
    ASSERT(0);
    return NULL;
}

static ERTS_INLINE void
put_nested_dbterm(DbTableNestedHash *tb, LinearHashTable *lht,
                  TrunkOrBranchDbTerm hdb, int is_trunk)
{
    int nix;
    HashValue ohval;
    NestedDbTerm *ntp, **ntpp;
    ohval = (is_trunk)
        ? MAKE_HASH(make_tuple_rel(hdb.trunk->dbterm.tpl,
                                   hdb.trunk->dbterm.tpl))
        : MAKE_HASH(make_tuple_rel(hdb.branch->dbterm.tpl,
                                   hdb.branch->dbterm.tpl));
    /* ??? Use a specialized version of hash_to_ix ??? */
    nix = hash_to_ix(tb, lht, ohval);
    ntpp = &NESTED_BUCKET(tb, lht, nix);
    ntp = (NestedDbTerm *)
        erts_db_alloc(ERTS_ALC_T_DB_TERM, /* ??? Use another type ??? */
                      (DbTable *)tb, sizeof(NestedDbTerm));
    ntp->next = *ntpp;
    ntp->ohvalue = ohval;
    ntp->hdbterm = hdb;
    *ntpp = ntp;
}

static ERTS_INLINE void
free_nested_term(DbTableNestedHash *tb, NestedDbTerm *ntp)
{
    erts_db_free(ERTS_ALC_T_DB_TERM, /* ??? Use another type ??? */
                 (DbTable *)tb, ntp, sizeof(NestedDbTerm));
}

/*
 * It is assumed that the TrunkOrBranchDbTerm to remove is present in
 * the nested LinearHashTable.
 */
static ERTS_INLINE void
remove_nested_dbterm(DbTableNestedHash *tb, LinearHashTable *lht,
                     TrunkOrBranchDbTerm hdb, int is_trunk)
{
    NestedDbTerm *ntp, **ntpp;
    ntpp = get_nested_dbterm(tb, lht, hdb, is_trunk);
    ntp = *ntpp;
    *ntpp = ntp->next;
    free_nested_term(tb, ntp);
}

static void
free_nested_table(DbTableNestedHash *tb, LinearHashTable *lht)
{
    while (lht->nslots)
        free_seg(tb, lht, 0, 1);
    erts_db_free(ERTS_ALC_T_DB_SEG, /* ??? Use another type ??? */
                 (DbTable *)tb, lht, sizeof(LinearHashTable));
}

/*
 * Grow table with one new bucket.
 * Allocate new segment if needed.
 */
static void
nested_grow(DbTableNestedHash *tb, LinearHashTable *lht)
{
    int from_ix, szm, nactive;
    NestedDbTerm *p;
    NestedDbTerm **pnext, **to_pnext;
    nactive=NACTIVE(lht);
    /* Ensure that the slot nactive exists */
    if (nactive == lht->nslots) {
        /* Time to get a new segment */
        ASSERT(!(nactive & SEGSZ_MASK));
        if (!alloc_seg(tb, lht))
            return;
    }
    ASSERT(nactive < lht->nslots);
    szm = erts_smp_atomic_read_nob(&lht->szm);
    if (nactive <= szm) {
        from_ix = nactive & (szm >> 1);
    } else {
        ASSERT(nactive == szm+1);
        from_ix = 0;
        szm = (szm<<1) | 1;
    }
    erts_smp_atomic_inc_nob(&lht->nactive);
    if (from_ix == 0) {
        if (DB_USING_FINE_LOCKING(tb)) {
            erts_smp_atomic_set_relb(&lht->szm, szm);
        } else {
            erts_smp_atomic_set_nob(&lht->szm, szm);
        }
    }
    /*
     * Finally, let's split the bucket. We try to do it in a smart way
     * to keep link order and avoid unnecessary updates of next-pointers.
     */
    pnext = &NESTED_BUCKET(tb, lht, from_ix);
    p = *pnext;
    to_pnext = &NESTED_BUCKET(tb, lht, nactive);
    while (p != NULL) {
        int ix = p->ohvalue & szm;
        if (ix != from_ix) {
            ASSERT(ix == (from_ix ^ ((szm>>1)+1)));
            *to_pnext = p;
            /* Swap "from" and "to": */
            from_ix = ix;
            to_pnext = pnext;
        }
        pnext = &p->next;
        p = *pnext;
    }
    *to_pnext = NULL;
}

/*
 * Shrink table by joining top bucket.
 * Remove top segment if it gets empty.
 */
static void
nested_shrink(DbTableNestedHash *tb, LinearHashTable* lht)
{
    int nactive, src_ix, dst_ix, low_szm;
    NestedDbTerm **src_bp, **dst_bp;
    nactive = NACTIVE(lht);
    src_ix = nactive - 1;
    low_szm = erts_smp_atomic_read_nob(&lht->szm) >> 1;
    dst_ix = src_ix & low_szm;
    ASSERT(dst_ix < src_ix);
    ASSERT(nactive > SEGSZ);
    src_bp = &NESTED_BUCKET(tb, lht, src_ix);
    dst_bp = &NESTED_BUCKET(tb, lht, dst_ix);
    while (*dst_bp != NULL)
        dst_bp = &(*dst_bp)->next;
    *dst_bp = *src_bp;
    *src_bp = NULL;
    erts_smp_atomic_set_nob(&lht->nactive, src_ix);
    if (dst_ix == 0)
        erts_smp_atomic_set_relb(&lht->szm, low_szm);
    if ((lht->nslots - src_ix) >= SEGSZ)
        free_seg(tb, lht, 0, 1);
}

static ERTS_INLINE void
try_nested_shrink(DbTableNestedHash *tb, LinearHashTable *lht, int nkitems)
{
    int nactive = NACTIVE(lht);
    if ((nactive > SEGSZ) && (nkitems < (nactive * CHAIN_LEN)))
        nested_shrink(tb, lht);
}

/*
 * Remove term '**kpp' from the chain.
 * '*bppp' and '*kpp' are updated appropriately to point to
 * the next Trunk/BranchDbTerm.
 * NOTE: It is assumed that bppp, *bppp, **bppp, kpp and *kpp are not NULL
 * Return: 0 -> the new **kpp Trunk/BranchDbTerm has the same key as the removed one's
 *         1 -> the new **kpp Trunk/BranchDbTerm has a different key from the removed one's
 *         2 -> pseudo deletion -- implies case 1 above
 */
static int
remove_dbterm(DbTableNestedHash *tb, TrunkDbTerm ***bppp,
              TrunkOrBranchDbTerm *kpp, int pseudo_delete)
{
    TrunkDbTerm **bpp = *bppp;
    TrunkDbTerm *bp = *bpp;
    TrunkOrBranchDbTerm kp = *kpp;
    if (bp == kp.trunk) {
        /* kp is a trunk term */
        if (bp->branch == NULL) {
            ASSERT(bp->nkitems == 1);
            if (pseudo_delete) {
                *bppp = &bp->next;
                kpp->trunk = bp->next;
                bp->hvalue = INVALID_HASH;
                return 2;
            }
            *bpp = kpp->trunk = bp->next;
            if (bp->lht != NULL) {
                remove_nested_dbterm(tb, bp->lht, kp, 1);
                free_nested_table(tb, bp->lht);
                bp->lht = NULL;
            }
            free_term(tb, (TrunkOrBranchDbTerm)bp, 1);
            return 1;
        }
        *bpp = kpp->trunk = bp = realloc_as_trunk_dbterm(tb, bp->branch, bp);
        --bp->nkitems;
        if (bp->lht != NULL) {
            remove_nested_dbterm(tb, bp->lht, kp, 1);
            try_nested_shrink(tb, bp->lht, bp->nkitems);
        }
        free_term(tb, kp, 1);
        return 0;
    }
    /* kp is a branch term */
    ASSERT(kp.branch->previous.trunk != NULL);
    if (kp.branch->previous.trunk == bp) {
        bp->branch = kp.branch->next;
    } else {
        kp.branch->previous.branch->next = kp.branch->next;
    }
    --bp->nkitems;
    if (bp->lht != NULL) {
        remove_nested_dbterm(tb, bp->lht, kp, 0);
        try_nested_shrink(tb, bp->lht, bp->nkitems);
    }
    if (kp.branch->next == NULL) {
        *bppp = &bp->next;
        kpp->trunk = bp->next;
        free_term(tb, kp, 0);
        return 1;
    }
    kp.branch->next->previous = kp.branch->previous;
    kpp->branch = kp.branch->next;
    free_term(tb, kp, 0);
    return 0;
}

/*
 * Remember a slot containing a pseudo-deleted item (INVALID_HASH)
 */
static ERTS_INLINE void
add_fixed_deletion(DbTableNestedHash *tb, int ix)
{
    erts_aint_t was_next, exp_next;
    NestedFixedDeletion *fixd;
    fixd = (NestedFixedDeletion *)
        erts_db_alloc(ERTS_ALC_T_DB_FIX_DEL, (DbTable *)tb,
                      sizeof(NestedFixedDeletion));
    ERTS_ETS_MISC_MEM_ADD(sizeof(NestedFixedDeletion));
    fixd->slot = ix;
    was_next = erts_smp_atomic_read_acqb(&tb->fixdel);
    do {
        /* Lockless atomic insertion in linked list: */
        exp_next = was_next;
        fixd->next = (NestedFixedDeletion *)exp_next;
        was_next =
            erts_smp_atomic_cmpxchg_relb(&tb->fixdel,
                                         (erts_aint_t)fixd, exp_next);
    } while (was_next != exp_next);
}

#ifdef DEBUG
/* Wait a while to provoke race and get code coverage */
static void
DEBUG_WAIT(void)
{
    unsigned long spin = 1UL << 20;
    while (--spin);
}
#else
#   define DEBUG_WAIT()
#endif

/*
 * Rare case of restoring the rest of the fixdel list
 * when "unfixer" gets interrupted by "fixer"
 */
static void
restore_fixdel(DbTableNestedHash *tb, NestedFixedDeletion *fixdel)
{
    DEBUG_WAIT();
    if (erts_smp_atomic_cmpxchg_relb(&tb->fixdel, (erts_aint_t)fixdel,
                                     (erts_aint_t)NULL)
        != (erts_aint_t)NULL) {
        /* Oboy, must join lists */
        erts_aint_t was_tail, exp_tail;
        NestedFixedDeletion *last = fixdel;
        while (last->next != NULL)
            last = last->next;
        was_tail = erts_smp_atomic_read_acqb(&tb->fixdel);
        do {
            /* Lockless atomic list insertion */
            exp_tail = was_tail;
            last->next = (NestedFixedDeletion *)exp_tail;
            DEBUG_WAIT();
            was_tail =
                erts_smp_atomic_cmpxchg_relb(&tb->fixdel,
                                             (erts_aint_t) fixdel, exp_tail);
        } while (was_tail != exp_tail);
    }
}

/*
 * Table interface routines ie what's called by the bif's
 */
void
db_unfix_table_nhash(DbTableNestedHash *tb)
{
    NestedFixedDeletion *fixdel;
    ERTS_SMP_LC_ASSERT(erts_smp_lc_rwmtx_is_rwlocked(&tb->common.rwlock)
                       ||(erts_smp_lc_rwmtx_is_rlocked(&tb->common.rwlock)
                          &&(!tb->common.is_thread_safe)));

restart:
    fixdel = (NestedFixedDeletion *)
        erts_smp_atomic_xchg_acqb(&tb->fixdel, (erts_aint_t)NULL);
    while (fixdel != NULL) {
        TrunkDbTerm **bpp;
        TrunkOrBranchDbTerm kb;
        NestedFixedDeletion *fx = fixdel;
        int ix = fx->slot;
        erts_smp_rwmtx_t *lck = WLOCK_HASH(tb,ix);
        if (IS_FIXED(tb)) {
            /* interrupted by fixer */
            WUNLOCK_HASH(lck);
            restore_fixdel(tb, fixdel);
            if (!IS_FIXED(tb))
                /* unfixed again! */
                goto restart;
            return;
        }
        if (ix < NACTIVE(&tb->linearht)) {
            bpp = &BUCKET(tb, ix);
            while ((kb.trunk = *bpp) != NULL)
                if ((kb.trunk->hvalue != INVALID_HASH)
                    || (!remove_dbterm(tb, &bpp, &kb, 0)))
                    /* Only the TrunkDbTerm can be pseudo-deleted */
                    bpp = &(*bpp)->next;
        }
        /* else slot has been joined and purged by shrink() */
        WUNLOCK_HASH(lck);
        fixdel = fx->next;
        erts_db_free(ERTS_ALC_T_DB_FIX_DEL, (DbTable *)tb,
                     (void *)fx, sizeof(NestedFixedDeletion));
        ERTS_ETS_MISC_MEM_ADD(-sizeof(NestedFixedDeletion));
    }
    /* ToDo: Maybe try grow/shrink the table as well */
}

/*
 * Only used by tests
 */
Uint
db_kept_items_nhash(DbTableNestedHash *tb)
{
    Uint kept_items, ix;
    TrunkDbTerm *bp;
    erts_smp_rwmtx_t* lck = RLOCK_HASH(tb,ix);
    kept_items = ix = 0;
    do {
        bp = BUCKET(tb, ix);
        while (bp != NULL) {
            if (bp->hvalue == INVALID_HASH)
                ++kept_items;
            /* Only the TrunkDbTerm can be pseudo-deleted */
            bp = bp->next;
        }
        ix = next_slot(tb, ix, &lck);
    } while (ix);
    return kept_items;
}

static void
create_linear_hash(DbTableNestedHash *tb, LinearHashTable *lht)
{
    erts_smp_atomic_init_nob(&lht->szm, SEGSZ_MASK);
    erts_smp_atomic_init_nob(&lht->nactive, SEGSZ);
    erts_smp_atomic_init_nob(&lht->segtab, (erts_aint_t)NULL);
    SET_SEGTAB(tb, lht, alloc_ext_seg(tb, lht, 0, NULL)->segtab);
    lht->nsegs = NSEG_1;
    lht->nslots = SEGSZ;
}

int
db_create_nhash(Process *p, DbTable *tbl)
{
    DbTableNestedHash *tb = &tbl->nested;
    erts_smp_atomic_init_nob(&tb->fixdel, (erts_aint_t)NULL);
    create_linear_hash(tb, &tb->linearht);
    erts_smp_atomic_init_nob(&tb->is_resizing, 0);
#ifdef ERTS_SMP
    if (tb->common.type & DB_FINE_LOCKED) {
        int i;
        erts_smp_rwmtx_opt_t rwmtx_opt = ERTS_SMP_RWMTX_OPT_DEFAULT_INITER;
        if (tb->common.type & DB_FREQ_READ)
            rwmtx_opt.type = ERTS_SMP_RWMTX_TYPE_FREQUENT_READ;
        tb->locks = (DbTableNestedHashFineLocks *)
            erts_db_alloc_fnf(ERTS_ALC_T_DB_SEG, /* Other type maybe? */
                              (DbTable *)tb,
                              sizeof(DbTableNestedHashFineLocks));
        if (tb->locks != NULL) {
            for (i=0; i<DB_NESTED_HASH_LOCK_CNT; ++i)
                erts_smp_rwmtx_init_opt_x(&tb->locks->lck_vec[i].lck,
                                          &rwmtx_opt, "db_nested_hash_slot",
                                          make_small(i));
            /*
             * This important property is needed to guarantee
             * that the buckets involved in a grow/shrink operation
             * it protected by the same lock:
             */
            ASSERT((erts_smp_atomic_read_nob(&tb->linearht->nactive)
                    % DB_NESTED_HASH_LOCK_CNT) == 0);
        }
    } else {
        /* coarse locking */
        tb->locks = NULL;
    }
    ERTS_THR_MEMORY_BARRIER;
#endif /* ERST_SMP */
    return DB_ERROR_NONE;
}

static int
db_first_nhash(Process *p, DbTable *tbl, Eterm *ret)
{
    Uint ix = 0;
    TrunkDbTerm *bp;
    DbTableNestedHash *tb = &tbl->nested;
    erts_smp_rwmtx_t *lck = RLOCK_HASH(tb, ix);
    for (;;) {
        bp = BUCKET(tb, ix);
        if (bp != NULL) {
            if (bp->hvalue == INVALID_HASH) {
                TrunkOrBranchDbTerm kp = {.trunk = bp};
                bp = next_dbterm(tb, &ix, &lck, &bp, kp);
            }
            break;
        }
        if ((ix = next_slot(tb, ix, &lck)) == 0) {
            bp = NULL;
            break;
        }
    }
    if (bp != NULL) {
        *ret = db_copy_key(p, tbl, &bp->dbterm);
        RUNLOCK_HASH(lck);
    } else {
        *ret = am_EOT;
    }
    return DB_ERROR_NONE;
}

static int
db_next_nhash(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    Uint ix;
    HashValue hval;
    TrunkDbTerm *bp;
    TrunkOrBranchDbTerm kp;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, &tb->linearht, hval);
    bp = BUCKET(tb, ix);
    for (;;) {
        if (bp == NULL) {
            RUNLOCK_HASH(lck);
            return DB_ERROR_BADKEY;
        }
        if (has_key(tb, bp, key, hval))
            break;
        bp = bp->next;
    }
    /* Key found */
    bp = bp->next;
    kp.trunk = bp;
    bp = next_dbterm(tb, &ix, &lck, &bp, kp);
    if (bp == NULL) {
        *ret = am_EOT;
    } else {
        *ret = db_copy_key(p, tbl, &bp->dbterm);
        RUNLOCK_HASH(lck);
    }
    return DB_ERROR_NONE;
}

int
db_put_nhash(DbTable *tbl, Eterm obj, int key_clash_fail)
{
    /* !!!! */
    int ix, nitems, ret = DB_ERROR_NONE;
    Eterm key;
    HashValue hval;
    TrunkDbTerm **bpp;
    TrunkDbTerm *bp, *qp;
    TrunkOrBranchDbTerm kp;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    key = GETKEY(tb, tuple_val(obj));
    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, &tb->linearht, hval);
    bpp = &BUCKET(tb, ix);
    bp = *bpp;
    for (;;) {
        if (bp == NULL) {
            qp = new_trunk_dbterm(tb, obj);
            qp->next = NULL;
            qp->nkitems = 1;
            qp->lht = NULL;
            qp->branch = NULL;
            /* !!!! qp->hvalue ???? */
            goto Lnew;
        }
        if (has_key(tb, bp, key, hval))
            break;
        bpp = &bp->next;
        bp = *bpp;
    }
    /*
     * Key found
     */
    ASSERT(tb->common.status & (DB_BAG | DB_DUPLICATE_BAG));
    if (key_clash_fail) { /* && (DB_BAG || DB_DUPLICATE_BAG) */
        if (bp->hvalue != INVALID_HASH) {
            ret = DB_ERROR_BADKEY;
            goto Ldone;
        }
    } else if (tb->common.status & DB_BAG) {
        if (bp->lht != NULL) {
            int nix;
            HashValue ohval;
            NestedDbTerm *ntp;
            ohval = MAKE_HASH(obj);
            /* ??? Use a specialized version of hash_to_ix ??? */
            nix = hash_to_ix(tb, bp->lht, ohval);
            ntp = NESTED_BUCKET(tb, bp->lht, nix);
            while (ntp != NULL) {
                kp = ntp->hdbterm;
                if (kp.trunk == bp) {
                    if (db_eq(&tb->common, obj, &kp.trunk->dbterm)) {
                        if (kp.trunk->hvalue == INVALID_HASH) {
                            erts_smp_atomic_inc_nob(&tb->common.nitems);
                            kp.trunk->hvalue = hval;
                        }
                        goto Ldone;
                    }
                } else {
                    if (db_eq(&tb->common, obj, &kp.branch->dbterm))
                        goto Ldone;
                }
                ntp = ntp->next;
            }
        } else {
            BranchDbTerm *jp;
            if (db_eq(&tb->common, obj, &bp->dbterm)) {
                if (bp->hvalue == INVALID_HASH) {
                    erts_smp_atomic_inc_nob(&tb->common.nitems);
                    bp->hvalue = hval;
                }
                goto Ldone;
            }
            jp = bp->branch;
            while (jp != NULL) {
                if (db_eq(&tb->common, obj, &jp->dbterm))
                    goto Ldone;
                jp = jp->next;
            }
        }
    }
    /* else DB_DUPLICATE_BAG */
    if (bp->hvalue == INVALID_HASH) {
        /*
         * Recycle the TrunkDbTerm if pseudo-deleted
         */
#ifdef DEBUG
        LinearHashTable *lht = bp->lht;
#endif
        ASSERT(bp->branch == NULL);
        erts_smp_atomic_inc_nob(&tb->common.nitems);
        if (bp->lht != NULL) {
            kp.trunk = bp;
            remove_nested_dbterm(tb, bp->lht, kp, 1);
        }
        bp = replace_trunk_dbterm(tb, bp, obj);
        bp->hvalue = hval;
        *bpp = bp;
        ASSERT(bp->lht == lht);
        if (bp->lht != NULL) {
            kp.trunk = bp;
            put_nested_dbterm(tb, bp->lht, kp, 1);
        }
        goto Ldone;
    }
    qp = new_trunk_dbterm(tb, obj);
    qp->next = bp->next;
    qp->nkitems = bp->nkitems + 1;
    qp->lht = bp->lht;
    kp.branch = realloc_as_branch_dbterm(tb, bp);
    kp.branch->previous.trunk = qp;
    qp->branch = kp.branch;

Lnew:
    qp->hvalue = hval;
    *bpp = qp;
    /* ???? */
    if (qp->lht != NULL) {
        put_nested_dbterm(tb, qp->lht, qp, 1);
        if (qp->nkitems > NACTIVE(qp->lht) * (CHAIN_LEN+1)) {
            nested_grow(tb, qp->lht);
        }
    } else if ((tb->common.status & DB_BAG)
               && (qp->nkitems > DB_BAG_LHT_LEN)) {
        /*
         * NOTE: Once created, the nested table is removed only
         * when all the elements of the 'knext' chain are deleted.
         */
        qp->lht = (LinearHashTable *)
            erts_db_alloc_fnf(ERTS_ALC_T_DB_SEG, /* ??? Use another type ??? */
                              (DbTable *)tb,
                              sizeof(LinearHashTable));
        /* If qp->lht == NULL try again next time */
        if (qp->lht != NULL) {
            create_linear_hash(tb, qp->lht);
            nitems = 0;
            bp = qp;
            do {
                put_nested_dbterm(tb, qp->lht, bp, ?);
                ++nitems;
                if (nitems > NACTIVE(qp->lht) * (CHAIN_LEN+1)) {
                    nested_grow(tb, qp->lht);
                }
                bp = bp->knext;
            } while (bp != NULL);
        }
    }
    nitems = erts_smp_atomic_inc_read_nob(&tb->common.nitems);
    WUNLOCK_HASH(lck);
    {
        int nactive = NACTIVE(&tb->linearht);
        /* ??? Instead of 'nitems' we should use ??? */
        /* ??? the number of distinct keys ??? */
        if (nitems > nactive * (CHAIN_LEN+1) && !IS_FIXED(tb)) {
            grow(tb, &tb->linearht, nactive);
        }
    }
    CHECK_TABLES();
    return DB_ERROR_NONE;

Ldone:
    WUNLOCK_HASH(lck);
    return ret;
}

int db_get_nhash(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableNestedHash *tb = &tbl->nested;
    HashValue hval;
    int ix;
    NestedHashDbTerm* b;
    erts_smp_rwmtx_t* lck;

    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb,hval);
    ix = hash_to_ix(tb, &tb->linearht, hval);
    b = BUCKET(tb, ix);

    while (b != 0) {
        /* let build_term_list() skip not-alive-key terms */
        if (has_key(tb, b, key, hval)) {
            Eterm copy;

            copy = build_term_list(p, b, b->next, tb);
            CHECK_TABLES();
            *ret = copy;
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
    DbTableNestedHash *tb = &tbl->nested;
    HashValue hval;
    int ix;
    NestedHashDbTerm *b1, *b2;
    int num = 0;
    int retval;
    erts_smp_rwmtx_t* lck;

    ASSERT(!IS_FIXED(tbl)); /* no support for fixed tables here */

    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, &tb->linearht, hval);
    b1 = BUCKET(tb, ix);

    while(b1 != 0) {
        if (has_key(tb, b1, key, hval)) {
            ASSERT(b1->hvalue != INVALID_HASH);
            if (ndex > arityval(b1->dbterm.tpl[0])) {
                retval = DB_ERROR_BADITEM;
                goto done;
            }
            ASSERT(tb->common.status & (DB_BAG | DB_DUPLICATE_BAG));
            b2 = b1->knext;

            while(b2 != NULL) {
                /* ??? Scan the whole 'knext' chain or only the first *num_ret items ??? */
                ASSERT(b2->hvalue != INVALID_HASH);
                if (ndex > arityval(b2->dbterm.tpl[0])) {
                    retval = DB_ERROR_BADITEM;
                    goto done;
                }
                b2 = b2->knext;
            }

            while(b1 != 0) {
                if (num < *num_ret) {
                    ret[num++] = b1->dbterm.tpl[ndex];
                } else {
                    retval = DB_ERROR_NONE;
                    goto done;
                }
                b1 = b1->knext;
            }
            *num_ret = num;
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

static int db_member_nhash(DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableNestedHash *tb = &tbl->nested;
    HashValue hval;
    int ix;
    NestedHashDbTerm* b1;
    erts_smp_rwmtx_t* lck;

    hval = MAKE_HASH(key);
    ix = hash_to_ix(tb, &tb->linearht, hval);
    lck = RLOCK_HASH(tb, hval);
    b1 = BUCKET(tb, ix);

    while(b1 != NULL) {
        if (has_key(tb, b1, key, hval)) {
            do {
                if (b1->hvalue != INVALID_HASH) {
                    *ret = am_true;
                    goto done;
                }
                b1 = b1->knext;
            } while (b1 != NULL);
            break;
        }
        b1 = b1->next;
    }
    *ret = am_false;
done:
    RUNLOCK_HASH(lck);
    return DB_ERROR_NONE;
}

static int db_get_element_nhash(Process *p, DbTable *tbl,
                                Eterm key,
                                int ndex,
                                Eterm *ret)
{
    DbTableNestedHash *tb = &tbl->nested;
    HashValue hval;
    int ix;
    NestedHashDbTerm *b1, *b2;
    erts_smp_rwmtx_t *lck;
    int retval;
    Eterm elem_list;

    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, &tb->linearht, hval);
    b1 = BUCKET(tb, ix);

    while (b1 != 0) {
        if (has_key(tb, b1, key, hval)) {
            while ((b1 != 0) && (b1->hvalue == INVALID_HASH)) {
                b1 = b1->knext;
            }
            if (b1 == 0)
                break;

            if (ndex > arityval(b1->dbterm.tpl[0])) {
                retval = DB_ERROR_BADITEM;
                goto done;
            }
            ASSERT(tb->common.status & (DB_BAG | DB_DUPLICATE_BAG));
            b2 = b1->knext;
            elem_list = NIL;

            while(b2 != NULL) {
                if (ndex > arityval(b2->dbterm.tpl[0])
                    && b2->hvalue != INVALID_HASH) {
                    retval = DB_ERROR_BADITEM;
                    goto done;
                }
                b2 = b2->knext;
            }
            do {
                if (b1->hvalue != INVALID_HASH) {
                    Eterm *hp;
                    Eterm copy = db_copy_element_from_ets(&tb->common, p,
                                                          &b1->dbterm, ndex, &hp, 2);
                    elem_list = CONS(hp, copy, elem_list);
                    hp += 2;
                }
                b1 = b1->knext;
            } while (b1 != NULL);
            *ret = elem_list;
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
    DbTableNestedHash *tb = &tbl->nested;
    HashValue hval;
    int ix;
    NestedHashDbTerm **bp;
    NestedHashDbTerm *b;
    erts_smp_rwmtx_t *lck;
    int found = 0;

    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb,hval);
    ix = hash_to_ix(tb, &tb->linearht, hval);
    bp = &BUCKET(tb, ix);

    ASSERT(!IS_FIXED(tb));
    ASSERT((tb->common.status & DB_BAG));
    ASSERT(!tb->common.compress);

    while((b = *bp) != 0) {
        if (has_key(tb, b, key, hval)) {
            found = 1;
            ASSERT(b->hvalue != INVALID_HASH);
            if (b->lht != NULL) {
                int nix;
                Eterm storage[3];
                NestedDbTerm *ntp;
                /* ??? Is this correct ??? */
                hval = MAKE_HASH(TUPLE2(storage, key, value));
                /* ??? Use a specialized version of hash_to_ix ??? */
                nix = hash_to_ix(tb, b->lht, hval);
                ntp = NESTED_BUCKET(tb, b->lht, nix);
                while (ntp != NULL) {
                    b = ntp->hdbterm;
                    if ((arityval(b->dbterm.tpl[0]) == 2) &&
                        EQ(value, b->dbterm.tpl[2])) {
                        remove_dbterm(tb, &bp, &b, 0);
                        erts_smp_atomic_dec_nob(&tb->common.nitems);
                        break;
                    }
                    ntp = ntp->next;
                }
            } else {
                do {
                    if ((arityval(b->dbterm.tpl[0]) == 2) &&
                        EQ(value, b->dbterm.tpl[2])) {
                        remove_dbterm(tb, &bp, &b, 0);
                        erts_smp_atomic_dec_nob(&tb->common.nitems);
                        break;
                    }
                    b = b->knext;
                } while (b != NULL);
            }
            break;
        }
        bp = &b->next;
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
int db_erase_nhash(DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableNestedHash *tb = &tbl->nested;
    HashValue hval;
    int ix;
    NestedHashDbTerm **bp;
    NestedHashDbTerm *b;
    erts_smp_rwmtx_t *lck;
    int nitems_diff = 0;

    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb,hval);
    ix = hash_to_ix(tb, &tb->linearht, hval);
    bp = &BUCKET(tb, ix);

    while((b = *bp) != NULL) {
        if (has_key(tb, b, key, hval)) {
            do {
                if (b->hvalue != INVALID_HASH) {
                    --nitems_diff;
                    switch (remove_dbterm(tb, &bp, &b, IS_FIXED(tb))) {
                    case 2: /* Pseudo deletion, implies no more terms with 'key' key */
                        add_fixed_deletion(tb, ix);
                    case 1: /* No more terms with 'key' key */
                        /* Break loop */
                        b = NULL;
                    /* case 0: More terms with 'key' key */
                    }
                } else {
                    b = b->knext;
                }
            } while (b != NULL);
            break;
        }
        bp = &b->next;
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
static int db_erase_object_nhash(DbTable *tbl, Eterm object, Eterm *ret)
{
    DbTableNestedHash *tb = &tbl->nested;
    HashValue hval;
    int ix;
    NestedHashDbTerm **bp;
    NestedHashDbTerm *b;
    erts_smp_rwmtx_t *lck;
    int nitems_diff = 0;
    Eterm key;

    key = GETKEY(tb, tuple_val(object));
    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb,hval);
    ix = hash_to_ix(tb, &tb->linearht, hval);
    bp = &BUCKET(tb, ix);

    while((b = *bp) != NULL) {
        if (has_key(tb, b, key, hval)) {
            if (b->lht != NULL) {
                int nix;
                NestedHashDbTerm **bp2;
                NestedDbTerm *ntp;
                /* ??? Is this correct ??? */
                hval = MAKE_HASH(object);
                /* ??? Use a specialized version of hash_to_ix ??? */
                nix = hash_to_ix(tb, b->lht, hval);
                ntp = NESTED_BUCKET(tb, b->lht, nix);
                while (ntp != NULL) {
                    b = ntp->hdbterm;
                    if ((b->hvalue != INVALID_HASH)
                        && db_eq(&tb->common, object, &b->dbterm)) {
                        --nitems_diff;
                        bp2=bp;
                        if (remove_dbterm(tb, &bp2, &b, IS_FIXED(tb)) == 2) {
                            add_fixed_deletion(tb, ix);
                            break;
                        }
                        if (!(tb->common.status & (DB_DUPLICATE_BAG))) {
                            break;
                        }
                    }
                    ntp = ntp->next;
                }
            } else {
                do {
                    if ((b->hvalue != INVALID_HASH)
                        && db_eq(&tb->common, object, &b->dbterm)) {
                        --nitems_diff;
                        switch (remove_dbterm(tb, &bp, &b, IS_FIXED(tb))) {
                        case 2: /* Pseudo deletion, implies no more terms with 'key' key */
                            add_fixed_deletion(tb, ix);
                        case 1: /* No more terms with 'key' key */
                            /* Break loop */
                            b = NULL;
                            break;
                        default: /* More terms with 'key' key */
                            if (!(tb->common.status & (DB_DUPLICATE_BAG))) {
                                b = NULL;
                            }
                        }
                    } else {
                        b = b->knext;
                    }
                } while (b != NULL);
            }
            break;
        }
        bp = &b->next;
    }
    WUNLOCK_HASH(lck);
    if (nitems_diff) {
        erts_smp_atomic_add_nob(&tb->common.nitems, nitems_diff);
        try_shrink(tb);
    }
    *ret = am_true;
    return DB_ERROR_NONE;
}

static int db_slot_nhash(Process *p, DbTable *tbl, Eterm slot_term, Eterm *ret)
{
    DbTableNestedHash *tb = &tbl->nested;
    erts_smp_rwmtx_t* lck;
    Sint slot;
    int retval;
    int nactive;

    if (is_not_small(slot_term) || ((slot = signed_val(slot_term)) < 0)) {
        return DB_ERROR_BADPARAM;
    }
    lck = RLOCK_HASH(tb, slot);
    nactive = NACTIVE(&tb->linearht);
    if (slot < nactive) {
        *ret = build_term_list(p, BUCKET(tb, slot), 0, tb);
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
static int db_select_continue_nhash(Process *p,
                                    DbTable *tbl,
                                    Eterm continuation,
                                    Eterm *ret)
{
    DbTableNestedHash *tb = &tbl->nested;
    Sint slot_ix;
    Sint save_slot_ix;
    Sint chunk_size;
    int all_objects;
    Binary *mp;
    int num_left = 1000;
    NestedHashDbTerm *current, *kcurrent;
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
    if (slot_ix >= NACTIVE(&tb->linearht)) {
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
    kcurrent = current;
    for(;;) {
        if (kcurrent->hvalue != INVALID_HASH &&
            (match_res = db_match_dbterm(&tb->common, p, mp, all_objects,
                                         &kcurrent->dbterm, &hp, 2),
             is_value(match_res))) {

            match_list = CONS(hp, match_res, match_list);
            ++got;
        }

        --num_left;
        save_slot_ix = slot_ix;
        /* !!! kcurrent MUST be a TrunkOrBranchDbTerm !!! */
        NEXT_DBTERM(current, kcurrent);
        if ((kcurrent = next_dbterm(tb, (Uint*)&slot_ix, &lck, &current, kcurrent)) == NULL) {
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

static int db_select_nhash(Process *p, DbTable *tbl,
                           Eterm pattern, int reverse,
                           Eterm *ret)
{
    return db_select_chunk_nhash(p, tbl, pattern, 0, reverse, ret);
}

static int db_select_chunk_nhash(Process *p, DbTable *tbl,
                                 Eterm pattern, Sint chunk_size,
                                 int reverse, /* not used */
                                 Eterm *ret)
{
    DbTableNestedHash *tb = &tbl->nested;
    struct mp_info mpi;
    Sint slot_ix;
    NestedHashDbTerm *current, *kcurrent;
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


#define RET_TO_BIF(Term,RetVal) do {            \
        if (mpi.mp != NULL) {                   \
            erts_bin_free(mpi.mp);              \
        }                                       \
        if (mpi.lists != mpi.dlists) {          \
            erts_free(ERTS_ALC_T_DB_SEL_LIST,   \
                      (void *) mpi.lists);      \
        }                                       \
        *ret = (Term);                          \
        return RetVal;                          \
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
            ASSERT(slot_ix < NACTIVE(&tb->linearht));
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
    kcurrent = current;

    for(;;) {
        if (kcurrent != NULL) {
            if (kcurrent->hvalue != INVALID_HASH) {
                match_res = db_match_dbterm(&tb->common, p, mpi.mp, 0,
                                            &kcurrent->dbterm, &hp, 2);
                if (is_value(match_res)) {
                    match_list = CONS(hp, match_res, match_list);
                    ++got;
                }
            }
            /* !!! kcurrent MUST be a TrunkOrBranchDbTerm !!! */
            NEXT_DBTERM(current, kcurrent);
        }
        else if (mpi.key_given) {  /* Key is bound */
            RUNLOCK_HASH(lck);
            if (current_list_pos == mpi.num_lists) {
                slot_ix = -1; /* EOT */
                goto done;
            } else {
                slot_ix = mpi.lists[current_list_pos].ix;
                lck = RLOCK_HASH(tb, slot_ix);
                kcurrent = current = *(mpi.lists[current_list_pos].bucket);
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
            kcurrent = current = BUCKET(tb, slot_ix);
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

static int db_select_count_nhash(Process *p,
                                 DbTable *tbl,
                                 Eterm pattern,
                                 Eterm *ret)
{
    DbTableNestedHash *tb = &tbl->nested;
    struct mp_info mpi;
    Uint slot_ix = 0;
    NestedHashDbTerm *current, *kcurrent;
    unsigned current_list_pos = 0;
    Eterm *hp;
    int num_left = 1000;
    Uint got = 0;
    Eterm continuation;
    int errcode;
    Eterm egot;
    Eterm mpb;
    erts_smp_rwmtx_t* lck;

#define RET_TO_BIF(Term,RetVal) do {            \
        if (mpi.mp != NULL) {                   \
            erts_bin_free(mpi.mp);              \
        }                                       \
        if (mpi.lists != mpi.dlists) {          \
            erts_free(ERTS_ALC_T_DB_SEL_LIST,   \
                      (void *) mpi.lists);      \
        }                                       \
        *ret = (Term);                          \
        return RetVal;                          \
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

    kcurrent = current;
    for(;;) {
        if (kcurrent != NULL) {
            if (kcurrent->hvalue != INVALID_HASH) {
                if (db_match_dbterm(&tb->common, p, mpi.mp, 0,
                                    &kcurrent->dbterm, NULL,0) == am_true) {
                    ++got;
                }
                --num_left;
            }
            /* !!! kcurrent MUST be a TrunkOrBranchDbTerm !!! */
            NEXT_DBTERM(current, kcurrent);
        }
        else { /* next bucket */
            if (mpi.key_given) {  /* Key is bound */
                RUNLOCK_HASH(lck);
                if (current_list_pos == mpi.num_lists) {
                    goto done;
                } else {
                    slot_ix = mpi.lists[current_list_pos].ix;
                    lck = RLOCK_HASH(tb, slot_ix);
                    kcurrent = current = *(mpi.lists[current_list_pos].bucket);
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
                kcurrent = current = BUCKET(tb, slot_ix);
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

static int db_select_delete_nhash(Process *p,
                                  DbTable *tbl,
                                  Eterm pattern,
                                  Eterm *ret)
{
    DbTableNestedHash *tb = &tbl->nested;
    struct mp_info mpi;
    Uint slot_ix = 0;
    NestedHashDbTerm **current, *kcurrent;
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

#define RET_TO_BIF(Term, RetVal) do {           \
        if (mpi.mp != NULL) {                   \
            erts_bin_free(mpi.mp);              \
        }                                       \
        if (mpi.lists != mpi.dlists) {          \
            erts_free(ERTS_ALC_T_DB_SEL_LIST,   \
                      (void *) mpi.lists);      \
        }                                       \
        *ret = (Term);                          \
        return RetVal;                          \
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


    kcurrent = *current;
    for(;;) {
        if (kcurrent == NULL) {
            if (mpi.key_given) {  /* Key is bound */
                WUNLOCK_HASH(lck);
                if (current_list_pos == mpi.num_lists) {
                    goto done;
                } else {
                    slot_ix = mpi.lists[current_list_pos].ix;
                    lck = WLOCK_HASH(tb, slot_ix);
                    current = mpi.lists[current_list_pos].bucket;
                    kcurrent = *current;
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
                kcurrent = *current;
            }
        }
        else if (kcurrent->hvalue == INVALID_HASH) {
            /* !!! kcurrent MUST be a TrunkOrBranchDbTerm !!! */
            NEXT_DBTERM_P(current, kcurrent);
        }
        else {
            if (db_match_dbterm(&tb->common, p, mpi.mp, 0,
                                &kcurrent->dbterm, NULL, 0) == am_true) {
                if (remove_dbterm(tb, &current, &kcurrent,
                                  /* fixated by others? */
                                  NFIXED(tb) > fixated_by_me) == 2) {
                    /* Pseudo deletion */
                    if (slot_ix != last_pseudo_delete) {
                        add_fixed_deletion(tb, slot_ix);
                        last_pseudo_delete = slot_ix;
                    }
                }
                erts_smp_atomic_dec_nob(&tb->common.nitems);
                ++got;
            } else {
                /* !!! kcurrent MUST be a TrunkOrBranchDbTerm !!! */
                NEXT_DBTERM_P(current, kcurrent);
            }
            --num_left;
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
static int db_select_delete_continue_nhash(Process *p,
                                           DbTable *tbl,
                                           Eterm continuation,
                                           Eterm *ret)
{
    DbTableNestedHash *tb = &tbl->nested;
    Uint slot_ix;
    Uint last_pseudo_delete = (Uint)-1;
    NestedHashDbTerm **current, *kcurrent;
    Eterm *hp;
    int num_left = 1000;
    Uint got;
    Eterm *tptr;
    Binary *mp;
    Eterm egot;
    int fixated_by_me = ONLY_WRITER(p,tb) ? 0 : 1; /* ToDo: something nicer */
    erts_smp_rwmtx_t* lck;

#define RET_TO_BIF(Term,RetVal) do {            \
        *ret = (Term);                          \
        return RetVal;                          \
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
    if (slot_ix >= NACTIVE(&tb->linearht)) {
        WUNLOCK_HASH(lck);
        goto done;
    }
    current = &BUCKET(tb,slot_ix);
    kcurrent = *current;

    for(;;) {
        if (kcurrent == NULL) {
            if ((slot_ix=next_slot_w(tb,slot_ix,&lck)) == 0) {
                goto done;
            }
            if (num_left <= 0) {
                WUNLOCK_HASH(lck);
                goto trap;
            }
            current = &BUCKET(tb,slot_ix);
            kcurrent = *current;
        }
        else if (kcurrent->hvalue == INVALID_HASH) {
            /* !!! kcurrent MUST be a TrunkOrBranchDbTerm !!! */
            NEXT_DBTERM_P(current, kcurrent);
        }
        else {
            if (db_match_dbterm(&tb->common, p, mp, 0,
                                &kcurrent->dbterm, NULL, 0) == am_true) {
                if (remove_dbterm(tb, &current, &kcurrent,
                                  /* fixated by others? */
                                  NFIXED(tb) > fixated_by_me) == 2) {
                    /* Pseudo deletion */
                    if (slot_ix != last_pseudo_delete) {
                        add_fixed_deletion(tb, slot_ix);
                        last_pseudo_delete = slot_ix;
                    }
                }
                erts_smp_atomic_dec_nob(&tb->common.nitems);
                ++got;
            } else {
                /* !!! kcurrent MUST be a TrunkOrBranchDbTerm !!! */
                NEXT_DBTERM_P(current, kcurrent);
            }
            --num_left;
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
static int db_select_count_continue_nhash(Process *p,
                                          DbTable *tbl,
                                          Eterm continuation,
                                          Eterm *ret)
{
    DbTableNestedHash *tb = &tbl->nested;
    Uint slot_ix;
    NestedHashDbTerm *current, *kcurrent;
    Eterm *hp;
    int num_left = 1000;
    Uint got;
    Eterm *tptr;
    Binary *mp;
    Eterm egot;
    erts_smp_rwmtx_t* lck;

#define RET_TO_BIF(Term,RetVal) do {            \
        *ret = (Term);                          \
        return RetVal;                          \
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
    if (slot_ix >= NACTIVE(&tb->linearht)) { /* Is this posible? */
        RUNLOCK_HASH(lck);
        goto done;
    }
    kcurrent = current = BUCKET(tb, slot_ix);

    for(;;) {
        if (kcurrent != NULL) {
            if (kcurrent->hvalue != INVALID_HASH) {
                if (db_match_dbterm(&tb->common, p, mp, 0, &kcurrent->dbterm,
                                    NULL, 0) == am_true) {
                    ++got;
                }
                --num_left;
            }
            /* !!! kcurrent MUST be a TrunkOrBranchDbTerm !!! */
            NEXT_DBTERM(current, kcurrent);
        }
        else { /* next bucket */
            if ((slot_ix = next_slot(tb,slot_ix,&lck)) == 0) {
                goto done;
            }
            if (num_left <= 0) {
                RUNLOCK_HASH(lck);
                goto trap;
            }
            kcurrent = current = BUCKET(tb, slot_ix);
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

/*
** Other interface routines (not directly coupled to one bif)
*/

void db_initialize_nhash(void)
{
}


int db_mark_all_deleted_nhash(DbTable *tbl)
{
    DbTableNestedHash *tb = &tbl->nested;
    NestedHashDbTerm **listp, *klist;
    int i;

    ERTS_SMP_LC_ASSERT(IS_TAB_WLOCKED(tb));

    for (i = 0; i < NACTIVE(&tb->linearht); i++) {
        listp = &BUCKET(tb, i);
        if ((klist = *listp) != NULL) {
            add_fixed_deletion(tb, i);
            do {
                remove_dbterm(tb, &listp, &klist, 1);
            } while (klist != NULL);
        }
    }
    erts_smp_atomic_set_nob(&tb->common.nitems, 0);
    return DB_ERROR_NONE;
}


/* Display hash table contents (for dump) */
static void db_print_nhash(int to, void *to_arg, int show, DbTable *tbl)
{
    DbTableNestedHash *tb = &tbl->nested;
    int i;

    erts_print(to, to_arg, "Buckets: %d\n", NACTIVE(&tb->linearht));

    if (show) {
        for (i = 0; i < NACTIVE(&tb->linearht); i++) {
            NestedHashDbTerm *list, *klist;
            list = BUCKET(tb, i);
            if (list == NULL)
                continue;
            klist = list;
            erts_print(to, to_arg, "%d: [", i);
            do {
                if (klist->hvalue == INVALID_HASH)
                    erts_print(to, to_arg, "*");
                if (tb->common.compress) {
                    Eterm key = GETKEY(tb, klist->dbterm.tpl);
                    erts_print(to, to_arg, "key=%R", key, klist->dbterm.tpl);
                }
                else {
                    Eterm obj = make_tuple_rel(klist->dbterm.tpl,klist->dbterm.tpl);
                    erts_print(to, to_arg, "%R", obj, klist->dbterm.tpl);
                }
                /* !!! kcurrent MUST be a TrunkOrBranchDbTerm !!! */
                NEXT_DBTERM(list, klist);
                if (klist != 0)
                    erts_print(to, to_arg, ",");
            } while (klist != 0);
            erts_print(to, to_arg, "]\n");
        }
    }
}

/* release all memory occupied by a single table */
static int db_free_table_nhash(DbTable *tbl)
{
    while (!db_free_table_continue_nhash(tbl));
    return 0;
}

static int db_free_table_continue_nhash(DbTable *tbl)
{
    DbTableNestedHash *tb = &tbl->nested;
    int done;
    NestedFixedDeletion* fixdel = (NestedFixedDeletion*) erts_smp_atomic_read_acqb(&tb->fixdel);
    ERTS_SMP_LC_ASSERT(IS_TAB_WLOCKED(tb));

    done = 0;
    while (fixdel != NULL) {
        NestedFixedDeletion *fx = fixdel;

        fixdel = fx->next;
        erts_db_free(ERTS_ALC_T_DB_FIX_DEL,
                     (DbTable *) tb,
                     (void *) fx,
                     sizeof(NestedFixedDeletion));
        ERTS_ETS_MISC_MEM_ADD(-sizeof(NestedFixedDeletion));
        if (++done >= 2*DELETE_RECORD_LIMIT) {
            erts_smp_atomic_set_relb(&tb->fixdel, (erts_aint_t)fixdel);
            return 0; /* Not done */
        }
    }
    erts_smp_atomic_set_relb(&tb->fixdel, (erts_aint_t)NULL);

    done /= 2;
    /* ??? Shouldn't we divide also by CHAIN_LEN * SEGSZ ??? */
    while(tb->linearht.nslots != 0) {
        free_seg(tb, &tb->linearht, 1, 0);

        /*
         * If we have done enough work, get out here.
         */
        if (++done >= (DELETE_RECORD_LIMIT / CHAIN_LEN / SEGSZ)) {
            return 0; /* Not done */
        }
    }
#ifdef ERTS_SMP
    if (tb->locks != NULL) {
        int i;
        for (i=0; i<DB_NESTED_HASH_LOCK_CNT; ++i) {
            erts_rwmtx_destroy(GET_LOCK(tb, i));
        }
        erts_db_free(ERTS_ALC_T_DB_SEG, (DbTable *)tb,
                     (void*)tb->locks, sizeof(DbTableNestedHashFineLocks));
        tb->locks = NULL;
    }
#endif
    ASSERT(erts_smp_atomic_read_nob(&tb->common.memory_size) == sizeof(DbTable));
    return 1; /* Done */
}



/*
** Utility routines. (static)
*/
/*
** For the select functions, analyzes the pattern and determines which
** slots should be searched. Also compiles the match program
*/
static int analyze_pattern(DbTableNestedHash *tb, Eterm pattern,
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
                    NestedHashDbTerm** bp;
                    erts_smp_rwmtx_t* lck;
                    hval = MAKE_HASH(key);
                    lck = RLOCK_HASH(tb,hval);
                    ix = hash_to_ix(tb, &tb->linearht, hval);
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

static struct ext_segment* alloc_ext_seg(DbTableNestedHash *tb, LinearHashTable *lht,
                                         unsigned seg_ix, struct segment **old_segtab)
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
    if (eseg == NULL)
        return NULL;
    sys_memset(&eseg->s, 0, sizeof(struct segment));
    IF_DEBUG(eseg->s.is_ext_segment = 1);
    eseg->prev_segtab = old_segtab;
    eseg->nsegs = nsegs;
    if (old_segtab) {
        ASSERT(nsegs > lht->nsegs);
        sys_memcpy(eseg->segtab, old_segtab, lht->nsegs*sizeof(struct segment*));
    }
#ifdef DEBUG
    sys_memset(&eseg->segtab[seg_ix], 0, (nsegs-seg_ix)*sizeof(struct segment*));
#endif
    eseg->segtab[seg_ix] = &eseg->s;
    return eseg;
}

/* Extend table with one new segment
*/
static int alloc_seg(DbTableNestedHash *tb, LinearHashTable *lht)
{
    int seg_ix = lht->nslots >> SEGSZ_EXP;

    if (seg_ix+1 == lht->nsegs) { /* New segtab needed (extended segment) */
        struct segment **segtab = SEGTAB(tb, lht);
        struct ext_segment *seg = alloc_ext_seg(tb, lht, seg_ix, segtab);
        if (seg == NULL) return 0;
        segtab[seg_ix] = &seg->s;
        /* We don't use the new segtab until next call (see "shrink race") */
    }
    else { /* Just a new plain segment */
        struct segment** segtab;
        if (seg_ix == lht->nsegs) { /* Time to start use segtab from last call */
            struct ext_segment* eseg;
            eseg = (struct ext_segment*) SEGTAB(tb, lht)[seg_ix-1];
            MY_ASSERT(eseg!=NULL && eseg->s.is_ext_segment);
            SET_SEGTAB(tb, lht, eseg->segtab);
            lht->nsegs = eseg->nsegs;
        }
        ASSERT(seg_ix < lht->nsegs);
        segtab = SEGTAB(tb, lht);
        ASSERT(segtab[seg_ix] == NULL);
        segtab[seg_ix] = (struct segment*) erts_db_alloc_fnf(ERTS_ALC_T_DB_SEG,
                                                             (DbTable *) tb,
                                                             sizeof(struct segment));
        if (segtab[seg_ix] == NULL) return 0;
        sys_memset(segtab[seg_ix], 0, sizeof(struct segment));
    }
    lht->nslots += SEGSZ;
    return 1;
}

/* Shrink table by freeing the top segment
** free_records: 1=free any records in segment, 0=assume segment is empty
*/
static int free_seg(DbTableNestedHash *tb, LinearHashTable *lht, int free_records, int is_nested)
{
    int seg_ix = (lht->nslots >> SEGSZ_EXP) - 1;
    int bytes;
    struct segment **segtab = SEGTAB(tb, lht);
    struct ext_segment* top = (struct ext_segment*) segtab[seg_ix];
    int nrecords = 0;

    ASSERT(top != NULL);
#ifndef DEBUG
    if (free_records)
#endif
    {
        int i;
        if (is_nested) {
            for (i=0; i<SEGSZ; ++i) {
                NestedDbTerm **ntpp,*ntp;
                ntpp = &top->s.buckets[i].nterm;
                while ((ntp = *ntpp) != 0) {
                    ASSERT(free_records); /* segment not empty as assumed? */
                    *ntpp = ntp->next;
                    free_nested_term(tb, ntp);
                    ++nrecords;
                }
            }
        } else {
            for (i=0; i<SEGSZ; ++i) {
                NestedHashDbTerm **bp,*p;
                bp = &top->s.buckets[i].hterm;
                p = *bp;
                while (p != NULL) {
                    ASSERT(free_records); /* segment not empty as assumed? */
                    remove_dbterm(tb, &bp, &p, 0);
                    ++nrecords;
                }
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

    if (seg_ix == lht->nsegs-1 || seg_ix==0) { /* Dealloc extended segment */
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
                SET_SEGTAB(tb, lht, newtop->prev_segtab);
                lht->nsegs = seg_ix;
                ASSERT(lht->nsegs == EXTSEG(SEGTAB(tb, lht))->nsegs);
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
        if (seg_ix < lht->nsegs) SEGTAB(tb, lht)[seg_ix] = NULL;
    } else {
        SET_SEGTAB(tb, lht, NULL);
    }
#endif
    lht->nslots -= SEGSZ;
    ASSERT(lht->nslots >= 0);
    return nrecords;
}


/*
** Copy terms from ptr1 until ptr2
** works for ptr1 == ptr2 == 0  => []
** or ptr2 == 0
*/
static Eterm build_term_list(Process* p, NestedHashDbTerm* ptr1, NestedHashDbTerm* ptr2,
                             DbTableNestedHash* tb)
{
    int sz = 0;
    NestedHashDbTerm *ptr, *kptr;
    Eterm list = NIL;
    Eterm copy;
    Eterm *hp, *hend;

    kptr = ptr = ptr1;
    while(kptr != ptr2) {
        if (kptr->hvalue != INVALID_HASH)
            sz += kptr->dbterm.size + 2;
        /* !!! kcurrent MUST be a TrunkOrBranchDbTerm !!! */
        NEXT_DBTERM(ptr, kptr);
    }

    hp = HAlloc(p, sz);
    hend = hp + sz;

    kptr = ptr = ptr1;
    while(kptr != ptr2) {
        if (kptr->hvalue != INVALID_HASH) {
            copy = db_copy_object_from_ets(&tb->common, &kptr->dbterm, &hp, &MSO(p));
            list = CONS(hp, copy, list);
            hp  += 2;
        }
        /* !!! kcurrent MUST be a TrunkOrBranchDbTerm !!! */
        NEXT_DBTERM(ptr, kptr);
    }
    HRelease(p,hend,hp);

    return list;
}

static ERTS_INLINE int
begin_resizing(DbTableNestedHash* tb)
{
    if (DB_USING_FINE_LOCKING(tb))
        return !erts_smp_atomic_xchg_acqb(&tb->is_resizing, 1);
    else {
        /* ??? erts_smp_atomic_xchg_nob could be more efficient ??? */
        if (erts_smp_atomic_read_nob(&tb->is_resizing))
            return 0;
        erts_smp_atomic_set_nob(&tb->is_resizing, 1);
        return 1;
    }
}

static ERTS_INLINE void
done_resizing(DbTableNestedHash* tb)
{
    if (DB_USING_FINE_LOCKING(tb)) {
        erts_smp_atomic_set_relb(&tb->is_resizing, 0);
    } else {
        erts_smp_atomic_set_nob(&tb->is_resizing, 0);
    }
}

/* Grow table with one new bucket.
** Allocate new segment if needed.
*/
static void grow(DbTableNestedHash *tb, LinearHashTable *lht, int nactive)
{
    NestedHashDbTerm** pnext;
    NestedHashDbTerm** to_pnext;
    NestedHashDbTerm* p;
    erts_smp_rwmtx_t* lck;
    int from_ix;
    int szm;

    if (!begin_resizing(tb))
        return; /* already in progress */
    if (NACTIVE(lht) != nactive) {
        goto abort; /* already done (race) */
    }

    /* Ensure that the slot nactive exists */
    if (nactive == lht->nslots) {
        /* Time to get a new segment */
        ASSERT((nactive & SEGSZ_MASK) == 0);
        if (!alloc_seg(tb, lht))
            goto abort;
    }
    ASSERT(nactive < lht->nslots);

    szm = erts_smp_atomic_read_nob(&lht->szm);
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
    erts_smp_atomic_inc_nob(&lht->nactive);
    if (from_ix == 0)
        if (DB_USING_FINE_LOCKING(tb)) {
            erts_smp_atomic_set_relb(&lht->szm, szm);
        } else {
            erts_smp_atomic_set_nob(&lht->szm, szm);
        }
    done_resizing(tb);

    /* Finally, let's split the bucket. We try to do it in a smart way
       to keep link order and avoid unnecessary updates of next-pointers */
    pnext = &BUCKET(tb, from_ix);
    p = *pnext;
    to_pnext = &BUCKET(tb, nactive);
    while (p != NULL) {
        if (p->hvalue == INVALID_HASH) { /* rare but possible with fine locking */
            remove_dbterm(tb, &pnext, &p, 0);
        }
        else {
            int ix = p->hvalue & szm;
            if (ix != from_ix) {
                ASSERT(ix == (from_ix ^ ((szm>>1)+1)));
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
static void shrink(DbTableNestedHash *tb, LinearHashTable *lht, int nactive)
{
    if (!begin_resizing(tb))
        return; /* already in progress */
    if (NACTIVE(lht) == nactive) {
        erts_smp_rwmtx_t* lck;
        int src_ix = nactive - 1;
        int low_szm = erts_smp_atomic_read_nob(&lht->szm) >> 1;
        int dst_ix = src_ix & low_szm;

        ASSERT(dst_ix < src_ix);
        ASSERT(nactive > SEGSZ);
        lck = WLOCK_HASH(tb, dst_ix);
        /* Double check for racing table fixers */
        if (!IS_FIXED(tb)) {
            NestedHashDbTerm** src_bp = &BUCKET(tb, src_ix);
            NestedHashDbTerm** dst_bp = &BUCKET(tb, dst_ix);
            NestedHashDbTerm **bp, *kb;
            bp = src_bp;

            /* Q: Why join lists by appending "dst" at the end of "src"?
               A: Must step through "src" anyway to purge pseudo deleted. */
            while ((kb = *bp) != NULL) {
                if ((kb->hvalue != INVALID_HASH)
                    || (!remove_dbterm(tb, &bp, &kb, 0))) {
                    /* Only the root NestedHashDbTerm can be pseudo-deleted */
                    bp = &(*bp)->next;
                }
            }
            *bp = *dst_bp;
            *dst_bp = *src_bp;
            *src_bp = NULL;

            erts_smp_atomic_set_nob(&lht->nactive, src_ix);
            if (dst_ix == 0) {
                erts_smp_atomic_set_relb(&lht->szm, low_szm);
            }
            WUNLOCK_HASH(lck);

            if (lht->nslots - src_ix >= SEGSZ) {
                free_seg(tb, lht, 0, 0);
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

static NestedHashDbTerm* search_list(DbTableNestedHash* tb, Eterm key,
                                     HashValue hval, NestedHashDbTerm *list)
{
    while (list != 0) {
        if (has_key(tb, list, key, hval)) {
            do {
                if (list->hvalue != INVALID_HASH)
                    return list;
                list = list->knext;
            } while (list != 0);
            return 0;
        }
        list = list->next;
    }
    return 0;
}

static int db_lookup_dbterm_nhash(DbTable *tbl, Eterm key, DbUpdateHandle* handle)
{
    DbTableNestedHash *tb = &tbl->nested;
    NestedHashDbTerm* b;
    NestedHashDbTerm** prevp;
    int ix;
    HashValue hval;
    erts_smp_rwmtx_t* lck;

    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, &tb->linearht, hval);
    prevp = &BUCKET(tb, ix);
    b = *prevp;

    while (b != 0) {
        if (has_key(tb, b, key, hval)) {
            do {
                if (b->hvalue != INVALID_HASH) {
                    handle->tb = tbl;
                    handle->bp = (void**) prevp;
                    handle->dbterm = &b->dbterm;
                    handle->mustResize = 0;
                    handle->new_size = b->dbterm.size;
#if HALFWORD_HEAP
                    handle->abs_vec = NULL;
#endif
                    handle->lck = lck;
                    /* KEEP hval WLOCKED, db_finalize_dbterm_nhash will WUNLOCK */
                    return 1;
                }
                prevp = &b->knext;
                b = *prevp;
            } while (b != 0);
            break;
        }
        prevp = &b->next;
        b = *prevp;
    }
    WUNLOCK_HASH(lck);
    return 0;
}

/* Must be called after call to db_lookup_dbterm
*/
static void db_finalize_dbterm_nhash(DbUpdateHandle* handle)
{
    DbTable* tbl = handle->tb;
    TrunkOrBranchDbTerm oldp = (TrunkOrBranchDbTerm*) *(handle->bp);
    erts_smp_rwmtx_t* lck = (erts_smp_rwmtx_t*) handle->lck;

    ERTS_SMP_LC_ASSERT(IS_HASH_WLOCKED(&tbl->nested,lck));  /* locked by db_lookup_dbterm_nhash */

    ASSERT((&oldp.trunk->dbterm == handle->dbterm) == !(tbl->common.compress && handle->mustResize));
    /* !!! Or: ASSERT((&oldp.branch->dbterm == handle->dbterm) == !(tbl->common.compress && handle->mustResize)); !!! */

    if (handle->mustResize) {
        db_finalize_resize(handle, offsetof(NestedHashDbTerm,dbterm));
        WUNLOCK_HASH(lck);

        /*
         * !!! FIX THIS !!!
         * All the pointers to the resize Trunk/BranchDbTerm must be updated.
         */
        free_term(&tbl->nested, oldp.trunk, 1);
    }
    else {
        WUNLOCK_HASH(lck);
    }
#ifdef DEBUG
    handle->dbterm = 0;
#endif
    return;
}

static int db_delete_all_objects_nhash(Process* p, DbTable* tbl)
{
    if (IS_FIXED(tbl)) {
        db_mark_all_deleted_nhash(tbl);
    } else {
        db_free_table_nhash(tbl);
        db_create_nhash(p, tbl);
        erts_smp_atomic_set_nob(&tbl->nested.common.nitems, 0);
    }
    return 0;
}

void db_foreach_offheap_nhash(DbTable *tbl,
                              void (*func)(ErlOffHeap *, void *),
                              void * arg)
{
    DbTableNestedHash *tb = &tbl->nested;
    NestedHashDbTerm *list, *klist;
    int i;
    int nactive = NACTIVE(&tb->linearht);

    for (i = 0; i < nactive; i++) {
        klist = list = BUCKET(tb,i);
        while(klist != 0) {
            ErlOffHeap tmp_offheap;
            tmp_offheap.first = klist->dbterm.first_oh;
            tmp_offheap.overhead = 0;
            (*func)(&tmp_offheap, arg);
            klist->dbterm.first_oh = tmp_offheap.first;
            /* !!! kcurrent MUST be a TrunkOrBranchDbTerm !!! */
            NEXT_DBTERM(list, klist);
        }
    }
}

void db_calc_stats_nhash(DbTableNestedHash *tb, DbNestedHashStats *stats)
{
    NestedHashDbTerm *b, *kb;
    erts_smp_rwmtx_t* lck;
    int sum = 0;
    int sq_sum = 0;
    int ix;
    int len;

    stats->min_chain_len = INT_MAX;
    stats->max_chain_len = 0;
    ix = 0;
    lck = RLOCK_HASH(tb,ix);
    do {
        len = 0;
        kb = b = BUCKET(tb,ix);
        while (kb!=NULL) {
            len++;
            /* !!! kcurrent MUST be a TrunkOrBranchDbTerm !!! */
            NEXT_DBTERM(b, kb);
        }
        sum += len;
        sq_sum += len*len;
        if (len < stats->min_chain_len) stats->min_chain_len = len;
        if (len > stats->max_chain_len) stats->max_chain_len = len;
        ix = next_slot(tb,ix,&lck);
    }while (ix);
    stats->avg_chain_len = (float)sum / NACTIVE(&tb->linearht);
    stats->std_dev_chain_len = sqrt((sq_sum - stats->avg_chain_len*sum) / NACTIVE(&tb->linearht));
    /* Expected standard deviation from a good uniform hash function,
       ie binomial distribution (not taking the linear hashing into acount) */
    stats->std_dev_expected = sqrt(stats->avg_chain_len * (1 - 1.0/NACTIVE(&tb->linearht)));
}

#ifdef HARDDEBUG
void
db_check_table_nhash(DbTable *tbl)
{
    int j, nk, nactive = NACTIVE(&tb->linearht);
    DbTableNestedHash *tb = &tbl->nested;
    NestedHashDbTerm *list, *klist;

    for (j = 0; j < nactive; j++) {
        list = BUCKET(tb, j);
        while (list != NULL) {
            klist = list;
            nk = 0;
            do {
                if (!is_tuple(make_tuple(klist->dbterm.tpl))) {
                    erl_exit(1, "Bad term in slot %d of ets table", j);
                }
                ++nk;
                klist = klist->knext;
            } while (klist != NULL);
            if (list->nkitems != nk) {
                erl_exit(1, "Invalid nkitems in slot %d of ets table", j);
            }
            if (list->lht != NULL) {
                int i, na, nk2 = 0;
                NestedDbTerm *ntp;
                na = NACTIVE(list->lht);
                for (i = 0; i < na; ++i) {
                    ntp = NESTED_BUCKET(tb, list->lht, i);
                    while (ntp != NULL) {
                        ++nk2;
                        ntp = ntp->next;
                    }
                }
                if (nk2 != nk) {
                    erl_exit(1, "Invalid number of terms in nested table of slot %d of ets table", j);
                }
            }
            list = list->next;
        }
    }
}
#endif
