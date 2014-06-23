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

/*
 * Implementation of unordered ETS tables.
 * The tables are implemented as a two levels linear dynamic hash tables --
 * the first level is keyed by the key's hash, the second (optional) level is
 * keyed by the object's hash.
 */

/*
 * SMP:
 * The hash table supports two different locking "modes",
 * coarse grained and fine grained locking.
 *
 * Coarse grained locking relies entirely on the caller (erl_db.c) to obtain
 * the right kind of lock on the entire table depending on operation (reading
 * or writing). No further locking is then done by the table itself.
 *
 * Fine grained locking is supported by this code to allow concurrent updates
 * (and reading) to different parts of the table. This works by keeping one
 * rw-mtx for every N'th bucket. Even dynamic growing and shrinking by
 * rehashing buckets can be done without exclusive table lock.
 *
 * A table will support fine grained locking if it is created with flag
 * DB_FINE_LOCKED set. The table variable is_thread_safe will then indicate
 * if operations need to obtain fine grained locks or not. Some operations
 * will for example always use exclusive table lock to guarantee
 * a higher level of atomicy.
 */

/*
 * FIXATION:
 * Fixating the table, by ets:safe_fixtable or as done by select-operations,
 * guarantees two things in current implementation.
 * (1) Keys will not *totaly* disappear from the table. A key can thus be used
 *     as an iterator to find the next key in iteration sequence. Note however
 *     that this does not mean that (pointers to) table objects are guaranteed
 *     to be maintained while the table is fixated. A BAG or DBAG may actually
 *     remove objects as long as there is at least one object left in the table
 *     with the same key (alive or pseudo-deleted).
 * (2) Objects will not be moved between buckets due to table grow/shrink.
 *     This will guarantee that iterations do not miss keys or get double-hits.
 *
 * With fine grained locking, a concurrent thread can fixate the table at any
 * time. A "dangerous" operation (delete or move) therefore needs to check
 * if the table is fixated while write-locking the bucket.
 */


#ifdef DEBUG
#define HARDDEBUG 1
#endif

#ifdef HAVE_CONFIG_H
#include "config.h"
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

#define MY_ASSERT(x) ASSERT(x)

#else

#define MY_ASSERT(x)

#endif

/*
 * The following symbols can be manipulated to "tune" the linear hash array
 */

/* Medium bucket chain len */
#define CHAIN_LEN 6

/* Trunk chains longer that this value will get a nested LHT */
#define NESTED_CHAIN_THRESHOLD (2*CHAIN_LEN)

/* Number of slots per segment */
#define SEGSZ_EXP  8
#define SEGSZ      (1 << SEGSZ_EXP)
#define SEGSZ_MASK (SEGSZ-1)

#define NSEG_1     2 /* Size of first segment table (must be at least 2) */
#define NSEG_2   256 /* Size of second segment table */
#define NSEG_INC 128 /* Number of segments to grow after that */


#ifdef ERTS_SMP

#define DB_USING_FINE_LOCKING(TB) (((TB))->common.type & DB_FINE_LOCKED)

#define DB_HASH_LOCK_MASK (DB_NESTED_HASH_LOCK_CNT - 1)
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

#define DB_USING_FINE_LOCKING(TB) 0

#define RLOCK_HASH(tb,hval) NULL
#define WLOCK_HASH(tb,hval) NULL

#define RUNLOCK_HASH(lck) ((void)lck)
#define WUNLOCK_HASH(lck) ((void)lck)

#endif /* ERTS_SMP */


#ifdef ETHR_ORDERED_READ_DEPEND
#define SEGTAB(tb) ((struct segment **)erts_smp_atomic_read_nob(&(tb)->segtab))
#else
#define SEGTAB(tb)                                                   \
    (DB_USING_FINE_LOCKING(tb)                                       \
     ? ((struct segment **)erts_smp_atomic_read_ddrb(&(tb)->segtab)) \
     : ((struct segment **)erts_smp_atomic_read_nob(&(tb)->segtab)))
#endif

#define NACTIVE(tb) ((int)erts_smp_atomic_read_nob(&(tb)->nactive))
#define NKEYS(tb) ((int)erts_smp_atomic_read_nob(&(tb)->nkeys))

#define BUCKET(tb, i) SEGTAB(tb)[(i) >> SEGSZ_EXP]->buckets[(i) & SEGSZ_MASK].hterm

#define HAS_LHT(rp) ((((UWord)(rp)->trunk) & 0x01) != 0)
#define TRUNK_PTR(tp) ((TrunkDbTerm *)(((UWord)(tp)) & ~0x01))
#define GET_TRUNK(rp) TRUNK_PTR((rp)->trunk)
#define SAFE_GET_TRUNK(rp) (((rp) == NULL) ? NULL : GET_TRUNK(rp))
#define SAFE_GET_TRUNK_P(rpp) ((*(rpp) == NULL) ? NULL : &(*(rpp))->trunk)

#define SET_TRUNK(rp, tp)                           \
    {                                               \
        ASSERT((((UWord)(tp)) & 0x01) == 0);        \
        (rp)->trunk = HAS_LHT(rp)                   \
            ? (TrunkDbTerm *)(((UWord)(tp)) | 0x01) \
            : (tp);                                 \
    }

#define NESTED_BUCKET(rp, i) GET_TRUNK(rp)->sp.segtab[(i) >> SEGSZ_EXP]->buckets[(i) & SEGSZ_MASK].nterm

/*
 * When deleting a table, the number of records to delete.
 * Approximate number, because we must delete entire buckets.
 */
#define DELETE_RECORD_LIMIT 10000

#define MAX_HASH     0xEFFFFFFFUL
#define INVALID_HASH 0xFFFFFFFFUL

/* optimised version of make_hash (normal case? atomic key) */
#define MAKE_HASH(term)                                \
    ((is_atom(term)                                    \
      ? (atom_tab(atom_val(term))->slot.bucket.hvalue) \
      : make_hash2(term))                              \
     % MAX_HASH)

/*
 * Some special binary flags
 */
#define BIN_FLAG_ALL_OBJECTS BIN_FLAG_USR1


#ifdef ERTS_ENABLE_LOCK_CHECK

#define IFN_EXCL(tb, cmd) (((tb)->common.is_thread_safe) || (cmd))
#define IS_HASH_RLOCKED(tb, hval) IFN_EXCL(tb, erts_smp_lc_rwmtx_is_rlocked(GET_LOCK(tb, hval)))
#define IS_TAB_WLOCKED(tb) erts_smp_lc_rwmtx_is_rwlocked(&(tb)->common.rwlock)

#else

#define IS_HASH_RLOCKED(tb, hval) (1)
#define IS_TAB_WLOCKED(tb) (1)

#endif


/*
 * Local types
 */
struct mp_prefound {
    RootDbTerm **bucket;
    int ix;
};

#define N_OF_PREFOUND 10

struct mp_info {
    /*
     * True if complete objects are always returned from the
     * match_spec (can use copy_shallow on the return value)
     */
    int all_objects;

    int something_can_match; /* The match_spec is not "impossible" */
    int key_given;

    /*
     * Default list of "pre-found" buckets
     */
    struct mp_prefound dlists[N_OF_PREFOUND];

    /*
     * Buckets to search if keys are given, = dlists initially
     */
    struct mp_prefound *lists;

    /*
     * Number of elements in "lists", = 0 initially
     */
    unsigned num_lists;

    Binary *mp; /* The compiled match program */
};

/*
 * How the table segments relate to each other:
 *
 *     ext_segment:                 ext_segment:            "plain" segment
 *    #=================#          #================#       #=============#
 *    | bucket[0]       |<-+ +---->| bucket[256]    |    +->| bucket[512] |
 *    | bucket[1]       |  | |     |       [257]    |    |  |       [513] |
 *    :                 :  | |     :                :    |  :             :
 *    | bucket[255]     |  | |     |       [511]    |    |  |       [767] |
 *    |-----------------|  | |     |----------------|    |  #=============#
 *    | prev_segtab=NULL|  | | +-<---prev_segtab    |    |
 *    | nsegs = 2       |  | | |   | nsegs = 256    |    |
 * +->| segtab[0] -->------+-|-|-<---segtab[0]      |<-+ |
 * |  | segtab[1] -->--------+-|-<---segtab[1]      |  | |
 * |  #=================#      |   | segtab[2] -->-----|-+   ext_segment:
 * |                           |   :                :  |    #================#
 * +----------------<----------+   | segtab[255] ->----|--->| bucket[255*256]|
 *                                 #================#  |    |                |
 *                                                     |    :                :
 *                                                     |    |----------------|
 *                                                     +--<---prev_segtab    |
 *                                                          :                :
 */

/* A table "plain" segment */
struct segment {
    union {
        RootDbTerm *hterm;
        TrunkDbTerm *nterm;
    } buckets[SEGSZ];
#ifdef MYDEBUG
    int is_ext_segment;
#endif
};

/* A segment that also contains a segment table */
struct ext_segment {
    struct segment s; /* The segment itself. Must be first */

    struct segment **prev_segtab; /* Used when table is shrinking */
    int nsegs;                    /* Size of segtab */
    struct segment *segtab[1];    /* The segment table */
};

#define SIZEOF_EXTSEG(n)                                                \
    (offsetof(struct ext_segment, segtab) + sizeof(struct segment *)*(n))

#if defined(DEBUG)
#define EXTSEG(p)                                                       \
    ((struct ext_segment *)(((char *)(p)) - offsetof(struct ext_segment, segtab)))
#endif

/*
 * 'rpp' and 'tpp' are updated such that '*tpp' points to the next TrunkDbTerm
 * in the bucket. At the end of the bucket 'tpp' is set to NULL.
 * NOTE: It is assumed that both *rpp and *tpp are not NULL
 */
#define NEXT_DBTERM_P(rpp, tpp)           \
    if ((*(tpp))->next == NULL) {         \
        (rpp) = &(*(rpp))->next;          \
        (tpp) = SAFE_GET_TRUNK_P(rpp);    \
    } else {                              \
        (tpp) = &TRUNK_PTR(*(tpp))->next; \
    }

/*
 * 'rp' and 'tp' are updated to point to the next
 * TrunkDbTerm in the chain.
 * NOTE: It is assumed that both rp and tp are not NULL
 */
#define NEXT_DBTERM(rp, tp)        \
    if ((tp)->next == NULL) {      \
        (rp) = (rp)->next;         \
        (tp) = SAFE_GET_TRUNK(rp); \
    } else {                       \
        (tp) = (tp)->next;         \
    }

static ERTS_INLINE void
SET_SEGTAB(DbTableNestedHash *tb, struct segment **segtab)
{
    if (DB_USING_FINE_LOCKING(tb)) {
        erts_smp_atomic_set_wb(&tb->segtab, (erts_aint_t)segtab);
    } else {
        erts_smp_atomic_set_nob(&tb->segtab, (erts_aint_t)segtab);
    }
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
#define DEBUG_WAIT()
#endif


/*
 * Utility routines (static)
 */

static struct ext_segment *
alloc_ext_seg(DbTableNestedHash *tb, unsigned seg_ix,
              struct segment **old_segtab, int old_nsegs)
{
    int nsegs;
    struct ext_segment *eseg;
    switch (seg_ix) {
    case 0:
        nsegs = NSEG_1;
        break;
    case 1:
        nsegs = NSEG_2;
        break;
    default:
        nsegs = seg_ix + NSEG_INC;
    }
    eseg = (struct ext_segment *)
        erts_db_alloc_fnf(ERTS_ALC_T_DB_SEG, (DbTable *)tb,
                          SIZEOF_EXTSEG(nsegs));
    ASSERT(eseg != NULL);
    if (eseg == NULL)
        return NULL;
    sys_memset(&eseg->s, 0, sizeof(struct segment));
#ifdef MYDEBUG
    eseg->s.is_ext_segment = 1;
#endif
    eseg->prev_segtab = old_segtab;
    eseg->nsegs = nsegs;
    if (old_segtab) {
        ASSERT(nsegs > old_nsegs);
        sys_memcpy(eseg->segtab, old_segtab,
                   old_nsegs * sizeof(struct segment *));
    }
#ifdef DEBUG
    sys_memset(&eseg->segtab[seg_ix], 0,
               (nsegs - seg_ix) * sizeof(struct segment *));
#endif
    eseg->segtab[seg_ix] = &eseg->s;
    return eseg;
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
    if (ix < NACTIVE(tb))
        return ix;
    RUNLOCK_HASH(*lck_ptr);
    ix = (ix + 1) & DB_HASH_LOCK_MASK;
    if (ix)
        *lck_ptr = RLOCK_HASH(tb, ix);
    return ix;
#else
    return (++ix < NACTIVE(tb)) ? ix : 0;
#endif
}

/*
 * Calculate slot index from hash value.
 * RLOCK_HASH or WLOCK_HASH must be done before.
 */
static ERTS_INLINE Uint
hash_to_ix(DbTableNestedHash *tb, HashValue hval)
{
    Uint mask = (DB_USING_FINE_LOCKING(tb)
                 ?erts_smp_atomic_read_acqb(&tb->szm)
                 :erts_smp_atomic_read_nob(&tb->szm));
    Uint ix = hval & mask;
    if (ix >= erts_smp_atomic_read_nob(&tb->nactive)) {
        ix &= mask >> 1;
        ASSERT(ix < erts_smp_atomic_read_nob(&tb->nactive));
    }
    return ix;
}

#define EQ_REL(x, y, y_base)                    \
    (is_same(x, NULL, y, y_base)                \
     || (is_not_both_immed((x), (y))            \
         && eq_rel((x), NULL, (y), y_base)))

/*
 * Has this object the specified key? Can be pseudo-deleted.
 */
static ERTS_INLINE int
has_key(DbTableNestedHash *tb, RootDbTerm *rp, Eterm key, HashValue hval)
{
    Eterm itemKey;
    Eterm *base;
    if ((rp->hvalue != hval) && (rp->hvalue != INVALID_HASH))
        return 0;
    base = GET_TRUNK(rp)->dbterm.tpl;
    itemKey = GETKEY(tb, base);
    ASSERT(!is_header(itemKey));
    return EQ_REL(key, itemKey, base);
}

static ERTS_INLINE TrunkDbTerm *
new_trunk_dbterm(DbTableNestedHash *tb, Eterm obj, int full)
{
    byte *p;
    Uint offset, size;
    if (full) {
        offset = 0;
        size = offsetof(TrunkDbTerm, dbterm);
    } else {
        offset = offsetof(TrunkDbTerm, next);
        size = offsetof(TrunkDbTerm, dbterm) - offset;
    }
    p = (tb->common.compress)
        ? db_store_term_comp(&tb->common, NULL, size, obj)
        : db_store_term(&tb->common, NULL, size, obj);
    return (TrunkDbTerm *)(p - offset);
}

static ERTS_INLINE RootDbTerm *
new_root_dbterm(DbTableNestedHash *tb)
{
    RootDbTerm *ret;
    /*
     * New RootDbTerms are always created without a LHT. It will be
     * added later using realloc_root_dbterm() when there will be more
     * than NESTED_CHAIN_THRESHOLD TrunkDbTerms in the trunk chain.
     */
    ret = erts_db_alloc(ERTS_ALC_T_DB_TERM, (DbTable *)&tb->common,
                        offsetof(RootDbTerm, szm));
    ret->next = NULL;
    /* ret->trunk = NULL; */
    /* ret->hvalue = INVALID_HASH; */
    ret->nkitems = 1;
    return ret;
}

static ERTS_INLINE TrunkDbTerm *
replace_trunk_dbterm(DbTableNestedHash *tb,
                     TrunkDbTerm *old, Eterm obj, int full)
{
    byte *p;
    Uint offset, size;
    ASSERT(old != NULL);
    if (full) {
        offset = 0;
        size = offsetof(TrunkDbTerm, dbterm);
    } else {
        offset = offsetof(TrunkDbTerm, next);
        size = offsetof(TrunkDbTerm, dbterm) - offset;
    }
    p = (tb->common.compress)
        ? db_store_term_comp(&tb->common, &(old->dbterm), size, obj)
        : db_store_term(&tb->common, &(old->dbterm), size, obj);
    return (TrunkDbTerm *)(p - offset);
}

/*
 * Calculate slot index from hash value.
 */
static ERTS_INLINE Uint
nested_hash_to_ix(RootDbTerm *rp, HashValue hval)
{
    Uint mask = rp->szm;
    Uint ix = hval & mask;
    if (ix >= rp->nactive) {
        ix &= mask >> 1;
        ASSERT(ix < rp->nactive);
    }
    return ix;
}

static ERTS_INLINE void
put_nested_dbterm(DbTableNestedHash *tb, RootDbTerm *rp,
                  TrunkDbTerm *tp, Eterm obj)
{
    int nix;
    HashValue ohval;
    TrunkDbTerm **ntpp;
    ohval = MAKE_HASH(obj);
    nix = nested_hash_to_ix(rp, ohval);
    ntpp = &NESTED_BUCKET(rp, nix);
    tp->onext = *ntpp;
    tp->ohvalue = ohval;
    *ntpp = tp;
}

/*
 * Extend table with one new segment
 */
static int
alloc_nested_seg(DbTableNestedHash *tb, RootDbTerm *rp)
{
    int seg_ix = rp->nslots >> SEGSZ_EXP;
    struct segment **segtab;
    struct ext_segment *seg;
    if ((seg_ix + 1) == rp->nsegs) {
        /* New segtab needed (extended segment) */
        segtab = GET_TRUNK(rp)->sp.segtab;
        seg = alloc_ext_seg(tb, seg_ix, segtab, rp->nsegs);
        if (seg == NULL)
            return 0;
        segtab[seg_ix] = &seg->s;
        /* We don't use the new segtab until next call (see "shrink race") */
    } else {
        /* Just a new plain segment */
        if (seg_ix == rp->nsegs) {
            /* Time to start use segtab from last call */
            seg = (struct ext_segment *)GET_TRUNK(rp)->sp.segtab[seg_ix-1];
            MY_ASSERT((seg != NULL) && seg->s.is_ext_segment);
            GET_TRUNK(rp)->sp.segtab = seg->segtab;
            rp->nsegs = seg->nsegs;
        }
        ASSERT(seg_ix < rp->nsegs);
        segtab = GET_TRUNK(rp)->sp.segtab;
        ASSERT(segtab[seg_ix] == NULL);
        segtab[seg_ix] = (struct segment *)
            erts_db_alloc_fnf(ERTS_ALC_T_DB_SEG, (DbTable *)tb,
                              sizeof(struct segment));
        if (segtab[seg_ix] == NULL)
            return 0;
        sys_memset(segtab[seg_ix], 0, sizeof(struct segment));
    }
    rp->nslots += SEGSZ;
    return 1;
}

/*
 * Grow table with one new bucket.
 * Allocate new segment if needed.
 */
static void
nested_grow(DbTableNestedHash *tb, RootDbTerm *rp)
{
    int ix, from_ix, szm, nactive;
    TrunkDbTerm *tp;
    TrunkDbTerm **pnext, **to_pnext;
    nactive = rp->nactive;
    /* Ensure that the slot nactive exists */
    if (nactive == rp->nslots) {
        /* Time to get a new segment */
        ASSERT(!(nactive & SEGSZ_MASK));
        if (!alloc_nested_seg(tb, rp))
            return;
    }
    ASSERT(nactive < rp->nslots);
    szm = rp->szm;
    if (nactive <= szm) {
        from_ix = nactive & (szm >> 1);
    } else {
        ASSERT(nactive == szm+1);
        from_ix = 0;
        szm = (szm<<1) | 1;
    }
    ++rp->nactive;
    if (from_ix == 0)
        rp->szm = szm;
    /*
     * Finally, let's split the bucket. We try to do it in a smart way
     * to keep link order and avoid unnecessary updates of next-pointers.
     */
    pnext = &NESTED_BUCKET(rp, from_ix);
    tp = *pnext;
    to_pnext = &NESTED_BUCKET(rp, nactive);
    while (tp != NULL) {
        ix = tp->ohvalue & szm;
        if (ix != from_ix) {
            ASSERT(ix == (from_ix ^ ((szm>>1)+1)));
            *to_pnext = tp;
            /* Swap "from" and "to": */
            from_ix = ix;
            to_pnext = pnext;
        }
        pnext = &tp->onext;
        tp = *pnext;
    }
    *to_pnext = NULL;
}

static ERTS_INLINE RootDbTerm *
realloc_root_dbterm(DbTableNestedHash *tb, RootDbTerm *old, TrunkDbTerm *tp)
{
    RootDbTerm *ret;
    ASSERT(!HAS_LHT(old));
    ret = erts_db_alloc(ERTS_ALC_T_DB_TERM, (DbTable *)&tb->common,
                        sizeof(RootDbTerm));
    ret->next = old->next;
    ret->trunk = (TrunkDbTerm *)(((UWord)tp) | 0x01);
    ret->hvalue = old->hvalue;
    ret->nkitems = old->nkitems;
    ret->szm = SEGSZ_MASK;
    ret->nslots = SEGSZ;
    ret->nsegs = NSEG_1;
    ret->nactive = SEGSZ;
    tp->sp.segtab = alloc_ext_seg(tb, 0, NULL, 0)->segtab;
    erts_db_free(ERTS_ALC_T_DB_TERM, (DbTable *)&tb->common,
                 old, offsetof(RootDbTerm, szm));
    return ret;
}

static ERTS_INLINE void
free_trunk_dbterm(DbTableNestedHash *tb, TrunkDbTerm *tp, int full)
{
    byte *p = (byte *)tp;
    Uint offset, size;
    if (full) {
        offset = 0;
        size = offsetof(TrunkDbTerm, dbterm);
    } else {
        offset = offsetof(TrunkDbTerm, next);
        size = offsetof(TrunkDbTerm, dbterm) - offset;
    }
    db_free_term((DbTable *)tb, p + offset, size);
}

/*
 * Once created, the nested table is removed
 * only when the root element is deleted.
 */
static RootDbTerm *
create_nested_table(DbTableNestedHash *tb, RootDbTerm *rp)
{
    int nitems = 0;
    Eterm object;
    Eterm *base;
    DbTerm *dbterm;
    TrunkDbTerm *tp1, *tp2, *tp3;
    TrunkDbTerm **tpp;
    ASSERT(!HAS_LHT(rp));
    tp1 = NULL;
    tpp = &rp->trunk;
    tp2 = *tpp;
    if (tb->common.compress) {
        do {
            dbterm = db_alloc_tmp_uncompressed(&tb->common, &tp2->dbterm);
            object = make_tuple_rel(dbterm->tpl, NULL);
            *tpp = new_trunk_dbterm(tb, object, 1);
            if (tp1 == NULL) {
                tp1 = *tpp;
                rp = realloc_root_dbterm(tb, rp, tp1);
            } else {
                (*tpp)->sp.previous = tp1;
                tp1 = *tpp;
            }
            put_nested_dbterm(tb, rp, tp1, object);
            db_free_tmp_uncompressed(dbterm);
            tp3 = tp2->next;
            free_trunk_dbterm(tb, tp2, 0);
            if (++nitems > rp->nactive * (CHAIN_LEN + 1))
                nested_grow(tb, rp);
            tpp = &tp1->next;
        } while ((tp2 = tp3) != NULL);
    } else {
        do {
            base = HALFWORD_HEAP ? tp2->dbterm.tpl : NULL;
            object = make_tuple_rel(tp2->dbterm.tpl, base);
            *tpp = new_trunk_dbterm(tb, object, 1);
            if (tp1 == NULL) {
                tp1 = *tpp;
                rp = realloc_root_dbterm(tb, rp, tp1);
            } else {
                (*tpp)->sp.previous = tp1;
                tp1 = *tpp;
            }
            put_nested_dbterm(tb, rp, tp1, object);
            tp3 = tp2->next;
            free_trunk_dbterm(tb, tp2, 0);
            if (++nitems > rp->nactive * (CHAIN_LEN + 1))
                nested_grow(tb, rp);
            tpp = &tp1->next;
        } while ((tp2 = tp3) != NULL);
    }
    *tpp = NULL;
    return rp;
}

static ERTS_INLINE int
begin_resizing(DbTableNestedHash *tb)
{
    if (DB_USING_FINE_LOCKING(tb))
        return !erts_smp_atomic_xchg_acqb(&tb->is_resizing, 1);
    return !erts_smp_atomic_xchg_nob(&tb->is_resizing, 1);
}

static ERTS_INLINE void
done_resizing(DbTableNestedHash *tb)
{
    if (DB_USING_FINE_LOCKING(tb)) {
        erts_smp_atomic_set_relb(&tb->is_resizing, 0);
    } else {
        erts_smp_atomic_set_nob(&tb->is_resizing, 0);
    }
}

/*
 * Extend table with one new segment
 */
static int
alloc_seg(DbTableNestedHash *tb)
{
    int seg_ix = tb->nslots >> SEGSZ_EXP;
    struct segment **segtab;
    struct ext_segment *seg;
    if ((seg_ix + 1) == tb->nsegs) {
        /* New segtab needed (extended segment) */
        segtab = SEGTAB(tb);
        seg = alloc_ext_seg(tb, seg_ix, segtab, tb->nsegs);
        if (seg == NULL)
            return 0;
        segtab[seg_ix] = &seg->s;
        /* We don't use the new segtab until next call (see "shrink race") */
    } else {
        /* Just a new plain segment */
        if (seg_ix == tb->nsegs) {
            /* Time to start use segtab from last call */
            seg = (struct ext_segment *)SEGTAB(tb)[seg_ix-1];
            MY_ASSERT((seg != NULL) && seg->s.is_ext_segment);
            SET_SEGTAB(tb, seg->segtab);
            tb->nsegs = seg->nsegs;
        }
        ASSERT(seg_ix < tb->nsegs);
        segtab = SEGTAB(tb);
        ASSERT(segtab[seg_ix] == NULL);
        segtab[seg_ix] = (struct segment *)
            erts_db_alloc_fnf(ERTS_ALC_T_DB_SEG, (DbTable *)tb,
                              sizeof(struct segment));
        if (segtab[seg_ix] == NULL)
            return 0;
        sys_memset(segtab[seg_ix], 0, sizeof(struct segment));
    }
    tb->nslots += SEGSZ;
    return 1;
}

/*
 * Free all the TrunkDbTerms of the top segment, except
 * the first (the one with the segment table).
 *
 * Return:
 *   1 -> done
 *   0 -> *max_free zeroed, need another round
 */
static int
free_nested_records(DbTableNestedHash *tb, RootDbTerm *rp, int *max_free)
{
    int i, seg_ix;
    TrunkDbTerm *tp, *ntp;
    TrunkDbTerm **ntpp;
    struct ext_segment *top;
    tp = GET_TRUNK(rp);
    seg_ix = (rp->nslots >> SEGSZ_EXP) - 1;
    top = (struct ext_segment *)tp->sp.segtab[seg_ix];
    ASSERT(top != NULL);
    for (i=0; i<SEGSZ; ++i) {
        ntpp = &top->s.buckets[i].nterm;
        while ((ntp = *ntpp) != NULL) {
            if (ntp == tp) {
                *ntpp = ntp->onext;
#ifdef DEBUG
                ntp->onext = NULL;
#endif
                continue;
            }
            if (*max_free <= 0)
                return 0;
            --*max_free;
            --rp->nkitems;
            *ntpp = ntp->onext;
            ntp->sp.previous->next = ntp->next;
            if (ntp->next != NULL)
                ntp->next->sp.previous = ntp->sp.previous;
            free_trunk_dbterm(tb, ntp, 1);
        }
    }
    return 1;
}

/*
 * Shrink table by freeing the top segment.
 */
static void
free_nested_seg(DbTableNestedHash *tb, RootDbTerm *rp)
{
    int bytes, seg_ix;
    struct segment **segtab;
    struct ext_segment *top;
    segtab = GET_TRUNK(rp)->sp.segtab;
    seg_ix = (rp->nslots >> SEGSZ_EXP) - 1;
    top = (struct ext_segment *)segtab[seg_ix];
    ASSERT(top != NULL);
#ifdef DEBUG
    {
        int i;
        for (i=0; i<SEGSZ; ++i)
            ASSERT(top->s.buckets[i].nterm == NULL);
    }
#endif
    /*
     * No "shrink race" here.
     * However we still stop use or allocate a new segtab one call earlier.
     * Maybe some day this "early" allocation will be dropped.
     */
    if ((seg_ix == (rp->nsegs - 1)) || (seg_ix == 0)) {
        /* Dealloc extended segment */
        MY_ASSERT(top->s.is_ext_segment);
        ASSERT((segtab != top->segtab) || (seg_ix == 0));
        bytes = SIZEOF_EXTSEG(top->nsegs);
    } else {
        /* Dealloc plain segment */
        struct ext_segment *newtop = (struct ext_segment *)segtab[seg_ix - 1];
        MY_ASSERT(!top->s.is_ext_segment);
        if (segtab == newtop->segtab) {
            /* New top segment is extended */
            MY_ASSERT(newtop->s.is_ext_segment);
            if (newtop->prev_segtab != NULL) {
                /* Time to use a smaller segtab */
                GET_TRUNK(rp)->sp.segtab = newtop->prev_segtab;
                rp->nsegs = seg_ix;
                ASSERT(rp->nsegs == EXTSEG(GET_TRUNK(rp)->sp.segtab)->nsegs);
            } else {
                ASSERT((NSEG_1 > 2) && (seg_ix == 1));
            }
        }
        bytes = sizeof(struct segment);
    }
    erts_db_free(ERTS_ALC_T_DB_SEG, (DbTable *)tb, (void *)top, bytes);
#ifdef DEBUG
    if (seg_ix > 0) {
        if (seg_ix < rp->nsegs)
            GET_TRUNK(rp)->sp.segtab[seg_ix] = NULL;
    } else {
        GET_TRUNK(rp)->sp.segtab = NULL;
    }
#endif
    rp->nslots -= SEGSZ;
    ASSERT(rp->nslots >= 0);
}

static ERTS_INLINE void
free_root_dbterm(DbTableNestedHash *tb, RootDbTerm *rp)
{
    if (HAS_LHT(rp)) {
        free_nested_seg(tb, rp);
        ASSERT(rp->nslots == 0);
        erts_db_free(ERTS_ALC_T_DB_TERM, (DbTable *)tb,
                     rp, sizeof(RootDbTerm));
    } else {
        erts_db_free(ERTS_ALC_T_DB_TERM, (DbTable *)tb,
                     rp, offsetof(RootDbTerm, szm));
    }
}

/*
 * Remove the chain rooted in '***rppp' from the table.
 * '*rppp' is updated appropriately to point to the next RootDbTerm.
 * NOTE: It is assumed that rppp, *rppp, **rppp are not NULL
 *
 * max_free:
 *   == NULL -> the RootDbTerm is pseudo-deleted -- the
 *              nested LHT is expected to be already empty
 *   != NULL -> free at most *max_free TrunkDbTerms
 *
 * Return:
 *   1 -> done
 *   0 -> *max_free zeroed, need another round
 */
static int
remove_key(DbTableNestedHash *tb, RootDbTerm ***rppp,
           int *max_free, int pseudo_delete)
{
    int hl;
    RootDbTerm **rpp = *rppp;
    RootDbTerm *rp = *rpp;
    TrunkDbTerm *tp;
    TrunkDbTerm **tpp;
    hl = HAS_LHT(rp);
    if (max_free != NULL) {
        tp = GET_TRUNK(rp);
        if (hl) {
            while (rp->nslots > SEGSZ) {
                if (!free_nested_records(tb, rp, max_free))
                    return 0;
                free_nested_seg(tb, rp);
            }
            ASSERT(rp->nslots == SEGSZ);
            if (!free_nested_records(tb, rp, max_free))
                return 0;
            tp->next = NULL;
        } else {
            tpp = &tp->next;
            while ((tp = *tpp) != NULL) {
                if (*max_free <= 0)
                    return 0;
                --*max_free;
                --rp->nkitems;
                *tpp = tp->next;
                free_trunk_dbterm(tb, tp, 0);
            }
        }
    }
    if (pseudo_delete) {
        ASSERT(rp->nkitems == 1);
        if (rp->hvalue != INVALID_HASH) {
            ASSERT(max_free != NULL);
            rp->hvalue = INVALID_HASH;
            --*max_free;
        }
        *rppp = &rp->next;
        return 1;
    }
    if (rp->hvalue != INVALID_HASH) {
        ASSERT(max_free != NULL);
        --*max_free;
    }
    *rpp = rp->next;
    tp = GET_TRUNK(rp);
    free_root_dbterm(tb, rp);
    free_trunk_dbterm(tb, tp, hl);
    return 1;
}

/*
 * Grow table with one new bucket.
 * Allocate new segment if needed.
 */
static void
grow(DbTableNestedHash *tb, int nactive)
{
    int from_ix, ix, szm;
    erts_smp_rwmtx_t *lck;
    RootDbTerm *rp;
    RootDbTerm **from_rpp, **to_rpp;
    if (!begin_resizing(tb))
        /* already in progress */
        return;
    if (NACTIVE(tb) != nactive) {
        /* already done (race) */
        done_resizing(tb);
        return;
    }
    /* Ensure that the slot nactive exists */
    if (nactive == tb->nslots) {
        /* Time to get a new segment */
        ASSERT((nactive & SEGSZ_MASK) == 0);
        if (!alloc_seg(tb)) {
            done_resizing(tb);
            return;
        }
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
    /*
     * Now a final double check (with the from_ix lock held)
     * that we did not get raced by a table fixer.
     */
    if (IS_FIXED(tb)) {
        WUNLOCK_HASH(lck);
        done_resizing(tb);
        return;
    }
    erts_smp_atomic_inc_nob(&tb->nactive);
    if (from_ix == 0) {
        if (DB_USING_FINE_LOCKING(tb)) {
            erts_smp_atomic_set_relb(&tb->szm, szm);
        } else {
            erts_smp_atomic_set_nob(&tb->szm, szm);
        }
    }
    done_resizing(tb);
    /*
     * Finally, let's split the bucket. We try to do it in a smart way
     * to keep link order and avoid unnecessary updates of next-pointers
     */
    from_rpp = &BUCKET(tb, from_ix);
    to_rpp = &BUCKET(tb, nactive);
    while ((rp = *from_rpp) != NULL) {
        if (rp->hvalue == INVALID_HASH) {
            /* rare but possible with fine locking */
            remove_key(tb, &from_rpp, NULL, 0);
        } else {
            ix = rp->hvalue & szm;
            if (ix != from_ix) {
                ASSERT(ix == (from_ix ^ ((szm >> 1) + 1)));
                *to_rpp = rp;
                /* Swap "from" and "to": */
                from_ix = ix;
                to_rpp = from_rpp;
            }
            from_rpp = &rp->next;
        }
    }
    *to_rpp = NULL;
    WUNLOCK_HASH(lck);
}

/*
 * Copy terms from rp1 until rp2
 * works for rp1 == rp2 == NULL  => []
 * or rp2 == NULL
 */
static Eterm
build_term_list(Process *p, RootDbTerm *rp1,
                RootDbTerm *rp2, DbTableNestedHash *tb)
{
    int sz = 0;
    Eterm copy, list = NIL;
    Eterm *hp, *hend;
    RootDbTerm *rp;
    TrunkDbTerm *tp;
    rp = rp1;
    while (rp != rp2) {
        if (rp->hvalue != INVALID_HASH) {
            tp = GET_TRUNK(rp);
            do {
                sz += tp->dbterm.size + 2;
                tp = tp->next;
            } while (tp != NULL);
        }
        rp = rp->next;
    }
    hp = HAlloc(p, sz);
    hend = hp + sz;
    while (rp1 != rp2) {
        if (rp1->hvalue != INVALID_HASH) {
            tp = GET_TRUNK(rp1);
            do {
                copy = db_copy_object_from_ets(&tb->common, &tp->dbterm,
                                               &hp, &MSO(p));
                list = CONS(hp, copy, list);
                hp += 2;
                tp = tp->next;
            } while (tp != NULL);
        }
        rp1 = rp1->next;
    }
    HRelease(p, hend, hp);
    return list;
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

/*
 * Return:
 *   1 -> done
 *   0 -> *max_free zeroed, need another round
 */
static int
free_bucket(DbTableNestedHash *tb, RootDbTerm **rpp,
            int *max_free, int pseudo_delete)
{
    while (*rpp != NULL)
        if (!remove_key(tb, &rpp, max_free, pseudo_delete))
            return 0;
    return 1;
}

/*
 * Shrink table by freeing the top segment.
 * free_records:
 *   != NULL -> free no more than *free_records records in segment
 *   == NULL -> assume segment is empty
 *
 * Return:
 *   1 -> done
 *   0 -> *max_free zeroed, need another round
 */
static int
free_seg(DbTableNestedHash *tb, int *max_free)
{
    int i, bytes, seg_ix;
    RootDbTerm **rpp;
    struct segment **segtab = SEGTAB(tb);
    struct ext_segment *top;
    seg_ix = (tb->nslots >> SEGSZ_EXP) - 1;
    top = (struct ext_segment *)segtab[seg_ix];
    ASSERT(top != NULL);
#ifndef DEBUG
    if (max_free != NULL)
#endif
    {
        for (i=0; i<SEGSZ; ++i) {
            rpp = &top->s.buckets[i].hterm;
            if (SAFE_GET_TRUNK(*rpp) != NULL) {
                ASSERT(max_free != NULL); /* segment not empty as assumed? */
                if (!free_bucket(tb, rpp, max_free, 0))
                    return 0;
            }
        }
    }
    /*
     * The "shrink race":
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
    if ((seg_ix == tb->nsegs - 1) || (seg_ix == 0)) {
        /* Dealloc extended segment */
        MY_ASSERT(top->s.is_ext_segment);
        ASSERT((segtab != top->segtab) || (seg_ix == 0));
        bytes = SIZEOF_EXTSEG(top->nsegs);
    } else {
        /* Dealloc plain segment */
        struct ext_segment *newtop = (struct ext_segment *)segtab[seg_ix - 1];
        MY_ASSERT(!top->s.is_ext_segment);
        if (segtab == newtop->segtab) {
            /* New top segment is extended */
            MY_ASSERT(newtop->s.is_ext_segment);
            if (newtop->prev_segtab != NULL) {
                /* Time to use a smaller segtab */
                SET_SEGTAB(tb, newtop->prev_segtab);
                tb->nsegs = seg_ix;
                ASSERT(tb->nsegs == EXTSEG(SEGTAB(tb))->nsegs);
            } else {
                ASSERT((NSEG_1 > 2) && (seg_ix == 1));
            }
        }
        bytes = sizeof(struct segment);
    }
    erts_db_free(ERTS_ALC_T_DB_SEG, (DbTable *)tb, (void *)top, bytes);
#ifdef DEBUG
    if (seg_ix > 0) {
        if (seg_ix < tb->nsegs)
            SEGTAB(tb)[seg_ix] = NULL;
    } else {
        SET_SEGTAB(tb, NULL);
    }
#endif
    tb->nslots -= SEGSZ;
    ASSERT(tb->nslots >= 0);
    return 1;
}

/*
 * Shrink table by joining top bucket.
 * Remove top segment if it gets empty.
 */
static void
shrink(DbTableNestedHash *tb, int nactive)
{
    int from_ix, low_szm, to_ix;
    RootDbTerm **from_rpp, **to_rpp;
    RootDbTerm **rpp, *rp;
    erts_smp_rwmtx_t *lck;
    if (!begin_resizing(tb))
        /* already in progress */
        return;
    if (NACTIVE(tb) == nactive) {
        /* already done */
        done_resizing(tb);
        return;
    }
    from_ix = nactive - 1;
    low_szm = erts_smp_atomic_read_nob(&tb->szm) >> 1;
    to_ix = from_ix & low_szm;
    ASSERT(to_ix < from_ix);
    ASSERT(nactive > SEGSZ);
    lck = WLOCK_HASH(tb, to_ix);
    /* Double check for racing table fixers */
    if (IS_FIXED(tb)) {
        WUNLOCK_HASH(lck);
        done_resizing(tb);
        return;
    }
    from_rpp = &BUCKET(tb, from_ix);
    to_rpp = &BUCKET(tb, to_ix);
    rpp = from_rpp;
    /*
     * Q: Why join lists by appending "to" at the end of "from"?
     * A: Must step through "from" anyway to purge pseudo deleted.
     */
    while ((rp = *rpp) != NULL) {
        if (rp->hvalue == INVALID_HASH) {
            remove_key(tb, &rpp, NULL, 0);
        } else {
            rpp = &(*rpp)->next;
        }
    }
    *rpp = *to_rpp;
    *to_rpp = *from_rpp;
    *from_rpp = NULL;
    erts_smp_atomic_set_nob(&tb->nactive, from_ix);
    if (to_ix == 0)
        erts_smp_atomic_set_relb(&tb->szm, low_szm);
    WUNLOCK_HASH(lck);
    if ((tb->nslots - from_ix) >= SEGSZ)
        free_seg(tb, NULL);
    done_resizing(tb);
}

static ERTS_INLINE void
try_shrink(DbTableNestedHash *tb)
{
    int nactive = NACTIVE(tb);
    if ((nactive > SEGSZ)
        && (NKEYS(tb) < (nactive * CHAIN_LEN))
        && !IS_FIXED(tb))
        shrink(tb, nactive);
}

/*
 * Shrink table by joining top bucket.
 * Remove top segment if it gets empty.
 */
static void
nested_shrink(DbTableNestedHash *tb, RootDbTerm *rp)
{
    int nactive, src_ix, dst_ix, low_szm;
    TrunkDbTerm **src_bp, **dst_bp;
    nactive = rp->nactive;
    src_ix = nactive - 1;
    low_szm = rp->szm >> 1;
    dst_ix = src_ix & low_szm;
    ASSERT(dst_ix < src_ix);
    ASSERT(nactive > SEGSZ);
    src_bp = &NESTED_BUCKET(rp, src_ix);
    dst_bp = &NESTED_BUCKET(rp, dst_ix);
    while (*dst_bp != NULL)
        dst_bp = &(*dst_bp)->onext;
    *dst_bp = *src_bp;
    *src_bp = NULL;
    rp->nactive = src_ix;
    if (dst_ix == 0)
        rp->szm = low_szm;
    if ((rp->nslots - src_ix) >= SEGSZ)
        free_nested_seg(tb, rp);
}

static ERTS_INLINE void
try_nested_shrink(DbTableNestedHash *tb, RootDbTerm *rp)
{
    int nactive = rp->nactive;
    if ((nactive > SEGSZ) && (rp->nkitems < (nactive * CHAIN_LEN)))
        nested_shrink(tb, rp);
}

/*
 * Remove term '**tpp' from the chain.
 * '*rppp' and '*tpp' are updated appropriately to point to
 * the next term.
 * *ntpp will point to the next TrunkDbTerm.
 * NOTE: It is assumed that rppp, *rppp, **rppp, tpp, *tpp, ntpp and *ntpp
 *       are not NULL and that **rppp points to a RootDbTerm with a lht
 * Return: 0 -> more terms left with the same key of the removed one's
 *         1 -> the removed term was the last with its key
 */
static int
remove_object_and_nested(DbTableNestedHash *tb, RootDbTerm **rpp,
                         TrunkDbTerm *tp, TrunkDbTerm **ntpp,
                         int pseudo_delete)
{
    RootDbTerm *rp = *rpp;
    TrunkDbTerm *ntp = *ntpp;
    ASSERT(HAS_LHT(rp));
    /*
     * Pseudo-deleted terms cannot be found
     * looking them up in the nested table.
     */
    ASSERT(rp->hvalue != INVALID_HASH);
    *ntpp = ntp->onext;
    if (GET_TRUNK(rp) == tp) {
        if (tp->next == NULL) {
            ASSERT(rp->nkitems == 1);
            if (pseudo_delete) {
                rp->hvalue = INVALID_HASH;
            } else {
                *rpp = rp->next;
                free_root_dbterm(tb, rp);
                free_trunk_dbterm(tb, tp, 1);
            }
            return 1;
        }
        rp->trunk = (TrunkDbTerm *)(((UWord)tp->next) | 0x01);
        tp->next->sp.segtab = tp->sp.segtab;
    } else {
        tp->sp.previous->next = tp->next;
        if (tp->next != NULL)
            tp->next->sp.previous = tp->sp.previous;
    }
    --rp->nkitems;
    free_trunk_dbterm(tb, tp, 1);
    try_nested_shrink(tb, rp);
    return 0;
}

/*
 * Remove term '**tpp' from the chain.
 * '*tpp' is updated appropriately to point to the next term, if there is one,
 * else it is set to NULL.
 * NOTE: It is assumed that rpp, *rpp, tpp and *tpp are not NULL
 *       and that *rpp points to a RootDbTerm without a lht.
 * Return: 0 -> more terms left with the same key of the removed one's
 *         1 -> the removed term was the last with its key
 */
static int
remove_object_no_nested(DbTableNestedHash *tb, RootDbTerm **rpp,
                        TrunkDbTerm **tpp, int pseudo_delete)
{
    RootDbTerm *rp = *rpp;
    TrunkDbTerm *tp = *tpp;
    ASSERT(!HAS_LHT(rp));
    if (&rp->trunk == tpp) {
        if (tp->next == NULL) {
            ASSERT(rp->nkitems == 1);
            if (pseudo_delete) {
                rp->hvalue = INVALID_HASH;
            } else {
                *rpp = rp->next;
                free_root_dbterm(tb, rp);
                free_trunk_dbterm(tb, tp, 0);
            }
            return 1;
        }
        rp->trunk = tp->next;
    } else {
        ASSERT(rp->trunk != tp);
        *tpp = tp->next;
    }
    --rp->nkitems;
    ASSERT(rp->hvalue != INVALID_HASH);
    free_trunk_dbterm(tb, tp, 0);
    return 0;
}

/* Search a list of tuples for a matching key */
static int
bucket_has_key(DbTableNestedHash *tb, Eterm key,
               HashValue hval, RootDbTerm *rp)
{
    while (rp != NULL) {
        if (has_key(tb, rp, key, hval))
            return (rp->hvalue == INVALID_HASH) ? 0 : 1;
        rp = rp->next;
    }
    return 0;
}

/*
 * For the select functions, analyzes the pattern and determines which
 * slots should be searched. Also compiles the match program
 */
static int
analyze_pattern(DbTableNestedHash *tb, Eterm pattern, struct mp_info *mpi)
{
    int i, ix, j, search_slot;
    int num_heads = 0;
    Eterm body, key, lst, tpl, ttpl;
    Eterm sbuff[3 * N_OF_PREFOUND];
    Eterm *ptpl, *matches, *guards, *bodies;
    Eterm *buff = sbuff;
    HashValue hval = NIL;
    RootDbTerm **rpp;
    erts_smp_rwmtx_t *lck;
    key = NIL;
    mpi->lists = mpi->dlists;
    mpi->num_lists = 0;
    mpi->key_given = 1;
    mpi->something_can_match = 0;
    mpi->all_objects = 1;
    mpi->mp = NULL;
    for (lst = pattern; is_list(lst); lst = CDR(list_val(lst)))
        ++num_heads;
    if (lst != NIL)
        /* improper list... */
        return DB_ERROR_BADPARAM;
    if (num_heads > N_OF_PREFOUND) {
        buff = erts_alloc(ERTS_ALC_T_DB_TMP, sizeof(Eterm) * num_heads * 3);
        mpi->lists =
            erts_alloc(ERTS_ALC_T_DB_SEL_LIST,
                       sizeof(*(mpi->lists)) * num_heads);
    }
    matches = &buff[0];
    guards = &buff[num_heads];
    bodies = &buff[num_heads * 2];
    for (i = 0, lst = pattern; is_list(lst); lst = CDR(list_val(lst))) {
        ttpl = CAR(list_val(lst));
        if ((!is_tuple(ttpl))
            || ((ptpl = tuple_val(ttpl))[0] != make_arityval(3U))) {
            if (buff != sbuff)
                erts_free(ERTS_ALC_T_DB_TMP, buff);
            return DB_ERROR_BADPARAM;
        }
        matches[i] = tpl = ptpl[1];
        guards[i] = ptpl[2];
        bodies[i] = body = ptpl[3];
        if (!is_list(body) || (CDR(list_val(body)) != NIL)
            || (CAR(list_val(body)) != am_DollarUnderscore))
            mpi->all_objects = 0;
        ++i;
        if (!(mpi->key_given))
            continue;
        if ((tpl == am_Underscore) || (db_is_variable(tpl) != -1)) {
            mpi->key_given = 0;
            mpi->something_can_match = 1;
            continue;
        }
        key = db_getkey(tb->common.keypos, tpl);
        if (is_value(key)) {
            if (!db_has_variable(key)) {
                /* Bound key */
                hval = MAKE_HASH(key);
                lck = RLOCK_HASH(tb, hval);
                ix = hash_to_ix(tb, hval);
                rpp = &BUCKET(tb, ix);
                if (lck == NULL) {
                    search_slot = bucket_has_key(tb, key, hval, *rpp);
                } else {
                    /*
                     * No point to verify if key exist now as there
                     * may be concurrent inserters/deleters anyway
                     */
                    RUNLOCK_HASH(lck);
                    search_slot = 1;
                }
                if (search_slot) {
                    for (j=0; ; ++j) {
                        if (j == mpi->num_lists) {
                            mpi->lists[mpi->num_lists].bucket = rpp;
                            mpi->lists[mpi->num_lists].ix = ix;
                            ++mpi->num_lists;
                            break;
                        }
                        if (mpi->lists[j].bucket == rpp) {
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
    /*
     * It would be nice not to compile the match_spec if nothing could match,
     * but then the select calls would not fail like they should on bad
     * match specs that happen to specify non existent keys etc.
     */
    mpi->mp =
        db_match_compile(matches, guards, bodies,
                         num_heads, DCOMP_TABLE, NULL);
    if (buff != sbuff)
        erts_free(ERTS_ALC_T_DB_TMP, buff);
    return (mpi->mp == NULL) ? DB_ERROR_BADPARAM : DB_ERROR_NONE;
}

/*
 * This is just here so I can take care of the return value that is
 * to be sent during a trap (the BIF_TRAP macros explicitly returns)
 */
static BIF_RETTYPE
bif_trap1(Export *bif, Process *p, Eterm p1)
{
    BIF_TRAP1(bif, p, p1);
}

static int
db_match_dbterm_nhash(DbTableNestedHash *tb, Process *c_p, Binary *bprog,
                      DbTerm *dbterm, Eterm *object, DbTerm **uncomp_dbterm)
{
    Uint32 dummy;
    Eterm res;
    Eterm *base;
    if (tb->common.compress) {
	*uncomp_dbterm = dbterm =
            db_alloc_tmp_uncompressed(&tb->common, dbterm);
	base = NULL;
    } else {
        *uncomp_dbterm = NULL;
        base = HALFWORD_HEAP ? dbterm->tpl : NULL;
    }
    *object = make_tuple_rel(dbterm->tpl, base);
    res = db_prog_match(c_p, bprog, *object, base, NULL, 0,
                        ERTS_PAM_COPY_RESULT | ERTS_PAM_CONTIGUOUS_TUPLE,
                        &dummy);
    return res == am_true;
}

/*
 * It is assumed that the TrunkDbTerm to remove
 * is present in the nested LHT.
 */
static ERTS_INLINE void
remove_nested_dbterm(DbTableNestedHash *tb, RootDbTerm *rp,
                     TrunkDbTerm *tp, Eterm object)
{
    int nix;
    HashValue ohval;
    TrunkDbTerm **ntpp;
    ohval = MAKE_HASH(object);
    nix = nested_hash_to_ix(rp, ohval);
    ntpp = &NESTED_BUCKET(rp, nix);
    while (1) {
        ASSERT(*ntpp != NULL);
        if (*ntpp == tp) {
            ASSERT((*ntpp)->ohvalue == ohval);
            *ntpp = (*ntpp)->onext;
            return;
        }
        ntpp = &(*ntpp)->onext;
    }
}

/*
 * Remove term ***tppp from the chain.
 * *rppp and **tppp are updated appropriately to point to the next
 * term. At the end of the bucket list **tppp is set to NULL.
 * NOTE: It is assumed that rppp, *rppp, **rppp, tppp, *tppp
 *       and **tppp are not NULL.
 * Return: 0 -> more terms left with the same key of the removed one's
 *         1 -> the removed term was the last with its key
 */
static int
remove_object(DbTableNestedHash *tb, RootDbTerm ***rppp,
              TrunkDbTerm ***tppp, Eterm object, int pseudo_delete)
{
    int hl;
    RootDbTerm **rpp = *rppp;
    RootDbTerm *rp = *rpp;
    TrunkDbTerm **tpp = *tppp;
    TrunkDbTerm *tp;
    hl = HAS_LHT(rp);
    if (&rp->trunk == tpp) {
        tp = TRUNK_PTR(*tpp);
        if (tp->next == NULL) {
            ASSERT(rp->nkitems == 1);
            if ((rp->hvalue != INVALID_HASH) && hl)
                remove_nested_dbterm(tb, rp, tp, object);
            if (pseudo_delete) {
                rp->hvalue = INVALID_HASH;
                *rppp = rpp = &rp->next;
            } else {
                *rpp = rp->next;
                free_root_dbterm(tb, rp);
                free_trunk_dbterm(tb, tp, hl);
            }
            *tppp = SAFE_GET_TRUNK_P(rpp);
            return 1;
        }
        SET_TRUNK(rp, tp->next);
        if (hl)
            tp->next->sp.segtab = tp->sp.segtab;
    } else {
        tp = *tpp;
        ASSERT(GET_TRUNK(rp) != tp);
        if ((*tpp = tp->next) == NULL) {
            *rppp = rpp = &rp->next;
            *tppp = SAFE_GET_TRUNK_P(rpp);
        } else if (hl) {
            tp->next->sp.previous = tp->sp.previous;
        }
    }
    --rp->nkitems;
    ASSERT(rp->hvalue != INVALID_HASH);
    if (hl) {
        remove_nested_dbterm(tb, rp, tp, object);
        try_nested_shrink(tb, rp);
    }
    free_trunk_dbterm(tb, tp, hl);
    return 0;
}

/*
 * Same as next_slot but with WRITE locking
 */
static ERTS_INLINE Sint
next_slot_w(DbTableNestedHash *tb, Uint ix, erts_smp_rwmtx_t **lck_ptr)
{
#ifdef ERTS_SMP
    ix += DB_NESTED_HASH_LOCK_CNT;
    if (ix < NACTIVE(tb))
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

static int
db_mark_all_deleted_nhash(DbTable *tbl)
{
    int i, max_free;
    RootDbTerm **rpp;
    DbTableNestedHash *tb = &tbl->nested;
    ERTS_SMP_LC_ASSERT(IS_TAB_WLOCKED(tb));
    for (i = 0; i < NACTIVE(tb); i++) {
        rpp = &BUCKET(tb, i);
        if (*rpp == NULL)
            continue;
        add_fixed_deletion(tb, i);
        max_free = DELETE_RECORD_LIMIT;
        /* !!! yield !!! */
        while (!free_bucket(tb, rpp, &max_free, 1))
            max_free += DELETE_RECORD_LIMIT;
    }
    erts_smp_atomic_set_nob(&tb->common.nitems, 0);
    erts_smp_atomic_set_nob(&tb->nkeys, 0);
    return DB_ERROR_NONE;
}


/*
 * Table interface routines ie what's called by the bif's
 */

int
db_create_nhash(Process *p, DbTable *tbl)
{
    DbTableNestedHash *tb = &tbl->nested;
    erts_smp_atomic_init_nob(&tb->nkeys, 0);
    erts_smp_atomic_init_nob(&tb->fixdel, (erts_aint_t)NULL);
    erts_smp_atomic_init_nob(&tb->szm, SEGSZ_MASK);
    erts_smp_atomic_init_nob(&tb->nactive, SEGSZ);
    erts_smp_atomic_init_nob(&tb->segtab, (erts_aint_t)NULL);
    tb->nsegs = NSEG_1;
    tb->nslots = SEGSZ;
    SET_SEGTAB(tb, alloc_ext_seg(tb, 0, NULL, 0)->segtab);
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
            for (i = 0; i<DB_NESTED_HASH_LOCK_CNT; ++i)
                erts_smp_rwmtx_init_opt_x(&tb->locks->lck_vec[i].lck,
                                          &rwmtx_opt, "db_nested_hash_slot",
                                          make_small(i));
            /*
             * This important property is needed to guarantee
             * that the buckets involved in a grow/shrink operation
             * is protected by the same lock:
             */
            ASSERT((erts_smp_atomic_read_nob(&tb->nactive)
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
    RootDbTerm *rp;
    TrunkDbTerm *tp;
    DbTableNestedHash *tb = &tbl->nested;
    erts_smp_rwmtx_t *lck = RLOCK_HASH(tb, ix);
    do {
        rp = BUCKET(tb, ix);
        while (rp != NULL) {
            if (rp->hvalue != INVALID_HASH) {
                tp = GET_TRUNK(rp);
                *ret = db_copy_key(p, tbl, &tp->dbterm);
                RUNLOCK_HASH(lck);
                return DB_ERROR_NONE;
            }
            rp = rp->next;
        }
        ix = next_slot(tb, ix, &lck);
    } while (ix);
    *ret = am_EOT;
    return DB_ERROR_NONE;
}

static int
db_next_nhash(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    Uint ix;
    HashValue hval;
    RootDbTerm *rp;
    TrunkDbTerm *tp;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, hval);
    rp = BUCKET(tb, ix);
    while (rp != NULL) {
        if (has_key(tb, rp, key, hval)) {
            /* Key found */
            rp = rp->next;
            for (;;) {
                while (rp != NULL) {
                    if (rp->hvalue != INVALID_HASH) {
                        tp = GET_TRUNK(rp);
                        *ret = db_copy_key(p, tbl, &tp->dbterm);
                        RUNLOCK_HASH(lck);
                        return DB_ERROR_NONE;
                    }
                    rp = rp->next;
                }
                ix = next_slot(tb, ix, &lck);
                if (!ix)
                    break;
                rp = BUCKET(tb, ix);
            }
            *ret = am_EOT;
            return DB_ERROR_NONE;
        }
        rp = rp->next;
    }
    RUNLOCK_HASH(lck);
    return DB_ERROR_BADKEY;
}

int
db_put_nhash(DbTable *tbl, Eterm obj, int key_clash_fail)
{
    int ix, nix, nactive, nkeys;
    Eterm key;
    HashValue hval, ohval;
    RootDbTerm **rpp, *rp;
    TrunkDbTerm *tp1, *tp2;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    key = GETKEY(tb, tuple_val(obj));
    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, hval);
    rpp = &BUCKET(tb, ix);
    while ((rp = *rpp) != NULL) {
        if (has_key(tb, rp, key, hval))
            break;
        rpp = &rp->next;
    }
    if (rp == NULL) {
        tp1 = new_trunk_dbterm(tb, obj, 0);
        tp1->next = NULL;
        rp = new_root_dbterm(tb);
        rp->trunk = tp1;
        rp->hvalue = hval;
        *rpp = rp;
        nkeys = erts_smp_atomic_inc_read_nob(&tb->nkeys);
    } else {
        /*
         * Key found
         */
        ASSERT(tb->common.status & (DB_BAG | DB_DUPLICATE_BAG));
        if (rp->hvalue == INVALID_HASH) {
            /* If pseudo-deleted then there's only one TrunkDbTerm */
            tp1 = GET_TRUNK(rp);
            ASSERT(tp1 != NULL);
            erts_smp_atomic_inc_nob(&tb->common.nitems);
            erts_smp_atomic_inc_nob(&tb->nkeys);
            /*
             * Recycle the Root and TrunkDbTerms
             */
            tp1 = replace_trunk_dbterm(tb, tp1, obj, HAS_LHT(rp));
            SET_TRUNK(rp, tp1);
            if (HAS_LHT(rp))
                put_nested_dbterm(tb, rp, tp1, obj);
            rp->hvalue = hval;
            ASSERT(rp->nkitems == 1);
            WUNLOCK_HASH(lck);
            return DB_ERROR_NONE;
        }
        if (key_clash_fail) {
            WUNLOCK_HASH(lck);
            return DB_ERROR_BADKEY;
        }
        if (tb->common.status & DB_BAG) {
            if (HAS_LHT(rp)) {
                ohval = MAKE_HASH(obj);
                nix = nested_hash_to_ix(rp, ohval);
                tp1 = NESTED_BUCKET(rp, nix);
                while (tp1 != NULL) {
                    if (db_eq(&tb->common, obj, &tp1->dbterm)) {
                        WUNLOCK_HASH(lck);
                        return DB_ERROR_NONE;
                    }
                    tp1 = tp1->onext;
                }
            } else {
                tp1 = rp->trunk;
                ASSERT(tp1 != NULL);
                do {
                    if (db_eq(&tb->common, obj, &tp1->dbterm)) {
                        WUNLOCK_HASH(lck);
                        return DB_ERROR_NONE;
                    }
                    tp1 = tp1->next;
                } while (tp1 != NULL);
            }
        }
        /* else DB_DUPLICATE_BAG */
        tp2 = GET_TRUNK(rp);
        tp1 = new_trunk_dbterm(tb, obj, HAS_LHT(rp));
        tp1->next = tp2;
        SET_TRUNK(rp, tp1);
        ++rp->nkitems;
        if (HAS_LHT(rp)) {
            tp1->sp.segtab = tp2->sp.segtab;
            tp2->sp.previous = tp1;
            put_nested_dbterm(tb, rp, tp1, obj);
            if (rp->nkitems > (rp->nactive * (CHAIN_LEN + 1)))
                nested_grow(tb, rp);
        } else {
            if (rp->nkitems > NESTED_CHAIN_THRESHOLD)
                *rpp = create_nested_table(tb, rp);
        }
        nkeys = erts_smp_atomic_read_nob(&tb->nkeys);
    }
    erts_smp_atomic_inc_nob(&tb->common.nitems);
    WUNLOCK_HASH(lck);
    nactive = NACTIVE(tb);
    if ((nkeys > nactive * (CHAIN_LEN + 1)) && (!IS_FIXED(tb)))
        grow(tb, nactive);
    CHECK_TABLES();
    return DB_ERROR_NONE;
}

static int
db_get_nhash(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    int ix;
    HashValue hval;
    RootDbTerm *rp;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    *ret = NIL;
    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb,hval);
    ix = hash_to_ix(tb, hval);
    rp = BUCKET(tb, ix);
    while (rp != NULL) {
        /* Let build_term_list() skip the pseudo-deleted RootDbTerm */
        if (has_key(tb, rp, key, hval)) {
            /* !!! yield !!! */
            *ret = build_term_list(p, rp, rp->next, tb);
            CHECK_TABLES();
            break;
        }
        rp = rp->next;
    }
    RUNLOCK_HASH(lck);
    return DB_ERROR_NONE;
}

static int
db_get_element_nhash(Process *p, DbTable *tbl, Eterm key, int ndex, Eterm *ret)
{
    int ix;
    Eterm copy, elem_list;
    Eterm *hp;
    HashValue hval;
    RootDbTerm *rp;
    TrunkDbTerm *tp;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    ASSERT(tb->common.status & (DB_BAG | DB_DUPLICATE_BAG));
    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, hval);
    rp = BUCKET(tb, ix);
    while (rp != NULL) {
        if (has_key(tb, rp, key, hval)) {
            if (rp->hvalue == INVALID_HASH)
                break;
            /* !!! yield !!! */
            tp = GET_TRUNK(rp);
            do {
                if (ndex > arityval(tp->dbterm.tpl[0])) {
                    RUNLOCK_HASH(lck);
                    return DB_ERROR_BADITEM;
                }
                tp = tp->next;
            } while (tp != NULL);
            elem_list = NIL;
            tp = GET_TRUNK(rp);
            do {
                copy = db_copy_element_from_ets(&tb->common, p,
                                                &tp->dbterm, ndex, &hp, 2);
                elem_list = CONS(hp, copy, elem_list);
                hp += 2;
                tp = tp->next;
            } while (tp != NULL);
            *ret = elem_list;
            RUNLOCK_HASH(lck);
            return DB_ERROR_NONE;
        }
        rp = rp->next;
    }
    RUNLOCK_HASH(lck);
    return DB_ERROR_BADKEY;
}

static int
db_member_nhash(DbTable *tbl, Eterm key, Eterm *ret)
{
    int ix;
    HashValue hval;
    RootDbTerm *rp;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    *ret = am_false;
    hval = MAKE_HASH(key);
    ix = hash_to_ix(tb, hval);
    lck = RLOCK_HASH(tb, hval);
    rp = BUCKET(tb, ix);
    while (rp != NULL) {
        if (has_key(tb, rp, key, hval)) {
            if (rp->hvalue != INVALID_HASH)
                *ret = am_true;
            break;
        }
        rp = rp->next;
    }
    RUNLOCK_HASH(lck);
    return DB_ERROR_NONE;
}

/*
 * NB: this is for the db_erase/2 bif.
 */
int
db_erase_nhash(DbTable *tbl, Eterm key, Eterm *ret)
{
    int ix, nitems_diff, max_free;
    HashValue hval;
    RootDbTerm **rpp, *rp;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    nitems_diff = 0;
    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, hval);
    rpp = &BUCKET(tb, ix);
    while ((rp = *rpp) != NULL) {
        if (has_key(tb, rp, key, hval)) {
            if (rp->hvalue == INVALID_HASH)
                break;
            nitems_diff = max_free = DELETE_RECORD_LIMIT;
            /* !!! yield !!! */
            while (!remove_key(tb, &rpp, &max_free, IS_FIXED(tb))) {
                nitems_diff += DELETE_RECORD_LIMIT;
                max_free += DELETE_RECORD_LIMIT;
            }
            nitems_diff -= max_free;
            if (IS_FIXED(tb))
                add_fixed_deletion(tb, ix);
            break;
        }
        rpp = &rp->next;
    }
    WUNLOCK_HASH(lck);
    if (nitems_diff) {
        erts_smp_atomic_add_nob(&tb->common.nitems, -nitems_diff);
        erts_smp_atomic_dec_nob(&tb->nkeys);
        try_shrink(tb);
    }
    *ret = am_true;
    return DB_ERROR_NONE;
}

/*
 * This is for the ets:delete_object BIF
 */
static int
db_erase_object_nhash(DbTable *tbl, Eterm object, Eterm *ret)
{
    int ix, nix, nitems_diff, nkeys_diff;
    Eterm key;
    HashValue hval;
    RootDbTerm **rpp, *rp;
    TrunkDbTerm *ntp;
    TrunkDbTerm **ntpp, **tpp;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    nitems_diff = nkeys_diff = 0;
    key = GETKEY(tb, tuple_val(object));
    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, hval);
    rpp = &BUCKET(tb, ix);
    while ((rp = *rpp) != NULL) {
        if (has_key(tb, rp, key, hval)) {
            if (rp->hvalue == INVALID_HASH)
                break;
            if (HAS_LHT(rp)) {
                hval = MAKE_HASH(object);
                nix = nested_hash_to_ix(rp, hval);
                ntpp = &NESTED_BUCKET(rp, nix);
                while ((ntp = *ntpp) != NULL) {
                    if (db_eq(&tb->common, object, &ntp->dbterm)) {
                        --nitems_diff;
                        if (IS_FIXED(tb)) {
                            if (remove_object_and_nested(tb, rpp, ntp,
                                                         ntpp, 1)) {
                                --nkeys_diff;
                                /* pseudo-deletion */
                                add_fixed_deletion(tb, ix);
                                break;
                            }
                        } else {
                            if (remove_object_and_nested(tb, rpp, ntp,
                                                         ntpp, 0)) {
                                --nkeys_diff;
                                break;
                            }
                        }
                        if (!(tb->common.status & (DB_DUPLICATE_BAG)))
                            break;
                    } else {
                        ntpp = &ntp->onext;
                    }
                }
            } else {
                tpp = &rp->trunk;
                do {
                    if (db_eq(&tb->common, object, &(*tpp)->dbterm)) {
                        --nitems_diff;
                        if (IS_FIXED(tb)) {
                            if (remove_object_no_nested(tb, rpp, tpp, 1)) {
                                --nkeys_diff;
                                /* pseudo-deletion */
                                add_fixed_deletion(tb, ix);
                                break;
                            }
                        } else {
                            if (remove_object_no_nested(tb, rpp, tpp, 0)) {
                                --nkeys_diff;
                                break;
                            }
                        }
                        if (!(tb->common.status & (DB_DUPLICATE_BAG)))
                            break;
                    } else {
                        tpp = &(*tpp)->next;
                    }
                } while (*tpp != NULL);
            }
            break;
        }
        rpp = &rp->next;
    }
    WUNLOCK_HASH(lck);
    if (nitems_diff) {
        erts_smp_atomic_add_nob(&tb->common.nitems, nitems_diff);
        if (nkeys_diff) {
            erts_smp_atomic_add_nob(&tb->nkeys, nkeys_diff);
            try_shrink(tb);
        }
    }
    *ret = am_true;
    return DB_ERROR_NONE;
}

static int
db_slot_nhash(Process *p, DbTable *tbl, Eterm slot_term, Eterm *ret)
{
    int nactive;
    Sint slot;
    erts_smp_rwmtx_t* lck;
    DbTableNestedHash *tb = &tbl->nested;
    if (is_not_small(slot_term) || ((slot = signed_val(slot_term)) < 0))
        return DB_ERROR_BADPARAM;
    lck = RLOCK_HASH(tb, slot);
    nactive = NACTIVE(tb);
    if (slot > nactive) {
        RUNLOCK_HASH(lck);
        return DB_ERROR_BADPARAM;
    }
    if (slot == nactive) {
        *ret = am_EOT;
    } else {
        /* !!! yield !!! */
        *ret = build_term_list(p, BUCKET(tb, slot), NULL, tb);
    }
    RUNLOCK_HASH(lck);
    return DB_ERROR_NONE;
}

static int
db_select_chunk_nhash(Process *p, DbTable *tbl, Eterm pattern,
                      Sint chunk_size, int reverse /* not used */, Eterm *ret)
{
    int errcode, num_left = 1000;
    unsigned int current_list_pos = 0;
    Uint got = 0;
    Sint slot_ix, rest_size;
    Eterm continuation, match_list, match_res, mpb, rest;
    Eterm *hp;
    struct mp_info mpi;
    erts_smp_rwmtx_t* lck;
    RootDbTerm *rp;
    TrunkDbTerm *tp;
    DbTableNestedHash *tb = &tbl->nested;
    errcode = analyze_pattern(tb, pattern, &mpi);
    if (errcode != DB_ERROR_NONE) {
        *ret = NIL;
        goto exit;
    }
    /* errcode == DB_ERROR_NONE; */
    if (!mpi.something_can_match)
        goto end_of_table;
    if (mpi.key_given) {
        /* We have at least one */
        slot_ix = mpi.lists[current_list_pos].ix;
        lck = RLOCK_HASH(tb, slot_ix);
        rp = *(mpi.lists[current_list_pos].bucket);
        ASSERT(rp == BUCKET(tb, slot_ix));
        ++current_list_pos;
    } else {
        /*
         * Run this code if pattern is variable
         * or GETKEY(pattern) is a variable
         */
        slot_ix = 0;
        lck = RLOCK_HASH(tb, slot_ix);
        for (;;) {
            ASSERT(slot_ix < NACTIVE(tb));
            rp = BUCKET(tb, slot_ix);
            if (rp != NULL)
                break;
            slot_ix = next_slot(tb,slot_ix,&lck);
            if (slot_ix == 0)
                goto end_of_table;
        }
    }
    match_list = NIL;
    tp = SAFE_GET_TRUNK(rp);
    for (;;) {
        /* !!! yield !!! */
        if (tp != NULL) {
            if (rp->hvalue != INVALID_HASH) {
                match_res = db_match_dbterm(&tb->common, p, mpi.mp, 0,
                                            &tp->dbterm, &hp, 2);
                if (is_value(match_res)) {
                    match_list = CONS(hp, match_res, match_list);
                    ++got;
                }
                --num_left;
            }
            NEXT_DBTERM(rp, tp);
        } else if (mpi.key_given) {
            /* Key is bound */
            RUNLOCK_HASH(lck);
            if (current_list_pos == mpi.num_lists) {
                /* EOT */
                slot_ix = -1;
                break;
            }
            slot_ix = mpi.lists[current_list_pos].ix;
            lck = RLOCK_HASH(tb, slot_ix);
            rp = *(mpi.lists[current_list_pos].bucket);
            ASSERT(mpi.lists[current_list_pos].bucket == &BUCKET(tb, slot_ix));
            tp = SAFE_GET_TRUNK(rp);
            ++current_list_pos;
        } else {
            /* Key is variable */
            slot_ix = next_slot(tb, slot_ix, &lck);
            if (slot_ix == 0) {
                slot_ix = -1;
                break;
            }
            if (chunk_size && (got >= chunk_size)) {
                RUNLOCK_HASH(lck);
                break;
            }
            if ((num_left <= 0) || MBUF(p)) {
                /*
                 * We have either reached our limit, or just created
                 * some heap fragments. Since many heap fragments
                 * will make the GC slower, trap and GC now.
                 */
                RUNLOCK_HASH(lck);
                goto trap;
            }
            rp = BUCKET(tb, slot_ix);
            tp = SAFE_GET_TRUNK(rp);
        }
    }
    BUMP_REDS(p, 1000 - num_left);
    if (!chunk_size) {
        *ret = match_list;
        goto exit;
    }
    rest_size = 0;
    rest = NIL;
    if (got > chunk_size) {
        /* Split list in return value and 'rest' */
        Eterm tmp = match_list;
        rest = match_list;
        while (--got > chunk_size) {
            tmp = CDR(list_val(tmp));
            ++rest_size;
        }
        ++rest_size;
        match_list = CDR(list_val(tmp));
        /* Destructive, the list has never been in 'user space' */
        CDR(list_val(tmp)) = NIL;
    }
    if ((rest != NIL) || (slot_ix >= 0)) {
        /* Need more calls */
        if (mpi.all_objects)
            (mpi.mp)->flags |= BIN_FLAG_ALL_OBJECTS;
        hp = HAlloc(p, 3 + 7 + PROC_BIN_SIZE);
        mpb = db_make_mp_binary(p, mpi.mp, &hp);
        continuation =
            TUPLE6(hp, tb->common.id, make_small(slot_ix),
                   make_small(chunk_size), mpb, rest, make_small(rest_size));
        mpi.mp = NULL;
        hp += 7;
        *ret = TUPLE2(hp, match_list, continuation);
        goto exit;
    }
    /* All data is exhausted */
    if (match_list != NIL) {
        /* No more data to search but still a result to return to the caller */
        hp = HAlloc(p, 3);
        *ret = TUPLE2(hp, match_list, am_EOT);
        goto exit;
    }
    /* Reached the end of the table with no data to return */
    *ret = am_EOT;
    goto exit;

end_of_table:
    *ret = (chunk_size) ? am_EOT : NIL;
    goto exit;

trap:
    BUMP_ALL_REDS(p);
    if (mpi.all_objects)
        (mpi.mp)->flags |= BIN_FLAG_ALL_OBJECTS;
    hp = HAlloc(p, 7 + PROC_BIN_SIZE);
    mpb = db_make_mp_binary(p, mpi.mp, &hp);
    continuation =
        TUPLE6(hp, tb->common.id, make_small(slot_ix),
               make_small(chunk_size), mpb, match_list, make_small(got));
    mpi.mp = NULL;
    *ret = bif_trap1(&ets_select_continue_exp, p, continuation);

exit:
    if (mpi.mp != NULL)
        erts_bin_free(mpi.mp);
    if (mpi.lists != mpi.dlists)
        erts_free(ERTS_ALC_T_DB_SEL_LIST, (void *)mpi.lists);
    return errcode;
}

static int
db_select_nhash(Process *p, DbTable *tbl,
                Eterm pattern, int reverse, Eterm *ret)
{
    return db_select_chunk_nhash(p, tbl, pattern, 0, reverse, ret);
}

static int
db_select_delete_nhash(Process *p, DbTable *tbl, Eterm pattern, Eterm *ret)
{
    int errcode, num_left = 1000;
    unsigned int current_list_pos = 0;
    Uint got = 0, last_pseudo_delete = (Uint)-1, slot_ix = 0;
    Eterm continuation, egot, mpb, object;
    Eterm *hp;
    DbTerm *uterm;
    erts_aint_t fixated_by_me;
    struct mp_info mpi;
    erts_smp_rwmtx_t *lck;
    RootDbTerm **rpp;
    TrunkDbTerm **tpp;
    DbTableNestedHash *tb = &tbl->nested;
#ifdef ERTS_SMP
    /* ToDo: something nicer */
    fixated_by_me = tb->common.is_thread_safe ? 0 : 1;
#else
    fixated_by_me = 0;
#endif
    errcode = analyze_pattern(tb, pattern, &mpi);
    if (errcode != DB_ERROR_NONE) {
        *ret = NIL;
        goto exit;
    }
    /* errcode = DB_ERROR_NONE; */
    if (!mpi.something_can_match) {
        /* can't possibly match anything */
        *ret = make_small(0);
        goto exit;
    }
    if (mpi.key_given) {
        /* We have at least one */
        slot_ix = mpi.lists[current_list_pos].ix;
        lck = WLOCK_HASH(tb, slot_ix);
        rpp = mpi.lists[current_list_pos++].bucket;
        ASSERT(*rpp == BUCKET(tb, slot_ix));
    } else {
        /*
         * Run this code if pattern is variable
         * or GETKEY(pattern) is a variable
         */
        lck = WLOCK_HASH(tb, slot_ix);
        rpp = &BUCKET(tb, slot_ix);
    }
    tpp = SAFE_GET_TRUNK_P(rpp);
    for (;;) {
        /* !!! yield !!! */
        if (tpp != NULL) {
            if ((*rpp)->hvalue != INVALID_HASH) {
                if (db_match_dbterm_nhash(tb, p, mpi.mp,
                                          &TRUNK_PTR(*tpp)->dbterm,
                                          &object, &uterm)) {
                    /* fixated by others? */
                    if (NFIXED(tb) > fixated_by_me) {
                        if (remove_object(tb, &rpp, &tpp, object, 1)) {
                            erts_smp_atomic_dec_nob(&tb->nkeys);
                            /* Pseudo deletion */
                            if (slot_ix != last_pseudo_delete) {
                                add_fixed_deletion(tb, slot_ix);
                                last_pseudo_delete = slot_ix;
                            }
                        }
                    } else {
                        if (remove_object(tb, &rpp, &tpp, object, 0))
                            erts_smp_atomic_dec_nob(&tb->nkeys);
                    }
                    erts_smp_atomic_dec_nob(&tb->common.nitems);
                    ++got;
                } else {
                    NEXT_DBTERM_P(rpp, tpp);
                }
                if (uterm != NULL)
                    db_free_tmp_uncompressed(uterm);
                --num_left;
            } else {
                NEXT_DBTERM_P(rpp, tpp);
            }
        } else if (mpi.key_given) {
            /* Key is bound */
            WUNLOCK_HASH(lck);
            if (current_list_pos == mpi.num_lists)
                break;
            slot_ix = mpi.lists[current_list_pos].ix;
            lck = WLOCK_HASH(tb, slot_ix);
            rpp = mpi.lists[current_list_pos].bucket;
            ASSERT(mpi.lists[current_list_pos].bucket == &BUCKET(tb, slot_ix));
            tpp = SAFE_GET_TRUNK_P(rpp);
            ++current_list_pos;
        } else {
            slot_ix = next_slot_w(tb, slot_ix, &lck);
            if (slot_ix == 0)
                break;
            if (num_left <= 0) {
                WUNLOCK_HASH(lck);
                goto trap;
            }
            rpp = &BUCKET(tb, slot_ix);
            tpp = SAFE_GET_TRUNK_P(rpp);
        }
    }
    BUMP_REDS(p, 1000 - num_left);
    if (got)
        try_shrink(tb);
    *ret = erts_make_integer(got, p);
    goto exit;

trap:
    BUMP_ALL_REDS(p);
    if (IS_USMALL(0, got)) {
        hp = HAlloc(p,  PROC_BIN_SIZE + 5);
        egot = make_small(got);
    } else {
        hp = HAlloc(p, BIG_UINT_HEAP_SIZE + PROC_BIN_SIZE + 5);
        egot = uint_to_big(got, hp);
        hp += BIG_UINT_HEAP_SIZE;
    }
    mpb = db_make_mp_binary(p, mpi.mp, &hp);
    continuation = TUPLE4(hp, tb->common.id, make_small(slot_ix), mpb, egot);
    mpi.mp = NULL;
    *ret = bif_trap1(&ets_select_delete_continue_exp, p, continuation);

exit:
    if (mpi.mp != NULL)
        erts_bin_free(mpi.mp);
    if (mpi.lists != mpi.dlists)
        erts_free(ERTS_ALC_T_DB_SEL_LIST, (void *)mpi.lists);
    return errcode;
}

/*
 * Continue collecting select matches, this may happen
 * either due to a trap or when the user calls ets:select/1
 */
static int
db_select_continue_nhash(Process *p, DbTable *tbl,
                         Eterm continuation, Eterm *ret)
{
    int all_objects, num_left = 1000;
    Sint chunk_size, got, rest_size, slot_ix;
    Binary *mp;
    Eterm match_list, match_res, rest;
    Eterm *hp, *tptr;
    RootDbTerm *rp;
    TrunkDbTerm *tp;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    *ret = NIL;
    /*
     * Decode continuation. We know it's a tuple
     * but not the arity or anything else
     */
    tptr = tuple_val(continuation);
    if (arityval(*tptr) != 6)
        return DB_ERROR_BADPARAM;
    if (!is_small(tptr[2]) || !is_small(tptr[3]) || !is_binary(tptr[4]) ||
        !(is_list(tptr[5]) || tptr[5] == NIL) || !is_small(tptr[6]))
        return DB_ERROR_BADPARAM;
    chunk_size = signed_val(tptr[3]);
    if (chunk_size < 0)
        return DB_ERROR_BADPARAM;
    if (thing_subtag(*binary_val(tptr[4])) != REFC_BINARY_SUBTAG)
        return DB_ERROR_BADPARAM;
    mp = ((ProcBin *)binary_val(tptr[4]))->val;
    if (!IsMatchProgBinary(mp))
        return DB_ERROR_BADPARAM;
    all_objects = mp->flags & BIN_FLAG_ALL_OBJECTS;
    match_list = tptr[5];
    got = signed_val(tptr[6]);
    if (got < 0)
        return DB_ERROR_BADPARAM;
    slot_ix = signed_val(tptr[2]);
    if ((slot_ix < 0) /* EOT */
        || (chunk_size && (got >= chunk_size)))
        /* Already got all or enough in the match_list */
        goto done;
    lck = RLOCK_HASH(tb, slot_ix);
    if (slot_ix >= NACTIVE(tb)) {
        RUNLOCK_HASH(lck);
        return DB_ERROR_BADPARAM;
    }
    while ((rp = BUCKET(tb, slot_ix)) == NULL) {
        slot_ix = next_slot(tb, slot_ix, &lck);
        if (slot_ix == 0) {
            /* EOT */
            slot_ix = -1;
            goto done;
        }
    }
    tp = GET_TRUNK(rp);
    for (;;) {
        /* !!! yield !!! */
        if (tp != NULL) {
            if (rp->hvalue != INVALID_HASH) {
                match_res = db_match_dbterm(&tb->common, p, mp, all_objects,
                                            &tp->dbterm, &hp, 2);
                if (is_value(match_res)) {
                    match_list = CONS(hp, match_res, match_list);
                    ++got;
                }
                --num_left;
            }
            NEXT_DBTERM(rp, tp);
        } else {
            slot_ix = next_slot(tb, slot_ix, &lck);
            if (slot_ix == 0) {
                /* EOT */
                slot_ix = -1;
                break;
            }
            if (chunk_size && (got >= chunk_size)) {
                RUNLOCK_HASH(lck);
                break;
            }
            if ((num_left <= 0) || MBUF(p)) {
                /*
                 * We have either reached our limit, or just created
                 * some heap fragments. Since many heap fragments
                 * will make the GC slower, trap and GC now.
                 */
                RUNLOCK_HASH(lck);
                goto trap;
            }
            rp = BUCKET(tb, slot_ix);
            tp = SAFE_GET_TRUNK(rp);
        }
    }

done:
    BUMP_REDS(p, 1000 - num_left);
    if (!chunk_size) {
        *ret = match_list;
        return DB_ERROR_NONE;
    }
    rest_size = 0;
    rest = NIL;
    if (got > chunk_size) {
        /*
         * Cannot write destructively here,
         * the list may have been in user space
         */
        hp = HAlloc(p, (got - chunk_size) * 2);
        while (got-- > chunk_size) {
            rest = CONS(hp, CAR(list_val(match_list)), rest);
            hp += 2;
            match_list = CDR(list_val(match_list));
            ++rest_size;
        }
    }
    if ((rest != NIL) || (slot_ix >= 0)) {
        hp = HAlloc(p, 3 + 7);
        continuation = TUPLE6(hp, tptr[1], make_small(slot_ix), tptr[3],
                              tptr[4], rest, make_small(rest_size));
        hp += 7;
        *ret = TUPLE2(hp, match_list, continuation);
    } else {
        if (match_list != NIL) {
            hp = HAlloc(p, 3);
            *ret = TUPLE2(hp, match_list, am_EOT);
        } else {
            *ret = am_EOT;
        }
    }
    return DB_ERROR_NONE;

trap:
    BUMP_ALL_REDS(p);
    hp = HAlloc(p, 7);
    continuation = TUPLE6(hp, tptr[1], make_small(slot_ix), tptr[3],
                          tptr[4], match_list, make_small(got));
    *ret = bif_trap1(&ets_select_continue_exp, p, continuation);
    return DB_ERROR_NONE;
}

/*
 * This is called when select_delete traps
 */
static int
db_select_delete_continue_nhash(Process *p, DbTable *tbl,
                                Eterm continuation, Eterm *ret)
{
    int fixated_by_me, num_left = 1000;
    Uint got, last_pseudo_delete = (Uint)-1, slot_ix;
    Eterm egot, object;
    Eterm *hp, *tptr;
    DbTerm *uterm;
    Binary *mp;
    erts_smp_rwmtx_t *lck;
    RootDbTerm **rpp;
    TrunkDbTerm **tpp;
    DbTableNestedHash *tb = &tbl->nested;
    /* ToDo: something nicer */
    fixated_by_me = ONLY_WRITER(p, tb) ? 0 : 1;
    tptr = tuple_val(continuation);
    slot_ix = unsigned_val(tptr[2]);
    mp = ((ProcBin *)binary_val(tptr[3]))->val;
    if (is_big(tptr[4])) {
        got = big_to_uint32(tptr[4]);
    } else {
        got = unsigned_val(tptr[4]);
    }
    lck = WLOCK_HASH(tb, slot_ix);
    if (slot_ix >= NACTIVE(tb)) {
        WUNLOCK_HASH(lck);
    } else {
        rpp = &BUCKET(tb, slot_ix);
        tpp = SAFE_GET_TRUNK_P(rpp);
        for (;;) {
            /* !!! yield !!! */
            if (tpp == NULL) {
                slot_ix = next_slot_w(tb, slot_ix, &lck);
                if (slot_ix == 0)
                    break;
                if (num_left <= 0) {
                    WUNLOCK_HASH(lck);
                    goto trap;
                }
                rpp = &BUCKET(tb, slot_ix);
                tpp = SAFE_GET_TRUNK_P(rpp);
            } else if ((*rpp)->hvalue == INVALID_HASH) {
                NEXT_DBTERM_P(rpp, tpp);
            } else {
                if (db_match_dbterm_nhash(tb, p, mp, &TRUNK_PTR(*tpp)->dbterm,
                                          &object, &uterm)) {
                    /* fixated by others? */
                    if (NFIXED(tb) > fixated_by_me) {
                        if (remove_object(tb, &rpp, &tpp, object, 1)) {
                            erts_smp_atomic_dec_nob(&tb->nkeys);
                            /* Pseudo deletion */
                            if (slot_ix != last_pseudo_delete) {
                                add_fixed_deletion(tb, slot_ix);
                                last_pseudo_delete = slot_ix;
                            }
                        }
                    } else {
                        if (remove_object(tb, &rpp, &tpp, object, 0))
                            erts_smp_atomic_dec_nob(&tb->nkeys);
                    }
                    erts_smp_atomic_dec_nob(&tb->common.nitems);
                    ++got;
                } else {
                    NEXT_DBTERM_P(rpp, tpp);
                }
                if (uterm != NULL)
                    db_free_tmp_uncompressed(uterm);
                --num_left;
            }
        }
    }
    BUMP_REDS(p, 1000 - num_left);
    if (got)
        try_shrink(tb);
    *ret = erts_make_integer(got, p);
    return DB_ERROR_NONE;

trap:
    BUMP_ALL_REDS(p);
    if (IS_USMALL(0, got)) {
        hp = HAlloc(p,  5);
        egot = make_small(got);
    } else {
        hp = HAlloc(p, BIG_UINT_HEAP_SIZE + 5);
        egot = uint_to_big(got, hp);
        hp += BIG_UINT_HEAP_SIZE;
    }
    continuation =
        TUPLE4(hp, tb->common.id, make_small(slot_ix), tptr[3], egot);
    *ret = bif_trap1(&ets_select_delete_continue_exp, p, continuation);
    return DB_ERROR_NONE;
}

static int
db_select_count_nhash(Process *p, DbTable *tbl, Eterm pattern, Eterm *ret)
{
    int errcode, num_left = 1000;
    unsigned current_list_pos = 0;
    Uint got = 0, slot_ix = 0;
    Eterm continuation, egot, mpb;
    Eterm *hp;
    struct mp_info mpi;
    erts_smp_rwmtx_t *lck;
    RootDbTerm *rp;
    TrunkDbTerm *tp;
    DbTableNestedHash *tb = &tbl->nested;
    errcode = analyze_pattern(tb, pattern, &mpi);
    if (errcode != DB_ERROR_NONE) {
        *ret = NIL;
        goto exit;
    }
    /* errcode = DB_ERROR_NONE; */
    if (!mpi.something_can_match) {
        /* can't possibly match anything */
        *ret = make_small(0);
        goto exit;
    }
    if (!mpi.key_given) {
        /*
         * Run this code if pattern is variable
         * or GETKEY(pattern) is a variable
         */
        slot_ix = 0;
        lck = RLOCK_HASH(tb, slot_ix);
        rp = BUCKET(tb, slot_ix);
    } else {
        /* We have at least one */
        slot_ix = mpi.lists[current_list_pos].ix;
        lck = RLOCK_HASH(tb, slot_ix);
        rp = *(mpi.lists[current_list_pos].bucket);
        ASSERT(rp == BUCKET(tb, slot_ix));
        ++current_list_pos;
    }
    tp = SAFE_GET_TRUNK(rp);
    for (;;) {
        /* !!! yield !!! */
        if (tp != NULL) {
            if (rp->hvalue != INVALID_HASH) {
                if (db_match_dbterm(&tb->common, p, mpi.mp, 0,
                                    &tp->dbterm, NULL, 0) == am_true)
                    ++got;
                --num_left;
            }
            NEXT_DBTERM(rp, tp);
        } else if (mpi.key_given) {
            /* Key is bound */
            RUNLOCK_HASH(lck);
            if (current_list_pos == mpi.num_lists)
                break;
            slot_ix = mpi.lists[current_list_pos].ix;
            lck = RLOCK_HASH(tb, slot_ix);
            rp = *(mpi.lists[current_list_pos].bucket);
            ASSERT(mpi.lists[current_list_pos].bucket == &BUCKET(tb, slot_ix));
            tp = SAFE_GET_TRUNK(rp);
            ++current_list_pos;
        } else {
            slot_ix = next_slot(tb, slot_ix, &lck);
            if (slot_ix == 0)
                break;
            if (num_left <= 0) {
                RUNLOCK_HASH(lck);
                goto trap;
            }
            rp = BUCKET(tb, slot_ix);
            tp = SAFE_GET_TRUNK(rp);
        }
    }
    BUMP_REDS(p, 1000 - num_left);
    *ret = erts_make_integer(got, p);
    goto exit;

trap:
    BUMP_ALL_REDS(p);
    if (IS_USMALL(0, got)) {
        hp = HAlloc(p,  PROC_BIN_SIZE + 5);
        egot = make_small(got);
    } else {
        hp = HAlloc(p, BIG_UINT_HEAP_SIZE + PROC_BIN_SIZE + 5);
        egot = uint_to_big(got, hp);
        hp += BIG_UINT_HEAP_SIZE;
    }
    mpb = db_make_mp_binary(p, mpi.mp, &hp);
    continuation = TUPLE4(hp, tb->common.id, make_small(slot_ix), mpb, egot);
    mpi.mp = NULL;
    *ret = bif_trap1(&ets_select_count_continue_exp, p, continuation);

exit:
    if (mpi.mp != NULL)
        erts_bin_free(mpi.mp);
    if (mpi.lists != mpi.dlists)
        erts_free(ERTS_ALC_T_DB_SEL_LIST, (void *)mpi.lists);
    return errcode;
}

/*
 * This is called when select_count traps
 */
static int
db_select_count_continue_nhash(Process *p, DbTable *tbl,
                               Eterm continuation, Eterm *ret)
{
    int num_left = 1000;
    Uint got, slot_ix;
    Eterm egot;
    Eterm *hp, *tptr;
    Binary *mp;
    erts_smp_rwmtx_t *lck;
    RootDbTerm *rp;
    TrunkDbTerm *tp;
    DbTableNestedHash *tb = &tbl->nested;
    tptr = tuple_val(continuation);
    slot_ix = unsigned_val(tptr[2]);
    mp = ((ProcBin *)binary_val(tptr[3]))->val;
    got = (is_big(tptr[4])) ? big_to_uint32(tptr[4]) : unsigned_val(tptr[4]);
    lck = RLOCK_HASH(tb, slot_ix);
    if (slot_ix >= NACTIVE(tb)) {
        /* Is this possible? */
        RUNLOCK_HASH(lck);
    } else {
        rp = BUCKET(tb, slot_ix);
        tp = SAFE_GET_TRUNK(rp);
        for (;;) {
            /* !!! yield !!! */
            if (tp != NULL) {
                if (rp->hvalue != INVALID_HASH) {
                    if (db_match_dbterm(&tb->common, p, mp, 0,
                                        &tp->dbterm, NULL, 0) == am_true)
                        ++got;
                    --num_left;
                }
                NEXT_DBTERM(rp, tp);
            } else {
                /* next bucket */
                slot_ix = next_slot(tb, slot_ix, &lck);
                if (slot_ix == 0)
                    break;
                if (num_left <= 0) {
                    RUNLOCK_HASH(lck);
                    goto trap;
                }
                rp = BUCKET(tb, slot_ix);
                tp = SAFE_GET_TRUNK(rp);
            }
        }
    }
    BUMP_REDS(p, 1000 - num_left);
    *ret = erts_make_integer(got, p);
    return DB_ERROR_NONE;

trap:
    BUMP_ALL_REDS(p);
    if (IS_USMALL(0, got)) {
        hp = HAlloc(p, 5);
        egot = make_small(got);
    } else {
        hp = HAlloc(p, BIG_UINT_HEAP_SIZE + 5);
        egot = uint_to_big(got, hp);
        hp += BIG_UINT_HEAP_SIZE;
    }
    continuation =
        TUPLE4(hp, tb->common.id, make_small(slot_ix), tptr[3], egot);
    *ret = bif_trap1(&ets_select_count_continue_exp, p, continuation);
    return DB_ERROR_NONE;
}

static int
db_free_table_continue_nhash(DbTable *tbl)
{
    int max_free = DELETE_RECORD_LIMIT;
    NestedFixedDeletion *fixdel, *fx;
    DbTableNestedHash *tb = &tbl->nested;
    ERTS_SMP_LC_ASSERT(IS_TAB_WLOCKED(tb));
    fixdel = (NestedFixedDeletion *)erts_smp_atomic_read_acqb(&tb->fixdel);
    while (fixdel != NULL) {
        fx = fixdel;
        fixdel = fx->next;
        erts_db_free(ERTS_ALC_T_DB_FIX_DEL, (DbTable *)tb,
                     (void *)fx, sizeof(NestedFixedDeletion));
        ERTS_ETS_MISC_MEM_ADD(-sizeof(NestedFixedDeletion));
        if (max_free <= 0) {
            erts_smp_atomic_set_relb(&tb->fixdel, (erts_aint_t)fixdel);
            /* Not done */
            return 0;
        }
        --max_free;
    }
    erts_smp_atomic_set_relb(&tb->fixdel, (erts_aint_t)NULL);
    while (tb->nslots != 0) {
        if (!free_seg(tb, &max_free))
            /* If we have done enough work, get out here. */
            return 0;
    }
#ifdef ERTS_SMP
    if (tb->locks != NULL) {
        int i;
        for (i = 0; i<DB_NESTED_HASH_LOCK_CNT; ++i)
            erts_rwmtx_destroy(GET_LOCK(tb, i));
        erts_db_free(ERTS_ALC_T_DB_SEG, (DbTable *)tb, (void *)tb->locks,
                     sizeof(DbTableNestedHashFineLocks));
        tb->locks = NULL;
    }
#endif
    ASSERT(erts_smp_atomic_read_nob(&tb->common.memory_size)
           == sizeof(DbTable));
    /* Done */
    return 1;
}

/*
 * release all memory occupied by a single table
 */
static int
db_free_table_nhash(DbTable *tbl)
{
    while (!db_free_table_continue_nhash(tbl));
    return 0;
}

/* !!! yield ??? */
static int
db_delete_all_objects_nhash(Process *p, DbTable *tbl)
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

/*
 * Display hash table contents (for dump)
 */
static void
db_print_nhash(int to, void *to_arg, int show, DbTable *tbl)
{
    int i;
    Eterm key, obj;
    RootDbTerm *rp;
    TrunkDbTerm *tp;
    DbTableNestedHash *tb = &tbl->nested;
    erts_print(to, to_arg, "Buckets: %d\n", NACTIVE(tb));
    if (!show)
        return;
    for (i = 0; i < NACTIVE(tb); i++) {
        rp = BUCKET(tb, i);
        if (rp == NULL)
            continue;
        tp = GET_TRUNK(rp);
        erts_print(to, to_arg, "%d: [", i);
        for (;;) {
            if (rp->hvalue == INVALID_HASH)
                erts_print(to, to_arg, "*");
            if (tb->common.compress) {
                key = GETKEY(tb, tp->dbterm.tpl);
                erts_print(to, to_arg, "key=%R", key, tp->dbterm.tpl);
            } else {
                obj = make_tuple_rel(tp->dbterm.tpl, tp->dbterm.tpl);
                erts_print(to, to_arg, "%R", obj, tp->dbterm.tpl);
            }
            NEXT_DBTERM(rp, tp);
            if (tp == NULL)
                break;
            erts_print(to, to_arg, ",");
        }
        erts_print(to, to_arg, "]\n");
    }
}

static void
db_foreach_offheap_nhash(DbTable *tbl,
                         void (*func)(ErlOffHeap *, void *), void *arg)
{
    int i, nactive;
    ErlOffHeap tmp_offheap;
    RootDbTerm *rp;
    TrunkDbTerm *tp;
    DbTableNestedHash *tb = &tbl->nested;
    nactive = NACTIVE(tb);
    for (i = 0; i < nactive; i++) {
        rp = BUCKET(tb, i);
        if (rp == NULL)
            continue;
        tp = GET_TRUNK(rp);
        while (tp != NULL) {
            tmp_offheap.first = tp->dbterm.first_oh;
            tmp_offheap.overhead = 0;
            (*func)(&tmp_offheap, arg);
            tp->dbterm.first_oh = tmp_offheap.first;
            NEXT_DBTERM(rp, tp);
        }
    }
}

#ifdef HARDDEBUG
static void
db_check_table_nhash(DbTable *tbl)
{
    int i, j, na, nk, nk2;
    int nactive, nitems, nkeys;
    RootDbTerm *rp;
    TrunkDbTerm *tp, *ntp;
    DbTableNestedHash *tb = &tbl->nested;
    nkeys = nitems = 0;
    nactive = NACTIVE(tb);
    if (nactive > tb->nslots)
        nactive = tb->nslots;
    for (j = 0; j < nactive; ++j) {
        rp = BUCKET(tb, j);
        while (rp != NULL) {
            tp = GET_TRUNK(rp);
            nk = 0;
            do {
                if (!is_tuple(make_tuple(tp->dbterm.tpl)))
                    erl_exit(1, "Bad term in slot %d of ets table", j);
                ++nk;
                tp = tp->next;
            } while (tp != NULL);
            if (rp->nkitems != nk)
                erl_exit(1, "Invalid nkitems in slot %d of ets table", j);
            if (rp->hvalue == INVALID_HASH) {
                if (nk != 1)
                    erl_exit(1, "Invalid number of terms in pseudo-deleted"
                             " RootDbTerm in slot %d of ets table", j);
                nk = 0;
            } else {
                ++nkeys;
                nitems += nk;
            }
            if (HAS_LHT(rp)) {
                nk2 = 0;
                na = rp->nactive;
                for (i = 0; i < na; ++i) {
                    ntp = NESTED_BUCKET(rp, i);
                    while (ntp != NULL) {
                        ++nk2;
                        ntp = ntp->onext;
                    }
                }
                if (nk2 != nk)
                    erl_exit(1, "Invalid number of terms in nested table"
                             " of slot %d of ets table", j);
            }
            rp = rp->next;
        }
    }
    if (erts_smp_atomic_read_nob(&tb->common.nitems) != nitems)
        erl_exit(1, "Invalid number of items in ets table");
    if (erts_smp_atomic_read_nob(&tb->nkeys) != nkeys)
        erl_exit(1, "Invalid number of keys in ets table");
}
#endif


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
    NULL,
    NULL
};


/*
 * Other interface routines (not directly coupled to one bif)
 */

void
db_initialize_nhash(void)
{
}

/*
 * Rare case of restoring the rest of the fixdel list
 * when "unfixer" gets interrupted by "fixer"
 */
static void
restore_fixdel(DbTableNestedHash *tb, NestedFixedDeletion *fixdel)
{
    erts_aint_t was_tail, exp_tail;
    NestedFixedDeletion *last;
    DEBUG_WAIT();
    if (erts_smp_atomic_cmpxchg_relb(&tb->fixdel, (erts_aint_t)fixdel,
                                     (erts_aint_t)NULL) == (erts_aint_t)NULL)
        return;
    /* Oboy, must join lists */
    last = fixdel;
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

void
db_unfix_table_nhash(DbTableNestedHash *tb)
{
    int ix;
    RootDbTerm **rpp;
    erts_smp_rwmtx_t *lck;
    NestedFixedDeletion *fx, *fixdel;
    ERTS_SMP_LC_ASSERT(erts_smp_lc_rwmtx_is_rwlocked(&tb->common.rwlock)
                       || (erts_smp_lc_rwmtx_is_rlocked(&tb->common.rwlock)
                           && (!tb->common.is_thread_safe)));

restart:
    fixdel = (NestedFixedDeletion *)
        erts_smp_atomic_xchg_acqb(&tb->fixdel, (erts_aint_t)NULL);
    while (fixdel != NULL) {
        fx = fixdel;
        ix = fx->slot;
        lck = WLOCK_HASH(tb, ix);
        if (IS_FIXED(tb)) {
            /* interrupted by fixer */
            WUNLOCK_HASH(lck);
            restore_fixdel(tb, fixdel);
            if (!IS_FIXED(tb))
                /* unfixed again! */
                goto restart;
            return;
        }
        if (ix < NACTIVE(tb)) {
            rpp = &BUCKET(tb, ix);
            while (*rpp != NULL) {
                if ((*rpp)->hvalue == INVALID_HASH) {
                    remove_key(tb, &rpp, NULL, 0);
                } else {
                    rpp = &(*rpp)->next;
                }
            }
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
    RootDbTerm *rp;
    erts_smp_rwmtx_t *lck;
    kept_items = ix = 0;
    lck = RLOCK_HASH(tb, ix);
    do {
        rp = BUCKET(tb, ix);
        while (rp != NULL) {
            if (rp->hvalue == INVALID_HASH)
                /*
                 * If pseudo-deleted then the whole trunk
                 * chain contains one term only.
                 */
                ++kept_items;
            rp = rp->next;
        }
        ix = next_slot(tb, ix, &lck);
    } while (ix);
    return kept_items;
}

int
db_get_element_array(DbTable *tbl, Eterm key, int ndex,
                     Eterm *ret, int *num_ret)
{
    int ix, num;
    HashValue hval;
    erts_smp_rwmtx_t *lck;
    RootDbTerm *rp;
    TrunkDbTerm *tp1, *tp2;
    DbTableNestedHash *tb = &tbl->nested;
    ASSERT(!IS_FIXED(tbl)); /* no support for fixed tables here */
    ASSERT(tb->common.status & (DB_BAG | DB_DUPLICATE_BAG));
    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, hval);
    rp = BUCKET(tb, ix);
    while (rp != NULL) {
        if (has_key(tb, rp, key, hval)) {
            ASSERT(rp->hvalue != INVALID_HASH);
            /* !!! yield !!! */
            tp1 = tp2 = GET_TRUNK(rp);
            do {
                if (ndex > arityval(tp1->dbterm.tpl[0]))
                    goto bad_item;
                tp1 = tp1->next;
            } while (tp1 != NULL);
            num = 0;
            do {
                if (num >= *num_ret)
                    goto done;
                ret[num++] = tp2->dbterm.tpl[ndex];
                tp2 = tp2->next;
            } while (tp2 != NULL);
            *num_ret = num;
            goto done;
        }
        rp = rp->next;
    }
    RUNLOCK_HASH(lck);
    return DB_ERROR_BADKEY;

bad_item:
    RUNLOCK_HASH(lck);
    return DB_ERROR_BADITEM;

done:
    RUNLOCK_HASH(lck);
    return DB_ERROR_NONE;
}

/*
 * Very internal interface, removes elements of arity two from
 * BAG. Used for the PID meta table
 */
int
db_erase_bag_exact2(DbTable *tbl, Eterm key, Eterm value)
{
    int ix, nix, found = 0;
    HashValue hval;
    RootDbTerm **rpp, *rp;
    TrunkDbTerm *ntp;
    TrunkDbTerm **tpp, **ntpp;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb,hval);
    ix = hash_to_ix(tb, hval);
    rpp = &BUCKET(tb, ix);
    ASSERT(!IS_FIXED(tb));
    ASSERT((tb->common.status & DB_BAG));
    ASSERT(!tb->common.compress);
    while ((rp = *rpp) != NULL) {
        if (has_key(tb, rp, key, hval)) {
            found = 1;
            ASSERT(rp->hvalue != INVALID_HASH);
            if (HAS_LHT(rp)) {
                Eterm storage[3];
                hval = MAKE_HASH(TUPLE2(storage, key, value));
                nix = nested_hash_to_ix(rp, hval);
                ntpp = &NESTED_BUCKET(rp, nix);
                while ((ntp = *ntpp) != NULL) {
                    if ((arityval(ntp->dbterm.tpl[0]) == 2) &&
                        EQ(value, ntp->dbterm.tpl[2])) {
                        if (remove_object_and_nested(tb, rpp, ntp, ntpp, 0))
                            erts_smp_atomic_dec_nob(&tb->nkeys);
                        erts_smp_atomic_dec_nob(&tb->common.nitems);
                        break;
                    }
                    ntpp = &ntp->onext;
                }
            } else {
                tpp = &rp->trunk;
                do {
                    if ((arityval((*tpp)->dbterm.tpl[0]) == 2) &&
                        EQ(value, (*tpp)->dbterm.tpl[2])) {
                        if (remove_object_no_nested(tb, rpp, tpp, 0))
                            erts_smp_atomic_dec_nob(&tb->nkeys);
                        erts_smp_atomic_dec_nob(&tb->common.nitems);
                        break;
                    }
                    tpp = &(*tpp)->next;
                } while (*tpp != NULL);
            }
            break;
        }
        rpp = &rp->next;
    }
    WUNLOCK_HASH(lck);
    if (found)
        try_shrink(tb);
    return DB_ERROR_NONE;
}

void
db_calc_stats_nhash(DbTableNestedHash *tb, DbNestedHashStats *stats)
{
    int ix, len, sq_sum, sum;
    RootDbTerm *rp;
    TrunkDbTerm *tp;
    erts_smp_rwmtx_t *lck;
    ix = sum = sq_sum = 0;
    stats->min_chain_len = INT_MAX;
    stats->max_chain_len = 0;
    lck = RLOCK_HASH(tb, ix);
    do {
        len = 0;
        rp = BUCKET(tb, ix);
        tp = SAFE_GET_TRUNK(rp);
        /* !!! yield !!! */
        while (tp != NULL) {
            ++len;
            NEXT_DBTERM(rp, tp);
        }
        sum += len;
        sq_sum += len*len;
        if (len < stats->min_chain_len)
            stats->min_chain_len = len;
        if (len > stats->max_chain_len)
            stats->max_chain_len = len;
        ix = next_slot(tb, ix, &lck);
    } while (ix);
    stats->avg_chain_len = (float)sum / NACTIVE(tb);
    stats->std_dev_chain_len =
        sqrt((sq_sum - stats->avg_chain_len * sum) / NACTIVE(tb));
    /*
     * Expected standard deviation from a good uniform hash function,
     * ie binomial distribution (not taking the linear hashing into acount)
     */
    stats->std_dev_expected =
        sqrt(stats->avg_chain_len * (1 - 1.0/NACTIVE(tb)));
}
