/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2012. All Rights Reserved.
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
 * Implementation of unordered ETS tables.
 * The tables are implemented as a two levels linear dynamic hash tables --
 * the first level is keyed by the key's hash, the second (optional) level is
 * keyed by the object's hash.
 *
 * To save as much memory as possible, objects sharing the same key are
 * collected into one or more RootDbTerm structs, whose structure changes
 * depending on the number of objects. There are three such structures, named
 * stage 1, stage 2 and stage 3 RootDbTerm. When first created, they are stage
 * 1 RootDbTerms. As more objects are added, more stage 1 RootDbTerms are
 * created for each one of them. However, when more than KEY_CHAIN_THRESHOLD
 * stage 1 RootDbTerms are create (for the same key), the first of them is
 * promoted to a stage 2 RootDbTerm, while the others are converted into
 * TrunkDbTerms. In this configuration the amount of memory allocated is
 * reduced for there is only one word used to store the HashValue of the key
 * instead of as many as the objects sharing the key.
 * When more than NESTED_CHAIN_THRESHOLD objects are collected under the stage
 * 2 RootDbTerm, it's time to promote it to a stage 3 RootDbTerm. In this
 * configuration the second level linear dynamic hash table (LHT for short) is
 * added to the RootDbTerm.
 *
 * NOTE: In a sequence of stage 1 RootDbTerms (with identical keys), only the
 *       first of them can be pseudo-deleted.
 *
 * To tell a stage 1 RootDbTerm from a stage 2 or 3 RootDbTerm, the lsb of the
 * pointer to the RootDbTerm struct is used. Whereas, to tell a stage 2
 * RootDbTerm from a stage 3 RootDbTerm, the lsb of the pointer to the first
 * TrunkDbTerm struct is used (see "erl_db_nested_hash.h").
 * Throughout this file rp1, rp3 and rp5 are pointers to RootDbTerm structs
 * whose lsb is set accordingly to the content of the pointed struct, whereas
 * rp2 and rp4 are RootDbTerm pointers with the lsb cleared.
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


/*
#ifdef DEBUG
#define HARDDEBUG 1
#endif
*/

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


#ifdef MYDEBUG /* Will fail test case ets_SUITE:memory */

#define MY_ASSERT(x) ASSERT(x)

#else

#define MY_ASSERT(x)

#endif

/*
 * The following symbols can be manipulated to "tune" the linear hash array
 */

/* Medium bucket chain len (apply to both the main and the nested LHT) */
#define CHAIN_LEN 6

/*
 * A sequence of stage 1 RootDbTerms longer than this value
 * will be promoted to (a single) stage 2 RootDbTerm.
 * NOTE: A sequence of N stage 1 RootDbTerm requires (M+2)*N words (plus
 *       dbterms), where M is erts_db_alloc() overhead. A stage 2 RootDbTerm
 *       with N TrunkDbTerms (and without nested LHT) requires M+4+(M+1)*N
 *       words (plus dbterms). When N > M+4, a stage 2 RootDbTerm is cheaper.
 */
#define KEY_CHAIN_THRESHOLD 5 /* Assuming M = 1 */

/*
 * A stage 2 RootDbTerm with a trunk chain longer than this value
 * will be promoted to a stage 3 RootDbTerm.
 * NOTE: A sequence of N stage 1 RootDbTerm requires (M+2)*N words (plus
 *       dbterms), where M is erts_db_alloc() overhead. A stage 3 RootDbTerm
 *       with N TrunkDbTerms and a minimum sized nested LHT (i.e., 256
 *       buckets) requires M+(M+4)*N+SEGSZ+12 words (plus dbterms).
 *       The memory overhead of a stage 3 RootDbTerm wrt a stage 1 RootDbTerm
 *       is thus M+2*N+SEGSZ+12 words, or 2+(M+SEGSZ+12)/N per object.
 *       To keep this value below some threshold X, then N must be greater
 *       than (M+SEGSZ+12)/(X-2).
 */
#define NESTED_CHAIN_THRESHOLD 134 /* Assuming M = 1, SEGSZ = 256 and X = 4 */

/* Number of slots per segment */
#define SEGSZ_EXP  8
#define SEGSZ      (1 << SEGSZ_EXP)
#define SEGSZ_MASK (SEGSZ-1)

#define NSEG_1     2 /* Size of first segment table (must be at least 2) */
#define NSEG_2   256 /* Size of second segment table */
#define NSEG_INC 128 /* Number of segments to grow after that */


#ifdef ERTS_SMP

#define DB_USING_FINE_LOCKING(tb) ((tb)->common.type & DB_FINE_LOCKED)

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

#define HAS_TAIL(rp) ((((UWord)(rp)) & 0x01) != 0)
#define ROOT_PTR(rp) ((RootDbTerm *)(((UWord)(rp)) & ~0x01))

#define MAKE_ROOT(rp, has_tail)                    \
    (                                              \
        ASSERT((((UWord)(rp)) & 0x01) == 0),       \
        (has_tail)                                 \
            ? (RootDbTerm *)(((UWord)(rp)) | 0x01) \
            : (rp)                                 \
    )

#define HAS_NLHT(rp) ((((UWord)(rp)->dt.tail.trunk) & 0x01) != 0)
#define TRUNK_PTR(tp) ((TrunkDbTerm *)(((UWord)(tp)) & ~0x01))

#define MAKE_TRUNK(tp, has_nlht)                    \
    (                                               \
        ASSERT((((UWord)(tp)) & 0x01) == 0),        \
        (has_nlht)                                  \
            ? (TrunkDbTerm *)(((UWord)(tp)) | 0x01) \
            : (tp)                                  \
    )

#define NESTED_BUCKET(rp, i) TRUNK_PTR((rp)->dt.tail.trunk)->sp.segtab[(i) >> SEGSZ_EXP]->buckets[(i) & SEGSZ_MASK].nterm

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
      : make_internal_hash(term))                      \
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
 * 'rpp', 'rp2' and 'tpp' are updated to point to the next
 * Root/TrunkDbTerm in the bucket.
 * NOTE: It is assumed that *rpp, rp2 and *tpp are not NULL
 */
#define NEXT_DBTERM_1_P(rpp, rp2, tpp) \
    (rpp) = &(rp2)->next;              \
    (rp2) = ROOT_PTR(*(rpp));          \
    if (HAS_TAIL(*(rpp)))              \
        (tpp) = &(rp2)->dt.tail.trunk

/*
 * 'rpp', 'rp2' and 'tpp' are updated to point to the next
 * Root/TrunkDbTerm in the bucket.
 * NOTE: It is assumed that *rpp, rp2 and *tpp are not NULL
 *       and that *rp2 is a stage 2 or 3 RootDbTerm
 */
#define NEXT_DBTERM_2_P(rpp, rp2, tpp)     \
    if (TRUNK_PTR(*(tpp))->next == NULL) { \
        NEXT_DBTERM_1_P(rpp, rp2, tpp);    \
    } else {                               \
        (tpp) = &TRUNK_PTR(*(tpp))->next;  \
    }

/*
 * 'rp1', 'rp2' and 'tp' are updated to point to the next
 * Root/TrunkDbTerm in the chain.
 * NOTE: It is assumed that rp1, rp2 and tp are not NULL
 */
#define NEXT_DBTERM_1(rp1, rp2, tp)             \
    (rp1) = (rp2)->next;                        \
    (rp2) = ROOT_PTR(rp1);                      \
    if (HAS_TAIL(rp1))                          \
        (tp) = TRUNK_PTR((rp2)->dt.tail.trunk)

/*
 * 'rp1', 'rp2' and 'tp' are updated to point to the next
 * Root/TrunkDbTerm in the chain.
 * NOTE: It is assumed that rp1, rp2 and tp are not NULL and
 *       that *rp2 is a stage 2 or 3 RootDbTerm
 */
#define NEXT_DBTERM_2(rp1, rp2, tp)  \
    if ((tp)->next == NULL) {        \
        NEXT_DBTERM_1(rp1, rp2, tp); \
    } else {                         \
        (tp) = (tp)->next;           \
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
has_key(DbTableNestedHash *tb, RootDbTerm *rp1, Eterm key, HashValue hval)
{
    Eterm itemKey;
    Eterm *base;
    RootDbTerm *rp2 = ROOT_PTR(rp1);
    if ((rp2->hvalue != hval) && (rp2->hvalue != INVALID_HASH))
        return 0;
    base = HAS_TAIL(rp1)
        ? TRUNK_PTR(rp2->dt.tail.trunk)->dbterm.tpl
        : rp2->dt.dbterm.tpl;
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

/*
 * Create a stage 1 RootDbTerm (i.e., without tail). It will be later
 * promoted to stage 2 and 3 using promote_root_to_stage_2() and
 * promote_root_to_stage_3().
 */
static ERTS_INLINE RootDbTerm *
new_root_dbterm(DbTableNestedHash *tb, Eterm obj)
{
    RootDbTerm *ret;
    ret = (RootDbTerm *)
        ((tb->common.compress)
         ? db_store_term_comp(&tb->common, NULL, offsetof(RootDbTerm, dt), obj)
         : db_store_term(&tb->common, NULL, offsetof(RootDbTerm, dt), obj));
    /* ret->next = NULL; */
    /* ret->hvalue = INVALID_HASH; */
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

static ERTS_INLINE RootDbTerm *
replace_root_dbterm(DbTableNestedHash *tb, RootDbTerm *old, Eterm obj)
{
    RootDbTerm *rp2;
    ASSERT(old != NULL);
    rp2 = (tb->common.compress)
        ? db_store_term_comp(&tb->common, &(old->dt.dbterm),
                             offsetof(RootDbTerm, dt), obj)
        : db_store_term(&tb->common, &(old->dt.dbterm),
                        offsetof(RootDbTerm, dt), obj);
    return rp2;
}

/*
 * Calculate slot index from hash value.
 */
static ERTS_INLINE Uint
nested_hash_to_ix(RootDbTerm *rp2, HashValue hval)
{
    Uint mask = rp2->dt.tail.szm;
    Uint ix = hval & mask;
    if (ix >= rp2->dt.tail.nactive) {
        ix &= mask >> 1;
        ASSERT(ix < rp2->dt.tail.nactive);
    }
    return ix;
}

static ERTS_INLINE void
put_nested_dbterm(DbTableNestedHash *tb, RootDbTerm *rp2,
                  TrunkDbTerm *tp, Eterm obj)
{
    int nix;
    HashValue ohval;
    TrunkDbTerm **ntpp;
    ohval = MAKE_HASH(obj);
    nix = nested_hash_to_ix(rp2, ohval);
    ntpp = &NESTED_BUCKET(rp2, nix);
    tp->onext = *ntpp;
    tp->ohvalue = ohval;
    *ntpp = tp;
}

/*
 * Extend table with one new segment
 */
static int
alloc_nested_seg(DbTableNestedHash *tb, RootDbTerm *rp2)
{
    int seg_ix = rp2->dt.tail.nslots >> SEGSZ_EXP;
    struct segment **segtab;
    struct ext_segment *seg;
    segtab = TRUNK_PTR(rp2->dt.tail.trunk)->sp.segtab;
    if ((seg_ix + 1) == rp2->dt.tail.nsegs) {
        /* New segtab needed (extended segment) */
        seg = alloc_ext_seg(tb, seg_ix, segtab, rp2->dt.tail.nsegs);
        if (seg == NULL)
            return 0;
        segtab[seg_ix] = &seg->s;
        /* We don't use the new segtab until next call (see "shrink race") */
    } else {
        /* Just a new plain segment */
        if (seg_ix == rp2->dt.tail.nsegs) {
            /* Time to start use segtab from last call */
            seg = (struct ext_segment *) segtab[seg_ix-1];
            MY_ASSERT((seg != NULL) && seg->s.is_ext_segment);
            TRUNK_PTR(rp2->dt.tail.trunk)->sp.segtab = segtab = seg->segtab;
            rp2->dt.tail.nsegs = seg->nsegs;
        }
        ASSERT(seg_ix < rp2->dt.tail.nsegs);
        ASSERT(segtab[seg_ix] == NULL);
        segtab[seg_ix] = (struct segment *)
            erts_db_alloc_fnf(ERTS_ALC_T_DB_SEG, (DbTable *)tb,
                              sizeof(struct segment));
        if (segtab[seg_ix] == NULL)
            return 0;
        sys_memset(segtab[seg_ix], 0, sizeof(struct segment));
    }
    rp2->dt.tail.nslots += SEGSZ;
    return 1;
}

/*
 * Grow table with one new bucket.
 * Allocate new segment if needed.
 */
static void
nested_grow(DbTableNestedHash *tb, RootDbTerm *rp2)
{
    int ix, from_ix, szm, nactive;
    TrunkDbTerm *tp;
    TrunkDbTerm **pnext, **to_pnext;
    nactive = rp2->dt.tail.nactive;
    /* Ensure that the slot nactive exists */
    if (nactive == rp2->dt.tail.nslots) {
        /* Time to get a new segment */
        ASSERT(!(nactive & SEGSZ_MASK));
        if (!alloc_nested_seg(tb, rp2))
            return;
    }
    ASSERT(nactive < rp2->dt.tail.nslots);
    szm = rp2->dt.tail.szm;
    if (nactive <= szm) {
        from_ix = nactive & (szm >> 1);
    } else {
        ASSERT(nactive == szm+1);
        from_ix = 0;
        szm = (szm<<1) | 1;
    }
    ++rp2->dt.tail.nactive;
    if (from_ix == 0)
        rp2->dt.tail.szm = szm;
    /*
     * Finally, let's split the bucket. We try to do it in a smart way
     * to keep link order and avoid unnecessary updates of next-pointers.
     */
    pnext = &NESTED_BUCKET(rp2, from_ix);
    tp = *pnext;
    to_pnext = &NESTED_BUCKET(rp2, nactive);
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
promote_root_to_stage_3(DbTableNestedHash *tb,
                        RootDbTerm *rp2, TrunkDbTerm *tp)
{
    RootDbTerm *ret;
    ASSERT(!HAS_NLHT(rp2));
    ret = erts_db_alloc(ERTS_ALC_T_DB_TERM, (DbTable *)&tb->common,
                        offsetof(RootDbTerm, dt) + sizeof(TailDbTerm));
    ret->next = rp2->next;
    ret->hvalue = rp2->hvalue;
    ret->dt.tail.trunk = MAKE_TRUNK(tp, 1);
    ret->dt.tail.nkitems = rp2->dt.tail.nkitems;
    ret->dt.tail.szm = SEGSZ_MASK;
    ret->dt.tail.nslots = SEGSZ;
    ret->dt.tail.nsegs = NSEG_1;
    ret->dt.tail.nactive = SEGSZ;
    tp->sp.segtab = alloc_ext_seg(tb, 0, NULL, 0)->segtab;
    erts_db_free(ERTS_ALC_T_DB_TERM, (DbTable *)&tb->common,
                 rp2, offsetof(RootDbTerm, dt) + offsetof(TailDbTerm, szm));
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

static RootDbTerm *
create_stage_3_root(DbTableNestedHash *tb, RootDbTerm *rp2)
{
    int nitems = 0;
    Eterm object;
    Eterm *base;
    DbTerm *dbterm = NULL;
    TrunkDbTerm *tp1, *tp2, *tp3;
    TrunkDbTerm **tpp;
    ASSERT(!HAS_NLHT(rp2));
    tp1 = NULL;
    tpp = &rp2->dt.tail.trunk;
    tp2 = *tpp; /* don't need the TRUNK_PTR macro */
    do {
        if (tb->common.compress) {
            dbterm = db_alloc_tmp_uncompressed(&tb->common, &tp2->dbterm);
            object = make_tuple_rel(dbterm->tpl, NULL);
        } else {
            base = HALFWORD_HEAP ? tp2->dbterm.tpl : NULL;
            object = make_tuple_rel(tp2->dbterm.tpl, base);
        }
        *tpp = new_trunk_dbterm(tb, object, 1);
        if (tp1 == NULL) {
            tp1 = *tpp;
            rp2 = promote_root_to_stage_3(tb, rp2, tp1);
        } else {
            (*tpp)->sp.previous = tp1;
            tp1 = *tpp;
        }
        put_nested_dbterm(tb, rp2, tp1, object);
        if (tb->common.compress)
            db_free_tmp_uncompressed(dbterm);
        tp3 = tp2->next;
        free_trunk_dbterm(tb, tp2, 0);
        if (++nitems > rp2->dt.tail.nactive * (CHAIN_LEN + 1))
            nested_grow(tb, rp2);
        tpp = &tp1->next;
    } while ((tp2 = tp3) != NULL);
    ASSERT(rp2->dt.tail.nkitems == nitems);
    *tpp = NULL;
    return rp2;
}

/*
 * Shrink table by freeing the top segment.
 */
static void
free_nested_seg(DbTableNestedHash *tb, RootDbTerm *rp2)
{
    int bytes, seg_ix;
    struct segment **segtab;
    struct ext_segment *top;
    segtab = TRUNK_PTR(rp2->dt.tail.trunk)->sp.segtab;
    seg_ix = (rp2->dt.tail.nslots >> SEGSZ_EXP) - 1;
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
    if ((seg_ix == (rp2->dt.tail.nsegs - 1)) || (seg_ix == 0)) {
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
                TRUNK_PTR(rp2->dt.tail.trunk)->sp.segtab =
                    newtop->prev_segtab;
                rp2->dt.tail.nsegs = seg_ix;
                ASSERT(rp2->dt.tail.nsegs ==
                       EXTSEG(newtop->prev_segtab)->nsegs);
            } else {
                ASSERT((NSEG_1 > 2) && (seg_ix == 1));
            }
        }
        bytes = sizeof(struct segment);
    }
    erts_db_free(ERTS_ALC_T_DB_SEG, (DbTable *)tb, (void *)top, bytes);
#ifdef DEBUG
    if (seg_ix > 0) {
        if (seg_ix < rp2->dt.tail.nsegs)
            TRUNK_PTR(rp2->dt.tail.trunk)->sp.segtab[seg_ix] = NULL;
    } else {
        TRUNK_PTR(rp2->dt.tail.trunk)->sp.segtab = NULL;
    }
#endif
    rp2->dt.tail.nslots -= SEGSZ;
    ASSERT(rp2->dt.tail.nslots >= 0);
}

static ERTS_INLINE void
free_root_dbterm(DbTableNestedHash *tb, RootDbTerm *rp1)
{
    RootDbTerm *rp2 = ROOT_PTR(rp1);
    if (!HAS_TAIL(rp1)) {
        db_free_term((DbTable *)tb, rp2, offsetof(RootDbTerm, dt));
    } else if (HAS_NLHT(rp2)) {
        free_nested_seg(tb, rp2);
        ASSERT(rp2->dt.tail.nslots == 0);
        erts_db_free(ERTS_ALC_T_DB_TERM, (DbTable *)tb, rp2,
                     offsetof(RootDbTerm, dt) + sizeof(TailDbTerm));
    } else {
        erts_db_free(ERTS_ALC_T_DB_TERM, (DbTable *)tb, rp2,
                     offsetof(RootDbTerm, dt) + offsetof(TailDbTerm, szm));
    }
}

/*
 * rp3 can be NULL
 */
static RootDbTerm *
create_stage_2_root(DbTableNestedHash *tb, RootDbTerm *rp2, RootDbTerm *rp3)
{
    Eterm object;
    Eterm *base;
    DbTerm *dbterm;
    RootDbTerm *nrp, *rp1;
    TrunkDbTerm *tp;
    TrunkDbTerm **tpp;
    nrp = erts_db_alloc(ERTS_ALC_T_DB_TERM, (DbTable *)&tb->common,
                        offsetof(RootDbTerm, dt) + offsetof(TailDbTerm, szm));
    nrp->next = rp3;
    nrp->hvalue = rp2->hvalue;
    tpp = &nrp->dt.tail.trunk; /* don't need the TRUNK_PTR macro */
    nrp->dt.tail.nkitems = 0;
    do {
        ASSERT(!HAS_TAIL(rp2));
        ASSERT(nrp->hvalue == rp2->hvalue);
        if (tb->common.compress) {
            dbterm = db_alloc_tmp_uncompressed(&tb->common, &rp2->dt.dbterm);
            object = make_tuple_rel(dbterm->tpl, NULL);
            tp = new_trunk_dbterm(tb, object, 0);
            db_free_tmp_uncompressed(dbterm);
        } else {
            base = HALFWORD_HEAP ? rp2->dt.dbterm.tpl : NULL;
            object = make_tuple_rel(rp2->dt.dbterm.tpl, base);
            tp = new_trunk_dbterm(tb, object, 0);
        }
        *tpp = MAKE_TRUNK(tp, 0);
        tpp = &tp->next;
        rp1 = rp2->next;
        free_root_dbterm(tb, rp2);
        ++nrp->dt.tail.nkitems;
        rp2 = rp1; /* don't need the ROOT_PTR macro */
    } while (rp2 != rp3);
    *tpp = NULL;
    return nrp;
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
free_nested_records(DbTableNestedHash *tb, RootDbTerm *rp2, int *max_free)
{
    int i, seg_ix;
    TrunkDbTerm *tp, *ntp;
    TrunkDbTerm **ntpp;
    struct ext_segment *top;
    tp = TRUNK_PTR(rp2->dt.tail.trunk);
    seg_ix = (rp2->dt.tail.nslots >> SEGSZ_EXP) - 1;
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
            --rp2->dt.tail.nkitems;
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
 * Remove the chain rooted in '***rppp' from the table.
 * '*rppp' is updated appropriately to point to the next RootDbTerm.
 * NOTE: It is assumed that rppp, *rppp and **rppp are not NULL
 *
 * max_free:
 *   == NULL -> the RootDbTerm is pseudo-deleted -- the nested
 *              LHT, if any, is expected to be empty already
 *   != NULL -> free at most *max_free Root/TrunkDbTerms
 *
 * Return:
 *   1 -> done
 *   0 -> *max_free zeroed, need another round
 */
static int
remove_root(DbTableNestedHash *tb, RootDbTerm ***rppp,
            int *max_free, int pseudo_delete)
{
    int hl;
    RootDbTerm **rpp;
    RootDbTerm *rp1, *rp2;
    TrunkDbTerm *tp;
    TrunkDbTerm **tpp;
    rpp = *rppp;
    rp1 = *rpp;
    rp2 = ROOT_PTR(rp1);
    hl = HAS_NLHT(rp2);
    if (max_free != NULL) {
        if (HAS_TAIL(rp1)) {
            tp = TRUNK_PTR(rp2->dt.tail.trunk);
            if (hl) {
                while (rp2->dt.tail.nslots > SEGSZ) {
                    if (!free_nested_records(tb, rp2, max_free))
                        return 0;
                    free_nested_seg(tb, rp2);
                }
                ASSERT(rp2->dt.tail.nslots == SEGSZ);
                if (!free_nested_records(tb, rp2, max_free))
                    return 0;
            } else {
                tpp = &tp->next;
                /* don't need the TRUNK_PTR macro */
                while ((tp = *tpp) != NULL) {
                    if (*max_free <= 0)
                        return 0;
                    --*max_free;
                    --rp2->dt.tail.nkitems;
                    *tpp = tp->next;
                    free_trunk_dbterm(tb, tp, 0);
                }
            }
        }
        if (*max_free <= 0)
            return 0;
        --*max_free;
    }
    ASSERT((rp2->hvalue == INVALID_HASH) || (max_free != NULL));
    if (pseudo_delete) {
        ASSERT((!HAS_TAIL(rp1)) || (rp2->dt.tail.nkitems == 1));
        rp2->hvalue = INVALID_HASH;
        *rppp = &rp2->next;
        return 1;
    }
    *rpp = rp2->next;
    if (HAS_TAIL(rp1)) {
        tp = TRUNK_PTR(rp2->dt.tail.trunk);
        free_root_dbterm(tb, rp1);
        free_trunk_dbterm(tb, tp, hl);
    } else {
        free_root_dbterm(tb, rp1);
    }
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
    RootDbTerm *rp1, *rp2;
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
    while ((rp1 = *from_rpp) != NULL) {
        rp2 = ROOT_PTR(rp1);
        if (rp2->hvalue == INVALID_HASH) {
            /* rare but possible with fine locking */
            remove_root(tb, &from_rpp, NULL, 0);
        } else {
            ix = rp2->hvalue & szm;
            if (ix != from_ix) {
                ASSERT(ix == (from_ix ^ ((szm >> 1) + 1)));
                *to_rpp = rp1;
                /* Swap "from" and "to": */
                from_ix = ix;
                to_rpp = from_rpp;
            }
            from_rpp = &rp2->next;
        }
    }
    *to_rpp = NULL;
    WUNLOCK_HASH(lck);
}

/*
 * Find the next stage 1 RootDbTerm containing a different key.
 * NOTE: *rp2 must be a stage 1 RootDbTerm
 */
static RootDbTerm *
next_key(DbTableNestedHash *tb, RootDbTerm *rp2)
{
    Eterm key;
    HashValue hval;
    RootDbTerm *rp1;
    key = GETKEY(tb, rp2->dt.dbterm.tpl);
    hval = rp2->hvalue;
    if (hval == INVALID_HASH)
        hval = MAKE_HASH(key);
    ASSERT(!is_header(key));
    do {
        rp1 = rp2->next;
        if (rp1 == NULL)
            break;
        rp2 = ROOT_PTR(rp1);
    } while (has_key(tb, rp1, key, hval));
    return rp1;
}

/*
 * If copy_all is true, copy all the terms from rp1 to the end of
 * the root chain.
 * Else copy only the terms with the same key as *rp1.
 * rp1 can be NULL
 */
static Eterm
build_term_list(Process *p, RootDbTerm *rp1,
                int copy_all, DbTableNestedHash *tb)
{
    int sz;
    Eterm copy, list;
    Eterm *hp, *hend;
    RootDbTerm *rp2, *rp3, *rp5;
    TrunkDbTerm *tp;
    if (rp1 == NULL)
        return NIL;
    sz = 0;
    list = NIL;
    rp3 = copy_all ? NULL
        : (HAS_TAIL(rp1) ? ROOT_PTR(rp1)->next
           : next_key(tb, rp1));
    rp5 = rp1;
    do {
        rp2 = ROOT_PTR(rp5);
        if (rp2->hvalue != INVALID_HASH) {
            if (HAS_TAIL(rp5)) {
                tp = TRUNK_PTR(rp2->dt.tail.trunk);
                do {
                    sz += tp->dbterm.size + 2;
                    tp = tp->next;
                } while (tp != NULL);
            } else {
                sz += rp2->dt.dbterm.size + 2;
            }
        }
        rp5 = rp2->next;
    } while (rp5 != rp3);
    hp = HAlloc(p, sz);
    hend = hp + sz;
    do {
        rp2 = ROOT_PTR(rp1);
        if (rp2->hvalue != INVALID_HASH) {
            if (HAS_TAIL(rp1)) {
                tp = TRUNK_PTR(rp2->dt.tail.trunk);
                do {
                    copy = db_copy_object_from_ets(&tb->common, &tp->dbterm,
                                                   &hp, &MSO(p));
                    list = CONS(hp, copy, list);
                    hp += 2;
                    tp = tp->next;
                } while (tp != NULL);
            } else {
                copy = db_copy_object_from_ets(&tb->common, &rp2->dt.dbterm,
                                               &hp, &MSO(p));
                list = CONS(hp, copy, list);
                hp += 2;
            }
        }
        rp1 = rp2->next;
    } while (rp1 != rp3);
    HRelease(p, hend, hp);
    return list;
}

/*
 * Remember a slot containing a pseudo-deleted item (INVALID_HASH)
 */
static ERTS_INLINE int
add_fixed_deletion(DbTableNestedHash *tb, int ix, erts_aint_t fixated_by_me)
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
        if (NFIXED(tb) <= fixated_by_me) {
            /* raced by unfixer */
            erts_db_free(ERTS_ALC_T_DB_FIX_DEL, (DbTable*)tb,
                         fixd, sizeof(FixedDeletion));
            return 0;
        }
        exp_next = was_next;
        fixd->next = (NestedFixedDeletion *)exp_next;
        was_next =
            erts_smp_atomic_cmpxchg_mb(&tb->fixdel,
                                       (erts_aint_t)fixd, exp_next);
    } while (was_next != exp_next);
    return 1;
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
        if (!remove_root(tb, &rpp, max_free, pseudo_delete))
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
            if (*rpp != NULL) {
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
    RootDbTerm **rpp;
    RootDbTerm *rp1, *rp2;
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
    while ((rp1 = *rpp) != NULL) {
        rp2 = ROOT_PTR(rp1);
        if (rp2->hvalue == INVALID_HASH) {
            remove_root(tb, &rpp, NULL, 0);
        } else {
            rpp = &rp2->next;
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
nested_shrink(DbTableNestedHash *tb, RootDbTerm *rp2)
{
    int nactive, src_ix, dst_ix, low_szm;
    TrunkDbTerm **src_bp, **dst_bp;
    nactive = rp2->dt.tail.nactive;
    src_ix = nactive - 1;
    low_szm = rp2->dt.tail.szm >> 1;
    dst_ix = src_ix & low_szm;
    ASSERT(dst_ix < src_ix);
    ASSERT(nactive > SEGSZ);
    src_bp = &NESTED_BUCKET(rp2, src_ix);
    dst_bp = &NESTED_BUCKET(rp2, dst_ix);
    while (*dst_bp != NULL)
        dst_bp = &(*dst_bp)->onext;
    *dst_bp = *src_bp;
    *src_bp = NULL;
    rp2->dt.tail.nactive = src_ix;
    if (dst_ix == 0)
        rp2->dt.tail.szm = low_szm;
    if ((rp2->dt.tail.nslots - src_ix) >= SEGSZ)
        free_nested_seg(tb, rp2);
}

static ERTS_INLINE void
try_nested_shrink(DbTableNestedHash *tb, RootDbTerm *rp2)
{
    int nactive = rp2->dt.tail.nactive;
    if ((nactive > SEGSZ) && (rp2->dt.tail.nkitems < (nactive * CHAIN_LEN)))
        nested_shrink(tb, rp2);
}

/*
 * Remove term '*tp' from the chain.
 * '*rpp' is updated to point to the next RootDbTerm, if '*tp' was the last
 * TrunkDbTerm.
 * '*ntpp' will be set to the next TrunkDbTerm of the nested bucket (may be
 * NULL).
 * NOTE: It is assumed that rppp, *rppp, **rppp, tpp, *tpp, ntpp and *ntpp
 *       are not NULL and that **rppp points to a stage 3 RootDbTerm
 * Return: 0 -> more terms left with the same key of the removed one's
 *         1 -> the removed term was the last with its key
 */
static int
remove_object_and_nested(DbTableNestedHash *tb, RootDbTerm **rpp,
                         TrunkDbTerm *tp, TrunkDbTerm **ntpp,
                         int pseudo_delete)
{
    RootDbTerm *rp1, *rp2;
    TrunkDbTerm *ntp;
    rp1 = *rpp;
    rp2 = ROOT_PTR(rp1);
    ntp = *ntpp;
    ASSERT(HAS_NLHT(rp2));
    /*
     * Pseudo-deleted terms cannot be found
     * looking them up in the nested table.
     */
    ASSERT(rp2->hvalue != INVALID_HASH);
    *ntpp = ntp->onext;
    if (TRUNK_PTR(rp2->dt.tail.trunk) == tp) {
        if (tp->next == NULL) {
            ASSERT(rp2->dt.tail.nkitems == 1);
            if (pseudo_delete) {
                rp2->hvalue = INVALID_HASH;
            } else {
                *rpp = rp2->next;
                free_root_dbterm(tb, rp1);
                free_trunk_dbterm(tb, tp, 1);
            }
            return 1;
        }
        rp2->dt.tail.trunk = MAKE_TRUNK(tp->next, 1);
        tp->next->sp.segtab = tp->sp.segtab;
    } else {
        tp->sp.previous->next = tp->next;
        if (tp->next != NULL)
            tp->next->sp.previous = tp->sp.previous;
    }
    --rp2->dt.tail.nkitems;
    free_trunk_dbterm(tb, tp, 1);
    try_nested_shrink(tb, rp2);
    return 0;
}

/*
 * Remove term '**tpp' from the chain.
 * '*rpp' is updated to point to the next RootDbTerm, if '**tpp' was the last
 * TrunkDbTerm.
 * '*tpp' will be set to the next TrunkDbTerm of the RootDbTerm (but only if
 * there is one, i.e., if 0 is returned).
 * NOTE: It is assumed that rpp, *rpp, tpp and *tpp are not NULL
 *       and that *rpp points to a stage 2 RootDbTerm
 * Return: 0 -> more terms left with the same key of the removed one's
 *         1 -> the removed term was the last with its key
 */
static int
remove_object_no_nested(DbTableNestedHash *tb, RootDbTerm **rpp,
                        TrunkDbTerm **tpp, int pseudo_delete)
{
    RootDbTerm *rp1, *rp2;
    TrunkDbTerm *tp;
    rp1 = *rpp;
    rp2 = ROOT_PTR(rp1);
    tp = *tpp;
    ASSERT(!HAS_NLHT(rp2));
    if (&rp2->dt.tail.trunk == tpp) {
        if (tp->next == NULL) {
            ASSERT(rp2->dt.tail.nkitems == 1);
            if (pseudo_delete) {
                rp2->hvalue = INVALID_HASH;
            } else {
                *rpp = rp2->next;
                free_root_dbterm(tb, rp1);
                free_trunk_dbterm(tb, tp, 0);
            }
            return 1;
        }
        rp2->dt.tail.trunk = tp->next;
    } else {
        ASSERT(rp2->dt.tail.trunk != tp);
        *tpp = tp->next;
    }
    --rp2->dt.tail.nkitems;
    ASSERT(rp2->hvalue != INVALID_HASH);
    free_trunk_dbterm(tb, tp, 0);
    return 0;
}

/* Search a list of tuples for a matching key */
static int
bucket_has_key(DbTableNestedHash *tb, Eterm key,
               HashValue hval, RootDbTerm *rp1)
{
    RootDbTerm *rp2;
    while (rp1 != NULL) {
        rp2 = ROOT_PTR(rp1);
        if (has_key(tb, rp1, key, hval)) {
            if (rp2->hvalue != INVALID_HASH)
                return 1;
            if (HAS_TAIL(rp1))
                return 0;
        }
        rp1 = rp2->next;
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
remove_nested_dbterm(DbTableNestedHash *tb, RootDbTerm *rp2,
                     TrunkDbTerm *tp, Eterm object)
{
    int nix;
    HashValue ohval;
    TrunkDbTerm **ntpp;
    ohval = MAKE_HASH(object);
    nix = nested_hash_to_ix(rp2, ohval);
    ntpp = &NESTED_BUCKET(rp2, nix);
    for (;;) {
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
 * Remove term '***tppp' from the chain.
 * '*rppp' and '*tppp' are updated appropriately to point to the next
 * Root and TrunkDbTerm of the bucket.
 * NOTE: It is assumed that rppp, *rppp, **rppp, tppp, *tppp
 *       and **tppp are not NULL and that **rppp points to a stage 2 or
 *       3 RootDbTerm
 * Return: 0 -> more terms left with the same key of the removed one's
 *         1 -> the removed term was the last with its key
 */
static int
remove_object(DbTableNestedHash *tb, RootDbTerm ***rppp,
              TrunkDbTerm ***tppp, Eterm object, int pseudo_delete)
{
    int hl;
    RootDbTerm **rpp = *rppp;
    RootDbTerm *rp1 = *rpp, *rp2;
    TrunkDbTerm **tpp = *tppp;
    TrunkDbTerm *tp = TRUNK_PTR(*tpp);
    ASSERT(HAS_TAIL(rp1));
    rp2 = ROOT_PTR(rp1);
    hl = HAS_NLHT(rp2);
    if (&rp2->dt.tail.trunk == tpp) {
        if (tp->next == NULL) {
            ASSERT(rp2->dt.tail.nkitems == 1);
            if ((rp2->hvalue != INVALID_HASH) && hl)
                remove_nested_dbterm(tb, rp2, tp, object);
            if (pseudo_delete) {
                rp2->hvalue = INVALID_HASH;
                *rppp = rpp = &rp2->next;
            } else {
                *rpp = rp2->next;
                free_root_dbterm(tb, rp1);
                free_trunk_dbterm(tb, tp, hl);
            }
            *tppp = &ROOT_PTR(*rpp)->dt.tail.trunk;
            return 1;
        }
        rp2->dt.tail.trunk = MAKE_TRUNK(tp->next, hl);
        if (hl)
            tp->next->sp.segtab = tp->sp.segtab;
    } else {
        ASSERT(TRUNK_PTR(rp2->dt.tail.trunk) != tp);
        if ((*tpp = tp->next) == NULL) {
            *rppp = rpp = &rp2->next;
            *tppp = &ROOT_PTR(*rpp)->dt.tail.trunk;
        } else if (hl) {
            tp->next->sp.previous = tp->sp.previous;
        }
    }
    --rp2->dt.tail.nkitems;
    ASSERT(rp2->hvalue != INVALID_HASH);
    if (hl) {
        remove_nested_dbterm(tb, rp2, tp, object);
        try_nested_shrink(tb, rp2);
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
        add_fixed_deletion(tb, i, 0);
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
	if (erts_ets_rwmtx_spin_count >= 0)
	    rwmtx_opt.main_spincount = erts_ets_rwmtx_spin_count;
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
    RootDbTerm *rp1, *rp2;
    TrunkDbTerm *tp;
    DbTableNestedHash *tb = &tbl->nested;
    erts_smp_rwmtx_t *lck = RLOCK_HASH(tb, ix);
    do {
        rp1 = BUCKET(tb, ix);
        while (rp1 != NULL) {
            rp2 = ROOT_PTR(rp1);
            if (rp2->hvalue != INVALID_HASH) {
                if (HAS_TAIL(rp1)) {
                    tp = TRUNK_PTR(rp2->dt.tail.trunk);
                    *ret = db_copy_key(p, tbl, &tp->dbterm);
                } else {
                    *ret = db_copy_key(p, tbl, &rp2->dt.dbterm);
                }
                RUNLOCK_HASH(lck);
                return DB_ERROR_NONE;
            }
            rp1 = rp2->next;
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
    RootDbTerm *rp1, *rp2;
    TrunkDbTerm *tp;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, hval);
    rp1 = BUCKET(tb, ix);
    while (rp1 != NULL) {
        rp2 = ROOT_PTR(rp1);
        if (!has_key(tb, rp1, key, hval)) {
            rp1 = rp2->next;
            continue;
        }
        /* Key found */
        rp1 = HAS_TAIL(rp1) ? rp2->next : next_key(tb, rp2);
        for (;;) {
            while (rp1 != NULL) {
                rp2 = ROOT_PTR(rp1);
                if (rp2->hvalue == INVALID_HASH) {
                    rp1 = rp2->next;
                    continue;
                }
                if (HAS_TAIL(rp1)) {
                    tp = TRUNK_PTR(rp2->dt.tail.trunk);
                    *ret = db_copy_key(p, tbl, &tp->dbterm);
                } else {
                    *ret = db_copy_key(p, tbl, &rp2->dt.dbterm);
                }
                RUNLOCK_HASH(lck);
                return DB_ERROR_NONE;
            }
            ix = next_slot(tb, ix, &lck);
            if (!ix) {
                *ret = am_EOT;
                return DB_ERROR_NONE;
            }
            rp1 = BUCKET(tb, ix);
        }
    }
    RUNLOCK_HASH(lck);
    return DB_ERROR_BADKEY;
}

int
db_put_nhash(DbTable *tbl, Eterm obj, int key_clash_fail)
{
    int hl, ix, nix, nactive, nkeys, nobj;
    Eterm key;
    HashValue hval, ohval;
    RootDbTerm **rpp;
    RootDbTerm *rp1, *rp2, *rp3, *rp4;
    TrunkDbTerm *tp1, *tp2;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    key = GETKEY(tb, tuple_val(obj));
    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, hval);
    rpp = &BUCKET(tb, ix);
    while ((rp1 = *rpp) != NULL) {
        rp2 = ROOT_PTR(rp1);
        if (has_key(tb, rp1, key, hval))
            break;
        rpp = &rp2->next;
    }
    if (rp1 == NULL) {
        rp1 = new_root_dbterm(tb, obj);
        rp1->next = NULL;
        rp1->hvalue = hval;
        *rpp = MAKE_ROOT(rp1, 0);
        nkeys = erts_smp_atomic_inc_read_nob(&tb->nkeys);
        goto done;
    }

    /*
     * Key found
     */
    ASSERT(tb->common.status & (DB_BAG | DB_DUPLICATE_BAG));
    nobj = 0;
    if (key_clash_fail) {
        rp3 = rp1;
        do {
            ++nobj;
            rp4 = ROOT_PTR(rp3);
            if (rp4->hvalue != INVALID_HASH) {
                WUNLOCK_HASH(lck);
                return DB_ERROR_BADKEY;
            }
            rp3 = rp4->next;
        } while ((rp3 != NULL) && has_key(tb, rp3, key, hval));
        ASSERT(nobj == 1);
    } else if (tb->common.status & DB_BAG) {
        if (!HAS_TAIL(rp1)) {
            rp3 = rp1;
            do {
                ++nobj;
                rp4 = ROOT_PTR(rp3);
                if (db_eq(&tb->common, obj, &rp4->dt.dbterm)) {
                    /*
                     * The pseudo-deleted RootDbTerm is always the first
                     * and only of the list: no need to move it to the
                     * front.
                     */
                    if (rp4->hvalue == INVALID_HASH) {
                        rp4->hvalue = hval;
                        /*
                         * If the next RootDbTerm has the same key then
                         * it isn't pseudo-deleted for sure and there's
                         * no need to increment 'nkeys'.
                         */
                        rp3 = rp4->next;
                        nkeys = ((rp3 != NULL) &&
                                 has_key(tb, rp3, key, hval))
                            ? erts_smp_atomic_read_nob(&tb->nkeys)
                            : erts_smp_atomic_inc_read_nob(&tb->nkeys);
                        goto done;
                    }
                    WUNLOCK_HASH(lck);
                    return DB_ERROR_NONE;
                }
                rp3 = rp4->next;
            } while ((rp3 != NULL) && has_key(tb, rp3, key, hval));
        } else if (HAS_NLHT(rp2)) {
            ohval = MAKE_HASH(obj);
            nix = nested_hash_to_ix(rp2, ohval);
            tp1 = NESTED_BUCKET(rp2, nix);
            while (tp1 != NULL) {
                if (db_eq(&tb->common, obj, &tp1->dbterm)) {
                    /*
                     * If the RootDbTerm is pseudo-deleted then its
                     * nested LHT is empty.
                     */
                    ASSERT(rp2->hvalue != INVALID_HASH);
                    WUNLOCK_HASH(lck);
                    return DB_ERROR_NONE;
                }
                tp1 = tp1->onext;
            }
        } else {
            tp1 = rp2->dt.tail.trunk;
            ASSERT(tp1 != NULL);
            do {
                if (db_eq(&tb->common, obj, &tp1->dbterm)) {
                    if (rp2->hvalue == INVALID_HASH) {
                        rp2->hvalue = hval;
                        nkeys = erts_smp_atomic_inc_read_nob(&tb->nkeys);
                        goto done;
                    }
                    WUNLOCK_HASH(lck);
                    return DB_ERROR_NONE;
                }
                tp1 = tp1->next;
            } while (tp1 != NULL);
        }
    } else if (!HAS_TAIL(rp1)) { /* DB_DUPLICATE_BAG */
        rp3 = rp1;
        do {
            ++nobj;
            rp3 = ROOT_PTR(rp3)->next;
        } while ((rp3 != NULL) && has_key(tb, rp3, key, hval));
    }
    if (rp2->hvalue == INVALID_HASH) {
        /*
         * Recycle the Root and TrunkDbTerms.
         */
        rp2->hvalue = hval;
        if (HAS_TAIL(rp1)) {
            tp1 = TRUNK_PTR(rp2->dt.tail.trunk);
            ASSERT(tp1 != NULL);
            hl = HAS_NLHT(rp2);
            tp1 = replace_trunk_dbterm(tb, tp1, obj, hl);
            rp2->dt.tail.trunk = MAKE_TRUNK(tp1, hl);
            if (hl)
                put_nested_dbterm(tb, rp2, tp1, obj);
            ASSERT(rp2->dt.tail.nkitems == 1);
            nkeys = erts_smp_atomic_inc_read_nob(&tb->nkeys);
        } else {
            rp2 = replace_root_dbterm(tb, rp2, obj);
            ASSERT(nobj > 0);
            if (nobj >= KEY_CHAIN_THRESHOLD) {
                rp1 = create_stage_2_root(tb, rp2, rp3);
                *rpp = MAKE_ROOT(rp1, 1);
            } else {
                *rpp = MAKE_ROOT(rp2, 0);
            }
            nkeys = (nobj == 1)
                ? erts_smp_atomic_inc_read_nob(&tb->nkeys)
                : erts_smp_atomic_read_nob(&tb->nkeys);
        }
        goto done;
    }
    if (HAS_TAIL(rp1)) {
        tp2 = TRUNK_PTR(rp2->dt.tail.trunk);
        hl = HAS_NLHT(rp2);
        tp1 = new_trunk_dbterm(tb, obj, hl);
        tp1->next = tp2;
        rp2->dt.tail.trunk = MAKE_TRUNK(tp1, hl);
        ++rp2->dt.tail.nkitems;
        if (hl) {
            tp1->sp.segtab = tp2->sp.segtab;
            tp2->sp.previous = tp1;
            put_nested_dbterm(tb, rp2, tp1, obj);
            if (rp2->dt.tail.nkitems >
                (rp2->dt.tail.nactive * (CHAIN_LEN + 1)))
                nested_grow(tb, rp2);
        } else {
            if (rp2->dt.tail.nkitems > NESTED_CHAIN_THRESHOLD) {
                rp1 = create_stage_3_root(tb, rp2);
                *rpp = MAKE_ROOT(rp1, 1);
            }
        }
    } else {
        rp2 = new_root_dbterm(tb, obj);
        rp2->next = rp1;
        rp2->hvalue = hval;
        ASSERT(nobj > 0);
        if (nobj >= KEY_CHAIN_THRESHOLD) {
            rp1 = create_stage_2_root(tb, rp2, rp3);
            *rpp = MAKE_ROOT(rp1, 1);
        } else {
            *rpp = MAKE_ROOT(rp2, 0);
        }
    }
    nkeys = erts_smp_atomic_read_nob(&tb->nkeys);

done:
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
    RootDbTerm *rp1;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    *ret = NIL;
    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, hval);
    rp1 = BUCKET(tb, ix);
    while (rp1 != NULL) {
        if (has_key(tb, rp1, key, hval)) {
            /* Let build_term_list() skip the pseudo-deleted RootDbTerms */
            /* !!! yield !!! */
            *ret = build_term_list(p, rp1, 0, tb);
            break;
        }
        rp1 = ROOT_PTR(rp1)->next;
    }
    RUNLOCK_HASH(lck);
    return DB_ERROR_NONE;
}

static int
db_get_element_nhash(Process *p, DbTable *tbl,
                     Eterm key, int ndex, Eterm *ret)
{
    int ix;
    Eterm copy, elem_list;
    Eterm *hp;
    HashValue hval;
    RootDbTerm *rp1, *rp2, *rp3;
    TrunkDbTerm *tp1, *tp2;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    ASSERT(tb->common.status & (DB_BAG | DB_DUPLICATE_BAG));
    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, hval);
    rp1 = BUCKET(tb, ix);
    while (rp1 != NULL) {
        rp2 = ROOT_PTR(rp1);
        if (has_key(tb, rp1, key, hval)) {
            if (rp2->hvalue == INVALID_HASH) {
                if (HAS_TAIL(rp1))
                    break;
            } else {
                if (HAS_TAIL(rp1)) {
                    /* !!! yield !!! */
                    tp1 = tp2 = TRUNK_PTR(rp2->dt.tail.trunk);
                    do {
                        if (ndex > arityval(tp1->dbterm.tpl[0])) {
                            RUNLOCK_HASH(lck);
                            return DB_ERROR_BADITEM;
                        }
                        tp1 = tp1->next;
                    } while (tp1 != NULL);
                    elem_list = NIL;
                    do {
                        copy = db_copy_element_from_ets(&tb->common, p,
                                                        &tp2->dbterm,
                                                        ndex, &hp, 2);
                        elem_list = CONS(hp, copy, elem_list);
                        hp += 2;
                        tp2 = tp2->next;
                    } while (tp2 != NULL);
                } else {
                    do {
                        /* Only the first RootDbTerm can be pseudo-deleted */
                        ASSERT(rp2->hvalue != INVALID_HASH);
                        if (ndex > arityval(rp2->dt.dbterm.tpl[0])) {
                            RUNLOCK_HASH(lck);
                            return DB_ERROR_BADITEM;
                        }
                        rp3 = rp2->next;
                        if (rp3 == NULL)
                            break;
                        rp2 = ROOT_PTR(rp3);
                    } while (has_key(tb, rp3, key, hval));
                    elem_list = NIL;
                    do {
                        rp2 = ROOT_PTR(rp1);
                        copy = db_copy_element_from_ets(&tb->common, p,
                                                        &rp2->dt.dbterm,
                                                        ndex, &hp, 2);
                        elem_list = CONS(hp, copy, elem_list);
                        hp += 2;
                        rp1 = rp2->next;
                    } while (rp1 != rp3);
                }
                *ret = elem_list;
                RUNLOCK_HASH(lck);
                return DB_ERROR_NONE;
            }
        }
        rp1 = rp2->next;
    }
    RUNLOCK_HASH(lck);
    return DB_ERROR_BADKEY;
}

static int
db_member_nhash(DbTable *tbl, Eterm key, Eterm *ret)
{
    int ix;
    HashValue hval;
    RootDbTerm *rp1, *rp2;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    *ret = am_false;
    hval = MAKE_HASH(key);
    ix = hash_to_ix(tb, hval);
    lck = RLOCK_HASH(tb, hval);
    rp1 = BUCKET(tb, ix);
    while (rp1 != NULL) {
        rp2 = ROOT_PTR(rp1);
        if (has_key(tb, rp1, key, hval)) {
            if (rp2->hvalue != INVALID_HASH) {
                *ret = am_true;
                break;
            }
            if (HAS_TAIL(rp1))
                break;
        }
        rp1 = rp2->next;
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
    int fixed, ix, nitems_diff, max_free, pseudo_delete;
    HashValue hval;
    RootDbTerm **rpp, *rp1, *rp2;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    pseudo_delete = 1;
    nitems_diff = max_free = DELETE_RECORD_LIMIT;
    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, hval);
    rpp = &BUCKET(tb, ix);
    while ((rp1 = *rpp) != NULL) {
        rp2 = ROOT_PTR(rp1);
        if (has_key(tb, rp1, key, hval)) {
            if (rp2->hvalue == INVALID_HASH) {
                if (HAS_TAIL(rp1))
                    break;
                rpp = &rp2->next;
            } else {
                fixed = pseudo_delete && IS_FIXED(tb) &&
                    add_fixed_deletion(tb, ix, 0);
                /* !!! yield !!! */
                while (!remove_root(tb, &rpp, &max_free, fixed)) {
                    nitems_diff += DELETE_RECORD_LIMIT;
                    max_free += DELETE_RECORD_LIMIT;
                }
                if (HAS_TAIL(rp1))
                    break;
            }
            pseudo_delete = 0; /* Pseudo-deleted the first RootDbTerm only */
        } else {
            rpp = &rp2->next;
        }
    }
    WUNLOCK_HASH(lck);
    nitems_diff -= max_free;
    if (nitems_diff) {
        erts_smp_atomic_add_nob(&tb->common.nitems, -nitems_diff);
        erts_smp_atomic_dec_nob(&tb->nkeys);
        try_shrink(tb);
        CHECK_TABLES();
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
    int found, ix, nix, nitems_diff, nkeys_diff, pseudo_delete;
    Eterm key;
    HashValue hval;
    RootDbTerm *rp1, *rp2;
    RootDbTerm **rpp, **rpp2;
    TrunkDbTerm *ntp;
    TrunkDbTerm **ntpp, **tpp;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    found = nitems_diff = nkeys_diff = 0;
    pseudo_delete = 1;
    key = GETKEY(tb, tuple_val(object));
    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, hval);
    rpp = &BUCKET(tb, ix);
    while ((rp1 = *rpp) != NULL) {
        rp2 = ROOT_PTR(rp1);
        if (has_key(tb, rp1, key, hval)) {
            if (rp2->hvalue == INVALID_HASH) {
                if (HAS_TAIL(rp1))
                    break;
            } else {
                if (!HAS_TAIL(rp1)) {
                    ++found;
                    if (db_eq(&tb->common, object, &rp2->dt.dbterm)) {
                        --nitems_diff;
                        /*
                         * Pseudo-delete the first RootDbTerm only, delete
                         * for real all the others.
                         */
                        if (pseudo_delete && (IS_FIXED(tb)) &&
                            add_fixed_deletion(tb, ix, 0)) {
                            rp2->hvalue = INVALID_HASH;
                            rpp = &rp2->next;
                        } else {
                            *rpp = rp2->next;
                            free_root_dbterm(tb, rp1);
                        }
                        if (!(tb->common.status & (DB_DUPLICATE_BAG))) {
                            /* Is there another RootDbTerm with same key? */
                            if ((*rpp != NULL) &&
                                has_key(tb, *rpp, key, hval))
                                /* Yes, and it won't be pseudo-deleted */
                                ++found;
                            break;
                        }
                        pseudo_delete = 0;
                        continue;
                    }
                } else if (HAS_NLHT(rp2)) {
                    hval = MAKE_HASH(object);
                    nix = nested_hash_to_ix(rp2, hval);
                    ntpp = &NESTED_BUCKET(rp2, nix);
                    while ((ntp = *ntpp) != NULL) {
                        if (db_eq(&tb->common, object, &ntp->dbterm)) {
                            --nitems_diff;
                            if (IS_FIXED(tb)) {
                                rpp2 = rpp;
                                if (remove_object_and_nested(tb, rpp, ntp,
                                                             ntpp, 1)) {
                                    --nkeys_diff;
                                    /* pseudo-deletion */
                                    if (!add_fixed_deletion(tb, ix, 0))
                                        remove_root(tb, &rpp2, NULL, 0);
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
                            continue;
                        }
                        ntpp = &ntp->onext;
                    }
                    break;
                } else {
                    /* don't need the TRUNK_PTR macro */
                    tpp = &rp2->dt.tail.trunk;
                    do {
                        if (db_eq(&tb->common, object, &(*tpp)->dbterm)) {
                            --nitems_diff;
                            if (IS_FIXED(tb)) {
                                rpp2 = rpp;
                                if (remove_object_no_nested(tb, rpp, tpp, 1)) {
                                    --nkeys_diff;
                                    /* pseudo-deletion */
                                    if (!add_fixed_deletion(tb, ix, 0))
                                        remove_root(tb, &rpp2, NULL, 0);
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
                            continue;
                        }
                        tpp = &(*tpp)->next;
                    } while (*tpp != NULL);
                    break;
                }
            }
            pseudo_delete = 0;
        }
        rpp = &rp2->next;
    }
    WUNLOCK_HASH(lck);
    /*
     * Stage 1 RootDbTerms only: if all the items found were removed,
     * decrement the number of keys
     */
    if ((found > 0) && (found + nitems_diff == 0))
        --nkeys_diff;
    if (nitems_diff) {
        erts_smp_atomic_add_nob(&tb->common.nitems, nitems_diff);
        if (nkeys_diff) {
            erts_smp_atomic_add_nob(&tb->nkeys, nkeys_diff);
            try_shrink(tb);
        }
        CHECK_TABLES();
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
        *ret = build_term_list(p, BUCKET(tb, slot), 1, tb);
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
    RootDbTerm *rp1, *rp2;
    TrunkDbTerm *tp = NULL;
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
        rp1 = *(mpi.lists[current_list_pos++].bucket);
        ASSERT(rp1 == BUCKET(tb, slot_ix));
    } else {
        /*
         * Run this code if pattern is variable
         * or GETKEY(pattern) is a variable
         */
        slot_ix = 0;
        lck = RLOCK_HASH(tb, slot_ix);
        for (;;) {
            ASSERT(slot_ix < NACTIVE(tb));
            rp1 = BUCKET(tb, slot_ix);
            if (rp1 != NULL)
                break;
            slot_ix = next_slot(tb,slot_ix,&lck);
            if (slot_ix == 0)
                goto end_of_table;
        }
    }
    match_list = NIL;
    rp2 = ROOT_PTR(rp1);
    if (HAS_TAIL(rp1))
        tp = TRUNK_PTR(rp2->dt.tail.trunk);
    for (;;) {
        /* !!! yield !!! */
        if (rp1 != NULL) {
            if (HAS_TAIL(rp1)) {
                if (rp2->hvalue != INVALID_HASH) {
                    match_res = db_match_dbterm(&tb->common, p, mpi.mp, 0,
                                                &tp->dbterm, &hp, 2);
                    if (is_value(match_res)) {
                        match_list = CONS(hp, match_res, match_list);
                        ++got;
                    }
                    --num_left;
                }
                NEXT_DBTERM_2(rp1, rp2, tp);
            } else {
                if (rp2->hvalue != INVALID_HASH) {
                    match_res = db_match_dbterm(&tb->common, p, mpi.mp, 0,
                                                &rp2->dt.dbterm, &hp, 2);
                    if (is_value(match_res)) {
                        match_list = CONS(hp, match_res, match_list);
                        ++got;
                    }
                    --num_left;
                }
                NEXT_DBTERM_1(rp1, rp2, tp);
            }
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
            rp1 = *(mpi.lists[current_list_pos].bucket);
            ASSERT(mpi.lists[current_list_pos].bucket == &BUCKET(tb, slot_ix));
            rp2 = ROOT_PTR(rp1);
            if (HAS_TAIL(rp1))
                tp = TRUNK_PTR(rp2->dt.tail.trunk);
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
            rp1 = BUCKET(tb, slot_ix);
            rp2 = ROOT_PTR(rp1);
            if (HAS_TAIL(rp1))
                tp = TRUNK_PTR(rp2->dt.tail.trunk);
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
    int deleted, errcode, found, num_left, pseudo_delete;
    unsigned int current_list_pos = 0;
    Uint got, last_pseudo_delete, slot_ix;
    Eterm continuation, egot, key, mpb, object;
    Eterm *hp;
    DbTerm *uterm;
    HashValue hval = INVALID_HASH;
    erts_aint_t fixated_by_me;
    struct mp_info mpi;
    erts_smp_rwmtx_t *lck;
    RootDbTerm **rpp, **rpp2;
    RootDbTerm *rp2, *rp4;
    TrunkDbTerm **tpp;
    DbTableNestedHash *tb = &tbl->nested;
    key = NIL;
    num_left = 1000;
    got = 0;
    last_pseudo_delete = (Uint)-1;
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
        slot_ix = 0;
        lck = WLOCK_HASH(tb, slot_ix);
        rpp = &BUCKET(tb, slot_ix);
    }
    rp2 = ROOT_PTR(*rpp);
    if (HAS_TAIL(*rpp))
        tpp = &rp2->dt.tail.trunk;
    deleted = found = 0;
    pseudo_delete = 1;
    rp4 = NULL;
    for (;;) {
        /* !!! yield !!! */
        if (*rpp != NULL) {
            if (HAS_TAIL(*rpp)) {
                if (rp2->hvalue == INVALID_HASH) {
                    NEXT_DBTERM_2_P(rpp, rp2, tpp);
                    continue;
                }
                if (db_match_dbterm_nhash(tb, p, mpi.mp,
                                          &TRUNK_PTR(*tpp)->dbterm,
                                          &object, &uterm)) {
                    /* fixated by others? */
                    if (NFIXED(tb) > fixated_by_me) {
                        rpp2 = rpp;
                        if (remove_object(tb, &rpp, &tpp, object, 1)) {
                            erts_smp_atomic_dec_nob(&tb->nkeys);
                            /* Pseudo deletion */
                            if (slot_ix != last_pseudo_delete) {
                                if (add_fixed_deletion(tb, slot_ix,
                                                       fixated_by_me)) {
                                    last_pseudo_delete = slot_ix;
                                } else {
                                    rpp = rpp2;
                                    remove_root(tb, &rpp, NULL, 0);
                                    if (HAS_TAIL(*rpp))
                                        tpp = &ROOT_PTR(*rpp)->dt.tail.trunk;
                                }
                            }
                        }
                    } else {
                        if (remove_object(tb, &rpp, &tpp, object, 0))
                            erts_smp_atomic_dec_nob(&tb->nkeys);
                    }
                    rp2 = ROOT_PTR(*rpp);
                    erts_smp_atomic_dec_nob(&tb->common.nitems);
                    ++got;
                } else {
                    NEXT_DBTERM_2_P(rpp, rp2, tpp);
                }
                if (uterm != NULL)
                    db_free_tmp_uncompressed(uterm);
                --num_left;
            } else {
                if (rp2->hvalue == INVALID_HASH) {
                    if (pseudo_delete) {
                        key = GETKEY(tb, rp2->dt.dbterm.tpl);
                        hval = MAKE_HASH(key);
                        ASSERT(!is_header(key));
                    }
                    NEXT_DBTERM_1_P(rpp, rp2, tpp);
                } else {
                    if (pseudo_delete) {
                        key = GETKEY(tb, rp2->dt.dbterm.tpl);
                        hval = rp2->hvalue;
                        ASSERT(!is_header(key));
                    }
                    ++found;
                    if (db_match_dbterm_nhash(tb, p, mpi.mp, &rp2->dt.dbterm,
                                              &object, &uterm)) {
                        ++deleted;
                        /*
                         * Pseudo-delete the first RootDbTerm only, delete
                         * for real all the others.
                         */
                        /* fixated by others? */
                        if (pseudo_delete && (NFIXED(tb) > fixated_by_me) &&
                            ((slot_ix == last_pseudo_delete) ||
                             add_fixed_deletion(tb, slot_ix, fixated_by_me))) {
                            last_pseudo_delete = slot_ix;
                            rp2->hvalue = INVALID_HASH;
                            rpp = &rp2->next;
                        } else {
                            *rpp = rp2->next;
                            if (found == 1) {
                                /*
                                 * For 'key' is referencing rp2's dbterm,
                                 * postpone deletion until done with 'key'.
                                 */
                                rp4 = rp2;
                            } else {
                                free_root_dbterm(tb, rp2);
                            }
                        }
                        rp2 = ROOT_PTR(*rpp);
                        if (HAS_TAIL(*rpp))
                            tpp = &rp2->dt.tail.trunk;
                        erts_smp_atomic_dec_nob(&tb->common.nitems);
                        ++got;
                    } else {
                        NEXT_DBTERM_1_P(rpp, rp2, tpp);
                    }
                    if (uterm != NULL)
                        db_free_tmp_uncompressed(uterm);
                    --num_left;
                }
                /* Is there another RootDbTerm with same key? */
                if ((*rpp != NULL) && has_key(tb, *rpp, key, hval)) {
                    ASSERT(!HAS_TAIL(*rpp));
                    pseudo_delete = 0;
                } else {
                    pseudo_delete = 1;
                    /*
                     * For each sequence of (stage 1) RootDbTerms with the
                     * same keys, compare how many of them were found and how
                     * many were deleted: if all the found RootDbTerms were
                     * deleted, decrement the number of keys stored in the ETS.
                     */
                    if (found > 0) {
                        if (rp4 != NULL) {
                            free_root_dbterm(tb, rp4);
                            rp4 = NULL;
                        }
                        if (deleted == found)
                            erts_smp_atomic_dec_nob(&tb->nkeys);
                        deleted = found = 0;
                    } else {
                        ASSERT((found == 0) && (deleted == 0) &&
                               (rp4 == NULL));
                    }
                }
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
            rp2 = ROOT_PTR(*rpp);
            if (HAS_TAIL(*rpp))
                tpp = &rp2->dt.tail.trunk;
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
            rp2 = ROOT_PTR(*rpp);
            if (HAS_TAIL(*rpp))
                tpp = &rp2->dt.tail.trunk;
        }
    }
    BUMP_REDS(p, 1000 - num_left);
    ASSERT((found == 0) && (deleted == 0) &&
           (pseudo_delete == 1) && (rp4 == NULL));
    if (got) {
        try_shrink(tb);
        CHECK_TABLES();
    }
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
    RootDbTerm *rp1, *rp2;
    TrunkDbTerm *tp = NULL;
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
    while ((rp1 = BUCKET(tb, slot_ix)) == NULL) {
        slot_ix = next_slot(tb, slot_ix, &lck);
        if (slot_ix == 0) {
            /* EOT */
            slot_ix = -1;
            goto done;
        }
    }
    rp2 =ROOT_PTR(rp1);
    if (HAS_TAIL(rp1))
        tp = TRUNK_PTR(rp2->dt.tail.trunk);
    for (;;) {
        /* !!! yield !!! */
        if (rp1 != NULL) {
            if (HAS_TAIL(rp1)) {
                if (rp2->hvalue != INVALID_HASH) {
                    match_res = db_match_dbterm(&tb->common, p,
                                                mp, all_objects,
                                                &tp->dbterm, &hp, 2);
                    if (is_value(match_res)) {
                        match_list = CONS(hp, match_res, match_list);
                        ++got;
                    }
                    --num_left;
                }
                NEXT_DBTERM_2(rp1, rp2, tp);
            } else {
                if (rp2->hvalue != INVALID_HASH) {
                    match_res = db_match_dbterm(&tb->common, p,
                                                mp, all_objects,
                                                &rp2->dt.dbterm, &hp, 2);
                    if (is_value(match_res)) {
                        match_list = CONS(hp, match_res, match_list);
                        ++got;
                    }
                    --num_left;
                }
                NEXT_DBTERM_1(rp1, rp2, tp);
            }
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
            rp1 = BUCKET(tb, slot_ix);
            rp2 = ROOT_PTR(rp1);
            if (HAS_TAIL(rp1))
                tp = TRUNK_PTR(rp2->dt.tail.trunk);
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
    int deleted, fixated_by_me, found, num_left, pseudo_delete;
    Uint got, last_pseudo_delete, slot_ix;
    Eterm egot, key, object;
    Eterm *hp, *tptr;
    DbTerm *uterm;
    Binary *mp;
    HashValue hval = INVALID_HASH;
    erts_smp_rwmtx_t *lck;
    RootDbTerm **rpp, **rpp2;
    RootDbTerm *rp2, *rp4;
    TrunkDbTerm **tpp;
    DbTableNestedHash *tb = &tbl->nested;
    key = NIL;
    num_left = 1000;
    last_pseudo_delete = (Uint)-1;
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
        goto done;
    }
    rpp = &BUCKET(tb, slot_ix);
    rp2 = ROOT_PTR(*rpp);
    if (HAS_TAIL(*rpp))
        tpp = &rp2->dt.tail.trunk;
    deleted = found = 0;
    pseudo_delete = 1;
    rp4 = NULL;
    for (;;) {
        /* !!! yield !!! */
        if (*rpp != NULL) {
            if (HAS_TAIL(*rpp)) {
                if (rp2->hvalue == INVALID_HASH) {
                    NEXT_DBTERM_2_P(rpp, rp2, tpp);
                    continue;
                }
                if (db_match_dbterm_nhash(tb, p, mp, &TRUNK_PTR(*tpp)->dbterm,
                                          &object, &uterm)) {
                    /* fixated by others? */
                    if (NFIXED(tb) > fixated_by_me) {
                        rpp2 = rpp;
                        if (remove_object(tb, &rpp, &tpp, object, 1)) {
                            erts_smp_atomic_dec_nob(&tb->nkeys);
                            /* Pseudo deletion */
                            if (slot_ix != last_pseudo_delete) {
                                if (add_fixed_deletion(tb, slot_ix,
                                                       fixated_by_me)) {
                                    last_pseudo_delete = slot_ix;
                                } else {
                                    rpp = rpp2;
                                    remove_root(tb, &rpp, NULL, 0);
                                    if (HAS_TAIL(*rpp))
                                        tpp = &ROOT_PTR(*rpp)->dt.tail.trunk;
                                }
                            }
                        }
                    } else {
                        if (remove_object(tb, &rpp, &tpp, object, 0))
                            erts_smp_atomic_dec_nob(&tb->nkeys);
                    }
                    rp2 = ROOT_PTR(*rpp);
                    erts_smp_atomic_dec_nob(&tb->common.nitems);
                    ++got;
                } else {
                    NEXT_DBTERM_2_P(rpp, rp2, tpp);
                }
                if (uterm != NULL)
                    db_free_tmp_uncompressed(uterm);
                --num_left;
            } else {
                if (rp2->hvalue == INVALID_HASH) {
                    if (pseudo_delete) {
                        key = GETKEY(tb, rp2->dt.dbterm.tpl);
                        hval = MAKE_HASH(key);
                        ASSERT(!is_header(key));
                    }
                    NEXT_DBTERM_1_P(rpp, rp2, tpp);
                } else {
                    if (pseudo_delete) {
                        key = GETKEY(tb, rp2->dt.dbterm.tpl);
                        hval = rp2->hvalue;
                        ASSERT(!is_header(key));
                    }
                    ++found;
                    if (db_match_dbterm_nhash(tb, p, mp, &rp2->dt.dbterm,
                                              &object, &uterm)) {
                        ++deleted;
                        /*
                         * Pseudo-delete the first RootDbTerm only, delete
                         * for real all the others.
                         */
                        /* fixated by others? */
                        if (pseudo_delete && (NFIXED(tb) > fixated_by_me) &&
                            ((slot_ix == last_pseudo_delete) ||
                             add_fixed_deletion(tb, slot_ix, fixated_by_me))) {
                            last_pseudo_delete = slot_ix;
                            rp2->hvalue = INVALID_HASH;
                            rpp = &rp2->next;
                        } else {
                            *rpp = rp2->next;
                            if (found == 1) {
                                /*
                                 * For 'key' is referencing rp2's dbterm,
                                 * postpone deletion until done with 'key'.
                                 */
                                rp4 = rp2;
                            } else {
                                free_root_dbterm(tb, rp2);
                            }
                        }
                        rp2 = ROOT_PTR(*rpp);
                        if (HAS_TAIL(*rpp))
                            tpp = &rp2->dt.tail.trunk;
                        erts_smp_atomic_dec_nob(&tb->common.nitems);
                        ++got;
                    } else {
                        NEXT_DBTERM_1_P(rpp, rp2, tpp);
                    }
                    if (uterm != NULL)
                        db_free_tmp_uncompressed(uterm);
                    --num_left;
                }
                /* Is there another RootDbTerm with same key? */
                if ((*rpp != NULL) && has_key(tb, *rpp, key, hval)) {
                    ASSERT(!HAS_TAIL(*rpp));
                    pseudo_delete = 0;
                } else {
                    pseudo_delete = 1;
                    /*
                     * For each sequence of (stage 1) RootDbTerms with the
                     * same keys, compare how many of them were found and how
                     * many were deleted: if all the found RootDbTerms were
                     * deleted, decrement the number of keys stored in the ETS.
                     */
                    if (found > 0) {
                        if (rp4 != NULL) {
                            free_root_dbterm(tb, rp4);
                            rp4 = NULL;
                        }
                        if (deleted == found)
                            erts_smp_atomic_dec_nob(&tb->nkeys);
                        deleted = found = 0;
                    } else {
                        ASSERT((found == 0) && (deleted == 0) &&
                               (rp4 == NULL));
                    }
                }
            }
        } else {
            slot_ix = next_slot_w(tb, slot_ix, &lck);
            if (slot_ix == 0)
                break;
            if (num_left <= 0) {
                WUNLOCK_HASH(lck);
                goto trap;
            }
            rpp = &BUCKET(tb, slot_ix);
            rp2 = ROOT_PTR(*rpp);
            if (HAS_TAIL(*rpp))
                tpp = &rp2->dt.tail.trunk;
        }
    }

done:
    BUMP_REDS(p, 1000 - num_left);
    ASSERT((found == 0) && (deleted == 0) &&
           (pseudo_delete == 1) && (rp4 == NULL));
    if (got) {
        try_shrink(tb);
        CHECK_TABLES();
    }
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
    RootDbTerm *rp1, *rp2;
    TrunkDbTerm *tp = NULL;
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
    if (mpi.key_given) {
        /* We have at least one */
        slot_ix = mpi.lists[current_list_pos].ix;
        lck = RLOCK_HASH(tb, slot_ix);
        rp1 = *(mpi.lists[current_list_pos++].bucket);
        ASSERT(rp1 == BUCKET(tb, slot_ix));
    } else {
        /*
         * Run this code if pattern is variable
         * or GETKEY(pattern) is a variable
         */
        slot_ix = 0;
        lck = RLOCK_HASH(tb, slot_ix);
        rp1 = BUCKET(tb, slot_ix);
    }
    rp2 = ROOT_PTR(rp1);
    if (HAS_TAIL(rp1))
        tp = TRUNK_PTR(rp2->dt.tail.trunk);
    for (;;) {
        /* !!! yield !!! */
        if (rp1 != NULL) {
            if (HAS_TAIL(rp1)) {
                if (rp2->hvalue != INVALID_HASH) {
                    if (db_match_dbterm(&tb->common, p, mpi.mp, 0,
                                        &tp->dbterm, NULL, 0) == am_true)
                        ++got;
                    --num_left;
                }
                NEXT_DBTERM_2(rp1, rp2, tp);
            } else {
                if (rp2->hvalue != INVALID_HASH) {
                    if (db_match_dbterm(&tb->common, p, mpi.mp, 0,
                                        &rp2->dt.dbterm, NULL, 0) == am_true)
                        ++got;
                    --num_left;
                }
                NEXT_DBTERM_1(rp1, rp2, tp);
            }
        } else if (mpi.key_given) {
            /* Key is bound */
            RUNLOCK_HASH(lck);
            if (current_list_pos == mpi.num_lists)
                break;
            slot_ix = mpi.lists[current_list_pos].ix;
            lck = RLOCK_HASH(tb, slot_ix);
            rp1 = *(mpi.lists[current_list_pos].bucket);
            ASSERT(mpi.lists[current_list_pos].bucket == &BUCKET(tb, slot_ix));
            rp2 = ROOT_PTR(rp1);
            if (HAS_TAIL(rp1))
                tp = TRUNK_PTR(rp2->dt.tail.trunk);
            ++current_list_pos;
        } else {
            slot_ix = next_slot(tb, slot_ix, &lck);
            if (slot_ix == 0)
                break;
            if (num_left <= 0) {
                RUNLOCK_HASH(lck);
                goto trap;
            }
            rp1 = BUCKET(tb, slot_ix);
            rp2 = ROOT_PTR(rp1);
            if (HAS_TAIL(rp1))
                tp = TRUNK_PTR(rp2->dt.tail.trunk);
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
    RootDbTerm *rp1, *rp2;
    TrunkDbTerm *tp = NULL;
    DbTableNestedHash *tb = &tbl->nested;
    tptr = tuple_val(continuation);
    slot_ix = unsigned_val(tptr[2]);
    mp = ((ProcBin *)binary_val(tptr[3]))->val;
    got = (is_big(tptr[4])) ? big_to_uint32(tptr[4]) : unsigned_val(tptr[4]);
    lck = RLOCK_HASH(tb, slot_ix);
    if (slot_ix >= NACTIVE(tb)) {
        /* Is this possible? */
        RUNLOCK_HASH(lck);
        goto done;
    }
    rp1 = BUCKET(tb, slot_ix);
    rp2 = ROOT_PTR(rp1);
    if (HAS_TAIL(rp1))
        tp = TRUNK_PTR(rp2->dt.tail.trunk);
    for (;;) {
        /* !!! yield !!! */
        if (rp1 != NULL) {
            if (HAS_TAIL(rp1)) {
                if (rp2->hvalue != INVALID_HASH) {
                    if (db_match_dbterm(&tb->common, p, mp, 0,
                                        &tp->dbterm, NULL, 0) == am_true)
                        ++got;
                    --num_left;
                }
                NEXT_DBTERM_2(rp1, rp2, tp);
            } else {
                if (rp2->hvalue != INVALID_HASH) {
                    if (db_match_dbterm(&tb->common, p, mp, 0,
                                        &rp2->dt.dbterm, NULL, 0) == am_true)
                        ++got;
                    --num_left;
                }
                NEXT_DBTERM_1(rp1, rp2, tp);
            }
        } else {
            /* next bucket */
            slot_ix = next_slot(tb, slot_ix, &lck);
            if (slot_ix == 0)
                break;
            if (num_left <= 0) {
                RUNLOCK_HASH(lck);
                goto trap;
            }
            rp1 = BUCKET(tb, slot_ix);
            rp2 = ROOT_PTR(rp1);
            if (HAS_TAIL(rp1))
                tp = TRUNK_PTR(rp2->dt.tail.trunk);
        }
    }

done:
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
db_take_nhash(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    int fixed, ix, max_free, nitems_diff, pseudo_delete;
    HashValue hval;
    RootDbTerm **rpp;
    RootDbTerm *rp1, *rp2;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    pseudo_delete = 1;
    *ret = NIL;
    nitems_diff = max_free = DELETE_RECORD_LIMIT;
    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, hval);
    rpp = &BUCKET(tb, ix);
    while ((rp1 = *rpp) != NULL) {
        rp2 = ROOT_PTR(rp1);
        if (has_key(tb, rp1, key, hval)) {
            if (rp2->hvalue == INVALID_HASH) {
                if (HAS_TAIL(rp1))
                    break;
                rpp = &rp2->next;
            } else {
                /*
                 * Copy the (maybe stage 1) RootDbTerms at the first
                 * occurrence...
                 */
                if (*ret == NIL)
                    *ret = build_term_list(p, rp1, 0, tb);
                /*
                 * ... then delete them one by one on every loop.
                 */
                fixed = pseudo_delete && IS_FIXED(tb) &&
                    add_fixed_deletion(tb, ix, 0);
                /* !!! yield !!! */
                while (!remove_root(tb, &rpp, &max_free, fixed)) {
                    nitems_diff += DELETE_RECORD_LIMIT;
                    max_free += DELETE_RECORD_LIMIT;
                }
                if (HAS_TAIL(rp1))
                    break;
            }
            pseudo_delete = 0; /* Pseudo-deleted the first RootDbTerm only */
        } else {
            rpp = &rp2->next;
        }
    }
    WUNLOCK_HASH(lck);
    nitems_diff -= max_free;
    if (nitems_diff) {
        erts_smp_atomic_add_nob(&tb->common.nitems, -nitems_diff);
        erts_smp_atomic_dec_nob(&tb->nkeys);
        try_shrink(tb);
        CHECK_TABLES();
    }
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
    RootDbTerm *rp1, *rp2;
    TrunkDbTerm *tp = NULL;
    DbNestedHashStats stats;
    DbTableNestedHash *tb = &tbl->nested;
    erts_print(to, to_arg, "Buckets: %d\n", NACTIVE(tb));
#ifdef ERTS_SMP
    i = tbl->common.is_thread_safe;
    /*
     * If crash dumping we set table to thread safe in order to
     * avoid taking any locks
     */
    if (ERTS_IS_CRASH_DUMPING)
        tbl->common.is_thread_safe = 1;
    db_calc_stats_nhash(&tbl->nested, &stats);
    tbl->common.is_thread_safe = i;
#else
    db_calc_stats_nhash(&tbl->nested, &stats);
#endif
    erts_print(to, to_arg, "Chain Length Avg: %f\n", stats.avg_chain_len);
    erts_print(to, to_arg, "Chain Length Max: %d\n", stats.max_chain_len);
    erts_print(to, to_arg, "Chain Length Min: %d\n", stats.min_chain_len);
    erts_print(to, to_arg, "Chain Length Std Dev: %f\n",
               stats.std_dev_chain_len);
    erts_print(to, to_arg, "Chain Length Expected Std Dev: %f\n",
               stats.std_dev_expected);
    if (IS_FIXED(tb)) {
        erts_print(to, to_arg, "Fixed: %d\n", stats.kept_items);
    } else {
        erts_print(to, to_arg, "Fixed: false\n");
    }
    if (!show)
        return;
    for (i = 0; i < NACTIVE(tb); i++) {
        rp1 = BUCKET(tb, i);
        if (rp1 == NULL)
            continue;
        rp2 = ROOT_PTR(rp1);
        if (HAS_TAIL(rp1))
            tp = TRUNK_PTR(rp2->dt.tail.trunk);
        erts_print(to, to_arg, "%d: [", i);
        for (;;) {
            if (rp2->hvalue == INVALID_HASH)
                erts_print(to, to_arg, "*");
            if (HAS_TAIL(rp1)) {
                if (tb->common.compress) {
                    key = GETKEY(tb, tp->dbterm.tpl);
                    erts_print(to, to_arg, "key=%R", key, tp->dbterm.tpl);
                } else {
                    obj = make_tuple_rel(tp->dbterm.tpl, tp->dbterm.tpl);
                    erts_print(to, to_arg, "%R", obj, tp->dbterm.tpl);
                }
                NEXT_DBTERM_2(rp1, rp2, tp);
            } else {
                if (tb->common.compress) {
                    key = GETKEY(tb, rp2->dt.dbterm.tpl);
                    erts_print(to, to_arg, "key=%R", key, rp2->dt.dbterm.tpl);
                } else {
                    obj = make_tuple_rel(rp2->dt.dbterm.tpl, tp->dbterm.tpl);
                    erts_print(to, to_arg, "%R", obj, rp2->dt.dbterm.tpl);
                }
                NEXT_DBTERM_1(rp1, rp2, tp);
            }
            if (rp1 == NULL)
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
    RootDbTerm *rp1, *rp2;
    TrunkDbTerm *tp = NULL;
    DbTableNestedHash *tb = &tbl->nested;
    nactive = NACTIVE(tb);
    for (i = 0; i < nactive; i++) {
        rp1 = BUCKET(tb, i);
        rp2 = ROOT_PTR(rp1);
        if (HAS_TAIL(rp1))
            tp = TRUNK_PTR(rp2->dt.tail.trunk);
        while (rp1 != NULL) {
            tmp_offheap.overhead = 0;
            if (HAS_TAIL(rp1)) {
                tmp_offheap.first = tp->dbterm.first_oh;
                (*func)(&tmp_offheap, arg);
                tp->dbterm.first_oh = tmp_offheap.first;
                NEXT_DBTERM_2(rp1, rp2, tp);
            } else {
                tmp_offheap.first = rp2->dt.dbterm.first_oh;
                (*func)(&tmp_offheap, arg);
                rp2->dt.dbterm.first_oh = tmp_offheap.first;
                NEXT_DBTERM_1(rp1, rp2, tp);
            }
        }
    }
}

#ifdef HARDDEBUG
static void
db_check_table_nhash(DbTable *tbl)
{
    int i, j, na, nk, nk2;
    int nactive, nitems, nkeys;
    Eterm key;
    HashValue hval;
    RootDbTerm *rp1, *rp2;
    TrunkDbTerm *tp, *ntp;
    DbTableNestedHash *tb = &tbl->nested;
    nkeys = nitems = 0;
    nactive = NACTIVE(tb);
    if (nactive > tb->nslots)
        nactive = tb->nslots;
    for (j = 0; j < nactive; ++j) {
        /* Forces the first invocation of has_key() not to compare keys. */
        hval = INVALID_HASH;
        rp1 = BUCKET(tb, j);
        while (rp1 != NULL) {
            rp2 = ROOT_PTR(rp1);
            if (HAS_TAIL(rp1)) {
                tp = TRUNK_PTR(rp2->dt.tail.trunk);
                nk = 0;
                do {
                    if (!is_tuple(make_tuple(tp->dbterm.tpl)))
                        erl_exit(1, "Bad term in slot %d of ets table", j);
                    ++nk;
                    tp = tp->next;
                } while (tp != NULL);
                if (rp2->dt.tail.nkitems != nk)
                    erl_exit(1, "Invalid nkitems in slot %d of ets table", j);
                if (rp2->hvalue == INVALID_HASH) {
                    if (nk != 1)
                        erl_exit(1, "Invalid number of terms in pseudo-deleted"
                                 " RootDbTerm in slot %d of ets table", j);
                    nk = 0;
                } else {
                    ++nkeys;
                    nitems += nk;
                }
                if (HAS_NLHT(rp2)) {
                    nk2 = 0;
                    na = rp2->dt.tail.nactive;
                    for (i = 0; i < na; ++i) {
                        ntp = NESTED_BUCKET(rp2, i);
                        while (ntp != NULL) {
                            ++nk2;
                            ntp = ntp->onext;
                        }
                    }
                    if (nk2 != nk)
                        erl_exit(1, "Invalid number of terms in nested table"
                                 " of slot %d of ets table", j);
                }
            } else {
                if (!is_tuple(make_tuple(rp2->dt.dbterm.tpl)))
                    erl_exit(1, "Bad term in slot %d of ets table", j);
                if (rp2->hvalue != INVALID_HASH) {
                    if (!has_key(tb, rp1, key, hval)) {
                        hval = rp2->hvalue;
                        key = GETKEY(tb, rp2->dt.dbterm.tpl);
                        ASSERT(!is_header(key));
                        ++nkeys;
                    }
                    ++nitems;
                }
            }
            rp1 = rp2->next;
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
    db_take_nhash,
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
    RootDbTerm *rp2;
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
                rp2 = ROOT_PTR(*rpp);
                if (rp2->hvalue == INVALID_HASH) {
                    remove_root(tb, &rpp, NULL, 0);
                } else {
                    rpp = &rp2->next;
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

int
db_get_element_array(DbTable *tbl, Eterm key, int ndex,
                     Eterm *ret, int *num_ret)
{
    int ix, num;
    HashValue hval;
    erts_smp_rwmtx_t *lck;
    RootDbTerm *rp1, *rp2, *rp3;
    TrunkDbTerm *tp1, *tp2;
    DbTableNestedHash *tb = &tbl->nested;
    ASSERT(!IS_FIXED(tbl)); /* no support for fixed tables here */
    ASSERT(tb->common.status & (DB_BAG | DB_DUPLICATE_BAG));
    hval = MAKE_HASH(key);
    lck = RLOCK_HASH(tb, hval);
    ix = hash_to_ix(tb, hval);
    rp1 = BUCKET(tb, ix);
    while (rp1 != NULL) {
        rp2 = ROOT_PTR(rp1);
        if (has_key(tb, rp1, key, hval)) {
            /* !!! yield !!! */
            if (HAS_TAIL(rp1)) {
                ASSERT(rp2->hvalue != INVALID_HASH);
                tp1 = tp2 = TRUNK_PTR(rp2->dt.tail.trunk);
                do {
                    if (ndex > arityval(tp1->dbterm.tpl[0])) {
                        RUNLOCK_HASH(lck);
                        return DB_ERROR_BADITEM;
                    }
                    tp1 = tp1->next;
                } while (tp1 != NULL);
                num = 0;
                do {
                    if (num >= *num_ret)
                        goto done;
                    ret[num++] = tp2->dbterm.tpl[ndex];
                    tp2 = tp2->next;
                } while (tp2 != NULL);
            } else {
                do {
                    ASSERT(rp2->hvalue != INVALID_HASH);
                    if (ndex > arityval(rp2->dt.dbterm.tpl[0])) {
                        RUNLOCK_HASH(lck);
                        return DB_ERROR_BADITEM;
                    }
                    rp3 = rp2->next;
                    if (rp3 == NULL)
                        break;
                    rp2 = ROOT_PTR(rp3);
                } while (has_key(tb, rp3, key, hval));
                num = 0;
                do {
                    if (num >= *num_ret)
                        goto done;
                    rp2 = ROOT_PTR(rp1);
                    ret[num++] = rp2->dt.dbterm.tpl[ndex];
                    rp1 = rp2->next;
                } while (rp1 != rp3);
            }
            *num_ret = num;
            goto done;
        }
        rp1 = rp2->next;
    }
    RUNLOCK_HASH(lck);
    return DB_ERROR_BADKEY;

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
    int deleted, found, ix, nix;
    HashValue hval;
    RootDbTerm **rpp;
    RootDbTerm *rp1, *rp2;
    TrunkDbTerm *ntp;
    TrunkDbTerm **tpp, **ntpp;
    erts_smp_rwmtx_t *lck;
    DbTableNestedHash *tb = &tbl->nested;
    deleted = found = 0;
    hval = MAKE_HASH(key);
    lck = WLOCK_HASH(tb,hval);
    ix = hash_to_ix(tb, hval);
    rpp = &BUCKET(tb, ix);
    ASSERT(!IS_FIXED(tb));
    ASSERT((tb->common.status & DB_BAG));
    ASSERT(!tb->common.compress);
    while ((rp1 = *rpp) != NULL) {
        rp2 = ROOT_PTR(rp1);
        if (has_key(tb, rp1, key, hval)) {
            ASSERT(rp2->hvalue != INVALID_HASH);
            if (!HAS_TAIL(rp1)) {
                ++found;
                if ((arityval(rp2->dt.dbterm.tpl[0]) == 2) &&
                    EQ(value, rp2->dt.dbterm.tpl[2])) {
                    ++deleted;
                    *rpp = rp2->next;
                    free_root_dbterm(tb, rp1);
                    erts_smp_atomic_dec_nob(&tb->common.nitems);
                    /* Is there another RootDbTerm with same key? */
                    if ((*rpp != NULL) && has_key(tb, *rpp, key, hval))
                        ++found;
                    break;
                }
            } else if (HAS_NLHT(rp2)) {
                Eterm storage[3];
                hval = MAKE_HASH(TUPLE2(storage, key, value));
                nix = nested_hash_to_ix(rp2, hval);
                ntpp = &NESTED_BUCKET(rp2, nix);
                while ((ntp = *ntpp) != NULL) {
                    if ((arityval(ntp->dbterm.tpl[0]) == 2) &&
                        EQ(value, ntp->dbterm.tpl[2])) {
                        ++deleted;
                        if (remove_object_and_nested(tb, rpp, ntp, ntpp, 0))
                            erts_smp_atomic_dec_nob(&tb->nkeys);
                        erts_smp_atomic_dec_nob(&tb->common.nitems);
                        break;
                    }
                    ntpp = &ntp->onext;
                }
                break;
            } else {
                tpp = &rp2->dt.tail.trunk;
                do {
                    /* don't need the TRUNK_PTR macro */
                    if ((arityval((*tpp)->dbterm.tpl[0]) == 2) &&
                        EQ(value, (*tpp)->dbterm.tpl[2])) {
                        ++deleted;
                        if (remove_object_no_nested(tb, rpp, tpp, 0))
                            erts_smp_atomic_dec_nob(&tb->nkeys);
                        erts_smp_atomic_dec_nob(&tb->common.nitems);
                        break;
                    }
                    tpp = &(*tpp)->next;
                } while (*tpp != NULL);
                break;
            }
        }
        rpp = &rp2->next;
    }
    WUNLOCK_HASH(lck);
    if ((found > 0) && (found == deleted))
        erts_smp_atomic_dec_nob(&tb->nkeys);
    if (deleted) {
        try_shrink(tb);
        CHECK_TABLES();
    }
    return DB_ERROR_NONE;
}

void
db_calc_stats_nhash(DbTableNestedHash *tb, DbNestedHashStats *stats)
{
    int ix, kept_items, len, sq_sum, sum;
    RootDbTerm *rp1, *rp2;
    TrunkDbTerm *tp = NULL;
    erts_smp_rwmtx_t *lck;
    ix = kept_items = sum = sq_sum = 0;
    stats->min_chain_len = INT_MAX;
    stats->max_chain_len = 0;
    lck = RLOCK_HASH(tb, ix);
    do {
        len = 0;
        rp1 = BUCKET(tb, ix);
        rp2 = ROOT_PTR(rp1);
        if (HAS_TAIL(rp1))
            tp = TRUNK_PTR(rp2->dt.tail.trunk);
        /* !!! yield !!! */
        while (rp1 != NULL) {
            ++len;
            if (rp2->hvalue == INVALID_HASH)
                ++kept_items;
            if (HAS_TAIL(rp1)) {
                NEXT_DBTERM_2(rp1, rp2, tp);
            } else {
                NEXT_DBTERM_1(rp1, rp2, tp);
            }
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
    stats->kept_items = kept_items;
}
