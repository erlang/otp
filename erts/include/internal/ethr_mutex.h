/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
 * Description: Mutex, rwmutex and condition variable implementation
 * Author: Rickard Green
 */

/*
 * IMPORTANT note about ethr_cond_signal() and ethr_cond_broadcast()
 *
 * POSIX allow a call to `pthread_cond_signal' or `pthread_cond_broadcast'
 * even though the associated mutex/mutexes isn't/aren't locked by the
 * caller. We do not allow that by default in order to avoid a performance
 * penalty on some platforms.
 *
 * Mutexes and condition variables can, however, be initialized as POSIX
 * compliant. When initialized as such ethr_cond_signal(), and
 * ethr_cond_broadcast() are allowed to be called even though the associated
 * mutexes aren't locked. This will, however, incur a performance penalty on
 * some platforms.
 *
 * POSIX compliant mutexes and condition variables *need* to be used together.
 */

#ifndef ETHR_MUTEX_H__
#define ETHR_MUTEX_H__

#define ETHR_RWMUTEX_INITIALIZED 	0x99999999
#define ETHR_MUTEX_INITIALIZED		0x77777777
#define ETHR_COND_INITIALIZED		0x55555555

#if 0
#  define ETHR_MTX_HARD_DEBUG
#endif

#if 0
#  define ETHR_MTX_CHK_EXCL
#if 1
#    define ETHR_MTX_CHK_NON_EXCL
#endif
#endif

/* #define ETHR_DBG_WIN_MTX_WITH_PTHREADS */
#ifdef ETHR_DBG_WIN_MTX_WITH_PTHREADS
typedef pthread_mutex_t CRITICAL_SECTION;
int TryEnterCriticalSection(CRITICAL_SECTION *);
void EnterCriticalSection(CRITICAL_SECTION *);
void LeaveCriticalSection(CRITICAL_SECTION *);
#endif

#ifdef ETHR_MTX_HARD_DEBUG
#  ifdef __GNUC__
#    warning ETHR_MTX_HARD_DEBUG
#  endif
/*#  define ETHR_MTX_HARD_DEBUG_LFS*/
/*#  define ETHR_MTX_HARD_DEBUG_FENCE*/
/*#  define ETHR_MTX_HARD_DEBUG_Q*/
#  define ETHR_MTX_HARD_DEBUG_WSQ

#  if !defined(ETHR_MTX_HARD_DEBUG_WSQ) && defined(ETHR_MTX_HARD_DEBUG_Q)
#    define ETHR_MTX_HARD_DEBUG_WSQ
#  endif
#endif

#ifndef ETHR_INLINE_MTX_FUNC_NAME_
#  define ETHR_INLINE_MTX_FUNC_NAME_(X) X
#endif

#if defined(ETHR_USE_OWN_RWMTX_IMPL__) || defined(ETHR_USE_OWN_MTX_IMPL__)

#ifdef ETHR_DEBUG
#  ifndef ETHR_MTX_CHK_EXCL
#    define ETHR_MTX_CHK_EXCL
#  endif
#  ifndef ETHR_MTX_CHK_NON_EXCL
#    define ETHR_MTX_CHK_NON_EXCL
#  endif
#endif

#if 0
#  define ETHR_MTX_Q_LOCK_SPINLOCK__
#  define ETHR_MTX_QLOCK_TYPE__ ethr_spinlock_t
#elif defined(ETHR_PTHREADS) || defined(ETHR_OSE_THREADS)
#  define ETHR_MTX_Q_LOCK_PTHREAD_MUTEX__
#  define ETHR_MTX_QLOCK_TYPE__ pthread_mutex_t
#elif defined(ETHR_WIN32_THREADS)
#  define ETHR_MTX_Q_LOCK_CRITICAL_SECTION__
#  define ETHR_MTX_QLOCK_TYPE__ CRITICAL_SECTION
#else
#  error Need a qlock implementation
#endif

#define ETHR_RWMTX_W_FLG__		(((ethr_sint32_t) 1) << 31)
#define ETHR_RWMTX_W_WAIT_FLG__		(((ethr_sint32_t) 1) << 30)
#define ETHR_RWMTX_R_WAIT_FLG__		(((ethr_sint32_t) 1) << 29)

/* frequent read kind */
#define ETHR_RWMTX_R_FLG__		(((ethr_sint32_t) 1) << 28)
#define ETHR_RWMTX_R_ABRT_UNLCK_FLG__	(((ethr_sint32_t) 1) << 27)
#define ETHR_RWMTX_R_PEND_UNLCK_MASK__	(ETHR_RWMTX_R_ABRT_UNLCK_FLG__ - 1)

/* normal kind */
#define ETHR_RWMTX_RS_MASK__		(ETHR_RWMTX_R_WAIT_FLG__ - 1)

#define ETHR_RWMTX_WAIT_FLGS__ \
  (ETHR_RWMTX_W_WAIT_FLG__|ETHR_RWMTX_R_WAIT_FLG__)

#define ETHR_CND_WAIT_FLG__		ETHR_RWMTX_R_WAIT_FLG__

#ifdef ETHR_DEBUG
#define ETHR_DBG_CHK_UNUSED_FLG_BITS(V)			\
  ETHR_ASSERT(!((V) & ~(ETHR_RWMTX_W_FLG__		\
			| ETHR_RWMTX_W_WAIT_FLG__	\
			| ETHR_RWMTX_R_WAIT_FLG__	\
			| ETHR_RWMTX_RS_MASK__)))
#else
#define ETHR_DBG_CHK_UNUSED_FLG_BITS(V)
#endif

#define ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(MTX)		\
  ETHR_DBG_CHK_UNUSED_FLG_BITS(ethr_atomic32_read(&(MTX)->mtxb.flgs))

struct ethr_mutex_base_ {
#ifdef ETHR_MTX_HARD_DEBUG_FENCE
    long pre_fence;
#endif
    ethr_atomic32_t flgs;
    short aux_scnt;
    short main_scnt;
    ETHR_MTX_QLOCK_TYPE__ qlck;
    ethr_ts_event *q;
#ifdef ETHR_MTX_HARD_DEBUG_WSQ
    int ws;
#endif
#ifdef ETHR_MTX_CHK_EXCL
    ethr_atomic32_t exclusive;
#endif
#ifdef ETHR_MTX_CHK_NON_EXCL
    ethr_atomic32_t non_exclusive;
#endif
#ifdef ETHR_MTX_HARD_DEBUG_LFS
    ethr_atomic32_t hdbg_lfs;
#endif
};

#endif

typedef struct {
    int main_spincount;
    int aux_spincount;
    int posix_compliant;
} ethr_mutex_opt;

#define ETHR_MUTEX_OPT_DEFAULT_INITER {-1, -1, 0}

typedef struct {
    int main_spincount;
    int aux_spincount;
    int posix_compliant;
} ethr_cond_opt;

#define ETHR_COND_OPT_DEFAULT_INITER {-1, -1, 0}

#ifdef ETHR_USE_OWN_MTX_IMPL__

typedef struct ethr_mutex_ ethr_mutex;
struct ethr_mutex_ {
    struct ethr_mutex_base_ mtxb;
#ifdef ETHR_MTX_HARD_DEBUG_FENCE
    long post_fence;
#endif
#if ETHR_XCHK
    int initialized;
#endif
};

typedef struct ethr_cond_ ethr_cond;
struct ethr_cond_ {
#ifdef ETHR_MTX_HARD_DEBUG_FENCE
    struct {
	long pre_fence;
    } mtxb; /* mtxb allows us to use same macro as for mutex and rwmutex... */
#endif
    ETHR_MTX_QLOCK_TYPE__ qlck;
    ethr_ts_event *q;
    short aux_scnt;
    short main_scnt;
#ifdef ETHR_MTX_HARD_DEBUG_FENCE
    long post_fence;
#endif
#if ETHR_XCHK
    int initialized;
#endif
};

#elif (defined(ETHR_PTHREADS) || defined(ETHR_OSE_THREADS)) && !defined(ETHR_DBG_WIN_MTX_WITH_PTHREADS)

typedef struct ethr_mutex_ ethr_mutex;
struct ethr_mutex_ {
    pthread_mutex_t pt_mtx;
#if ETHR_XCHK
    int initialized;
#endif
};

typedef struct ethr_cond_ ethr_cond;
struct ethr_cond_ {
    pthread_cond_t pt_cnd;
#if ETHR_XCHK
    int initialized;
#endif
};

#elif defined(ETHR_WIN32_THREADS) || defined(ETHR_DBG_WIN_MTX_WITH_PTHREADS)
#  define ETHR_WIN_MUTEX__

typedef struct ethr_mutex_ ethr_mutex;
struct ethr_mutex_ {
    int posix_compliant;
    CRITICAL_SECTION cs;
    ethr_ts_event *wakeups;
    ethr_atomic32_t have_wakeups; /* only when posix compliant */
    ethr_atomic32_t locked;       /* only when posix compliant */
    ethr_spinlock_t lock;         /* only when posix compliant */
#if ETHR_XCHK
    int initialized;
#endif
};

typedef struct ethr_cond_ ethr_cond;
struct ethr_cond_ {
    int posix_compliant;
    CRITICAL_SECTION cs;
    ethr_ts_event *waiters;
    int spincount;
#if ETHR_XCHK
    int initialized;
#endif
};

#else
#  error "no mutex implementation"
#endif

int ethr_mutex_init_opt(ethr_mutex *, ethr_mutex_opt *);
int ethr_mutex_init(ethr_mutex *);
int ethr_mutex_destroy(ethr_mutex *);
#if !defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_MUTEX_IMPL__)
int ethr_mutex_trylock(ethr_mutex *);
void ethr_mutex_lock(ethr_mutex *);
void ethr_mutex_unlock(ethr_mutex *);
#endif
int ethr_cond_init_opt(ethr_cond *, ethr_cond_opt *);
int ethr_cond_init(ethr_cond *);
int ethr_cond_destroy(ethr_cond *);
void ethr_cond_signal(ethr_cond *);
void ethr_cond_broadcast(ethr_cond *);
int ethr_cond_wait(ethr_cond *, ethr_mutex *);

typedef enum {
    ETHR_RWMUTEX_TYPE_NORMAL,
    ETHR_RWMUTEX_TYPE_FREQUENT_READ,
    ETHR_RWMUTEX_TYPE_EXTREMELY_FREQUENT_READ
} ethr_rwmutex_type;

typedef enum {
    ETHR_RWMUTEX_LONG_LIVED,
    ETHR_RWMUTEX_SHORT_LIVED,
    ETHR_RWMUTEX_UNKNOWN_LIVED
} ethr_rwmutex_lived;

typedef struct {
    ethr_rwmutex_type type;
    ethr_rwmutex_lived lived;
    int main_spincount;
    int aux_spincount;
} ethr_rwmutex_opt;

#define ETHR_RWMUTEX_OPT_DEFAULT_INITER \
  {ETHR_RWMUTEX_TYPE_NORMAL, ETHR_RWMUTEX_UNKNOWN_LIVED, -1, -1}

#ifdef ETHR_USE_OWN_RWMTX_IMPL__

typedef union {
    struct {
	ethr_atomic32_t readers;
	int waiting_readers;
	int byte_offset;
	ethr_rwmutex_lived lived;
    } data;
    char align__[ETHR_CACHE_LINE_SIZE];
} ethr_rwmtx_readers_array__;

typedef struct ethr_rwmutex_ ethr_rwmutex;
struct ethr_rwmutex_ {
    struct ethr_mutex_base_ mtxb;
    ethr_rwmutex_type type;
    ethr_ts_event *rq_end;
    union {
	ethr_rwmtx_readers_array__ *ra;
	int rs;
    } tdata;
#ifdef ETHR_MTX_HARD_DEBUG_FENCE
    long post_fence;
#endif
#if ETHR_XCHK
    int initialized;
#endif
};

#else /* pthread_rwlock */

typedef struct ethr_rwmutex_ ethr_rwmutex;
struct ethr_rwmutex_ {
    pthread_rwlock_t pt_rwlock;
#if ETHR_XCHK
    int initialized;
#endif
};

#endif /* pthread_rwlock */

int ethr_rwmutex_set_reader_group(int);
int ethr_rwmutex_init_opt(ethr_rwmutex *, ethr_rwmutex_opt *);
int ethr_rwmutex_init(ethr_rwmutex *);
int ethr_rwmutex_destroy(ethr_rwmutex *);
#if defined(ETHR_USE_OWN_RWMTX_IMPL__) \
    || !defined(ETHR_TRY_INLINE_FUNCS) \
    || defined(ETHR_MUTEX_IMPL__)
int ethr_rwmutex_tryrlock(ethr_rwmutex *);
void ethr_rwmutex_rlock(ethr_rwmutex *);
void ethr_rwmutex_runlock(ethr_rwmutex *);
int ethr_rwmutex_tryrwlock(ethr_rwmutex *);
void ethr_rwmutex_rwlock(ethr_rwmutex *);
void ethr_rwmutex_rwunlock(ethr_rwmutex *);
#endif

#ifdef ETHR_MTX_HARD_DEBUG
#define ETHR_MTX_HARD_ASSERT(A) \
  ((void) ((A) ? 1 : ethr_assert_failed(__FILE__, __LINE__, __func__,#A)))
#else
#define ETHR_MTX_HARD_ASSERT(A) ((void) 1)
#endif

#ifdef ETHR_MTX_HARD_DEBUG_LFS
#  define ETHR_MTX_HARD_DEBUG_LFS_INIT(MTXB)				\
do {									\
    ethr_atomic32_init(&(MTXB)->hdbg_lfs, 0);				\
} while (0)
#  define ETHR_MTX_HARD_DEBUG_LFS_RLOCK(MTXB)				\
do {									\
    ethr_sint32_t val__;						\
    ETHR_COMPILER_BARRIER;						\
    val__ = ethr_atomic32_inc_read(&(MTXB)->hdbg_lfs);			\
    ETHR_MTX_HARD_ASSERT(val__ > 0);					\
} while (0)
#  define ETHR_MTX_HARD_DEBUG_LFS_TRYRLOCK(MTXB, RES)			\
do {									\
    ETHR_COMPILER_BARRIER;						\
    if ((RES) == 0)							\
	ETHR_MTX_HARD_DEBUG_LFS_RLOCK((MTXB));				\
    else								\
	ETHR_MTX_HARD_ASSERT((RES) == EBUSY);				\
} while (0)
#  define ETHR_MTX_HARD_DEBUG_LFS_RUNLOCK(MTXB)				\
do {									\
    ethr_sint32_t val__ = ethr_atomic32_dec_read(&(MTXB)->hdbg_lfs);	\
    ETHR_MTX_HARD_ASSERT(val__ >= 0);					\
    ETHR_COMPILER_BARRIER;						\
} while (0)
#  define ETHR_MTX_HARD_DEBUG_LFS_RWLOCK(MTXB)				\
do {									\
    ethr_sint32_t val__;						\
    ETHR_COMPILER_BARRIER;						\
    val__ = ethr_atomic32_dec_read(&(MTXB)->hdbg_lfs);			\
    ETHR_MTX_HARD_ASSERT(val__ == -1);					\
} while (0)
#  define ETHR_MTX_HARD_DEBUG_LFS_TRYRWLOCK(MTXB, RES)			\
do {									\
    ETHR_COMPILER_BARRIER;						\
    if ((RES) == 0)							\
	ETHR_MTX_HARD_DEBUG_LFS_RWLOCK((MTXB));				\
    else								\
	ETHR_MTX_HARD_ASSERT((RES) == EBUSY);				\
} while (0)
#  define ETHR_MTX_HARD_DEBUG_LFS_RWUNLOCK(MTXB)			\
do {									\
    ethr_sint32_t val__ = ethr_atomic32_inctest(&(MTXB)->hdbg_lfs);	\
    ETHR_MTX_HARD_ASSERT(val__ == 0);					\
    ETHR_COMPILER_BARRIER;						\
} while (0)
#else
#  define ETHR_MTX_HARD_DEBUG_LFS_INIT(MTXB)
#  define ETHR_MTX_HARD_DEBUG_LFS_RLOCK(MTXB)
#  define ETHR_MTX_HARD_DEBUG_LFS_TRYRLOCK(MTXB, RES)
#  define ETHR_MTX_HARD_DEBUG_LFS_RUNLOCK(MTXB)
#  define ETHR_MTX_HARD_DEBUG_LFS_RWLOCK(MTXB)
#  define ETHR_MTX_HARD_DEBUG_LFS_TRYRWLOCK(MTXB, RES)
#  define ETHR_MTX_HARD_DEBUG_LFS_RWUNLOCK(MTXB)
#endif

#ifdef ETHR_MTX_HARD_DEBUG_FENCE

#if ETHR_SIZEOF_PTR == 8
#  define ETHR_MTX_HARD_DEBUG_PRE_FENCE		0xdeadbeefdeadbeefL
#  define ETHR_MTX_HARD_DEBUG_POST_FENCE	0xdeaddeaddeaddeadL
#else
#  define ETHR_MTX_HARD_DEBUG_PRE_FENCE		0xdeaddeadL
#  define ETHR_MTX_HARD_DEBUG_POST_FENCE	0xdeaddeadL
#endif

#define ETHR_MTX_HARD_DEBUG_FENCE_CHK(X) \
do { \
    ETHR_COMPILER_BARRIER; \
    ETHR_MTX_HARD_ASSERT((X)->mtxb.pre_fence == ETHR_MTX_HARD_DEBUG_PRE_FENCE);\
    ETHR_MTX_HARD_ASSERT((X)->post_fence == ETHR_MTX_HARD_DEBUG_POST_FENCE); \
    ETHR_COMPILER_BARRIER; \
} while (0)
#define ETHR_MTX_HARD_DEBUG_FENCE_INIT(X) \
do { \
    (X)->mtxb.pre_fence = ETHR_MTX_HARD_DEBUG_PRE_FENCE; \
    (X)->post_fence = ETHR_MTX_HARD_DEBUG_POST_FENCE; \
} while (0)
#else
#define ETHR_MTX_HARD_DEBUG_FENCE_CHK(X)
#define ETHR_MTX_HARD_DEBUG_FENCE_INIT(X)
#endif

#ifdef ETHR_MTX_CHK_EXCL

#if !defined(ETHR_DEBUG) && defined(__GNUC__)
#warning "check exclusive is enabled"
#endif

#  define ETHR_MTX_CHK_EXCL_INIT__(MTXB)		\
    ethr_atomic32_init(&(MTXB)->exclusive, 0)

#  define ETHR_MTX_CHK_EXCL_IS_EXCL(MTXB)		\
do {							\
    ETHR_COMPILER_BARRIER;				\
    if (!ethr_atomic32_read(&(MTXB)->exclusive))	\
	ethr_assert_failed(__FILE__, __LINE__, __func__,\
			   "is exclusive");		\
    ETHR_COMPILER_BARRIER;				\
} while (0)
#  define ETHR_MTX_CHK_EXCL_IS_NOT_EXCL(MTXB)		\
do {							\
    ETHR_COMPILER_BARRIER;				\
    if (ethr_atomic32_read(&(MTXB)->exclusive))		\
	ethr_assert_failed(__FILE__, __LINE__, __func__,\
			   "is not exclusive");		\
    ETHR_COMPILER_BARRIER;				\
} while (0)
#  define ETHR_MTX_CHK_EXCL_SET_EXCL(MTXB)		\
do {							\
    ETHR_MTX_CHK_EXCL_IS_NOT_EXCL((MTXB));		\
    ethr_atomic32_set(&(MTXB)->exclusive, 1);		\
    ETHR_COMPILER_BARRIER;				\
} while (0)
#  define ETHR_MTX_CHK_EXCL_UNSET_EXCL(MTXB)		\
do {							\
    ETHR_MTX_CHK_EXCL_IS_EXCL((MTXB));			\
    ethr_atomic32_set(&(MTXB)->exclusive, 0);		\
    ETHR_COMPILER_BARRIER;				\
} while (0)

#ifdef ETHR_MTX_CHK_NON_EXCL

#if !defined(ETHR_DEBUG) && defined(__GNUC__)
#warning "check non-exclusive is enabled"
#endif

#    define ETHR_MTX_CHK_NON_EXCL_INIT__(MTXB)		\
    ethr_atomic32_init(&(MTXB)->non_exclusive, 0)
#    define ETHR_MTX_CHK_EXCL_IS_NON_EXCL(MTXB)		\
do {							\
    ETHR_COMPILER_BARRIER;				\
    if (!ethr_atomic32_read(&(MTXB)->non_exclusive))	\
	ethr_assert_failed(__FILE__, __LINE__, __func__,\
			   "is non-exclusive");		\
    ETHR_COMPILER_BARRIER;				\
} while (0)
#    define ETHR_MTX_CHK_EXCL_IS_NOT_NON_EXCL(MTXB)	\
do {							\
    ETHR_COMPILER_BARRIER;				\
    if (ethr_atomic32_read(&(MTXB)->non_exclusive))	\
	ethr_assert_failed(__FILE__, __LINE__, __func__,\
			   "is not non-exclusive");	\
    ETHR_COMPILER_BARRIER;				\
} while (0)
#    define ETHR_MTX_CHK_EXCL_SET_NON_EXCL(MTXB)	\
do {							\
    ETHR_COMPILER_BARRIER;				\
    ethr_atomic32_inc(&(MTXB)->non_exclusive);		\
    ETHR_COMPILER_BARRIER;				\
} while (0)
#    define ETHR_MTX_CHK_EXCL_SET_NON_EXCL_NO(MTXB, NO)	\
do {							\
    ETHR_COMPILER_BARRIER;				\
    ethr_atomic32_add(&(MTXB)->non_exclusive, (NO));	\
    ETHR_COMPILER_BARRIER;				\
} while (0)
#    define ETHR_MTX_CHK_EXCL_UNSET_NON_EXCL(MTXB)	\
do {							\
    ETHR_COMPILER_BARRIER;				\
    ethr_atomic32_dec(&(MTXB)->non_exclusive);		\
    ETHR_COMPILER_BARRIER;				\
} while (0)
#else
#    define ETHR_MTX_CHK_NON_EXCL_INIT__(MTXB)
#    define ETHR_MTX_CHK_EXCL_IS_NON_EXCL(MTXB)
#    define ETHR_MTX_CHK_EXCL_IS_NOT_NON_EXCL(MTXB)
#    define ETHR_MTX_CHK_EXCL_SET_NON_EXCL_NO(MTXB, NO)
#    define ETHR_MTX_CHK_EXCL_SET_NON_EXCL(MTXB)
#    define ETHR_MTX_CHK_EXCL_UNSET_NON_EXCL(MTXB)
#endif

#else
#  define ETHR_MTX_CHK_EXCL_INIT__(MTXB)
#  define ETHR_MTX_CHK_EXCL_IS_EXCL(MTXB)
#  define ETHR_MTX_CHK_EXCL_IS_NOT_EXCL(MTXB)
#  define ETHR_MTX_CHK_EXCL_SET_EXCL(MTXB)
#  define ETHR_MTX_CHK_EXCL_UNSET_EXCL(MTXB)
#  define ETHR_MTX_CHK_NON_EXCL_INIT__(MTXB)
#  define ETHR_MTX_CHK_EXCL_IS_NON_EXCL(MTXB)
#  define ETHR_MTX_CHK_EXCL_IS_NOT_NON_EXCL(MTXB)
#  define ETHR_MTX_CHK_EXCL_SET_NON_EXCL_NO(MTXB, NO)
#  define ETHR_MTX_CHK_EXCL_SET_NON_EXCL(MTXB)
#  define ETHR_MTX_CHK_EXCL_UNSET_NON_EXCL(MTXB)
#endif

#  define ETHR_MTX_CHK_EXCL_INIT(MTXB)			\
do {							\
    ETHR_MTX_CHK_EXCL_INIT__((MTXB));			\
    ETHR_MTX_CHK_NON_EXCL_INIT__((MTXB));		\
} while (0)


#ifdef ETHR_USE_OWN_MTX_IMPL__

#define ETHR_MTX_DEFAULT_MAIN_SPINCOUNT_MAX 2000
#define ETHR_MTX_DEFAULT_MAIN_SPINCOUNT_BASE 800
#define ETHR_MTX_DEFAULT_MAIN_SPINCOUNT_INC 50
#define ETHR_MTX_DEFAULT_AUX_SPINCOUNT 50

#define ETHR_CND_DEFAULT_MAIN_SPINCOUNT 0
#define ETHR_CND_DEFAULT_AUX_SPINCOUNT 0

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_MUTEX_IMPL__)

void ethr_mutex_lock_wait__(ethr_mutex *, ethr_sint32_t);
void ethr_mutex_unlock_wake__(ethr_mutex *, ethr_sint32_t);

static ETHR_INLINE int
ETHR_INLINE_MTX_FUNC_NAME_(ethr_mutex_trylock)(ethr_mutex *mtx)
{
    ethr_sint32_t act;
    int res;
    ETHR_MTX_HARD_DEBUG_FENCE_CHK(mtx);
    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(mtx);

    act = ethr_atomic32_cmpxchg_acqb(&mtx->mtxb.flgs, ETHR_RWMTX_W_FLG__, 0);
    res = (act == 0) ? 0 : EBUSY;

#ifdef ETHR_MTX_CHK_EXCL
    if (res == 0)
	ETHR_MTX_CHK_EXCL_SET_EXCL(&mtx->mtxb);
#endif

    ETHR_MTX_HARD_DEBUG_LFS_TRYRWLOCK(&mtx->mtxb, res);
    ETHR_MTX_HARD_DEBUG_FENCE_CHK(mtx);
    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(mtx);

    ETHR_COMPILER_BARRIER;
    return res;
}

static ETHR_INLINE void
ETHR_INLINE_MTX_FUNC_NAME_(ethr_mutex_lock)(ethr_mutex *mtx)
{
    ethr_sint32_t act;
    ETHR_MTX_HARD_DEBUG_FENCE_CHK(mtx);
    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(mtx);

    act = ethr_atomic32_cmpxchg_acqb(&mtx->mtxb.flgs, ETHR_RWMTX_W_FLG__, 0);
    if (act != 0)
	ethr_mutex_lock_wait__(mtx, act);

    ETHR_MTX_CHK_EXCL_SET_EXCL(&mtx->mtxb);

    ETHR_MTX_HARD_DEBUG_LFS_RWLOCK(&mtx->mtxb);
    ETHR_MTX_HARD_DEBUG_FENCE_CHK(mtx);
    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(mtx);

    ETHR_COMPILER_BARRIER;
}

static ETHR_INLINE void
ETHR_INLINE_MTX_FUNC_NAME_(ethr_mutex_unlock)(ethr_mutex *mtx)
{
    ethr_sint32_t act;
    ETHR_COMPILER_BARRIER;
    ETHR_MTX_HARD_DEBUG_FENCE_CHK(mtx);
    ETHR_MTX_HARD_DEBUG_LFS_RWUNLOCK(&mtx->mtxb);
    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(mtx);

    ETHR_MTX_CHK_EXCL_UNSET_EXCL(&mtx->mtxb);

    act = ethr_atomic32_cmpxchg_relb(&mtx->mtxb.flgs, 0, ETHR_RWMTX_W_FLG__);
    if (act != ETHR_RWMTX_W_FLG__)
	ethr_mutex_unlock_wake__(mtx, act);

    ETHR_MTX_HARD_DEBUG_FENCE_CHK(mtx);
    ETHR_MTX_DBG_CHK_UNUSED_FLG_BITS(mtx);
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#elif (defined(ETHR_PTHREADS) || defined(ETHR_OSE_THREADS)) && !defined(ETHR_DBG_WIN_MTX_WITH_PTHREADS)

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_MUTEX_IMPL__)

static ETHR_INLINE int
ETHR_INLINE_MTX_FUNC_NAME_(ethr_mutex_trylock)(ethr_mutex *mtx)
{
    int res;
    res = pthread_mutex_trylock(&mtx->pt_mtx);
    if (res != 0 && res != EBUSY)
	ETHR_FATAL_ERROR__(res);
    return res;
}

static ETHR_INLINE void
ETHR_INLINE_MTX_FUNC_NAME_(ethr_mutex_lock)(ethr_mutex *mtx)
{
    int res = pthread_mutex_lock(&mtx->pt_mtx);
    if (res != 0)
	ETHR_FATAL_ERROR__(res);
}

static ETHR_INLINE void
ETHR_INLINE_MTX_FUNC_NAME_(ethr_mutex_unlock)(ethr_mutex *mtx)
{
    int res = pthread_mutex_unlock(&mtx->pt_mtx);
    if (res != 0)
	ETHR_FATAL_ERROR__(res);
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#elif defined(ETHR_WIN32_THREADS) || defined(ETHR_DBG_WIN_MTX_WITH_PTHREADS)

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_MUTEX_IMPL__)

static ETHR_INLINE int
ETHR_INLINE_MTX_FUNC_NAME_(ethr_mutex_trylock)(ethr_mutex *mtx)
{
    if (!TryEnterCriticalSection(&mtx->cs))
	return EBUSY;
    if (mtx->posix_compliant)
	ethr_atomic32_set(&mtx->locked, 1);
    return 0;
}

static ETHR_INLINE void
ETHR_INLINE_MTX_FUNC_NAME_(ethr_mutex_lock)(ethr_mutex *mtx)
{
    EnterCriticalSection(&mtx->cs);
    if (mtx->posix_compliant)
	ethr_atomic32_set(&mtx->locked, 1);
}

void ethr_mutex_cond_wakeup__(ethr_mutex *mtx);

static ETHR_INLINE void
ETHR_INLINE_MTX_FUNC_NAME_(ethr_mutex_unlock)(ethr_mutex *mtx)
{
    if (mtx->posix_compliant) {
	ethr_atomic32_set_mb(&mtx->locked, 0);
	if (ethr_atomic32_read_acqb(&mtx->have_wakeups))
	    goto cond_wakeup;
	else
	    goto leave_cs;
    }

    if (mtx->wakeups) {
    cond_wakeup:
	ethr_mutex_cond_wakeup__(mtx);
    }
    else {
    leave_cs:
	LeaveCriticalSection(&mtx->cs);
    }
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif

#ifdef ETHR_USE_OWN_RWMTX_IMPL__

#define ETHR_RWMTX_DEFAULT_MAIN_SPINCOUNT_MAX 2000
#define ETHR_RWMTX_DEFAULT_MAIN_SPINCOUNT_BASE 800
#define ETHR_RWMTX_DEFAULT_MAIN_SPINCOUNT_INC 50
#define ETHR_RWMTX_DEFAULT_AUX_SPINCOUNT 50

#else /* pthread_rwlock */

#if defined(ETHR_TRY_INLINE_FUNCS) || defined(ETHR_MUTEX_IMPL__)

static ETHR_INLINE int
ETHR_INLINE_MTX_FUNC_NAME_(ethr_rwmutex_tryrlock)(ethr_rwmutex *rwmtx)
{
    int res = pthread_rwlock_tryrdlock(&rwmtx->pt_rwlock);
    if (res != 0 && res != EBUSY)
	ETHR_FATAL_ERROR__(res);
    return res;
}

static ETHR_INLINE void
ETHR_INLINE_MTX_FUNC_NAME_(ethr_rwmutex_rlock)(ethr_rwmutex *rwmtx)
{
    int res = pthread_rwlock_rdlock(&rwmtx->pt_rwlock);
    if (res != 0)
	ETHR_FATAL_ERROR__(res);
}

static ETHR_INLINE void
ETHR_INLINE_MTX_FUNC_NAME_(ethr_rwmutex_runlock)(ethr_rwmutex *rwmtx)
{
    int res = pthread_rwlock_unlock(&rwmtx->pt_rwlock);
    if (res != 0)
	ETHR_FATAL_ERROR__(res);
}

static ETHR_INLINE int
ETHR_INLINE_MTX_FUNC_NAME_(ethr_rwmutex_tryrwlock)(ethr_rwmutex *rwmtx)
{
    int res = pthread_rwlock_trywrlock(&rwmtx->pt_rwlock);
    if (res != 0 && res != EBUSY)
	ETHR_FATAL_ERROR__(res);
    return res;
}

static ETHR_INLINE void
ETHR_INLINE_MTX_FUNC_NAME_(ethr_rwmutex_rwlock)(ethr_rwmutex *rwmtx)
{
    int res = pthread_rwlock_wrlock(&rwmtx->pt_rwlock);
    if (res != 0)
	ETHR_FATAL_ERROR__(res);
}

static ETHR_INLINE void
ETHR_INLINE_MTX_FUNC_NAME_(ethr_rwmutex_rwunlock)(ethr_rwmutex *rwmtx)
{
    int res = pthread_rwlock_unlock(&rwmtx->pt_rwlock);
    if (res != 0)
	ETHR_FATAL_ERROR__(res);
}

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* pthread_rwlock */

int ethr_mutex_lib_init(int);
int ethr_mutex_lib_late_init(int, int);

#endif /* #ifndef ETHR_MUTEX_H__ */
